// src/ir_test.cpp
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cstdlib>
#include <cstdio>
#include <memory>
#include <array>
#include <sstream>

namespace fs = std::filesystem;

// 执行命令并捕获输出
std::pair<int, std::string>           execute_command(const std::string& cmd, const std::string& input = "") {
    std::array<char, 128> buffer;
    std::string result;

#ifdef _WIN32
    std::string temp_cmd = cmd + " > temp_output.txt 2>&1";
    if (!input.empty()) {
        // 将输入写入临时文件
        std::ofstream temp_in("temp_input.txt");
        temp_in << input;
        temp_in.close();
        temp_cmd = "type temp_input.txt | " + temp_cmd;
    }

    int ret = system(temp_cmd.c_str());

    // 读取输出
    std::ifstream output_file("temp_output.txt");
    if (output_file.is_open()) {
        std::string line;
        while (std::getline(output_file, line)) {
            result += line + "\n";
        }
        output_file.close();
        std::remove("temp_output.txt");
    }

    if (!input.empty()) {
        std::remove("temp_input.txt");
    }

    return {ret, result};
#else
    std::string full_cmd;
    if (!input.empty()) {
        // 将输入写入临时文件
        std::ofstream temp_in("/tmp/temp_input.txt");
        temp_in << input;
        temp_in.close();
        full_cmd = "cat /tmp/temp_input.txt | " + cmd + " > /tmp/temp_output.txt 2>&1";
    } else {
        full_cmd = cmd + " > /tmp/temp_output.txt 2>&1";
    }

    int ret = system(full_cmd.c_str());

    // 读取输出
    std::ifstream output_file("/tmp/temp_output.txt");
    if (output_file.is_open()) {
        std::string line;
        while (std::getline(output_file, line)) {
            result += line + "\n";
        }
        output_file.close();
        std::remove("/tmp/temp_output.txt");
    }

    if (!input.empty()) {
        std::remove("/tmp/temp_input.txt");
    }

    return {ret, result};
#endif
}

// 读取文件内容
std::string read_file_content(const fs::path& file_path) {
    std::ifstream file(file_path);
    if (!file.is_open()) {
        return "";
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

// 运行 IR 测试（编译 -> llc -> clang -> 运行并比对输出）。
// 如遇未实现特性，可抛异常；外层可根据 STRICT_IR 决定是否视为失败。
bool run_ir_test(const fs::path& test_file, const fs::path& compiler_path) {
    // 获取测试文件的基本名称（不带扩展名）
    std::string base_name = test_file.stem().string();

    // 构建相关文件路径
    fs::path in_file = test_file.parent_path() / (base_name + ".in");
    fs::path out_file = test_file.parent_path() / (base_name + ".out");
    fs::path ir_file = test_file.parent_path() / (base_name + ".ll");
    fs::path exe_file = test_file.parent_path() / (base_name + 
#ifdef _WIN32
        ".exe"
#else
        ""
#endif
    );

    // 调试期保留 .ll/.s/可执行，方便排查 IR；如需清理可手动删除。

    std::cout << "Running test: " << base_name << std::endl;

    fs::path asm_file = test_file.parent_path() / (base_name + ".s");

    // 1. 编译 Rust 代码为 LLVM IR
    std::cout << "  Compiling to LLVM IR..." << std::endl;
    auto quote = [](const fs::path &p) {
        const std::string s = p.string();
        if (s.find(' ') != std::string::npos || s.find('\"') != std::string::npos) {
            return std::string("\"") + s + "\"";
        }
        return s;
    };

    std::string compile_cmd = quote(compiler_path) + " " + quote(test_file) + " --emit-llvm";
    auto [compile_ret, compile_err] = execute_command(compile_cmd);

    if (compile_ret != 0) {
        std::cerr << "[ir_test] compile_ret=" << compile_ret << " output:\n" << compile_err << std::endl;
        throw std::runtime_error("Compilation failed or unsupported IR feature: " + compile_err);
    }

    // 2. 将 LLVM IR 编译为可执行文件
    std::cout << "  Compiling IR to executable..." << std::endl;
    std::string llc_cmd = std::string("llc ") + quote(ir_file);
    auto [llc_ret, llc_err] = execute_command(llc_cmd);

    if (llc_ret != 0) {
        throw std::runtime_error("llc failed (likely unsupported IR): " + llc_err);
    }

    asm_file = test_file.parent_path() / (base_name + ".s");

    // 3. 汇编并链接为可执行文件
    std::cout << "  Assembling and linking..." << std::endl;
    #ifdef _WIN32
    std::string clang_cmd = std::string("clang ") + quote(asm_file) + " -o " + quote(exe_file);
    #else
    std::string clang_cmd = std::string("clang -no-pie ") + quote(asm_file) + " -o " + quote(exe_file);
    #endif
    auto [clang_ret, clang_err] = execute_command(clang_cmd);

    if (clang_ret != 0) {
        throw std::runtime_error("clang failed (likely unsupported IR): " + clang_err);
    }

    // 4. 运行程序并捕获输出
    std::cout << "  Running program..." << std::endl;
    std::string input_data = "";
    if (fs::exists(in_file)) {
        input_data = read_file_content(in_file);
    }

    std::string run_cmd = quote(exe_file);
    auto [run_ret, run_output] = execute_command(run_cmd, input_data);

    // 5. 比较输出
    if (fs::exists(out_file)) {
        std::string expected_output = read_file_content(out_file);

        if (run_output == expected_output) {
            std::cout << "  ✓ Test passed" << std::endl;
            return true;
        } else {
            std::cout << "  ✗ Test failed" << std::endl;
            std::cout << "  Expected:\n" << expected_output << std::endl;
            std::cout << "  Got:\n" << run_output << std::endl;
            return false;
        }
    } else {
        std::cout << "  Warning: No .out file to compare against" << std::endl;
        std::cout << "  Output:\n" << run_output << std::endl;
        return true;
    }
}

int main(int argc, char* argv[]) {
    // 只跑 test_case/semantic-2 的用例，要求完整 IR 流程
    fs::path exe_dir = fs::absolute(argv[0]).parent_path();

    // 尝试找到编译器可执行文件
    std::vector<fs::path> compiler_candidates = {
        exe_dir / "compiler",
        exe_dir / "build" / "compiler",
        exe_dir.parent_path() / "build" / "compiler",
        fs::current_path() / "build" / "compiler",
        fs::current_path() / "compiler"
    };
    fs::path compiler_path;
    for (const auto &c : compiler_candidates) {
        std::error_code ec;
        if (fs::exists(c, ec)) {
            compiler_path = c;
            break;
        }
    }

    if (compiler_path.empty()) {
        std::cerr << "Cannot find compiler binary (tried ./compiler, ./build/compiler, ../build/compiler)" << std::endl;
        return 1;
    }

    auto collect_tests = [&](const fs::path &root, std::vector<fs::path> &out) {
        std::error_code ec;
        if (!fs::exists(root, ec)) return;
        for (const auto &entry : fs::recursive_directory_iterator(root, ec)) {
            if (entry.is_regular_file(ec) && entry.path().extension() == ".rx") {
                out.push_back(entry.path());
            }
        }
    };

    std::vector<fs::path> test_files;
    std::vector<fs::path> root_candidates;
    for (const std::string &name : {"semantic-2"}) {
        root_candidates.push_back(fs::current_path() / "test_case" / name);
        root_candidates.push_back(exe_dir / "test_case" / name);
        root_candidates.push_back(exe_dir.parent_path() / "test_case" / name);
    }
    for (const auto &root : root_candidates) {
        collect_tests(root, test_files);
    }

    if (test_files.empty()) {
        std::cout << "No .rx test files found under test_case/semantic-2 (checked relative to cwd and exe dir)" << std::endl;
        return 0;
    }

    std::cout << "Found " << test_files.size() << " test files" << std::endl;

    int passed = 0;
    int total = test_files.size();

    for (const auto& test_file : test_files) {
        try {
            if (run_ir_test(test_file, compiler_path)) {
                passed++;
            }
        } catch (const std::exception& e) {
            std::cerr << "  IR Error: " << e.what() << std::endl;
        }
        std::cout << std::endl;  // 空行分隔测试结果
    }

    std::cout << "Results: " << passed << "/" << total << " tests passed" << std::endl;

    return (passed == total) ? 0 : 1;
}
