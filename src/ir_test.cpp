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
#include <algorithm>

namespace fs = std::filesystem;

// Execute command and capture output
std::pair<int, std::string> execute_command(const std::string& cmd, const std::string& input = "", int timeoutSeconds = 0) {
    std::array<char, 128> buffer;
    std::string result;

#ifdef _WIN32
    std::string temp_cmd = cmd + " > temp_output.txt 2>&1";
    if (!input.empty()) {
        std::ofstream temp_in("temp_input.txt");
        temp_in << input;
        temp_in.close();
        temp_cmd = "type temp_input.txt | " + temp_cmd;
    }

    int ret = system(temp_cmd.c_str());

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
    std::string base_cmd = cmd;
    if (timeoutSeconds > 0) {
        base_cmd = "timeout -s KILL " + std::to_string(timeoutSeconds) + " " + cmd;
    }

    std::string full_cmd;
    if (!input.empty()) {
        std::ofstream temp_in("/tmp/temp_input.txt");
        temp_in << input;
        temp_in.close();
        full_cmd = "cat /tmp/temp_input.txt | " + base_cmd + " > /tmp/temp_output.txt 2>&1";
    } else {
        full_cmd = base_cmd + " > /tmp/temp_output.txt 2>&1";
    }

    int ret = system(full_cmd.c_str());

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

// Read entire file
std::string read_file_content(const fs::path& file_path) {
    std::ifstream file(file_path);
    if (!file.is_open()) {
        return "";
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

// Run IR test: compile -> llc -> clang -> run and compare output.
bool run_ir_test(const fs::path& test_file, const fs::path& compiler_path) {
    std::string base_name = test_file.stem().string();

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

    std::cout << "Running test: " << base_name << std::endl;

    fs::path asm_file = test_file.parent_path() / (base_name + ".s");

    // 1. Compile to LLVM IR
    std::cout << "  Compiling to LLVM IR..." << std::endl;
    auto quote = [](const fs::path &p) {
        const std::string s = p.string();
        if (s.find(' ') != std::string::npos || s.find('"') != std::string::npos) {
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

    // 2. Compile LLVM IR to executable
    std::cout << "  Compiling IR to executable..." << std::endl;
    std::string llc_cmd = std::string("llc ") + quote(ir_file);
    auto [llc_ret, llc_err] = execute_command(llc_cmd);

    if (llc_ret != 0) {
        throw std::runtime_error("llc failed (likely unsupported IR): " + llc_err);
    }

    asm_file = test_file.parent_path() / (base_name + ".s");

    // 3. Assemble and link
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

    // 4. Run program and capture output
    std::cout << "  Running program..." << std::endl;
    std::string input_data = "";
    if (fs::exists(in_file)) {
        input_data = read_file_content(in_file);
    }

    constexpr int kRunTimeoutSeconds = 8;
    std::string run_cmd = quote(exe_file);
    auto [run_ret, run_output] = execute_command(run_cmd, input_data, kRunTimeoutSeconds);

    // 5. Compare output
    if (fs::exists(out_file)) {
        std::string expected_output = read_file_content(out_file);
        auto trim_newlines = [](std::string s) {
            while (!s.empty() && (s.back() == '\n' || s.back() == '\r')) s.pop_back();
            return s;
        };
        std::string expected_norm = trim_newlines(expected_output);
        std::string got_norm = trim_newlines(run_output);
        auto strip_all_newlines = [](std::string s) {
            s.erase(std::remove(s.begin(), s.end(), '\n'), s.end());
            s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());
            return s;
        };

        if (expected_output == run_output || expected_norm == got_norm ||
            strip_all_newlines(expected_norm) == strip_all_newlines(got_norm)) {
            std::cout << "  \u2713 Test passed" << std::endl;
            return true;
        } else {
            std::cout << "  \u2717 Test failed" << std::endl;
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
    fs::path exe_dir = fs::absolute(argv[0]).parent_path();

    std::vector<std::string> filters;
    for (int i = 1; i < argc; ++i) {
        if (argv[i]) filters.emplace_back(argv[i]);
    }

    auto should_run = [&](const fs::path &p) {
        if (filters.empty()) return true;
        std::string s = p.string();
        for (const auto &f : filters) {
            if (s.find(f) != std::string::npos) return true;
        }
        return false;
    };

    std::vector<fs::path> compiler_candidates = {
        exe_dir / "compiler",
        exe_dir / "code",
        exe_dir / "build" / "compiler",
        exe_dir / "build" / "code",
        exe_dir.parent_path() / "build" / "compiler",
        exe_dir.parent_path() / "build" / "code",
        fs::current_path() / "build" / "compiler",
        fs::current_path() / "build" / "code",
        fs::current_path() / "compiler",
        fs::current_path() / "code"
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

    std::cout << "Using compiler: " << compiler_path << std::endl;

    auto collect_tests = [&](const fs::path &root, std::vector<fs::path> &out) {
        std::error_code ec;
        if (!fs::exists(root, ec)) return;
        for (const auto &entry : fs::recursive_directory_iterator(root, ec)) {
            if (entry.is_regular_file(ec) && entry.path().extension() == ".rx") {
                out.push_back(entry.path().lexically_normal());
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

    std::sort(test_files.begin(), test_files.end(), [](const fs::path &a, const fs::path &b) {
        return a.string() < b.string();
    });
    test_files.erase(std::unique(test_files.begin(), test_files.end(), [](const fs::path &a, const fs::path &b) {
        return a.string() == b.string();
    }), test_files.end());

    if (test_files.empty()) {
        std::cout << "No .rx test files found under test_case/semantic-2 (checked relative to cwd and exe dir)" << std::endl;
        return 0;
    }

    std::cout << "Found " << test_files.size() << " test files" << std::endl;

    bool all_passed = true;
    for (const auto &test_file : test_files) {
        if (!should_run(test_file)) continue;
        try {
            if (!run_ir_test(test_file, compiler_path)) {
                all_passed = false;
            }
        } catch (const std::exception &e) {
            std::cerr << "  Exception: " << e.what() << std::endl;
            all_passed = false;
        }
    }

    return all_passed ? 0 : 1;
}
