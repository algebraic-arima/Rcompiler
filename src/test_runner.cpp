#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cstdlib>
#include <sys/stat.h>

#include "lexer.h"
#include "parser.h"

namespace fs = std::filesystem;

// 查找目录及其子目录中的所有 .rx 文件
std::vector<std::string> find_rx_files(const std::string& directory) {
    std::vector<std::string> rx_files;

    for (const auto& entry : fs::recursive_directory_iterator(directory)) {
        if (entry.is_regular_file() && entry.path().extension() == ".rx") {
            rx_files.push_back(entry.path().string());
        }
    }

    return rx_files;
}

// 从文件读取内容
void read_from_file(std::string& input, const std::string& filename) {
    std::ifstream fin(filename, std::ios::in);
    if (!fin) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    std::ostringstream oss;
    oss << fin.rdbuf();
    input = oss.str();
}

// 运行单个测试文件
bool run_test(const std::string& rx_file) {
    std::cout << "Running test: " << rx_file << std::endl;

    try {
        // 读取文件内容
        std::string input;
        read_from_file(input, rx_file);

        // 词法分析
        Lexer lexer(input);
        std::vector<Token> tokens = lexer.tokenize_all();
        tokens.push_back(Token(TokenKind::Eof, "", 0));

        // 语法分析
        Parser parser(tokens);
        auto ast = parser.parse_program();

        // 输出 AST
        ast->dump(0);

        //std::cout << "✓ PASSED" << std::endl;
        return true;

    } catch (const std::exception& e) {
        //std::cout << "✗ FAILED: " << e.what() << std::endl;
        return false;
    }
}

int main() {
    // 测试目录
    std::string test_dir = "/mnt/c/Users/hejia/Desktop/Rcompiler/test_case";

    // 查找所有 .rx 文件
    std::vector<std::string> rx_files = find_rx_files(test_dir);

    if (rx_files.empty()) {
        std::cout << "No .rx files found in the test directory." << std::endl;
        return 0;
    }

    std::cout << "Found " << rx_files.size() << " test files." << std::endl;

    // 运行测试
    int passed = 0;
    int failed = 0;

    for (const auto& rx_file : rx_files) {
        if (run_test(rx_file)) {
            passed++;
        } else {
            failed++;
        }
        std::cout << std::string(50, '-') << std::endl;
    }

    // 输出总结
    std::cout << "Test Summary: " << passed << " passed, " << failed 
              << " failed out of " << rx_files.size() << " tests." << std::endl;

    return failed > 0 ? 1 : 0;
}
