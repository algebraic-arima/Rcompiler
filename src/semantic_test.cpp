#include "semantic.h"
#include "ast.h"
#include "parser.h"
#include "lexer.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <filesystem>
#include <cstdlib>
#include <sys/stat.h>

namespace fs = std::filesystem;

bool should_run_test(const std::string& path) {
    static const char* filter = std::getenv("TEST_FILTER");
    if (!filter || *filter == '\0') {
        return true;
    }
    return path.find(filter) != std::string::npos;
}

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
        std::cerr << "Cannot open file: " << filename << std::endl;
        std::exit(1);
    }
    std::ostringstream oss;
    oss << fin.rdbuf();
    input = oss.str();
}

// 获取目录中的所有文件
std::vector<std::string> getFilesInDirectory(const std::string& directory) {
    std::vector<std::string> files;
    
    try {
        for (const auto& entry : std::filesystem::directory_iterator(directory)) {
            if (entry.is_regular_file()) {
                files.push_back(entry.path().string());
            }
        }
    } catch (const std::filesystem::filesystem_error& e) {
        std::cerr << "无法读取目录 " << directory << ": " << e.what() << std::endl;
    }
    
    return files;
}

// 从文件路径中提取文件名（不含扩展名）
std::string extractFileName(const std::string& filepath) {
    std::filesystem::path path(filepath);
    return path.stem().string();
}

// 测试单个文件
void testFile(const std::string& filepath, bool shouldPass) {
    std::string filename = extractFileName(filepath);
    std::string code;
    read_from_file(code, filepath);
    
    if (code.empty()) {
        std::cout << "跳过测试 " << filename << ": 无法读取文件" << std::endl;
        return;
    }
    
    std::cout << "--- 测试 " << filename << " ---" << std::endl;
    std::cout << "文件: " << filepath << std::endl;

    try {
        // 1. 词法分析
        Lexer lexer(code);
        auto tokens = lexer.tokenize_all();
        tokens.push_back(Token(TokenKind::Eof, "", 0));
    
        // 2. 语法分析
        Parser parser(tokens);
        auto ast = parser.parse_program();

        // 3. 语义分析
        SemanticAnalyzer analyzer;
        bool success = analyzer.analyze(ast.get());
        if (success) {
            std::cout << "语义分析成功，没有错误" << std::endl;
            if (shouldPass) {
                std::cout << "✓ 测试通过: 预期通过且语义分析成功" << std::endl;
            } else {
                std::cout << "❌ 测试失败: 预期失败但语义分析成功" << std::endl;
            }
        } else {
            std::cout << "语义分析失败，发现以下错误:" << std::endl;
            for (const auto& error : analyzer.errors()) {
                std::cout << "  - " << error.message << " (位置: " << error.position << ")" << std::endl;
            }
            if (shouldPass) {
                std::cout << "❌ 测试失败: 预期通过但语义分析失败" << std::endl;
            } else {
                std::cout << "✓ 测试通过: 预期失败且语义分析失败" << std::endl;
            }
        }
    } catch (const std::exception& ex) {
        std::cout << "解析失败: " << ex.what() << std::endl;
        if (shouldPass) {
            std::cout << "❌ 测试失败: 预期通过但解析失败" << std::endl;
        } else {
            std::cout << "✓ 测试通过: 预期失败且解析失败" << std::endl;
        }
    } catch (...) {
        std::cout << "解析失败: 未知异常" << std::endl;
        if (shouldPass) {
            std::cout << "❌ 测试失败: 预期通过但解析失败" << std::endl;
        } else {
            std::cout << "✓ 测试通过: 预期失败且解析失败" << std::endl;
        }
    }
}

// 测试目录中的所有文件
void testDirectory(const std::string& directory, bool shouldPass) {
    std::vector<std::string> files = find_rx_files(directory);
    
    if (files.empty()) {
        std::cout << "目录 " << directory << " 中没有找到测试文件" << std::endl;
        return;
    }
    
    for (const auto& file : files) {
        if (!should_run_test(file)) {
            continue;
        }
        testFile(file, shouldPass);
    }
}

int main() {
    std::cout << "=== 语义分析测试 (debug mode) ===" << std::endl;
    std::string base_dir = "/mnt/c/Users/hejia/Desktop/Rcompiler/mytest";
    testDirectory(base_dir + "/true", true);
    testDirectory(base_dir + "/false", false);
    return 0;
}