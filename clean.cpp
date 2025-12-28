#include <iostream>
#include <filesystem>
#include <string>

namespace fs = std::filesystem;

// 判断文件是否应该保留
bool shouldKeepFile(const fs::path& filePath) {
    std::string extension = filePath.extension().string();
    return extension == ".in" || extension == ".out" || extension == ".rx";
}

// 清理semantic-2目录
void cleanSemantic2Directory(const fs::path& basePath) {
    if (!fs::exists(basePath)) {
        std::cerr << "错误: 路径 " << basePath << " 不存在" << std::endl;
        return;
    }

    std::cout << "开始清理semantic-2目录..." << std::endl;

    // 遍历semantic-2目录中的所有子文件夹
    for (const auto& entry : fs::directory_iterator(basePath)) {
        if (entry.is_directory()) {
            // 遍历每个子文件夹中的所有文件
            for (const auto& fileEntry : fs::directory_iterator(entry.path())) {
                if (fileEntry.is_regular_file() && !shouldKeepFile(fileEntry.path())) {
                    std::cout << "删除文件: " << fileEntry.path() << std::endl;
                    fs::remove(fileEntry.path());
                }
            }
        }
    }

    std::cout << "semantic-2目录清理完成！" << std::endl;
}

// 清空tmp目录
void cleanTmpDirectory(const fs::path& tmpPath) {
    if (!fs::exists(tmpPath)) {
        std::cerr << "错误: tmp目录 " << tmpPath << " 不存在" << std::endl;
        return;
    }

    std::cout << "开始清空tmp目录..." << std::endl;

    // 遍历tmp目录中的所有文件
    for (const auto& fileEntry : fs::directory_iterator(tmpPath)) {
        if (fileEntry.is_regular_file()) {
            std::cout << "删除文件: " << fileEntry.path() << std::endl;
            fs::remove(fileEntry.path());
        }
    }

    std::cout << "tmp目录清空完成！" << std::endl;
}

int main() {
    // 获取当前程序所在目录
    fs::path currentPath = fs::current_path();

    // 构建semantic-2目录路径
    fs::path semantic2Path = currentPath / "test_case" / "semantic-2";

    // 构建tmp目录路径
    fs::path tmpPath = currentPath / "tmp";

    // 执行清理
    cleanSemantic2Directory(semantic2Path);
    cleanTmpDirectory(tmpPath);

    std::cout << "所有清理任务完成！" << std::endl;
    return 0;
}
