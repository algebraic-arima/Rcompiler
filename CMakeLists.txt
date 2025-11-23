cmake_minimum_required(VERSION 3.28)
project(compiler)

set(CMAKE_CXX_STANDARD 20)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# 主程序源文件
set(MAIN_SOURCES
        src/main.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
)

# 测试程序源文件
set(TEST_SOURCES
        src/test_runner.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
)

# 创建主程序可执行文件
add_executable(code ${MAIN_SOURCES})

# 创建测试程序可执行文件
add_executable(test_runner ${TEST_SOURCES})