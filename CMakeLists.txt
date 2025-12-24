cmake_minimum_required(VERSION 3.28)
project(compiler)

set(CMAKE_CXX_STANDARD 20)

find_package(Boost REQUIRED)

include_directories(${Boost_INCLUDE_DIRS})

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# 主程序源文件
set(MAIN_SOURCES
        src/main.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
        src/semantic.cpp
)

# 测试程序源文件
set(TEST_SOURCES
        src/parser_test.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
)

# 语义分析测试程序源文件
set(SEMANTIC_TEST_SOURCES
        src/semantic_test.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
        src/semantic.cpp
)

# 创建主程序可执行文件
add_executable(code ${MAIN_SOURCES})

# 创建测试程序可执行文件
add_executable(parser_test ${TEST_SOURCES})

# 创建语义分析测试程序可执行文件
add_executable(semantic_test ${SEMANTIC_TEST_SOURCES})