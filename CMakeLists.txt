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
        src/ir.cpp
)

# 测试程序源文件
set(TEST_SOURCES
        src/parser_test.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
        src/semantic.cpp
        src/ir.cpp
)

# 语义分析测试程序源文件
set(SEMANTIC_TEST_SOURCES
        src/semantic_test.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
        src/semantic.cpp
        src/ir.cpp
)

# IR 测试驱动（调用已构建的 compiler 可执行程序）
set(IR_TEST_SOURCES
        src/ir_test.cpp
        src/ast.cpp
        src/lexer.cpp
        src/parser.cpp
        src/token.cpp
        src/semantic.cpp
        src/ir.cpp
)

# 创建主程序可执行文件
add_executable(code ${MAIN_SOURCES})

# 创建测试程序可执行文件
add_executable(parser_test ${TEST_SOURCES})

# 创建语义分析测试程序可执行文件
add_executable(semantic_test ${SEMANTIC_TEST_SOURCES})

# 创建 IR 测试程序可执行文件
add_executable(ir_test ${IR_TEST_SOURCES})