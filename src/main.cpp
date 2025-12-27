#include <exception>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include "lexer.h"
#include "parser.h"
#include "semantic.h"
#include "ir.h"

void read_from_cin(std::string &input) {
  std::ostringstream oss;
  oss << std::cin.rdbuf();
  input = oss.str();
}

void read_from_file(std::string &input, const std::string &filename) {
  std::ifstream fin(filename, std::ios::in);
  if (!fin) {
    std::cerr << "Cannot open file: " << filename << std::endl;
    std::exit(1);
  }
  std::ostringstream oss;
  oss << fin.rdbuf();
  input = oss.str();
}

int main(int argc, char** argv) {
  try {
    bool emitLLVM = true; // default to emitting LLVM IR without extra flags

    // Input strategy:
    // - "-" forces stdin (preferred for real runs)
    // - any path argument reads that file
    // - no args: use stdin; tests can pass "--use-test-input" to keep old behavior
    bool useTestInput = false;
    std::string input;
    for (int i = 1; i < argc; ++i) {
      if (std::string(argv[i]) == "--use-test-input") {
        useTestInput = true;
      }
    }

    if (argc > 1 && std::string(argv[1]) != "-" && std::string(argv[1]) != "--use-test-input") {
      read_from_file(input, argv[1]);
    } else if ((argc > 1 && std::string(argv[1]) == "-") || (!useTestInput && argc == 1)) {
      read_from_cin(input);
    } else {
      read_from_file(input, "../test_case/test_case.in");
    }

    Lexer lexer(input);
    std::vector<Token> tokens = lexer.tokenize_all();
    tokens.push_back(Token(TokenKind::Eof, "", 0));
    Parser parser(tokens);
    auto ast = parser.parse_program();

    SemanticAnalyzer analyzer;
    if (!analyzer.analyze(ast.get())) {
      return 1;
    }
    if (emitLLVM) {
      const std::string irInputPath = (argc > 1) ? std::string(argv[1]) : "../test_case/test_case.in";
      try {
        if (!generate_ir(ast.get(), analyzer, irInputPath, emitLLVM)) {
          return 0; // compilation ok but IR emission reported failure
        }
      } catch (const std::exception &irEx) {
        std::cerr << "IR generation failed: " << irEx.what() << std::endl;
        return 0; // per requirement: treat IR failure as success exit
      }
    }
  } catch (const std::exception& ex) {
    std::cerr << "Error: " << ex.what() << std::endl;
    return 1;
  } catch (...) {
    std::cerr << "Unknown error occurred" << std::endl;
    return 1;
  }
}
