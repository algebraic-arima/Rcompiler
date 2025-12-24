#include <exception>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include "lexer.h"
#include "parser.h"
#include "semantic.h"

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
    std::string input;
    if (argc > 1 && std::string(argv[1]) != "-") {
      read_from_file(input, argv[1]);
    } else if (argc > 1 && std::string(argv[1]) == "-") {
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
    if (!analyzer.analyze(ast.get())) return 1;
  } catch (const std::exception& ex) {
    std::cerr << "Error: " << ex.what() << std::endl;
    return 1;
  } catch (...) {
    std::cerr << "Unknown error occurred" << std::endl;
    return 1;
  }
}
