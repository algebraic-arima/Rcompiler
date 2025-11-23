#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "lexer.h"
#include "parser.h"

void read_from_cin(std::string &input) {
  std::ostringstream oss;
  oss << std::cin.rdbuf();
  input = oss.str();
}

void read_from_file(std::string &input, const std::string &filename) {
  std::ifstream fin(filename, std::ios::in);
  if (!fin) throw std::runtime_error("Cannot open file" + filename);
  std::ostringstream oss;
  oss << fin.rdbuf();
  input = oss.str();
}

int main() {
  std::string input;
  read_from_file(input, "../test_case/test_case.in");
  //read_from_file(input, "../test_case/semantic-1/array7/array7.rx");
  Lexer lexer(input);
  std::vector<Token> tokens = lexer.tokenize_all();
  tokens.push_back(Token(TokenKind::Eof, "", 0));
  Parser parser(tokens);
  auto ast = parser.parse_program();
  ast->dump(0);
}
