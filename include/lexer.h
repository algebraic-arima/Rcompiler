#ifndef LEXER_H
#define LEXER_H
#include <regex>
#include "token.h"
#include <vector>
#include <string>

class Lexer {
public:
  Lexer(const std::string &);

  Token next_token();

  bool is_eof() const;

  std::vector<Token> tokenize_all();

  std::pair<int, int> getLineAndCol(int);

private:
  void advance();

  bool match(const std::regex &, const std::string &, size_t &, std::string &);

  void skip_whitespace();

  void skip_comment();

  std::string src_;
  size_t pos_;
  char currentChar;
};
#endif //LEXER_H
