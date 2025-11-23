#include "token.h"

Token::Token(TokenKind kind, const std::string &text, size_t pos = 0)
  : kind_(kind), text_(text), pos_(pos){
}

TokenKind Token::kind() const {
  return kind_;
}

std::string Token::text() const {
  return text_;
}

int Token::position() const {
  return pos_;
}

