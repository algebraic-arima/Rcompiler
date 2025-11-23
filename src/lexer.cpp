#include "lexer.h"
#include <iostream>
#include <regex>
#include <string>

#define RUST_SUFFIX "(i32|u32|isize|usize)"

struct LexRule {
  TokenKind kind;
  std::regex pattern;
};

static const std::regex re_identifier(R"([a-zA-Z]\w*)");
static const std::regex re_raw_string(R"(r(#*)\"(.*?)\"\1)");
static const std::vector<LexRule> lex_rules = {
  {TokenKind::Number, std::regex("0[bB][01](?:_?[01])*_?" RUST_SUFFIX "?")},
  {TokenKind::Number, std::regex("0[oO][0-7](?:_?[0-7])*_?" RUST_SUFFIX "?")},
  {TokenKind::Number, std::regex("0[xX][0-9a-fA-F](?:_?[0-9a-fA-F])*_?" RUST_SUFFIX "?")},
  {TokenKind::Number, std::regex("\\d(?:_?\\d)*_?" RUST_SUFFIX "?")},
  {TokenKind::Float, std::regex(R"(\d+\.\d+)")},
  {TokenKind::String, std::regex(R"('([^'\\]|\\.)')")}, //char
  {TokenKind::String, std::regex(R"("([^"\\]|\\.)*")")}, //string


  {TokenKind::Operator, std::regex(R"(\->)")}, // ->
  {TokenKind::Punctuation, std::regex(R"(=>)")}, // =>
  {TokenKind::Punctuation, std::regex(R"(<\-)")}, // <-
  {TokenKind::Comparison, std::regex(R"(==)")}, // ==
  {TokenKind::Comparison, std::regex(R"(!=)")}, // !=
  {TokenKind::Comparison, std::regex(R"(<=)")}, // <=
  {TokenKind::Comparison, std::regex(R"(<)")}, // <
  {TokenKind::Comparison, std::regex(R"(>=)")}, // >=
  {TokenKind::Comparison, std::regex(R"(>)")}, // >

  {TokenKind::Operator, std::regex(R"(=)")}, // =
  {TokenKind::Operator, std::regex(R"(\+=)")}, // +=
  {TokenKind::Operator, std::regex(R"(\+)")}, // +
  {TokenKind::Operator, std::regex(R"(\-=)")}, // -=
  {TokenKind::Operator, std::regex(R"(\-)")}, // -
  {TokenKind::Operator, std::regex(R"(\*=)")}, // *=
  {TokenKind::Operator, std::regex(R"(\*)")}, // *
  {TokenKind::Operator, std::regex(R"(\/=)")}, // /=
  {TokenKind::Operator, std::regex(R"(\/)")}, // /
  {TokenKind::Operator, std::regex(R"(%=)")}, // %=
  {TokenKind::Operator, std::regex(R"(%)")}, // %
  {TokenKind::Operator, std::regex(R"(&&)")}, // &&
  {TokenKind::Operator, std::regex(R"(&=)")}, // &=
  {TokenKind::Operator, std::regex(R"(&)")}, // &
  {TokenKind::Operator, std::regex(R"(\|\|)")}, // ||
  {TokenKind::Operator, std::regex(R"(\|=)")}, // \=
  {TokenKind::Operator, std::regex(R"(\|)")}, // |
  {TokenKind::Operator, std::regex(R"(\^=)")}, // ^=
  {TokenKind::Operator, std::regex(R"(\^)")}, // ^
  {TokenKind::Operator, std::regex(R"(!)")}, // !
  {TokenKind::Operator, std::regex(R"(<<=)")}, // <<=
  {TokenKind::Operator, std::regex(R"(<<)")}, // <<
  {TokenKind::Operator, std::regex(R"(>>=)")}, // >>=
  {TokenKind::Operator, std::regex(R"(>>)")}, // >>

  {TokenKind::Punctuation, std::regex(R"(\()")}, // (
  {TokenKind::Punctuation, std::regex(R"(\))")}, // )
  {TokenKind::Punctuation, std::regex(R"(\[)")}, // [
  {TokenKind::Punctuation, std::regex(R"(\])")}, // ]
  {TokenKind::Punctuation, std::regex(R"(\{)")}, // {
  {TokenKind::Punctuation, std::regex(R"(\})")}, // }
  {TokenKind::Punctuation, std::regex(R"(;)")}, // ;
  {TokenKind::Punctuation, std::regex(R"(_)")}, // _
  {TokenKind::Punctuation, std::regex(R"(,)")}, // ,
  {TokenKind::Punctuation, std::regex(R"(\.\.\.)")}, // ...
  {TokenKind::Punctuation, std::regex(R"(\..=)")}, // ..=
  {TokenKind::Punctuation, std::regex(R"(\.\.)")}, // ..
  {TokenKind::Punctuation, std::regex(R"(\.)")}, // .
  {TokenKind::Punctuation, std::regex(R"(::)")}, // ::
  {TokenKind::Punctuation, std::regex(R"(:)")}, // :
  {TokenKind::Punctuation, std::regex(R"(\?)")}, // ?
  {TokenKind::Punctuation, std::regex(R"(\@)")}, // @
  {TokenKind::Operator, std::regex(R"(~)")}, // ~
  {TokenKind::Punctuation, std::regex(R"(#)")}, // #
  {TokenKind::Punctuation, std::regex(R"($)")}, // $
}; //正则表达式多为GPT生成


Lexer::Lexer(const std::string &src) : src_(src), pos_(0) {
  currentChar = src_.empty() ? EOF : src_[0];
}

void Lexer::advance() {
  ++pos_;
  currentChar = pos_ < src_.size() ? src_[pos_] : EOF;
}

bool Lexer::match(const std::regex &re, const std::string &src, size_t &pos, std::string &matched) {
  std::smatch m;
  std::string cur = src.substr(pos);
  if (std::regex_search(cur, m, re) && m.position() == 0) {
    matched = m.str();
    pos += matched.length();
    currentChar = pos < src_.size() ? src_[pos] : EOF;
    return true;
  }
  return false;
}

void Lexer::skip_whitespace() {
  while (currentChar != EOF && isspace(currentChar)) {
    advance();
  }
}

void Lexer::skip_comment() {
  if (currentChar == '/') {
    if (pos_ + 1 == src_.size()) {
      throw std::runtime_error("Invalid Code");
    }
    if (src_[pos_ + 1] == '/') {
      while (pos_ < src_.size() && src_[pos_] != '\n') ++pos_;
      currentChar = pos_ < src_.size() ? src_[pos_] : EOF;
    } else if (src_[pos_ + 1] == '*') {
      int count = 1;
      bool match = false;
      pos_ += 2;
      while (count > 0 && pos_ < src_.size()) {
        match = false;
        if (src_[pos_] == '/' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '*') {
          ++count;
          match = true;
        }
        if (src_[pos_] == '*' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '/') {
          --count;
          match = true;
        }
        if (match) {
          pos_ += 2;
        } else {
          ++pos_;
        }
      }
      if (count > 0) {
        throw std::runtime_error("Invalid Comment");
      }
      currentChar = pos_ < src_.size() ? src_[pos_] : EOF;
    }
  }
}

Token Lexer::next_token() {
  skip_whitespace();
  skip_comment();
  skip_whitespace();
  if (pos_ >= src_.size()) {
    return Token(TokenKind::Eof, "", src_.size());
  }
  std::string matched;

  size_t old_pos = pos_;
  if (match(re_raw_string, src_, pos_, matched)) {
    return Token(TokenKind::String, matched, old_pos);
  }
  if (match(re_identifier, src_, pos_, matched)) {
    if (keywords.find(matched) != keywords.end()) {
      return Token(TokenKind::Keyword, matched, old_pos);
    } else {
      return Token(TokenKind::Identifier, matched, old_pos);
    }
  }
  for (const auto &rule: lex_rules)
    if (match(rule.pattern, src_, pos_, matched))
      return Token(rule.kind, matched, old_pos);

  ++pos_;
  return Token(TokenKind::Unknown, "Invalid", pos_ - 1);
}

std::pair<int, int> Lexer::getLineAndCol(int p) {
  int line = 1, col = 1;
  for (int i = 0; i < p; ++i) {
    if (src_[i] == '\n') {
      ++line;
      col = 1;
    } else {
      ++col;
    }
  }
  return {line, col};
}

bool Lexer::is_eof() const {
  return pos_ >= src_.size();
}

std::vector<Token> Lexer::tokenize_all() {
  std::vector<Token> tokens;
  while (!is_eof()) {
    Token t = next_token();
    if (t.kind() == TokenKind::Eof) break;
    tokens.push_back(t);
  }
  /*for (auto x:tokens) {
    std::cerr << x.position() << ' ' << x.text() << '\n';
  }*/
  return tokens;
}
