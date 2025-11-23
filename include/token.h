#ifndef TOKEN_H
#define TOKEN_H
#include <string>
#include <unordered_set>
static const std::unordered_set<std::string> keywords = {
  "as", "break", "const", "continue",
  "crate", "dyn", "else", "enum", "exit",
  "false",  "fn",  "for",  "if",
  "impl",  "in",  "let",  "loop",
  "match",  "mod",  "move",  "mut",
  "pub", "ref",  "return",  "self",
  "Self",  "static",  "struct",  "super",
  "trait",  "true",  "type",  "unsafe",
  "use",  "where",  "while"
};
enum class TokenKind {
  // 关键字Keyword
  Keyword,
  // 标识符
  Identifier,
  Number, Float, String,
  // 运算符
  Operator,
  // 比较运算符
  Comparison,
  // 标点
  Punctuation,
  // 文件末尾
  Eof,
  // 错误
  Unknown
};

class Token {
public:
  Token(TokenKind, const std::string &, size_t);

  TokenKind kind() const;
  std::string text() const;

  int position() const;

private:
  TokenKind kind_;
  std::string text_;
  //调试用
  int pos_;
};
#endif //TOKEN_H
