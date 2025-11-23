#ifndef PARSER_H
#define PARSER_H
#include "token.h"
#include "lexer.h"
#include "ast.h"
#include <vector>

class Parser {
  std::vector<Token> tokens;
  int pos;

public:
  Parser(std::vector<Token> tokens_) : tokens(std::move(tokens_)), pos(0) {
  }

  Token &current() { return tokens[pos]; }
  void advance() { if (pos < tokens.size() - 1) ++pos; }
  void retreat() { if (pos > 0) --pos; }

  bool match(TokenKind kind, const std::string &text = "") {
    return current().kind() == kind && (text.empty() || current().text() == text);
  }

  void expect(TokenKind kind, const std::string &text = "") {
    if (!match(kind, text)) {
      std::cerr << "Unexpected token: " << current().text() << std::endl;
      std::exit(1);
    }
    advance();
  }

  string expect_identifier() {
    if (current().kind() == TokenKind::Identifier) {
      std::string name = current().text();
      advance();
      return name;
    }
    std::cerr << "Expected identifier, but got: " << current().text() << '\n';
    std::exit(1);
  }

  string parse_type() {
    // 处理数组类型 [i32; 3]
    if (current().kind() == TokenKind::Punctuation && current().text() == "[") {
      advance();
      std::string elem_type = parse_type();
      expect(TokenKind::Punctuation, ";");
      std::string size = current().text();
      advance();
      expect(TokenKind::Punctuation, "]");
      return "[" + elem_type + "; " + size + "]";
    }
    
    // 处理引用类型 &T 和 &mut T
    if (current().kind() == TokenKind::Operator && current().text() == "&") {
      advance();
      std::string ref_type = "&";
      
      // 检查是否是可变引用
      if (current().kind() == TokenKind::Keyword && current().text() == "mut") {
        ref_type += "mut ";
        advance();
      }
      
      // 递归解析被引用的类型
      ref_type += parse_type();
      return ref_type;
    }
    
    // 处理原始指针类型 *const T 和 *mut T
    if (current().kind() == TokenKind::Operator && current().text() == "*") {
      advance();
      std::string ptr_type = "*";
      
      // 检查是 const 还是 mut 指针
      if (current().kind() == TokenKind::Keyword && 
          (current().text() == "const" || current().text() == "mut")) {
        ptr_type += current().text() + " ";
        advance();
      }
      
      // 递归解析指针指向的类型
      ptr_type += parse_type();
      return ptr_type;
    }
    
    // 处理普通类型
    if (current().kind() == TokenKind::Identifier) {
      std::string type_name = current().text();
      advance();
      return type_name;
    }
    
    std::cerr << "Expected type identifier, but got: " << current().text() << '\n';
    std::exit(1);
  }

  std::vector<std::pair<string, string>> parse_fn_params() {
    std::vector<std::pair<string, string>> params;
    if (!match(TokenKind::Punctuation, ")")) { // 空参
      // 检查是否是 self 参数（实例方法）
      // 注意：在 Rust 中，self 参数必须是第一个参数
      string self_param = "";
      
      // 检查是否是引用形式 &self 或 &mut self
      if (match(TokenKind::Operator, "&")) {
        self_param = "&";
        advance();
        
        // 检查是否是可变引用 &mut self
        if (match(TokenKind::Keyword, "mut")) {
          self_param += "mut ";
          advance();
        }
      }
      
      // 检查 self 关键字
      if (match(TokenKind::Keyword, "self")) {
        self_param += "self";
        advance();
        
        // 引用和可变修饰符已经在前面处理过了
        
        params.emplace_back(self_param, "Self");
        
        // 如果还有其他参数，需要逗号分隔
        if (!match(TokenKind::Punctuation, ")")) {
          expect(TokenKind::Punctuation, ",");
        }
      }
      
      // 处理其他参数
      while (!match(TokenKind::Punctuation, ")")) {
        // 参数名
        std::string param_name = expect_identifier();
        // 冒号
        expect(TokenKind::Punctuation, ":");
        // 参数类型
        std::string type_name = parse_type();
        params.emplace_back(param_name, type_name);
        // 下一个参数/结束
        if (match(TokenKind::Punctuation, ",")) {
          advance();
        } else if (match(TokenKind::Punctuation, ")")) {
          break; // 参数列表结束
        } else {
          std::cerr << "Unexpected token in function parameter list: " << current().text() << '\n';
          std::exit(1);
        }
      }
    }
    expect(TokenKind::Punctuation, ")");
    return params;
  }

  std::string parse_fn_return_type() {
    if (match(TokenKind::Operator, "->")) {
      advance();
      return parse_type();
    }
    return "";
  }

  unique_ptr<BlockStmtAST> parse_program();

  unique_ptr<ExprAST> parse_expr();

  unique_ptr<ExprAST> parse_comparison();

  unique_ptr<ExprAST> parse_term();

  unique_ptr<BlockStmtAST> parse_block();

  unique_ptr<ExprAST> parse_factor();
  
  unique_ptr<ExprAST> parse_value();

  unique_ptr<StmtAST> parse_stmt();
  
  unique_ptr<ImplStmtAST> parse_impl();
};
#endif //PARSER_H
