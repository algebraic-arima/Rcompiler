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
      std::string error_msg = "Unexpected token: " + current().text() + " at position " + std::to_string(current().position());
      throw std::runtime_error(error_msg);
    }
    advance();
  }

  string expect_identifier() {
    if (current().kind() == TokenKind::Identifier || 
        (current().kind() == TokenKind::Keyword && current().text() == "self")) {
      std::string name = current().text();
      advance();
      return name;
    }
    std::string error_msg = "Expected identifier, but got: " + current().text() + " at position " + std::to_string(current().position());
    throw std::runtime_error(error_msg);
  }

  unique_ptr<TypeAST> parse_type() {
    // 处理元组类型 (i32, i32)
    if (current().kind() == TokenKind::Punctuation && current().text() == "(") {
      advance();
      
      // 检查是否是空元组 ()
      if (match(TokenKind::Punctuation, ")")) {
        advance();
        return make_unique<TupleTypeAST>(std::vector<std::unique_ptr<TypeAST>>());
      }
      
      // 解析元组元素类型
      std::vector<std::unique_ptr<TypeAST>> elements;
      elements.push_back(parse_type());
      
      // 解析剩余元素
      while (match(TokenKind::Punctuation, ",")) {
        advance();
        // 允许尾随逗号
        if (match(TokenKind::Punctuation, ")")) {
          break;
        }
        elements.push_back(parse_type());
      }
      
      expect(TokenKind::Punctuation, ")");
      return make_unique<TupleTypeAST>(std::move(elements));
    }

    // 处理数组类型 [i32; 3]
    if (current().kind() == TokenKind::Punctuation && current().text() == "[") {
      advance();
      auto elem_type = parse_type();
      expect(TokenKind::Punctuation, ";");
      // 不再需要额外的advance()，因为expect()已经移动了指针

      // 解析数组大小表达式
      auto size_expr = parse_expr();
      expect(TokenKind::Punctuation, "]");
      
      // 返回ArrayTypeAST
      return make_unique<ArrayTypeAST>(std::move(elem_type), std::move(size_expr));
    }
    
    // 处理引用类型 &T 和 &mut T
    if (current().kind() == TokenKind::Operator && current().text() == "&") {
      advance();
      // 检查是否是可变引用
      bool is_mutable = false;
      
      if (current().kind() == TokenKind::Keyword && current().text() == "mut") {
        is_mutable = true;
        advance();
      }
      
      // 递归解析被引用的类型
      auto referenced_type = parse_type();
      return make_unique<ReferenceTypeAST>(std::move(referenced_type), is_mutable);
    }
    
    // 处理原始指针类型 *const T 和 *mut T
    if (current().kind() == TokenKind::Operator && current().text() == "*") {
      advance();
      bool is_const = false;
      bool is_mut = false;
      
      // 检查是 const 还是 mut 指针
      if (current().kind() == TokenKind::Keyword && 
          (current().text() == "const" || current().text() == "mut")) {
        if (current().text() == "const") {
          is_const = true;
        } else {
          is_mut = true;
        }
        advance();
      }
      
      // 递归解析指针指向的类型
      auto pointed_type = parse_type();
      // 暂时返回PrimitiveTypeAST，后续可以添加PointerTypeAST
      std::string prefix = "*";
      if (is_const) prefix += "const ";
      else if (is_mut) prefix += "mut ";
      return make_unique<PrimitiveTypeAST>(prefix + pointed_type->toString());
    }
    
    // 处理普通类型
    if (current().kind() == TokenKind::Identifier) {
      std::string type_name = current().text();
      advance();
      return make_unique<PrimitiveTypeAST>(type_name);
    }
    
    // 处理Self关键字作为类型
    if (current().kind() == TokenKind::Keyword && current().text() == "Self") {
      advance();
      return make_unique<PrimitiveTypeAST>("Self");
    }

    std::string error_msg = "Expected type identifier, but got: " + current().text() + " at position " + std::to_string(current().position());
    throw std::runtime_error(error_msg);
  }

  std::vector<std::pair<std::unique_ptr<IdentPatternAST>, std::string>> parse_fn_params() {
    std::vector<std::pair<std::unique_ptr<IdentPatternAST>, std::string>> params;
    if (!match(TokenKind::Punctuation, ")")) { // 空参
      // 检查是否是 self 参数（实例方法）
      // 注意：在 Rust 中，self 参数必须是第一个参数
      bool self_is_ref = false;
      bool self_is_mut = false;
      
      // 检查是否是引用形式 &self 或 &mut self
      if (match(TokenKind::Operator, "&")) {
        self_is_ref = true;
        advance();
        
        // 检查是否是可变引用 &mut self
        if (match(TokenKind::Keyword, "mut")) {
          self_is_mut = true;
          advance();
        }
      }
      
      // 检查 self 关键字
      if (match(TokenKind::Keyword, "self")) {
        // self关键字处理
        advance();
        
        // 引用和可变修饰符已经在前面处理过了
        
        // 创建self参数的IdentPatternAST对象
        auto self_pattern = std::make_unique<IdentPatternAST>("self", self_is_mut, self_is_ref, false, current().position());
        
        // 添加self参数
        params.emplace_back(std::move(self_pattern), "Self");
        
        // 如果还有其他参数，需要逗号分隔
        if (!match(TokenKind::Punctuation, ")")) {
          expect(TokenKind::Punctuation, ",");
        }
      }
      
      // 处理其他参数
      while (!match(TokenKind::Punctuation, ")")) {
        std::string param_prefix = "";
        
        // 检查是否有引用修饰符 &
        bool is_ref = false;
        bool is_mut = false;
        
        if (match(TokenKind::Operator, "&")) {
          is_ref = true;
          param_prefix += "&";
          advance();
          
          // 检查是否是可变引用 &mut
          if (match(TokenKind::Keyword, "mut")) {
            is_mut = true;
            param_prefix += "mut ";
            advance();
          }
        }
        // 检查是否有mut修饰符（非引用情况）
        else if (match(TokenKind::Keyword, "mut")) {
          is_mut = true;
          param_prefix += "mut ";
          advance();
        }
        
        // 参数名
        std::string param_name = expect_identifier();
        
        // 冒号
        expect(TokenKind::Punctuation, ":");
        
        // 参数类型
        auto type_ast = parse_type();
        std::string type_name = type_ast->toString();
        
        // 如果是引用，需要将引用符号添加到类型前
        if (is_ref) {
          if (is_mut) {
            type_name = "&mut " + type_name;
          } else {
            type_name = "&" + type_name;
          }
        }
        
        // 创建IdentPatternAST对象
        auto pattern = std::make_unique<IdentPatternAST>(param_name, is_mut, is_ref, false, current().position());

        // 保留完整的类型AST以便后续阶段能获取精确的长度信息
        pattern->type = std::move(type_ast);
        
        // 添加参数到列表
        params.emplace_back(std::move(pattern), type_name);
        // 下一个参数/结束
        if (match(TokenKind::Punctuation, ",")) {
          advance();
        } else if (match(TokenKind::Punctuation, ")")) {
          break; // 参数列表结束
        } else {
          std::string error_msg = "Unexpected token in function parameter list: " + current().text() + " at position " + std::to_string(current().position());
          throw std::runtime_error(error_msg);
        }
      }
    }
    expect(TokenKind::Punctuation, ")");
    return params;
  }

  unique_ptr<TypeAST> parse_fn_return_type() {
    if (match(TokenKind::Operator, "->")) {
      advance();
      return parse_type();
    }
    return nullptr;
  }

  unique_ptr<BlockStmtAST> parse_program();

  unique_ptr<ExprAST> parse_expr();

  unique_ptr<ExprAST> parse_logical();

  unique_ptr<ExprAST> parse_bitwise_or();

  unique_ptr<ExprAST> parse_bitwise_xor();

  unique_ptr<ExprAST> parse_bitwise_and();

  unique_ptr<ExprAST> parse_equality();

  unique_ptr<ExprAST> parse_comparison();

  unique_ptr<ExprAST> parse_shift();

  unique_ptr<ExprAST> parse_additive();

  unique_ptr<ExprAST> parse_term();

  unique_ptr<ExprAST> parse_cast_expr();

  unique_ptr<BlockStmtAST> parse_block();

  unique_ptr<ExprAST> parse_factor();

  unique_ptr<ExprAST> parse_if_expr();

  unique_ptr<ExprAST> parse_loop_expr();

  unique_ptr<ExprAST> parse_block_expr();
  
  unique_ptr<ExprAST> parse_value();

  unique_ptr<StmtAST> parse_stmt();
  
  unique_ptr<ImplStmtAST> parse_impl();
};

// 辅助函数：将BlockStmtAST*尝试转换为BlockExprAST*，失败则返回nullptr
static std::unique_ptr<BlockExprAST> try_convert_block_to_expr(BlockStmtAST* block);

// 辅助函数：将IfStmtAST*尝试转换为IfExprAST*，失败则返回nullptr
static std::unique_ptr<IfExprAST> try_convert_if_to_expr(IfStmtAST* if_stmt);

// 辅助函数：将LoopStmtAST*尝试转换为LoopExprAST*，失败则返回nullptr
static std::unique_ptr<LoopExprAST> try_convert_loop_to_expr(LoopStmtAST* loop_stmt);

#endif //PARSER_H
