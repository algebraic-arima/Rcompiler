#include "parser.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <memory>

namespace {

int64_t parseIntegerLiteralToken(const std::string& text, size_t position) {
  static const std::array<std::string, 4> suffixes = {"i32", "u32", "isize", "usize"};

  std::string cleaned = text;
  for (const auto& suffix : suffixes) {
    if (cleaned.size() >= suffix.size() &&
        cleaned.compare(cleaned.size() - suffix.size(), suffix.size(), suffix) == 0) {
      cleaned.erase(cleaned.size() - suffix.size());
      break;
    }
  }

  cleaned.erase(std::remove(cleaned.begin(), cleaned.end(), '_'), cleaned.end());
  if (cleaned.empty()) {
    std::cerr << "Invalid numeric literal '" << text << "' at position " << position << std::endl;
    throw std::runtime_error("Invalid numeric literal");
  }

  try {
    size_t consumed = 0;
    int64_t value = std::stoll(cleaned, &consumed, 0);
    if (consumed != cleaned.size()) {
      std::cerr << "Invalid numeric literal '" << text << "' at position " << position << std::endl;
      throw std::runtime_error("Invalid numeric literal");
    }
    return value;
  } catch (const std::exception& ex) {
    std::cerr << "Invalid numeric literal '" << text << "' at position " << position
              << "': " << ex.what() << std::endl;
    throw std::runtime_error("Invalid numeric literal");
  }
}

} // namespace


// 辅助函数：将BlockStmtAST*尝试转换为BlockExprAST*，失败则返回nullptr
static std::unique_ptr<BlockExprAST> try_convert_block_to_expr(BlockStmtAST* block) {
  if (!block || block->statements.empty()) {
    return nullptr;
  }
  
  // 查找代码块中的最后一个表达式或返回语句
  std::unique_ptr<ExprAST> last_expr = nullptr;
  auto& last_stmt = block->statements.back();
  
  // 检查最后一个语句是否是返回语句
  if (auto return_stmt = dynamic_cast<ReturnStmtAST*>(last_stmt.get())) {
    // 如果是返回语句，根据其是否为隐式返回决定如何处理
    if (return_stmt->is_implicit) {
      last_expr = std::move(return_stmt->value);
    } else {
      last_expr = std::make_unique<ReturnExprAST>(return_stmt->position(), std::move(return_stmt->value), true);
    }
  } else if (auto expr_stmt = dynamic_cast<ExprStmtAST*>(last_stmt.get())) {
    // 如果是表达式语句，使用表达式的值
    last_expr = std::move(expr_stmt->expr);
  } else if (auto if_expr_stmt = dynamic_cast<IfExprAST*>(last_stmt.get())) {
    // 如果是if表达式语句，直接使用if表达式
    last_expr = std::unique_ptr<IfExprAST>(if_expr_stmt);
  } else if (auto if_stmt = dynamic_cast<IfStmtAST*>(last_stmt.get())) {
    // 如果是if语句，尝试转换为if表达式
    last_expr = try_convert_if_to_expr(if_stmt);
  } else if (auto loop_expr_stmt = dynamic_cast<LoopExprAST*>(last_stmt.get())) {
    // 如果是loop表达式语句，直接使用loop表达式
    last_expr = std::unique_ptr<LoopExprAST>(loop_expr_stmt);
  } else if (auto loop_stmt = dynamic_cast<LoopStmtAST*>(last_stmt.get())) {
    // 如果是loop语句，尝试转换为loop表达式
    last_expr = try_convert_loop_to_expr(loop_stmt);
  } else {
    // 其他类型语句，无法作为表达式返回
    return nullptr;
  }
  
  if (!last_expr) {
    return nullptr;
  }
  
  // 创建一个新的语句列表，不包含最后一个语句
  std::vector<std::unique_ptr<StmtAST>> statements;
  for (size_t i = 0; i < block->statements.size() - 1; ++i) {
    // 现在我们可以直接移动语句，因为block不再是const的
    statements.push_back(std::move(block->statements[i]));
  }
  
  // 创建BlockExprAST节点
  return std::make_unique<BlockExprAST>(std::move(statements), std::move(last_expr), block->position());
}

// 辅助函数：检查语句中是否包含不带返回值的break语句
static bool contains_break_without_value(StmtAST* stmt) {
  if (!stmt) {
    return false;
  }
  
  // 如果是break语句，检查是否有返回值
  if (auto break_stmt = dynamic_cast<BreakStmtAST*>(stmt)) {
    return !break_stmt->value; // 如果没有返回值，返回true
  }
  
  // 如果是代码块，递归检查所有语句
  if (auto block_stmt = dynamic_cast<BlockStmtAST*>(stmt)) {
    for (const auto& s : block_stmt->statements) {
      if (contains_break_without_value(s.get())) {
        return true;
      }
    }
    return false;
  }
  
  // 如果是if语句，递归检查then和else分支
  if (auto if_stmt = dynamic_cast<IfStmtAST*>(stmt)) {
    if (contains_break_without_value(if_stmt->then_branch.get())) {
      return true;
    }
    if (if_stmt->else_branch && contains_break_without_value(if_stmt->else_branch.get())) {
      return true;
    }
    return false;
  }
  
  // 如果是loop语句，递归检查循环体
  if (auto loop_stmt = dynamic_cast<LoopStmtAST*>(stmt)) {
    return contains_break_without_value(loop_stmt->body.get());
  }
  
  // 其他类型的语句不包含break语句
  return false;
}

// 辅助函数：将LoopStmtAST*尝试转换为LoopExprAST*，失败则返回nullptr
static std::unique_ptr<LoopExprAST> try_convert_loop_to_expr(LoopStmtAST* loop_stmt) {
  if (!loop_stmt) {
    return nullptr;
  }

  // 检查loop体中是否包含不带返回值的break语句
  if (contains_break_without_value(loop_stmt->body.get())) {
    return nullptr; // 如果有不带返回值的break，不能转换为表达式
  }

  // 所有break语句都有返回值，可以转换为表达式
  return std::make_unique<LoopExprAST>(
    std::move(loop_stmt->body),
    loop_stmt->position()
  );
}

// 辅助函数：将IfStmtAST*尝试转换为IfExprAST*，失败则返回nullptr
static std::unique_ptr<IfExprAST> try_convert_if_to_expr(IfStmtAST* if_stmt) {
  if (!if_stmt) {
    return nullptr;
  }
  
  // 检查then分支是否有返回值
  std::unique_ptr<ExprAST> then_expr;
  if (auto then_block = dynamic_cast<BlockStmtAST*>(if_stmt->then_branch.get())) {
    then_expr = try_convert_block_to_expr(then_block);
  } else {
    // 非代码块分支，无法转换为表达式
    return nullptr;
  }
  
  if (!then_expr) {
    return nullptr;
  }
  
  // 检查else分支是否有返回值
  std::unique_ptr<ExprAST> else_expr;
  if (!if_stmt->else_branch) {
    // 没有else分支，无法转换为表达式
    return nullptr;
  } else if (auto else_block = dynamic_cast<BlockStmtAST*>(if_stmt->else_branch.get())) {
    else_expr = try_convert_block_to_expr(else_block);
  } else if (auto else_if = dynamic_cast<IfStmtAST*>(if_stmt->else_branch.get())) {
    else_expr = try_convert_if_to_expr(else_if);
  } else {
    // 非代码块或if语句分支，无法转换为表达式
    return nullptr;
  }
  
  if (!else_expr) {
    return nullptr;
  }
  
  // 创建IfExprAST节点
  return std::make_unique<IfExprAST>(
    std::move(if_stmt->cond),
    std::move(then_expr),
    std::move(else_expr),
    if_stmt->position()
  );
}

std::unique_ptr<BlockStmtAST> Parser::parse_program() {
  std::vector<std::unique_ptr<StmtAST> > stmts;
  while (!match(TokenKind::Eof)) {
    auto stmt = parse_stmt();
    if (!stmt) {
      std::cerr << "Invalid statement at position " << current().position() << std::endl;
      throw std::runtime_error("Invalid statement");
    }
    stmts.push_back(std::move(stmt));
  }
  return std::make_unique<BlockStmtAST>(std::move(stmts), 0);
}

//parse逻辑表达式
std::unique_ptr<ExprAST> Parser::parse_logical() {
  auto lhs = parse_equality();
  while (match(TokenKind::Operator, "&&") || match(TokenKind::Operator, "||")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_equality();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_bitwise_or() {
  auto lhs = parse_bitwise_xor();
  while (match(TokenKind::Operator, "|")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_bitwise_xor();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_bitwise_xor() {
  auto lhs = parse_bitwise_and();
  while (match(TokenKind::Operator, "^")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_bitwise_and();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_bitwise_and() {
  auto lhs = parse_comparison();
  while (match(TokenKind::Operator, "&")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_comparison();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_equality() {
  auto lhs = parse_bitwise_or();
  while (match(TokenKind::Comparison, "==") || match(TokenKind::Comparison, "!=")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_bitwise_or();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

//parse比较表达式
std::unique_ptr<ExprAST> Parser::parse_comparison() {
  auto lhs = parse_shift();
  while (match(TokenKind::Comparison, "<") || match(TokenKind::Comparison, "<=") ||
         match(TokenKind::Comparison, ">") || match(TokenKind::Comparison, ">=")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_shift();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_shift() {
  auto lhs = parse_additive();
  while (match(TokenKind::Operator, "<<") || match(TokenKind::Operator, ">>")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_additive();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_additive() {
  auto lhs = parse_term();
  while (match(TokenKind::Operator, "+") || match(TokenKind::Operator, "-")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_term();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_loop_expr() {
  size_t pos_ = current().position();
  expect(TokenKind::Keyword, "loop");

  // 解析loop体
  std::unique_ptr<StmtAST> body_stmt;
  if (match(TokenKind::Punctuation, "{")) {
    // 如果是代码块，使用parse_block解析
    body_stmt = parse_block();
  } else {
    // 如果是单个表达式，将其转换为表达式语句
    auto expr = parse_expr();
    body_stmt = std::make_unique<ExprStmtAST>(std::move(expr), pos_);
  }
  
  // 创建LoopExprAST节点
  return std::make_unique<LoopExprAST>(std::move(body_stmt), pos_);
}

std::unique_ptr<ExprAST> Parser::parse_block_expr() {
  size_t pos_ = current().position();
  
  // 解析代码块
  auto block = parse_block();
  
  // 尝试将代码块转换为表达式
  auto block_expr = try_convert_block_to_expr(block.get());
  if (block_expr) {
    return block_expr;
  } else {
    // 转换失败，无法作为表达式返回
    std::cerr << "Expected expression or return statement as last statement in block at position " << pos_ << std::endl;
    throw std::runtime_error("Expected expression or return statement as last statement in block");
  }
}

// 解析if表达式
std::unique_ptr<ExprAST> Parser::parse_if_expr() {
  size_t pos_ = current().position();
  expect(TokenKind::Keyword, "if");
  
  // 解析if条件
  expect(TokenKind::Punctuation, "(");
  auto cond = parse_expr();
  expect(TokenKind::Punctuation, ")");
  
  // 解析then分支，这里需要解析为表达式而不是语句
  std::unique_ptr<ExprAST> then_branch;
  if (match(TokenKind::Punctuation, "{")) {
    // 如果是代码块，需要处理隐式返回
    // 使用parse_block来解析整个代码块
    auto block = parse_block();

    // 尝试将代码块转换为表达式
    auto block_expr = try_convert_block_to_expr(block.get());
    if (block_expr) {
      then_branch = std::move(block_expr);
    } else {
      // 转换失败，无法作为表达式返回
      std::cerr << "Expected expression or return statement as last statement in if block at position " << pos_ << std::endl;
      throw std::runtime_error("Expected expression or return statement as last statement in if block");
    }
  } else {
    // 如果是单个表达式
    // 尝试解析表达式
    then_branch = parse_expr();
  }
  
  // 解析else分支
  expect(TokenKind::Keyword, "else");
  std::unique_ptr<ExprAST> else_branch;
  if (match(TokenKind::Punctuation, "{")) {
    // 如果是代码块，需要处理隐式返回
    // 使用parse_block来解析整个代码块
    auto block = parse_block();

    // 尝试将代码块转换为表达式
    auto block_expr = try_convert_block_to_expr(block.get());
    if (block_expr) {
      else_branch = std::move(block_expr);
    } else {
      // 转换失败，无法作为表达式返回
      std::cerr << "Expected expression or return statement as last statement in else block at position " << pos_ << std::endl;
      throw std::runtime_error("Expected expression or return statement as last statement in else block");
    }
  } else if (match(TokenKind::Keyword, "if")) {
    // 如果是else if，递归解析
    else_branch = parse_if_expr();
  } else {
    // 如果是单个表达式
    // 尝试解析表达式
    else_branch = parse_expr();
  }
  
  // 创建IfExprAST节点
  return std::make_unique<IfExprAST>(std::move(cond), std::move(then_branch), std::move(else_branch), pos_);
}


//parse一个低级优先运算
std::unique_ptr<ExprAST> Parser::parse_expr() {
  auto lhs = parse_logical();
  while (match(TokenKind::Keyword, "as")) {
    advance();
    auto type_ast = parse_type();
    lhs = std::make_unique<CastExprAST>(std::move(lhs), std::move(type_ast), lhs->position());
  }

  return lhs;
}

//parse一个中优先级运算
std::unique_ptr<ExprAST> Parser::parse_term() {
  auto lhs = parse_cast_expr();
  while (match(TokenKind::Operator, "*") || match(TokenKind::Operator, "/") || match(TokenKind::Operator, "%")) {
    std::string op = current().text();
    size_t pos_ = current().position(); // 记录运算符位置
    advance();
    auto rhs = parse_cast_expr();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

std::unique_ptr<ExprAST> Parser::parse_cast_expr() {
  auto expr = parse_factor();
  while (match(TokenKind::Keyword, "as")) {
    size_t castPos = expr ? expr->position() : current().position();
    advance();
    auto type_ast = parse_type();
    expr = std::make_unique<CastExprAST>(std::move(expr), std::move(type_ast), castPos);
  }
  return expr;
}


//parse一个基本运算单元
std::unique_ptr<ExprAST> Parser::parse_factor() {
  size_t pos_ = current().position(); // 定义位置变量

  if (match(TokenKind::Keyword, "return")) {
    Token tok = current();
    advance();
    std::unique_ptr<ExprAST> value = nullptr;
    if (!match(TokenKind::Punctuation, ";")) {
      value = parse_expr();
    }
    return std::make_unique<ReturnExprAST>(tok.position(), std::move(value), true);
  }

  // 处理一元运算符
  if (match(TokenKind::Operator, "-") || match(TokenKind::Operator, "!") || match(TokenKind::Operator, "&") || match(TokenKind::Operator, "*")) {
    std::string op = current().text();
    advance();
    
    // 特殊处理&mut表达式
    if (op == "&" && match(TokenKind::Keyword, "mut")) {
      advance(); // 跳过mut关键字
      auto operand = parse_factor();
      return std::make_unique<UnaryExprAST>("&mut", pos_, std::move(operand));
    }
    
    auto operand = parse_factor();
    return std::make_unique<UnaryExprAST>(op, pos_, std::move(operand));
  }

  // 处理字面量
  if (match(TokenKind::Number)) {
    const std::string literal = current().text();
    int64_t val = parseIntegerLiteralToken(literal, pos_);
    advance();
    
    // 特殊处理3.to_string()语法
    if (match(TokenKind::Punctuation, ".")) {
      advance();
      std::string method_name = expect_identifier();
      
      // 如果是to_string方法调用
      if (method_name == "to_string" && match(TokenKind::Punctuation, "(")) {
        advance();
        expect(TokenKind::Punctuation, ")");
        // 直接返回一个字符串表达式，表示3.to_string()的结果
        return std::make_unique<StringExprAST>(std::to_string(val), pos_, false);
      }
      
      // 其他方法调用或字段访问，报错
      std::cerr << "Method calls or field access on number literals are not supported at position " << pos_ << std::endl;
      throw std::runtime_error("Method calls or field access on number literals are not supported");
    }
    
    return std::make_unique<NumberExprAST>(val, pos_);
  }
  if (match(TokenKind::Float)) {
    double val = std::stod(current().text());
    advance();
    return std::make_unique<FloatExprAST>(val, pos_);
  }
  if (match(TokenKind::String)) {
    std::string val = current().text();
    bool isCharLiteral = !val.empty() && val.front() == '\'';
    advance();
    return std::make_unique<StringExprAST>(val, pos_, isCharLiteral);
  }

  // 处理布尔字面量
  if (match(TokenKind::Keyword, "true") || match(TokenKind::Keyword, "false")) {
    bool value = current().text() == "true";
    advance();
    return std::make_unique<BoolExprAST>(value, pos_);
  }
  
  // 处理所有标识符（包括关键字和普通标识符）
  if (match(TokenKind::Identifier) || match(TokenKind::Keyword, "self") || match(TokenKind::Keyword, "Self")) {
    std::string name = current().text();
    advance();
    
    // 检查是否是类型关联函数调用或枚举值
    if (match(TokenKind::Punctuation, "::")) {
      advance();
      std::string right = expect_identifier();
      

      
      // 检查是否是函数调用
      if (match(TokenKind::Punctuation, "(")) {
        advance();
        std::vector<std::unique_ptr<ExprAST>> args;
        if (!match(TokenKind::Punctuation, ")")) {
          while (true) {
            args.push_back(parse_expr());
            if (match(TokenKind::Punctuation, ",")) {
              advance();
              // 允许最后一个参数后有逗号
              if (match(TokenKind::Punctuation, ")")) {
                break;
              }
            } else if (match(TokenKind::Punctuation, ")")) {
              break;
            } else {
              std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
              throw std::runtime_error("Expected ',' or ')' in function arguments");
            }
          }
        }
        expect(TokenKind::Punctuation, ")");
        
        // 创建类型关联函数调用表达式
        return std::make_unique<StaticCallExprAST>(name, right, pos_, std::move(args));
      } else {
        // 处理枚举值，如 Ordering::Less
        return std::make_unique<EnumValueExprAST>(name, right, pos_);
      }
    }

    // 检查是否是结构体构造
    if (match(TokenKind::Punctuation, "{")) {
      advance();
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> fields;
      
      // 解析字段初始化
      while (!match(TokenKind::Punctuation, "}")) {
        std::string field_name = expect_identifier();
        expect(TokenKind::Punctuation, ":");
        auto field_value = parse_expr();
        fields.push_back(std::make_pair(field_name, std::move(field_value)));
        
        // 如果不是最后一个字段，需要逗号分隔
        if (!match(TokenKind::Punctuation, "}")) {
          expect(TokenKind::Punctuation, ",");
        }
      }
      
      expect(TokenKind::Punctuation, "}");
      
      // 创建结构体表达式
      std::unique_ptr<ExprAST> struct_expr = std::make_unique<StructExprAST>(name, std::move(fields), pos_);
      
      // 检查结构体表达式后是否有数组索引和成员访问，支持链式访问
      while (true) {
        // 检查数组索引，如 struct_expr[2]
        if (match(TokenKind::Punctuation, "[")) {
          advance();
          auto index = parse_expr();
          expect(TokenKind::Punctuation, "]");
          struct_expr = std::make_unique<ArrayIndexExprAST>(pos_, std::move(struct_expr), std::move(index));
          continue;
        }

        // 检查成员访问，如 struct_expr.field 或 struct_expr.method()
        if (match(TokenKind::Punctuation, ".")) {
          advance();
          if (current().kind() != TokenKind::Identifier && 
              !(current().kind() == TokenKind::Keyword && current().text() == "self")) {
            std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
            throw std::runtime_error("Expected identifier after '.'");
          }
          string member_name = current().text();
          advance();

          // 检查是否是方法调用，如 struct_expr.method()
          if (match(TokenKind::Punctuation, "(")) {
            advance();
            std::vector<std::unique_ptr<ExprAST>> method_args;
            if (!match(TokenKind::Punctuation, ")")) {
              while (true) {
                method_args.push_back(parse_expr());
                if (match(TokenKind::Punctuation, ",")) {
                  advance();
                  // 允许最后一个参数后有逗号
                  if (match(TokenKind::Punctuation, ")")) {
                    break;
                  }
                } else if (match(TokenKind::Punctuation, ")")) {
                  break;
                } else {
                  std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
                  throw std::runtime_error("Expected ',' or ')' in function arguments");
                }
              }
            }
            expect(TokenKind::Punctuation, ")");

            // 创建成员方法调用表达式
            struct_expr = std::make_unique<CallExprAST>(member_name, pos_, std::move(struct_expr), std::move(method_args));
          } else {
            // 普通成员访问
            struct_expr = std::make_unique<MemberAccessExprAST>(pos_, std::move(struct_expr), member_name);
          }
          continue;
        }

        // 如果没有数组索引或成员访问，退出循环
        break;
      }
      
      return std::move(struct_expr);
    }

    // 检查是否是函数调用
    if (match(TokenKind::Punctuation, "(")) {
      advance();
      std::vector<std::unique_ptr<ExprAST> > func_args;
      if (!match(TokenKind::Punctuation, ")")) {
        while (true) {
          func_args.push_back(parse_expr());
          if (match(TokenKind::Punctuation, ",")) {
            advance();
            // 允许最后一个参数后有逗号
            if (match(TokenKind::Punctuation, ")")) {
              break;
            }
          } else if (match(TokenKind::Punctuation, ")")) {
            break;
          } else {
            std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
            throw std::runtime_error("Expected ',' or ')' in function arguments");
          }
        }
      }
      expect(TokenKind::Punctuation, ")");

      // 检查函数调用后是否有数组索引和成员访问，支持链式访问
      std::unique_ptr<ExprAST> expr = std::make_unique<CallExprAST>(name, pos_, std::move(func_args));

      while (true) {
        // 检查数组索引，如 func()[2]
        if (match(TokenKind::Punctuation, "[")) {
          advance();
          auto index = parse_expr();
          expect(TokenKind::Punctuation, "]");
          expr = std::make_unique<ArrayIndexExprAST>(pos_, std::move(expr), std::move(index));
          continue;
        }

        // 检查成员访问，如 func().field 或 func().method()
        if (match(TokenKind::Punctuation, ".")) {
          advance();
          if (current().kind() != TokenKind::Identifier && 
              !(current().kind() == TokenKind::Keyword && current().text() == "self")) {
            std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
            throw std::runtime_error("Expected identifier after '.'");
          }
          string member_name = current().text();
          advance();
          
          // 检查是否是方法调用，如 func().method()
          if (match(TokenKind::Punctuation, "(")) {
            advance();
            std::vector<std::unique_ptr<ExprAST>> method_args;
            if (!match(TokenKind::Punctuation, ")")) {
              while (true) {
                method_args.push_back(parse_expr());
                if (match(TokenKind::Punctuation, ",")) {
                  advance();
                  // 允许最后一个参数后有逗号
                  if (match(TokenKind::Punctuation, ")")) {
                    break;
                  }
                } else if (match(TokenKind::Punctuation, ")")) {
                  break;
                } else {
                  std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
                  throw std::runtime_error("Expected ',' or ')' in function arguments");
                }
              }
            }
            expect(TokenKind::Punctuation, ")");
            
            // 创建成员方法调用表达式
            expr = std::make_unique<CallExprAST>(member_name, pos_, std::move(expr), std::move(method_args));
          } else {
            // 普通成员访问
            expr = std::make_unique<MemberAccessExprAST>(pos_, std::move(expr), member_name);
          }
          continue;
        }

        // 如果没有数组索引或成员访问，退出循环
        break;
      }

      return std::move(expr);
    }
    
    // 创建变量表达式
    std::unique_ptr<ExprAST> expr = std::make_unique<VariableExprAST>(name, pos_);

    // 检查变量后是否有数组索引和成员访问，支持链式访问
    while (true) {
      // 检查数组索引，如 f[2]
      if (match(TokenKind::Punctuation, "[")) {
        advance();
        auto index = parse_expr();
        expect(TokenKind::Punctuation, "]");
        expr = std::make_unique<ArrayIndexExprAST>(pos_, std::move(expr), std::move(index));
        continue;
      }

      // 检查成员访问，如 obj.field 或 obj.method()
      if (match(TokenKind::Punctuation, ".")) {
        advance();
        if (current().kind() != TokenKind::Identifier) {
          std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
          throw std::runtime_error("Expected identifier after '.'");
        }
        string member_name = current().text();
        advance();

        // 检查是否是方法调用，如 obj.method()
        if (match(TokenKind::Punctuation, "(")) {
          advance();
          std::vector<std::unique_ptr<ExprAST> > args;
          if (!match(TokenKind::Punctuation, ")")) {
            while (true) {
              args.push_back(parse_expr());
              if (match(TokenKind::Punctuation, ",")) {
                advance();
                // 允许最后一个参数后有逗号
                if (match(TokenKind::Punctuation, ")")) {
                  break;
                }
              } else if (match(TokenKind::Punctuation, ")")) {
                break;
              } else {
                std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
                throw std::runtime_error("Expected ',' or ')' in function arguments");
              }
            }
          }
          expect(TokenKind::Punctuation, ")");

          // 创建成员方法调用表达式
          expr = std::make_unique<CallExprAST>(member_name, pos_, std::move(expr), std::move(args));
        } else {
          // 普通成员访问
          expr = std::make_unique<MemberAccessExprAST>(pos_, std::move(expr), member_name);
        }
        continue;
      }

      // 如果没有数组索引或成员访问，退出循环
      break;
    }

    return std::move(expr);
  }




  // 处理if表达式
  if (match(TokenKind::Keyword, "if")) {
    return parse_if_expr();
  }
  
  // 处理loop表达式
  if (match(TokenKind::Keyword, "loop")) {
    return parse_loop_expr();
  }

  // 处理代码块表达式
  if (match(TokenKind::Punctuation, "{")) {
    return parse_block_expr();
  }

  // 处理括号表达式
  if (match(TokenKind::Punctuation, "(")) {
    advance();
    auto expr = parse_expr();
    expect(TokenKind::Punctuation, ")");

    // 检查括号表达式后是否有数组索引和成员访问，支持链式访问
    while (true) {
      // 检查数组索引，如 (expr)[2]
      if (match(TokenKind::Punctuation, "[")) {
        advance();
        auto index = parse_expr();
        expect(TokenKind::Punctuation, "]");
        expr = std::make_unique<ArrayIndexExprAST>(pos_, std::move(expr), std::move(index));
        continue;
      }

      // 检查成员访问，如 (expr).field
      if (match(TokenKind::Punctuation, ".")) {
        advance();
        if (current().kind() != TokenKind::Identifier) {
          std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
          throw std::runtime_error("Expected identifier after '.'");
        }
        string member_name = current().text();
        advance();
        expr = std::make_unique<MemberAccessExprAST>(pos_, std::move(expr), member_name);
        continue;
      }

      // 如果没有数组索引或成员访问，退出循环
      break;
    }

    return expr;
  }

  // 处理结构体构造表达式
  if (match(TokenKind::Identifier)) {
    std::string struct_name = current().text();
    advance();
    
    // 检查是否是结构体构造
    if (match(TokenKind::Punctuation, "{")) {
      advance();
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> fields;
      
      // 解析字段初始化
      while (!match(TokenKind::Punctuation, "}")) {
        std::string field_name = expect_identifier();
        expect(TokenKind::Punctuation, ":");
        auto field_value = parse_expr();
        fields.push_back(std::make_pair(field_name, std::move(field_value)));
        
        // 如果不是最后一个字段，需要逗号分隔
        if (!match(TokenKind::Punctuation, "}")) {
          expect(TokenKind::Punctuation, ",");
        }
      }
      
      expect(TokenKind::Punctuation, "}");
      return std::make_unique<StructExprAST>(struct_name, std::move(fields), pos_);
    }
    
    // 如果不是结构体构造，回退并作为普通标识符处理
    // 这里需要实现token回退机制，或者使用其他方法处理
    // 暂时先作为普通标识符处理
    std::unique_ptr<ExprAST> expr = std::make_unique<VariableExprAST>(struct_name, pos_);
    
    // 检查变量后是否有数组索引和成员访问，支持链式访问
    while (true) {
      // 检查数组索引，如 f[2]
      if (match(TokenKind::Punctuation, "[")) {
        advance();
        auto index = parse_expr();
        expect(TokenKind::Punctuation, "]");
        expr = std::make_unique<ArrayIndexExprAST>(pos_, std::move(expr), std::move(index));
        continue;
      }
      
      // 检查成员访问，如 obj.field 或 obj.method()
      if (match(TokenKind::Punctuation, ".")) {
        advance();
        if (current().kind() != TokenKind::Identifier) {
          std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
          throw std::runtime_error("Expected identifier after '.'");
        }
        string member_name = current().text();
        advance();
        
        // 检查是否是方法调用，如 obj.method()
        if (match(TokenKind::Punctuation, "(")) {
          advance();
          std::vector<std::unique_ptr<ExprAST>> args;
          if (!match(TokenKind::Punctuation, ")")) {
            while (true) {
              args.push_back(parse_expr());
              if (match(TokenKind::Punctuation, ",")) {
                advance();
                // 允许最后一个参数后有逗号
                if (match(TokenKind::Punctuation, ")")) {
                  break;
                }
              } else if (match(TokenKind::Punctuation, ")")) {
                break;
              } else {
                std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
                throw std::runtime_error("Expected ',' or ')'");
              }
            }
          }
          expect(TokenKind::Punctuation, ")");
          
          // 创建成员方法调用表达式
          expr = std::make_unique<CallExprAST>(member_name, pos_, std::move(expr), std::move(args));
        } else {
          // 普通成员访问
          expr = std::make_unique<MemberAccessExprAST>(pos_, std::move(expr), member_name);
        }
        continue;
      }
      
      // 如果没有数组索引或成员访问，退出循环
      break;
    }
    
    return std::move(expr);
  }

  // 处理数组表达式
  if (match(TokenKind::Punctuation, "[")) {
    advance();
    
    // 检查是否是空数组
    if (match(TokenKind::Punctuation, "]")) {
      advance();
      return std::make_unique<ArrayExprAST>(std::vector<std::unique_ptr<ExprAST>>(), pos_);
    }
    
    // 解析第一个元素
    auto first_element = parse_expr();
    
    // 检查是否是重复元素语法 [element; count]
    if (match(TokenKind::Punctuation, ";")) {
      advance();
      auto count = parse_expr();
      expect(TokenKind::Punctuation, "]");
      return std::make_unique<ArrayExprAST>(std::move(first_element), std::move(count), pos_);
    }
    
    // 否则是普通数组初始化 [element1, element2, ...]
    std::vector<std::unique_ptr<ExprAST>> elements;
    elements.push_back(std::move(first_element));
    
    while (true) {
      if (match(TokenKind::Punctuation, ",")) {
        advance();
        // 检查是否是尾随逗号
        if (match(TokenKind::Punctuation, "]")) {
          advance();
          break;
        }
        elements.push_back(parse_expr());
      } else if (match(TokenKind::Punctuation, "]")) {
        advance();
        break;
      } else {
        std::cerr << "Expected ',' or ']' in array elements at position " << current().position() << std::endl;
        throw std::runtime_error("Expected ',' or ']'");
      }
    }
    
    return std::make_unique<ArrayExprAST>(std::move(elements), pos_);
  }

  std::cerr << "Invalid factor: " << current().text() << " at position " << current().position() << std::endl;
  throw std::runtime_error("Invalid factor");
}

//parse一个statement
std::unique_ptr<StmtAST> Parser::parse_stmt() {
  Token tok = current();
  // 如果是单独的分号，直接忽略
  if (tok.kind() == TokenKind::Punctuation && tok.text() == ";") {
    advance();
    // 递归调用parse_stmt来解析下一个语句
    return parse_stmt();
  }
  
  if (tok.kind() == TokenKind::Keyword) {
    if (tok.text() == "as") {
      std::cerr << "Unexpected keyword: as at position " << current().position() << "\n";
      throw std::runtime_error("Unexpected keyword: as");
    } else if (tok.text() == "break") {
      advance();
      // 解析break语句，可以带一个可选的返回值表达式
      std::unique_ptr<ExprAST> break_value = nullptr;
      // 如果下一个token不是分号，则解析返回值表达式
      if (current().kind() != TokenKind::Punctuation || current().text() != ";") {
        break_value = parse_expr();
        // 检查下一个token是否是右大括号，如果是，说明可能是代码块的最后一个语句
        if (current().kind() == TokenKind::Punctuation && current().text() == "}") {
          // 如果是代码块的最后一个语句，可以允许不加分号
          if (break_value) {
            return std::make_unique<BreakStmtAST>(tok.position(), std::move(break_value));
          } else {
            return std::make_unique<BreakStmtAST>(tok.position());
          }
        } else {
          // 否则，必须要有分号
          expect(TokenKind::Punctuation, ";");
        }
      } else {
        // 如果是分号，需要跳过它
        advance();
      }
      if (break_value) {
        return std::make_unique<BreakStmtAST>(tok.position(), std::move(break_value));
      } else {
        return std::make_unique<BreakStmtAST>(tok.position());
      }
    } else if (tok.text() == "const") {
      advance();
      if (current().kind() == TokenKind::Keyword && current().text() == "fn") {
        advance();
        std::string fn_name = expect_identifier();
        expect(TokenKind::Punctuation, "(");
        auto params = parse_fn_params();
        auto ret_type = parse_fn_return_type();
        // 可解析返回类型、泛型等
        // 解析函数体
        auto body = parse_block();
        // 返回 const 函数节点
        return std::make_unique<FnStmtAST>(fn_name, std::move(params), std::move(ret_type), std::move(body), true, tok.position());
      } else {
        // 否则是 const 常量
        std::string name = expect_identifier();
        expect(TokenKind::Punctuation, ":");
        auto type_ast = parse_type();
        expect(TokenKind::Operator, "=");
        auto value = parse_expr();
        expect(TokenKind::Punctuation, ";");
        return std::make_unique<ConstStmtAST>(name, std::move(type_ast), std::move(value), tok.position());
      }
    } else if (tok.text() == "continue") {
      advance();
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ContinueStmtAST>(tok.position());
    } else if (tok.text() == "crate") {
      std::cerr << "Keyword not supported: crate at position " << current().position();
      throw std::runtime_error("Keyword not supported: crate");
    } else if (tok.text() == "dyn") {
      std::cerr << "Keyword not supported: dyn at position " << current().position();
      throw std::runtime_error("Keyword not supported: dyn");
    } else if (tok.text() == "else") {
      std::cerr << "Unexpected keyword: else (must be part of if statement) at position " << current().position();
      throw std::runtime_error("Unexpected keyword: else");
    } else if (tok.text() == "enum") {
      advance();
      // 解析枚举名称
      std::string name = expect_identifier();
      expect(TokenKind::Punctuation, "{");

      std::vector<std::pair<std::string, std::unique_ptr<TypeAST>>> variants;

      // 解析枚举变体
      while (!match(TokenKind::Punctuation, "}")) {
        std::string variant_name = expect_identifier();
        variants.push_back(std::make_pair(variant_name, nullptr));

        // 如果不是最后一个变体，需要逗号分隔
        if (!match(TokenKind::Punctuation, "}")) {
          expect(TokenKind::Punctuation, ",");
          // 允许最后一个变体后有逗号
          if (match(TokenKind::Punctuation, "}")) {
            break;
          }
        }
      }

      expect(TokenKind::Punctuation, "}");
      return std::make_unique<EnumStmtAST>(name, std::move(variants), tok.position());
    } else if (tok.text() == "exit") {
      advance();
      // 解析exit语句，可以带一个可选的退出码表达式
      std::unique_ptr<ExprAST> exit_code = nullptr;
      // 如果下一个token是左括号，则解析括号内的退出码表达式
      if (match(TokenKind::Punctuation, "(")) {
        advance(); // 跳过左括号
        exit_code = parse_expr();
        expect(TokenKind::Punctuation, ")"); // 期望右括号
      }
      // 检查是否有分号，如果没有，也允许（用于处理函数调用等情况）
      if (match(TokenKind::Punctuation, ";")) {
        advance();
      }
      return std::make_unique<ExitStmtAST>(tok.position(), std::move(exit_code));
    } else if (tok.text() == "fn") {
      advance();
      std::string fn_name = expect_identifier();
      expect(TokenKind::Punctuation, "(");
      auto params = parse_fn_params();
      auto ret_type = parse_fn_return_type();
      // 可解析返回类型、泛型等
      // 解析函数体
      auto body = parse_block();
      // 返回函数节点
      return std::make_unique<FnStmtAST>(fn_name, std::move(params), std::move(ret_type), std::move(body), false, tok.position());
    } else if (tok.text() == "for") {
      advance();
      // 解析for循环
      // 这里简化处理，实际可能需要更复杂的解析逻辑
      std::cerr << "Keyword not fully implemented: for at position " << current().position();
      throw std::runtime_error("Keyword not fully implemented: for");
    } else if (tok.text() == "if") {
      advance();
      // 解析if条件
      expect(TokenKind::Punctuation, "(");
      auto cond = parse_expr();
      expect(TokenKind::Punctuation, ")");
      // 解析then分支
      auto then_branch = parse_stmt();
      // 解析可选的else分支
      std::unique_ptr<StmtAST> else_branch = nullptr;
      if (current().kind() == TokenKind::Keyword && current().text() == "else") {
        advance();
        else_branch = parse_stmt();
      }
      // 如果if语句后面有分号，跳过分号（允许if语句后跟分号，但不要求）
      if (match(TokenKind::Punctuation, ";")) {
        advance();
      }
      return std::make_unique<IfStmtAST>(std::move(cond), std::move(then_branch), std::move(else_branch), tok.position());
    } else if (tok.text() == "impl") {
      advance();
      return parse_impl();
    } else if (tok.text() == "in") {
      std::cerr << "Keyword not supported: in at position " << current().position();
      throw std::runtime_error("Keyword not supported: in");
    } else if (tok.text() == "let") {
      advance();
      // 解析let变量声明
      bool is_mut = false;
      bool is_ref = false;
      bool is_addr_of = false;
      // 检查是否有修饰符 - 支持ref, mut, ref mut组合
      while (current().text() == "ref" || current().text() == "mut") {
        if (current().text() == "ref") {
          is_ref = true;
          advance();
        } else if (current().text() == "mut") {
          is_mut = true;
          advance();
        }
      }
      // 检查地址引用
      if (current().text() == "&") {
        is_addr_of = true;
        advance();
      }

      std::string name = expect_identifier();

      // 检查是否有类型注解
      std::string type_name = "";
      unique_ptr<TypeAST> type_ast = nullptr;
      if (current().kind() == TokenKind::Punctuation && current().text() == ":") {
        advance();
        // 保存类型注解
        type_ast = parse_type();
      }

      // 创建IdentPatternAST
      auto pattern = std::make_unique<IdentPatternAST>(name, is_mut, is_ref, is_addr_of, tok.position());
      
      // 设置类型
      if (type_ast) {
        pattern->type = std::move(type_ast);
      }

      // 检查是否有初始值
      if (current().kind() == TokenKind::Operator && current().text() == "=") {
        advance();
        auto value = parse_expr();
        expect(TokenKind::Punctuation, ";");
        return std::make_unique<LetStmtAST>(std::move(pattern), type_name, std::move(value), tok.position());
      } else if (!type_name.empty()) {
        // 有类型注解但没有初始值，如 let &x: i32;
        expect(TokenKind::Punctuation, ";");
        // 创建一个空的表达式作为值
        std::unique_ptr<ExprAST> value = nullptr;
        return std::make_unique<LetStmtAST>(std::move(pattern), type_name, std::move(value), tok.position());
      } else {
        // 既没有类型注解也没有初始值，这是错误的
        std::cerr << "Expected '=' or ':' after identifier in let statement at position " << current().position();
        throw std::runtime_error("Expected '=' or ':' after identifier in let statement");
      }
    } else if (tok.text() == "loop") {
      // 解析loop语句
      advance(); // 跳过loop关键字
      auto body = parse_stmt();
      return std::make_unique<LoopStmtAST>(std::move(body), tok.position());
    } else if (tok.text() == "match") {
      std::cerr << "Keyword not supported: match at position " << current().position();
      throw std::runtime_error("Keyword not supported: match");
    } else if (tok.text() == "mod") {
      std::cerr << "Keyword not supported: mod at position " << current().position();
      throw std::runtime_error("Keyword not supported: mod");
    } else if (tok.text() == "move") {
      std::cerr << "Keyword not supported: move at position " << current().position();
      throw std::runtime_error("Keyword not supported: move");
    } else if (tok.text() == "mut") {
      std::cerr << "Keyword not supported: mut at position " << current().position();
      throw std::runtime_error("Keyword not supported: mut");
    } else if (tok.text() == "pub") {
      std::cerr << "Keyword not supported: pub at position " << current().position();
      throw std::runtime_error("Keyword not supported: pub");
    } else if (tok.text() == "ref") {
      std::cerr << "Keyword not supported: ref at position " << current().position();
      throw std::runtime_error("Keyword not supported: ref");
    } else if (tok.text() == "return") {
      advance();
      // 解析return语句
      std::unique_ptr<ExprAST> value = nullptr;
      if (!match(TokenKind::Punctuation, ";")) {
        value = parse_expr();
        // 检查下一个token是否是右大括号，如果是，说明可能是函数的最后一个语句
        if (current().kind() == TokenKind::Punctuation && current().text() == "}") {
          // 如果是函数的最后一个语句，可以允许不加分号
          return std::make_unique<ReturnStmtAST>(tok.position(), std::move(value), false);
        } else {
          // 否则，必须要有分号
          expect(TokenKind::Punctuation, ";");
        }
      } else {
        // 如果是分号，需要跳过它
        advance();
      }
      return std::make_unique<ReturnStmtAST>(tok.position(), std::move(value), false);
    } else if (tok.text() == "self") {
      // self关键字可以作为表达式使用，如self.value、self.method()等
      // 这里我们将其作为左值表达式处理，类似于普通标识符
      auto lhs_expr = parse_value();
      
      // 检查是否是赋值语句
      if (match(TokenKind::Operator, "=") || match(TokenKind::Operator, "+=") ||
          match(TokenKind::Operator, "-=") || match(TokenKind::Operator, "*=") ||
          match(TokenKind::Operator, "/=") || match(TokenKind::Operator, "%=") ||
          match(TokenKind::Operator, "&=") || match(TokenKind::Operator, "|=") ||
          match(TokenKind::Operator, "^=") || match(TokenKind::Operator, "<<=") ||
          match(TokenKind::Operator, ">>=")) {
        std::string op = current().text();
        advance();
        auto value = parse_expr();
        expect(TokenKind::Punctuation, ";");
        return std::make_unique<AssignStmtAST>(std::move(lhs_expr), std::move(value), tok.position(), op);
      }
      
      // 检查是否是返回值（例如在函数末尾的隐式返回）
      if (match(TokenKind::Punctuation, "}")) {
        // self作为返回值，不需要分号
        return std::make_unique<ReturnStmtAST>(tok.position(), std::move(lhs_expr), true);
      }

      // 其他情况作为表达式语句处理
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ExprStmtAST>(std::move(lhs_expr), tok.position());
    } else if (tok.text() == "Self") {
      // Self关键字可以作为表达式使用，类似于self
      // 这里我们将其作为表达式处理，使用parse_factor来处理Self及其后续操作符
      auto lhs_expr = parse_cast_expr();
      
      // 检查是否是返回值（例如在函数末尾的隐式返回）
      if (match(TokenKind::Punctuation, "}")) {
        // Self作为返回值，不需要分号
        return std::make_unique<ReturnStmtAST>(tok.position(), std::move(lhs_expr), true);
      }
      
      // 其他情况作为表达式语句处理
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ExprStmtAST>(std::move(lhs_expr), tok.position());
    } else if (tok.text() == "static") {
      std::cerr << "Keyword not supported: static at position " << current().position();
      throw std::runtime_error("Keyword not supported: static");
    } else if (tok.text() == "struct") {
      advance();
      // 解析结构体名称
      std::string name = expect_identifier();
      expect(TokenKind::Punctuation, "{");

      std::vector<std::pair<std::string, std::string>> fields;

      // 解析结构体字段
      while (!match(TokenKind::Punctuation, "}")) {
        std::string field_name = expect_identifier();
        expect(TokenKind::Punctuation, ":");
        auto field_type_ast = parse_type();
        std::string field_type = field_type_ast->toString();
        fields.push_back(std::make_pair(field_name, field_type));

        // 如果不是最后一个字段，需要逗号分隔
        if (!match(TokenKind::Punctuation, "}")) {
          expect(TokenKind::Punctuation, ",");
          // 允许最后一个字段后有逗号
          if (match(TokenKind::Punctuation, "}")) {
            break;
          }
        }
      }

      expect(TokenKind::Punctuation, "}");
      return std::make_unique<StructStmtAST>(name, std::move(fields), tok.position());
    } else if (tok.text() == "super") {
      std::cerr << "Keyword not supported: super at position " << current().position();
      throw std::runtime_error("Keyword not supported: super");
    } else if (tok.text() == "trait") {
      std::cerr << "Keyword not supported: trait at position " << current().position();
      throw std::runtime_error("Keyword not supported: trait");
    } else if (tok.text() == "true" || tok.text() == "false") {
      // 将布尔字面量作为表达式处理
      auto expr = parse_expr();
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ExprStmtAST>(std::move(expr), tok.position());
    } else if (tok.text() == "type") {
      std::cerr << "Keyword not supported: type at position " << current().position();
      throw std::runtime_error("Keyword not supported: type");
    } else if (tok.text() == "unsafe") {
      std::cerr << "Keyword not supported: unsafe at position " << current().position();
      throw std::runtime_error("Keyword not supported: unsafe");
    } else if (tok.text() == "use") {
      std::cerr << "Keyword not supported: use at position " << current().position();
      throw std::runtime_error("Keyword not supported: use");
    } else if (tok.text() == "where") {
      std::cerr << "Keyword not supported: where at position " << current().position();
      throw std::runtime_error("Keyword not supported: where");
    } else if (tok.text() == "while") {
      advance();
      // 解析while循环
      expect(TokenKind::Punctuation, "(");
      auto cond = parse_expr();
      expect(TokenKind::Punctuation, ")");
      auto body = parse_stmt();
      // 如果while语句后面有分号，跳过分号（允许while语句后跟分号，但不要求）
      if (match(TokenKind::Punctuation, ";")) {
        advance();
      }
      return std::make_unique<WhileStmtAST>(std::move(cond), std::move(body), tok.position());
    } else {
      std::cerr << "Unknow Keyword: " << tok.text() << " at position " << tok.position() << '\n';
      throw std::runtime_error("Unknow Keyword");
    }
  } else if (tok.kind() == TokenKind::Identifier || tok.kind() == TokenKind::Operator) {
    // 使用parse_value解析值表达式
    auto lhs_expr = parse_value();
    
    // 检查是否是赋值语句
    if (match(TokenKind::Operator, "=") || match(TokenKind::Operator, "+=") ||
        match(TokenKind::Operator, "-=") || match(TokenKind::Operator, "*=") ||
        match(TokenKind::Operator, "/=") || match(TokenKind::Operator, "%=") ||
        match(TokenKind::Operator, "&=") || match(TokenKind::Operator, "|=") ||
        match(TokenKind::Operator, "^=") || match(TokenKind::Operator, "<<=") ||
        match(TokenKind::Operator, ">>=")) {
      std::string op = current().text();
      advance();
      auto value = parse_expr();
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<AssignStmtAST>(std::move(lhs_expr), std::move(value), tok.position(), op);
    }

    // 其他情况作为表达式语句处理
    expect(TokenKind::Punctuation, ";");
    return std::make_unique<ExprStmtAST>(std::move(lhs_expr), tok.position());
  } else if (tok.kind() == TokenKind::Punctuation && tok.text() == "{") {
    // 处理代码块语句
    return parse_block();
  }

  // 默认情况下不到达这里
  std::cerr << "Unexpected token in parse_stmt: " << tok.text() << " at position " << tok.position() << std::endl;
  throw std::runtime_error("Unexpected token in parse_stmt");
  return nullptr;
}

// 解析值表达式，可以作为左值或右值
std::unique_ptr<ExprAST> Parser::parse_value() {
  // 先解析基本表达式
  auto lhs = parse_cast_expr();
  
  // 然后处理链式访问，包括函数调用、数组索引和成员访问
  while (true) {
    // 检查函数调用
    if (match(TokenKind::Punctuation, "(")) {
      advance();
      std::vector<std::unique_ptr<ExprAST>> args;
      if (!match(TokenKind::Punctuation, ")")) {
        while (true) {
          args.push_back(parse_expr());
          if (match(TokenKind::Punctuation, ",")) {
            advance();
            // 允许最后一个参数后有逗号
            if (match(TokenKind::Punctuation, ")")) {
              break;
            }
          } else if (match(TokenKind::Punctuation, ")")) {
            break;
          } else {
            std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
            throw std::runtime_error("Expected ',' or ')' in function arguments");
          }
        }
      }
      expect(TokenKind::Punctuation, ")");
      
      // 创建函数调用表达式
      if (auto var_expr = dynamic_cast<VariableExprAST*>(lhs.get())) {
        // 普通函数调用
        lhs = std::make_unique<CallExprAST>(var_expr->name, lhs->position(), std::move(args));
      } else if (auto call_expr = dynamic_cast<CallExprAST*>(lhs.get())) {
        // 如果lhs已经是一个函数调用，那么这是一个连续的函数调用
        // 例如 foo().goo()，这里我们需要将foo()作为对象，goo作为方法名
        // 但是当前AST设计不支持这种情况，我们需要修改AST或者使用另一种方式
        // 暂时我们使用成员方法调用的方式来处理
        std::cerr << "Consecutive function calls like foo().goo() are not directly supported at position " << current().position() << std::endl;
        throw std::runtime_error("Consecutive function calls like foo().goo() are not directly supported");
      } else {
        // 对于其他类型的表达式，我们无法直接创建函数调用
        std::cerr << "Direct function call on this expression type is not supported at position " << current().position() << std::endl;
        throw std::runtime_error("Direct function call on this expression type is not supported");
      }
      continue;
    }
    
    // 检查数组索引
    if (match(TokenKind::Punctuation, "[")) {
      advance();
      auto index = parse_expr();
      expect(TokenKind::Punctuation, "]");
      lhs = std::make_unique<ArrayIndexExprAST>(lhs->position(), std::move(lhs), std::move(index));
      continue;
    }
    
    // 检查成员访问
    if (match(TokenKind::Punctuation, ".")) {
      advance();
      if (current().kind() != TokenKind::Identifier && 
          !(current().kind() == TokenKind::Keyword && current().text() == "self")) {
        std::cerr << "Expected identifier after '.' at position " << current().position() << std::endl;
        throw std::runtime_error("Expected identifier after '.'");
      }
      string member_name = current().text();
      advance();
      
      // 检查是否是方法调用
      if (match(TokenKind::Punctuation, "(")) {
        advance();
        std::vector<std::unique_ptr<ExprAST>> args;
        if (!match(TokenKind::Punctuation, ")")) {
          while (true) {
            args.push_back(parse_expr());
            if (match(TokenKind::Punctuation, ",")) {
              advance();
              // 允许最后一个参数后有逗号
              if (match(TokenKind::Punctuation, ")")) {
                break;
              }
            } else if (match(TokenKind::Punctuation, ")")) {
              break;
            } else {
              std::cerr << "Expected ',' or ')' in function arguments at position " << current().position() << std::endl;
              throw std::runtime_error("Expected ',' or ')' in function arguments");
            }
          }
        }
        expect(TokenKind::Punctuation, ")");
        
        // 创建成员方法调用表达式
        lhs = std::make_unique<CallExprAST>(member_name, lhs->position(), std::move(lhs), std::move(args));
      } else {
        // 普通成员访问
        lhs = std::make_unique<MemberAccessExprAST>(lhs->position(), std::move(lhs), member_name);
      }
      continue;
    }
    
    // 如果没有函数调用、数组索引、成员访问或类型转换，退出循环
    break;
  }
  
  return lhs;
}

// 解析代码块
std::unique_ptr<BlockStmtAST> Parser::parse_block() {
  expect(TokenKind::Punctuation, "{");
  std::vector<std::unique_ptr<StmtAST>> statements;

  while (!match(TokenKind::Punctuation, "}") && !match(TokenKind::Eof)) {
    // 处理嵌套代码块
    if (match(TokenKind::Punctuation, "{")) {
      // 直接解析嵌套代码块
      auto nested_block = parse_block();
      statements.push_back(std::move(nested_block));
      continue;
    }

    // 先尝试解析表达式
    if (current().kind() == TokenKind::Identifier || 
      current().kind() == TokenKind::Number ||
      current().kind() == TokenKind::Float ||
      current().kind() == TokenKind::String ||
      current().kind() == TokenKind::Punctuation ||
      current().kind() == TokenKind::Operator ||
      (current().kind() == TokenKind::Keyword && (current().text() == "true" || current().text() == "false" ||
                   current().text() == "self" || current().text() == "Self" ||
                   current().text() == "loop"))) {

      // 保存当前位置，以便回退
      int saved_pos = pos;

      // 尝试解析表达式
      std::unique_ptr<ExprAST> expr = nullptr;
      
      // 特殊处理if表达式
      if (current().kind() == TokenKind::Keyword && current().text() == "if") {
        // 尝试解析if表达式
        int saved_pos_if = pos;
        expr = parse_if_expr();
        // 如果解析失败，回退
        if (!expr) {
          pos = saved_pos_if;
          expr = nullptr;
        }
      }
      
      // 特殊处理loop表达式
      if (!expr && current().kind() == TokenKind::Keyword && current().text() == "loop") {
        // 尝试解析loop表达式
        int saved_pos_loop = pos;
        expr = parse_loop_expr();
        // 如果解析失败，回退
        if (!expr) {
          pos = saved_pos_loop;
          expr = nullptr;
        }
      }
      
      // 如果不是if表达式或if表达式解析失败，尝试解析普通表达式
      if (!expr) {
        expr = parse_expr();
      }
      
      // 如果解析失败，回退并按常规语句处理
      if (!expr) {
        pos = saved_pos;
        auto stmt = parse_stmt();
        statements.push_back(std::move(stmt));
        continue;
      }
      
      // 检查是否是if表达式
      bool is_if_expr = dynamic_cast<IfExprAST*>(expr.get()) != nullptr;
      
      // 检查是否是loop表达式
      bool is_loop_expr = dynamic_cast<LoopExprAST*>(expr.get()) != nullptr;

      // 检查表达式后面是否是分号或闭合大括号
      if (match(TokenKind::Punctuation, ";")) {
        // 带分号的表达式语句
        advance();
        statements.push_back(std::make_unique<ExprStmtAST>(std::move(expr), current().position()));
        continue;
      }

      if (match(TokenKind::Punctuation, "}")) {
        // 不带分号且紧跟右大括号，视为隐式返回
        auto return_stmt = std::make_unique<ReturnStmtAST>(expr->position(), std::move(expr), true);
        statements.push_back(std::move(return_stmt));
        break;
      }

      if (is_if_expr || is_loop_expr) {
        // 允许if/loop表达式作为语句使用，即使没有分号
        statements.push_back(std::make_unique<ExprStmtAST>(std::move(expr), current().position()));
        continue;
      }

      // 其他情况，回退并按常规语句重新解析
      pos = saved_pos;
      auto stmt = parse_stmt();
      statements.push_back(std::move(stmt));
    } else {
      // 不是表达式，按常规方式解析语句
      auto stmt = parse_stmt();
      statements.push_back(std::move(stmt));
    }
  }

  expect(TokenKind::Punctuation, "}");
  return std::make_unique<BlockStmtAST>(std::move(statements), current().position());
}

// 解析 impl 块
std::unique_ptr<ImplStmtAST> Parser::parse_impl() {
  // 检查是否是 trait 实现 (impl Trait for Type)
  string trait_name = "";
  string type_name;
  
  // 先读取第一个标识符，可能是类型名或 trait 名
  string first_name = expect_identifier();
  
  // 检查是否有 "for" 关键字，但不作为trait实现处理
  if (match(TokenKind::Keyword, "for")) {
    // 跳过"for"关键字，将后面的标识符作为类型名
    advance();
    type_name = expect_identifier();
  } else {
    type_name = first_name;
  }
  
  expect(TokenKind::Punctuation, "{");
  
  std::vector<std::unique_ptr<FnStmtAST>> methods;
  
  // 解析方法列表
  while (!match(TokenKind::Punctuation, "}")) {
    // 检查是否是 const 方法
    bool is_const = false;
    if (match(TokenKind::Keyword, "const")) {
      advance();
      is_const = true;
    }
    
    expect(TokenKind::Keyword, "fn");
    string method_name = expect_identifier();
    expect(TokenKind::Punctuation, "(");
    auto params = parse_fn_params();
    auto return_type = parse_fn_return_type();
    
    // 解析方法体
    auto body = parse_block();
    
    methods.push_back(std::make_unique<FnStmtAST>(method_name, std::move(params), std::move(return_type), 
                                                std::move(body), is_const, current().position()));
  }
  
  expect(TokenKind::Punctuation, "}");
  return std::make_unique<ImplStmtAST>(type_name, trait_name, std::move(methods), current().position());
}
