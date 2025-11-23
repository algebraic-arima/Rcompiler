#include "parser.h"

std::unique_ptr<BlockStmtAST> Parser::parse_program() {
  std::vector<std::unique_ptr<StmtAST> > stmts;
  while (!match(TokenKind::Eof)) {
    auto stmt = parse_stmt();
    if (!stmt) {
      std::cerr << "Invalid statement." << std::endl;
      std::exit(1);
    }
    stmts.push_back(std::move(stmt));
  }
  return std::make_unique<BlockStmtAST>(std::move(stmts), 0);
}

//parse比较表达式
std::unique_ptr<ExprAST> Parser::parse_comparison() {
  auto lhs = parse_term();
  while (match(TokenKind::Comparison, "==") || match(TokenKind::Comparison, "!=") ||
         match(TokenKind::Comparison, "<") || match(TokenKind::Comparison, "<=") ||
         match(TokenKind::Comparison, ">") || match(TokenKind::Comparison, ">=")) {
    std::string op = current().text();
    size_t pos_ = current().position();
    advance();
    auto rhs = parse_term();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}

//parse一个低级优先运算
std::unique_ptr<ExprAST> Parser::parse_expr() {
  auto lhs = parse_comparison();
  while (match(TokenKind::Operator, "+") || match(TokenKind::Operator, "-") || 
         match(TokenKind::Operator, "&") || match(TokenKind::Operator, "|") || 
         match(TokenKind::Operator, "^") || match(TokenKind::Operator, "<<") || 
         match(TokenKind::Operator, ">>")) {
    std::string op = current().text();
    size_t pos_ = current().position(); // 记录运算符位置
    advance();

    // 根据运算符类型决定优先级
    if (op == "+" || op == "-") {
      auto rhs = parse_comparison();
      lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
    } 
    else if (op == "&" || op == "|" || op == "^") {
      auto rhs = parse_comparison();
      lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
    }
    else if (op == "<<" || op == ">>") {
      auto rhs = parse_comparison();
      lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
    }
  }
  return lhs;
}

//parse一个中优先级运算
std::unique_ptr<ExprAST> Parser::parse_term() {
  auto lhs = parse_factor();
  while (match(TokenKind::Operator, "*") || match(TokenKind::Operator, "/")) {
    std::string op = current().text();
    size_t pos_ = current().position(); // 记录运算符位置
    advance();
    auto rhs = parse_factor();
    lhs = std::make_unique<BinaryExprAST>(op, pos_, std::move(lhs), std::move(rhs));
  }
  return lhs;
}


//parse一个基本运算单元
std::unique_ptr<ExprAST> Parser::parse_factor() {
  size_t pos_ = current().position(); // 定义位置变量

  // 处理一元运算符
  if (match(TokenKind::Operator, "-") || match(TokenKind::Operator, "!")) {
    std::string op = current().text();
    advance();
    auto operand = parse_factor();
    return std::make_unique<UnaryExprAST>(op, pos_, std::move(operand));
  }

  // 处理字面量
  if (match(TokenKind::Number)) {
    int val = std::stoi(current().text());
    advance();
    return std::make_unique<NumberExprAST>(val, pos_);
  }
  if (match(TokenKind::Float)) {
    double val = std::stod(current().text());
    advance();
    return std::make_unique<FloatExprAST>(val, pos_);
  }
  if (match(TokenKind::String)) {
    std::string val = current().text();
    advance();
    return std::make_unique<StringExprAST>(val, pos_);
  }

  // 处理变量和函数调用
  if (match(TokenKind::Identifier) || match(TokenKind::Keyword, "true") || match(TokenKind::Keyword, "false")) {
    std::string name = current().text();
    advance();

    // 检查是否是函数调用
    if (match(TokenKind::Punctuation, "(")) {
      advance();
      std::vector<std::unique_ptr<ExprAST> > args;
      if (!match(TokenKind::Punctuation, ")")) {
        while (true) {
          args.push_back(parse_expr());
          if (match(TokenKind::Punctuation, ",")) {
            advance();
          } else if (match(TokenKind::Punctuation, ")")) {
            break;
          } else {
            std::cerr << "Expected ',' or ')' in function arguments" << std::endl;
            std::exit(1);
          }
        }
      }
      expect(TokenKind::Punctuation, ")");

      // 检查函数调用后是否有数组索引和成员访问，支持链式访问
      std::unique_ptr<ExprAST> expr = std::make_unique<CallExprAST>(name, pos_, std::move(args));

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
          if (current().kind() != TokenKind::Identifier) {
            std::cerr << "Expected identifier after '.'" << std::endl;
            std::exit(1);
          }
          string member_name = current().text();
          advance();
          
          // 检查是否是方法调用，如 func().method()
          if (match(TokenKind::Punctuation, "(")) {
            advance();
            std::vector<std::unique_ptr<ExprAST>> args;
            if (!match(TokenKind::Punctuation, ")")) {
              while (true) {
                args.push_back(parse_expr());
                if (match(TokenKind::Punctuation, ",")) {
                  advance();
                } else if (match(TokenKind::Punctuation, ")")) {
                  break;
                } else {
                  std::cerr << "Expected ',' or ')' in function arguments" << std::endl;
                  std::exit(1);
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
          std::cerr << "Expected identifier after '.'" << std::endl;
          std::exit(1);
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
              } else if (match(TokenKind::Punctuation, ")")) {
                break;
              } else {
                std::cerr << "Expected ',' or ')' in function arguments" << std::endl;
                std::exit(1);
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
          std::cerr << "Expected identifier after '.'" << std::endl;
          std::exit(1);
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
        std::cerr << "Expected ',' or ']' in array elements" << std::endl;
        std::exit(1);
      }
    }
    
    return std::make_unique<ArrayExprAST>(std::move(elements), pos_);
  }

  std::cerr << "Invalid factor: " << current().text() << std::endl;
  std::exit(1);
}

//parse一个statement
std::unique_ptr<StmtAST> Parser::parse_stmt() {
  Token tok = current();
  if (tok.kind() == TokenKind::Keyword) {
    if (tok.text() == "as") {
      std::cerr << "Unexpected keyword: as\n";
      std::exit(1);
    } else if (tok.text() == "break") {
      advance();
      return std::make_unique<BreakStmtAST>(tok.position());
    } else if (tok.text() == "const") {
      advance();
      if (current().kind() == TokenKind::Keyword && current().text() == "fn") {
        advance();
        std::string fn_name = expect_identifier();
        expect(TokenKind::Punctuation, "(");
        auto params = parse_fn_params();
        std::string ret_type = parse_fn_return_type();
        // 可解析返回类型、泛型等
        // 解析函数体
        auto body = parse_block();
        // 返回 const 函数节点
        return std::make_unique<FnStmtAST>(fn_name, params, ret_type, std::move(body), true, tok.position());
      } else {
        // 否则是 const 常量
        std::string name = expect_identifier();
        expect(TokenKind::Punctuation, ":");
        std::string type = parse_type();
        expect(TokenKind::Operator, "=");
        auto value = parse_expr();
        expect(TokenKind::Punctuation, ";");
        return std::make_unique<ConstStmtAST>(name, type, std::move(value), tok.position());
      }
    } else if (tok.text() == "continue") {
      advance();
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ContinueStmtAST>(tok.position());
    } else if (tok.text() == "crate") {
      std::cerr << "Keyword not supported: crate";
      std::exit(1);
    } else if (tok.text() == "dyn") {
      std::cerr << "Keyword not supported: dyn";
      std::exit(1);
    } else if (tok.text() == "else") {
      std::cerr << "Unexpected keyword: else (must be part of if statement)";
      std::exit(1);
    } else if (tok.text() == "enum") {
      std::cerr << "Keyword not supported: enum";
      std::exit(1);
    } else if (tok.text() == "exit") {
      advance();
      // 解析exit语句，可以带一个可选的退出码表达式
      std::unique_ptr<ExprAST> exit_code = nullptr;
      if (current().kind() != TokenKind::Punctuation || current().text() != ";") {
        exit_code = parse_expr();
      }
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<ExitStmtAST>(tok.position(), std::move(exit_code));
    } else if (tok.text() == "false") {
      std::cerr << "Unexpected keyword: false (should be used as a literal value)";
      std::exit(1);
    } else if (tok.text() == "fn") {
      advance();
      std::string fn_name = expect_identifier();
      expect(TokenKind::Punctuation, "(");
      auto params = parse_fn_params();
      std::string ret_type = parse_fn_return_type();
      // 可解析返回类型、泛型等
      // 解析函数体
      auto body = parse_block();
      // 返回函数节点
      return std::make_unique<FnStmtAST>(fn_name, params, ret_type, std::move(body), false, tok.position());
    } else if (tok.text() == "for") {
      advance();
      // 解析for循环
      // 这里简化处理，实际可能需要更复杂的解析逻辑
      std::cerr << "Keyword not fully implemented: for";
      std::exit(1);
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
      return std::make_unique<IfStmtAST>(std::move(cond), std::move(then_branch), std::move(else_branch), tok.position());
    } else if (tok.text() == "false") {
      std::cerr << "Unexpected keyword: false (should be used as a literal value)";
      std::exit(1);
    } else if (tok.text() == "impl") {
      advance();
      return parse_impl();
    } else if (tok.text() == "in") {
      std::cerr << "Keyword not supported: in";
      std::exit(1);
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
      if (current().kind() == TokenKind::Punctuation && current().text() == ":") {
        advance();
        // 保存类型注解
        type_name = parse_type();
      }

      // 创建IdentPatternAST
      auto pattern = std::make_unique<IdentPatternAST>(name, is_mut, is_ref, is_addr_of, tok.position());

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
        std::cerr << "Expected '=' or ':' after identifier in let statement";
        std::exit(1);
      }
    } else if (tok.text() == "loop") {
      std::cerr << "Keyword not supported: loop";
      std::exit(1);
    } else if (tok.text() == "match") {
      std::cerr << "Keyword not supported: match";
      std::exit(1);
    } else if (tok.text() == "mod") {
      std::cerr << "Keyword not supported: mod";
      std::exit(1);
    } else if (tok.text() == "move") {
      std::cerr << "Keyword not supported: move";
      std::exit(1);
    } else if (tok.text() == "mut") {
      std::cerr << "Keyword not supported: mut";
      std::exit(1);
    } else if (tok.text() == "pub") {
      std::cerr << "Keyword not supported: pub";
      std::exit(1);
    } else if (tok.text() == "ref") {
      std::cerr << "Keyword not supported: ref";
      std::exit(1);
    } else if (tok.text() == "return") {
      advance();
      // 解析return语句
      std::unique_ptr<ExprAST> value = nullptr;
      if (!match(TokenKind::Punctuation, ";")) {
        value = parse_expr();
        expect(TokenKind::Punctuation, ";");
      }
      return std::make_unique<ReturnStmtAST>(tok.position(), std::move(value));
    } else if (tok.text() == "self") {
      std::cerr << "Unexpected keyword: self";
      std::exit(1);
    } else if (tok.text() == "Self") {
      std::cerr << "Unexpected keyword: Self";
      std::exit(1);
    } else if (tok.text() == "static") {
      std::cerr << "Keyword not supported: static";
      std::exit(1);
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
        std::string field_type = parse_type();
        fields.push_back(std::make_pair(field_name, field_type));

        // 如果不是最后一个字段，需要逗号分隔
        if (!match(TokenKind::Punctuation, "}")) {
          expect(TokenKind::Punctuation, ",");
        }
      }

      expect(TokenKind::Punctuation, "}");
      return std::make_unique<StructStmtAST>(name, std::move(fields), tok.position());
    } else if (tok.text() == "super") {
      std::cerr << "Keyword not supported: super";
      std::exit(1);
    } else if (tok.text() == "trait") {
      std::cerr << "Keyword not supported: trait";
      std::exit(1);
    } else if (tok.text() == "true") {
      std::cerr << "Unexpected keyword: true (should be used as a literal value)";
      std::exit(1);
    } else if (tok.text() == "type") {
      std::cerr << "Keyword not supported: type";
      std::exit(1);
    } else if (tok.text() == "unsafe") {
      std::cerr << "Keyword not supported: unsafe";
      std::exit(1);
    } else if (tok.text() == "use") {
      std::cerr << "Keyword not supported: use";
      std::exit(1);
    } else if (tok.text() == "where") {
      std::cerr << "Keyword not supported: where";
      std::exit(1);
    } else if (tok.text() == "while") {
      advance();
      // 解析while循环
      expect(TokenKind::Punctuation, "(");
      auto cond = parse_expr();
      expect(TokenKind::Punctuation, ")");
      auto body = parse_stmt();
      return std::make_unique<WhileStmtAST>(std::move(cond), std::move(body), tok.position());
    } else {
      std::cerr << "Unknow Keyword: " << tok.text() << '\n';
      std::exit(1);
    }
  } else if (tok.kind() == TokenKind::Identifier) {
    // 使用parse_value解析值表达式
    auto lhs_expr = parse_value();
    
    // 检查是否是赋值语句
    if (match(TokenKind::Operator, "=")) {
      advance();
      auto value = parse_expr();
      expect(TokenKind::Punctuation, ";");
      return std::make_unique<AssignStmtAST>(std::move(lhs_expr), std::move(value), tok.position());
    }

    // 其他情况作为表达式语句处理
    expect(TokenKind::Punctuation, ";");
    return std::make_unique<ExprStmtAST>(std::move(lhs_expr), tok.position());
  }

  // 默认情况下不到达这里
  std::cerr << "Unexpected token in parse_stmt: " << tok.text() << std::endl;
  std::exit(1);
  return nullptr;
}

// 解析值表达式，可以作为左值或右值
std::unique_ptr<ExprAST> Parser::parse_value() {
  // 先解析基本表达式
  auto lhs = parse_factor();
  
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
          } else if (match(TokenKind::Punctuation, ")")) {
            break;
          } else {
            std::cerr << "Expected ',' or ')' in function arguments" << std::endl;
            std::exit(1);
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
        std::cerr << "Consecutive function calls like foo().goo() are not directly supported" << std::endl;
        std::exit(1);
      } else {
        // 对于其他类型的表达式，我们无法直接创建函数调用
        std::cerr << "Direct function call on this expression type is not supported" << std::endl;
        std::exit(1);
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
      if (current().kind() != TokenKind::Identifier) {
        std::cerr << "Expected identifier after '.'" << std::endl;
        std::exit(1);
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
            } else if (match(TokenKind::Punctuation, ")")) {
              break;
            } else {
              std::cerr << "Expected ',' or ')' in function arguments" << std::endl;
              std::exit(1);
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
    
    // 如果没有函数调用、数组索引或成员访问，退出循环
    break;
  }
  
  return lhs;
}

// 解析代码块
std::unique_ptr<BlockStmtAST> Parser::parse_block() {
  expect(TokenKind::Punctuation, "{");
  std::vector<std::unique_ptr<StmtAST>> statements;

  while (!match(TokenKind::Punctuation, "}") && !match(TokenKind::Eof)) {
    auto stmt = parse_stmt();
    statements.push_back(std::move(stmt));
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
  
  // 检查是否有 "for" 关键字，表示 trait 实现
  if (match(TokenKind::Keyword, "for")) {
    advance();
    trait_name = first_name;
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
    string return_type = parse_fn_return_type();
    
    // 解析方法体
    auto body = parse_block();
    
    methods.push_back(std::make_unique<FnStmtAST>(method_name, params, return_type, 
                                                std::move(body), is_const, current().position()));
  }
  
  expect(TokenKind::Punctuation, "}");
  return std::make_unique<ImplStmtAST>(type_name, trait_name, std::move(methods), current().position());
}
