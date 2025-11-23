#ifndef AST_H
#define AST_H
#include "lexer.h"
#include <iostream>
using std::string;
using std::unique_ptr;
using std::make_unique;
constexpr int DumpSpaceNumber = 4;


//--------------------------------------------------------------
class ExprAST {
public:
  size_t pos;

  ExprAST() = default;
  ExprAST(size_t);

  virtual void dump(int indent = 0) const{}
  size_t position() {
    return pos;
  }
  virtual ~ExprAST() = default;
};

class NumberExprAST : public ExprAST {
public:
  int value;

  NumberExprAST(int, size_t);
  void dump(int indent) const override;
};

class FloatExprAST : public ExprAST {
public:
  double value;

  FloatExprAST(double, size_t);
  void dump(int indent) const override;
};

class VariableExprAST : public ExprAST {
public:
  string name;

  VariableExprAST(const string &, size_t);
  void dump(int indent) const override;
};

class StringExprAST : public ExprAST {
public:
  string str;

  StringExprAST(const string &, size_t);
  void dump(int indent) const override;
};

// 一元运算
class UnaryExprAST : public ExprAST {
public:
  string op;
  unique_ptr<ExprAST> expr;

  UnaryExprAST(const string &, size_t, unique_ptr<ExprAST>);
  void dump(int indent) const override;
};

// 二元运算
class BinaryExprAST : public ExprAST {
public:
  string op;
  unique_ptr<ExprAST> left_expr;
  unique_ptr<ExprAST> right_expr;

  BinaryExprAST(const string &, size_t, unique_ptr<ExprAST>, unique_ptr<ExprAST>);
  void dump(int indent) const override;
};

// 数组索引
class ArrayIndexExprAST : public ExprAST {
public:
  unique_ptr<ExprAST> array_expr;
  unique_ptr<ExprAST> index_expr;

  ArrayIndexExprAST(size_t, unique_ptr<ExprAST>, unique_ptr<ExprAST>);
  void dump(int indent) const override;
};

// 结构体成员访问
class MemberAccessExprAST : public ExprAST {
public:
  unique_ptr<ExprAST> struct_expr;
  string member_name;

  MemberAccessExprAST(size_t, unique_ptr<ExprAST>, const string&);
  void dump(int indent) const override;
};

// 函数
class CallExprAST : public ExprAST {
public:
  string call;
  std::vector<unique_ptr<ExprAST> > args;
  // 成员方法调用的对象表达式，如果是普通函数调用则为nullptr
  unique_ptr<ExprAST> object_expr;

  // 普通函数调用构造函数
  CallExprAST(const string &, size_t, std::vector<unique_ptr<ExprAST> >);
  // 成员方法调用构造函数
  CallExprAST(const string &, size_t, unique_ptr<ExprAST>, std::vector<unique_ptr<ExprAST> >);
  void dump(int indent) const override;
};

// struct
class StructExprAST : public ExprAST {
public:
  std::string name;
  std::vector<std::pair<string, unique_ptr<ExprAST> > > fields;

  StructExprAST(const string &, std::vector<std::pair<std::string, unique_ptr<ExprAST> > >, size_t);
  void dump(int indent) const override;
};

class ArrayExprAST : public ExprAST {
public:
  std::vector<unique_ptr<ExprAST>> elements;
  // [element; count]
  unique_ptr<ExprAST> element;
  unique_ptr<ExprAST> count;
  bool is_repeated;

  // 普通构造
  ArrayExprAST(std::vector<unique_ptr<ExprAST>> elems, size_t pos_);
  // 重复元素构造
  ArrayExprAST(unique_ptr<ExprAST> elem, unique_ptr<ExprAST> cnt, size_t pos_);

  void dump(int indent = 0) const override;
};
//--------------------------------------------------------------
class PatternAST {
public:
  size_t pos;
  PatternAST() = default;
  PatternAST(size_t);
  virtual void dump(int indent = 0) const {}
  virtual ~PatternAST() = default;
};

class IdentPatternAST : public PatternAST {
public:
  string name;
  bool is_mut;
  bool is_ref;
  bool is_addr_of; // &
  IdentPatternAST(const string &, bool, bool, bool, size_t);
  void dump(int indent) const override;
};
//--------------------------------------------------------------
class StmtAST {
public:
  size_t pos;

  StmtAST() = default;
  StmtAST(size_t);

  virtual void dump(int indent = 0) const {}
  size_t position() {
    return pos;
  }
  virtual ~StmtAST() = default;
};

class ExprStmtAST : public StmtAST {
public:
  unique_ptr<ExprAST> expr;

  ExprStmtAST(unique_ptr<ExprAST>, size_t);
  void dump(int indent) const override;
};

class LetStmtAST : public StmtAST {
public:
  unique_ptr<PatternAST> pattern;
  string type; // 类型注解
  unique_ptr<ExprAST> value;

  LetStmtAST(unique_ptr<PatternAST>, const string&, unique_ptr<ExprAST>, size_t);
  void dump(int indent) const override;
};

class AssignStmtAST : public StmtAST {
public:
  unique_ptr<ExprAST> lhs_expr;  // 左侧表达式，可以是变量、数组索引等
  unique_ptr<ExprAST> value;      // 右侧值

  AssignStmtAST(unique_ptr<ExprAST>, unique_ptr<ExprAST>, size_t);
  void dump(int indent) const override;
};

class IfStmtAST : public StmtAST {
public:
  unique_ptr<ExprAST> cond;
  unique_ptr<StmtAST> then_branch;
  unique_ptr<StmtAST> else_branch;

  IfStmtAST(unique_ptr<ExprAST>, unique_ptr<StmtAST>, unique_ptr<StmtAST>, size_t);
  void dump(int indent) const override;
};

class WhileStmtAST : public StmtAST {
public:
  unique_ptr<ExprAST> cond;
  unique_ptr<StmtAST> body;

  WhileStmtAST(unique_ptr<ExprAST>, unique_ptr<StmtAST>, size_t);
  void dump(int indent) const override;
};

class ForStmtAST : public StmtAST {
public:
  unique_ptr<StmtAST> init;
  unique_ptr<ExprAST> cond;
  unique_ptr<StmtAST> incr;
  unique_ptr<StmtAST> body;

  ForStmtAST(unique_ptr<StmtAST>, unique_ptr<ExprAST>, unique_ptr<StmtAST>, unique_ptr<StmtAST>, size_t);
  void dump(int indent) const override;
};

class BlockStmtAST : public StmtAST {
public:
  std::vector<unique_ptr<StmtAST> > statements;

  BlockStmtAST(std::vector<unique_ptr<StmtAST> >, size_t);

  void dump(int indent) const override;
};

class FnStmtAST : public StmtAST {
public:
  string name;
  std::vector<std::pair<string, string>> params;
  string return_type;
  bool is_const;
  unique_ptr<BlockStmtAST> body;

  FnStmtAST(const string &,const std::vector<std::pair<string, string>>&, const string &, unique_ptr<BlockStmtAST>, bool, size_t);

  void dump(int indent) const override;
};

class ConstStmtAST : public StmtAST {
public:
  string name;
  string type;
  unique_ptr<ExprAST> value;
  ConstStmtAST(const string &, const string &, unique_ptr<ExprAST>, size_t);
  void dump(int indent) const override;
};

class StaticStmtAST : public StmtAST {
public:
  string name;
  string type;
  unique_ptr<ExprAST> value;
  bool is_mut;
  StaticStmtAST(const string &, const string &, unique_ptr<ExprAST>, bool, size_t);
  void dump(int indent) const override;
};

class ReturnStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> value;

  ReturnStmtAST(size_t, unique_ptr<ExprAST>);

  void dump(int indent) const override;
};

class BreakStmtAST : public StmtAST {
public:
  BreakStmtAST(size_t);

  void dump(int indent) const override;
};

class ContinueStmtAST : public StmtAST {
public:
  ContinueStmtAST(size_t);

  void dump(int indent) const override;
};

class ExitStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> value;
  
  ExitStmtAST(size_t, std::unique_ptr<ExprAST>);
  
  void dump(int indent) const override;
};

class StructStmtAST : public StmtAST {
public:
  string name;
  std::vector<std::pair<string, string>> fields;

  StructStmtAST(const string &, std::vector<std::pair<string, string>>, size_t);
  void dump(int indent) const override;
};

class ImplStmtAST : public StmtAST {
public:
  string type_name;           // 要实现的类型名
  string trait_name;          // 如果是 trait 实现，则为 trait 名称，否则为空
  std::vector<std::unique_ptr<FnStmtAST>> methods;  // 方法列表

  ImplStmtAST(const string&, const string&, std::vector<std::unique_ptr<FnStmtAST>>, size_t);
  void dump(int indent) const override;
};
#endif //AST_H
