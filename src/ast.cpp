#include "ast.h"

void dump_space(int indent) {
  while (indent--) std::cout << ' ';
}

// PrimitiveTypeAST 实现
PrimitiveTypeAST::PrimitiveTypeAST(const string& name) : name(name) {}

void PrimitiveTypeAST::dump(int indent) const {
  std::cout << string(indent, ' ') << "PrimitiveType: " << name << std::endl;
}

string PrimitiveTypeAST::toString() const {
  return name;
}

// ArrayTypeAST 实现
ArrayTypeAST::ArrayTypeAST(unique_ptr<TypeAST> elem, unique_ptr<ExprAST> size)
    : element_type(std::move(elem)), size_expr(std::move(size)) {}

void ArrayTypeAST::dump(int indent) const {
  std::cout << string(indent, ' ') << "ArrayType:" << std::endl;
  std::cout << string(indent + DumpSpaceNumber, ' ') << "ElementType:" << std::endl;
  element_type->dump(indent + DumpSpaceNumber * 2);
  std::cout << string(indent + DumpSpaceNumber, ' ') << "SizeExpression:" << std::endl;
  size_expr->dump(indent + DumpSpaceNumber * 2);
}

string ArrayTypeAST::toString() const {
  return "[" + element_type->toString() + "; " + "<expr>" + "]";
}

// ReferenceTypeAST 实现
ReferenceTypeAST::ReferenceTypeAST(unique_ptr<TypeAST> ref_type, bool mut)
    : referenced_type(std::move(ref_type)), is_mutable(mut) {}

void ReferenceTypeAST::dump(int indent) const {
  std::cout << string(indent, ' ') << "ReferenceType (" << (is_mutable ? "mutable" : "immutable") << "):" << std::endl;
  referenced_type->dump(indent + DumpSpaceNumber);
}

string ReferenceTypeAST::toString() const {
  string result = "&";
  if (is_mutable) result += "mut ";
  result += referenced_type->toString();
  return result;
}

// TupleTypeAST 实现
TupleTypeAST::TupleTypeAST(std::vector<unique_ptr<TypeAST>> elems) 
    : elements(std::move(elems)) {}

void TupleTypeAST::dump(int indent) const {
  std::cout << string(indent, ' ') << "TupleType:" << std::endl;
  for (size_t i = 0; i < elements.size(); ++i) {
    std::cout << string(indent + DumpSpaceNumber, ' ') << "Element " << i << ":" << std::endl;
    elements[i]->dump(indent + DumpSpaceNumber * 2);
  }
}

string TupleTypeAST::toString() const {
  string result = "(";
  for (size_t i = 0; i < elements.size(); ++i) {
    result += elements[i]->toString();
    if (i < elements.size() - 1) {
      result += ", ";
    }
  }
  result += ")";
  return result;
}

// EnumTypeAST 实现
EnumTypeAST::EnumTypeAST(const string& name, std::vector<std::pair<string, std::unique_ptr<TypeAST>>> variants)
    : name(name), variants(std::move(variants)) {}

void EnumTypeAST::dump(int indent) const {
  std::cout << string(indent, ' ') << "EnumType: " << name << std::endl;
  for (const auto& variant : variants) {
    std::cout << string(indent + DumpSpaceNumber, ' ') << "Variant: " << variant.first;
    if (variant.second) {
      std::cout << " (" << variant.second->toString() << ")";
    }
    std::cout << std::endl;
  }
}

string EnumTypeAST::toString() const {
  return name;
}

ExprAST::ExprAST(size_t pos_) : pos(pos_) {
}

NumberExprAST::NumberExprAST(int64_t v, size_t pos_) : ExprAST(pos_), value(v) {
}

void NumberExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Number: " << value << '\n';
}


FloatExprAST::FloatExprAST(double v, size_t pos_) : ExprAST(pos_), value(v) {
}

void FloatExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Float: " << value << '\n';
}


VariableExprAST::VariableExprAST(const string &name_, size_t pos_): ExprAST(pos_), name(name_) {
}

void VariableExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Variable: " << name << '\n';
}

IfExprAST::IfExprAST(unique_ptr<ExprAST> cond_, unique_ptr<ExprAST> then_, unique_ptr<ExprAST> else_,
                     size_t pos_) : ExprAST(pos_), cond(std::move(cond_)), then_branch(std::move(then_)),
                                    else_branch(std::move(else_)) {
}

void IfExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "IfExpr:\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Condition:\n";
  cond->dump(indent + 2 * DumpSpaceNumber);

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Then:\n";
  then_branch->dump(indent + 2 * DumpSpaceNumber);

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Else:\n";
  if (else_branch) {
    else_branch->dump(indent + 2 * DumpSpaceNumber);
  } else {
    dump_space(indent + 2 * DumpSpaceNumber);
    std::cout << "null\n";
  }
}

// BlockExprAST 实现
BlockExprAST::BlockExprAST(std::vector<unique_ptr<StmtAST>> statements_, unique_ptr<ExprAST> value_, size_t pos_)
    : ExprAST(pos_), statements(std::move(statements_)), value(std::move(value_)) {
}

void BlockExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "BlockExpr:\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Statements:\n";
  for (const auto& stmt : statements) {
    if (stmt) {
      stmt->dump(indent + 2 * DumpSpaceNumber);
    } else {
      dump_space(indent + 2 * DumpSpaceNumber);
      std::cout << "<null>\n";
    }
  }

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Value:\n";
  if (value) {
    value->dump(indent + 2 * DumpSpaceNumber);
  } else {
    dump_space(indent + 2 * DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}

// LoopExprAST 实现
LoopExprAST::LoopExprAST(std::unique_ptr<StmtAST> body_, size_t pos_) : ExprAST(pos_), body(std::move(body_)) {
}

void LoopExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "LoopExpr:\n";
  
  dump_space(indent + DumpSpaceNumber);
  std::cout << "Body:\n";
  if (body) {
    body->dump(indent + 2 * DumpSpaceNumber);
  } else {
    dump_space(indent + 2 * DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}

ReturnExprAST::ReturnExprAST(size_t pos_, std::unique_ptr<ExprAST> value_, bool propagates_return_)
    : ExprAST(pos_), value(std::move(value_)), propagates_return(propagates_return_) {
}

void ReturnExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ReturnExpr:\n";
  if (value) {
    value->dump(indent + DumpSpaceNumber);
  } else {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}


StringExprAST::StringExprAST(const string &s, size_t pos_, bool is_char)
  : ExprAST(pos_), str(s), is_char_literal(is_char) {
}

BoolExprAST::BoolExprAST(bool v, size_t pos_) : ExprAST(pos_), value(v) {
}

void StringExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << (is_char_literal ? "Char" : "String") << ": " << str << '\n';
}

void BoolExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Boolean: " << (value ? "true" : "false") << '\n';
}

// EnumExprAST 实现
EnumExprAST::EnumExprAST(const string& enum_name, const string& variant_name, unique_ptr<ExprAST> value, size_t pos)
    : ExprAST(pos), enum_name(enum_name), variant_name(variant_name), value(std::move(value)) {}

void EnumExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "EnumExpr: " << enum_name << "::" << variant_name;
  if (value) {
    std::cout << "(";
    value->dump(0);
    std::cout << ")";
  }
  std::cout << '\n';
}



UnaryExprAST::UnaryExprAST(const string &s, size_t pos_, unique_ptr<ExprAST> expr_) : ExprAST(pos_), op(s),
  expr(std::move(expr_)) {
}

void UnaryExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "UnaryExpr: " << op << '\n';
  expr->dump(indent + DumpSpaceNumber);
}


BinaryExprAST::BinaryExprAST(const string &s, size_t pos_, unique_ptr<ExprAST> l,
                             unique_ptr<ExprAST> r) : ExprAST(pos_), op(s), left_expr(std::move(l)),
                                                      right_expr(std::move(r)) {
}

void BinaryExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "BinaryExpr: " + op << '\n';
  left_expr->dump(indent + DumpSpaceNumber);
  right_expr->dump(indent + DumpSpaceNumber);
}


ArrayIndexExprAST::ArrayIndexExprAST(size_t pos_, unique_ptr<ExprAST> array_expr_, unique_ptr<ExprAST> index_expr_)
    : ExprAST(pos_), array_expr(std::move(array_expr_)), index_expr(std::move(index_expr_)) {
}

void ArrayIndexExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ArrayIndex:\n";
  array_expr->dump(indent + DumpSpaceNumber);
  index_expr->dump(indent + DumpSpaceNumber);
}

MemberAccessExprAST::MemberAccessExprAST(size_t pos_, unique_ptr<ExprAST> struct_expr_, const string& member_name_)
  : ExprAST(pos_), struct_expr(std::move(struct_expr_)), member_name(member_name_) {
}

void MemberAccessExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Struct:\n";
  struct_expr->dump(indent + DumpSpaceNumber);
  dump_space(indent + DumpSpaceNumber);
  std::cout << "MemberAccess: ." << member_name << "\n";
}


CallExprAST::CallExprAST(const string &s, size_t pos_, std::vector<unique_ptr<ExprAST> > args_) : ExprAST(pos_),
  call(s), args(std::move(args_)), object_expr(nullptr) {
}

CallExprAST::CallExprAST(const string &s, size_t pos_, unique_ptr<ExprAST> object_expr_, std::vector<unique_ptr<ExprAST> > args_) : ExprAST(pos_),
  call(s), args(std::move(args_)), object_expr(std::move(object_expr_)) {
}

void CallExprAST::dump(int indent) const {
  dump_space(indent);
  if (object_expr) {
    std::cout << "Object:\n";
    object_expr->dump(indent + DumpSpaceNumber);
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Method Call: ." << call << "\n";
    dump_space(indent + 2 * DumpSpaceNumber);
    std::cout << "Arguments:\n";
  } else {
    std::cout << "CallExpr: " << call << '\n';
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Arguments:\n";
  }
  for (const auto &x: args) x->dump(indent + 3 * DumpSpaceNumber);
}

StructExprAST::StructExprAST(const string &name_, std::vector<std::pair<std::string, unique_ptr<ExprAST> > > flds,
                             size_t pos_): ExprAST(pos_), name(name_),
                                           fields(std::move(flds)) {
}

void StructExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "StructExpr: " << name << '\n';
  for (const auto &field: fields) {
    dump_space(indent + DumpSpaceNumber);
    std::cout << field.first << ":\n";
    field.second->dump(indent + 2 * DumpSpaceNumber);
  }
}

StaticCallExprAST::StaticCallExprAST(const string &type_name_, const string &method_name_, size_t pos_, std::vector<unique_ptr<ExprAST>> args_)
  : ExprAST(pos_), type_name(type_name_), method_name(method_name_), args(std::move(args_)) {
}

void StaticCallExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "StaticCall: " << type_name << "::" << method_name << "\n";
  dump_space(indent + DumpSpaceNumber);
  std::cout << "Arguments:\n";
  for (const auto &arg : args) {
    arg->dump(indent + 2 * DumpSpaceNumber);
  }
}

EnumValueExprAST::EnumValueExprAST(const string &enum_type_, const string &enum_value_, size_t pos_)
  : ExprAST(pos_), enum_type(enum_type_), enum_value(enum_value_) {
}

void EnumValueExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "EnumValue: " << enum_type << "::" << enum_value << "\n";
}

//--------------------------------------------------------------
PatternAST::PatternAST(size_t pos_) : pos(pos_) {
}

IdentPatternAST::IdentPatternAST(const string &name_, bool is_mut_, bool is_ref_, bool is_addr_of_,
                                 size_t pos_) : PatternAST(pos_), name(name_), is_mut(is_mut_), is_ref(is_ref_),
                                                is_addr_of(is_addr_of_), type(nullptr) {
}

void IdentPatternAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "IdentPattern: " << name << '\n';
  dump_space(indent + DumpSpaceNumber);
  std::cout << "is_mut: " << is_mut << '\n';
  dump_space(indent + DumpSpaceNumber);
  std::cout << "is_ref: " << is_ref << '\n';
  dump_space(indent + DumpSpaceNumber);
  std::cout << "is_addr_of: " << is_addr_of << '\n';
}

ArrayExprAST::ArrayExprAST(std::vector<unique_ptr<ExprAST>> elems, size_t pos_)
  : ExprAST(pos_), elements(std::move(elems)), is_repeated(false) {}

ArrayExprAST::ArrayExprAST(unique_ptr<ExprAST> elem, unique_ptr<ExprAST> cnt, size_t pos_)
  : ExprAST(pos_), element(std::move(elem)), count(std::move(cnt)), is_repeated(true) {}

void ArrayExprAST::dump(int indent) const {
  dump_space(indent);
  if (is_repeated) {
    std::cout << "ArrayExpr:\n";
    element->dump(indent + DumpSpaceNumber);
    count->dump(indent + DumpSpaceNumber);
  } else {
    std::cout << "ArrayExpr: \n";
    for (const auto &elem : elements) {
      elem->dump(indent + DumpSpaceNumber);
    }
  }
}
//--------------------------------------------------------------
StmtAST::StmtAST(size_t pos_) : pos(pos_) {
}

ExprStmtAST::ExprStmtAST(unique_ptr<ExprAST> ast, size_t pos_) : StmtAST(pos_), expr(std::move(ast)) {
}

void ExprStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ExprStmt:\n";
  expr->dump(indent + DumpSpaceNumber);
}


LetStmtAST::LetStmtAST(unique_ptr<PatternAST> pattern_, const string& type_, unique_ptr<ExprAST> value_, size_t pos_) : StmtAST(pos_),
  pattern(std::move(pattern_)), type(type_), value(std::move(value_)) {
}

void LetStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "LetStmt:\n";
  pattern->dump(indent + DumpSpaceNumber);
  if (!type.empty()) {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Type: " << type << '\n';
  }
  if (value) {
    value->dump(indent + DumpSpaceNumber);
  } else {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Value: <null>\n";
  }
}


AssignStmtAST::AssignStmtAST(unique_ptr<ExprAST> lhs, unique_ptr<ExprAST> rhs, size_t pos_, string op_) : StmtAST(pos_),
  lhs_expr(std::move(lhs)), value(std::move(rhs)), op(op_) {
}

void AssignStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "AssignStmt: " << op << "\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "LHS:\n";
  lhs_expr->dump(indent + 2 * DumpSpaceNumber);

  dump_space(indent + DumpSpaceNumber);
  std::cout << "RHS:\n";
  value->dump(indent + 2 * DumpSpaceNumber);
}


IfStmtAST::IfStmtAST(unique_ptr<ExprAST> cond_, unique_ptr<StmtAST> then_, unique_ptr<StmtAST> else_,
                     size_t pos_) : StmtAST(pos_), cond(std::move(cond_)), then_branch(std::move(then_)),
                                    else_branch(std::move(else_)) {
}

void IfStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "IfStmt:\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Condition:\n";
  cond->dump(indent + 2 * DumpSpaceNumber);

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Then:\n";
  then_branch->dump(indent + 2 * DumpSpaceNumber);

  if (else_branch) {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Else:\n";
    else_branch->dump(indent + 2 * DumpSpaceNumber);
  }
}

WhileStmtAST::WhileStmtAST(unique_ptr<ExprAST> cond_, unique_ptr<StmtAST> body_, size_t pos_) : StmtAST(pos_),
  cond(std::move(cond_)), body(std::move(body_)) {
}

void WhileStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "WhileStmt:\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Condition:\n";
  cond->dump(indent + 2 * DumpSpaceNumber);

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Body: ";
  if (body) {
    std::cout << '\n';
    body->dump(indent + 2 * DumpSpaceNumber);
  } else {
    std::cout << "<null>\n";
  }
}

ForStmtAST::ForStmtAST(unique_ptr<StmtAST> init_, unique_ptr<ExprAST> cond_, unique_ptr<StmtAST> incr_,
                       unique_ptr<StmtAST> body_, size_t pos_) : StmtAST(pos_), init(std::move(init_)),
                                                                 cond(std::move(cond_)), incr(std::move(incr_)),
                                                                 body(std::move(body_)) {
}

void ForStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ForStmt:\n";

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Init:\n";
  if (init) {
    init->dump(indent + 2 * DumpSpaceNumber);
  } else {
    std::cout << "<null>\n";
  }

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Condition:\n";
  if (cond) {
    cond->dump(indent + 2 * DumpSpaceNumber);
  } else {
    std::cout << "<null>\n";
  }

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Increment:\n";
  if (incr) {
    incr->dump(indent + 2 * DumpSpaceNumber);
  } else {
    std::cout << "<null>\n";
  }

  dump_space(indent + DumpSpaceNumber);
  std::cout << "Body:";
  if (body) {
    body->dump(indent + 2 * DumpSpaceNumber);
  } else {
    std::cout << "<null>";
  }
}


BlockStmtAST::BlockStmtAST(std::vector<unique_ptr<StmtAST> > statements_, size_t pos_): StmtAST(pos_),
  statements(std::move(statements_)) {
}

void BlockStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "BlockStmt: {\n";
  for (const auto &x: statements) {
    if (x) x->dump(indent + DumpSpaceNumber);
    else {
      dump_space(indent + DumpSpaceNumber);
      std::cout << "<null>\n";
    }
  }
  dump_space(indent);
  std::cout << "}\n";
}


FnStmtAST::FnStmtAST(const string &name_, std::vector<std::pair<std::unique_ptr<IdentPatternAST>, std::string>>&& params_, unique_ptr<TypeAST> return_type_,
                     unique_ptr<BlockStmtAST> body_, bool is_const_, size_t pos_) : StmtAST(pos_), name(name_), params(std::move(params_)),
                                                                    return_type(std::move(return_type_)), is_const(is_const_), body(std::move(body_)) {
}

void FnStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "FnStmt: " << name << "(";
  for (int i = 0; i < params.size(); ++i) {
    std::cout << "name:" << params[i].first->name << " type:" << params[i].second;
    if (params[i].first->is_mut) std::cout << " mut";
    if (params[i].first->is_ref) std::cout << " ref";
    if (i < params.size() - 1) std::cout << ", ";
  }
  std::cout << ") ->" << (return_type ? return_type->toString() : "void") << '\n';
  if (body) {
    body->dump(indent + DumpSpaceNumber);
  } else {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}

ConstStmtAST::ConstStmtAST(const string &name_, unique_ptr<TypeAST> type_, unique_ptr<ExprAST> value_,
                           size_t pos_) : StmtAST(pos_), name(name_), type(std::move(type_)), value(std::move(value_)) {
}

void ConstStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ConstStmtAST: " << name << ' ' << type->toString() << '\n';
  value->dump(indent + DumpSpaceNumber);
}

StaticStmtAST::StaticStmtAST(const string &name_, const string &type_, unique_ptr<ExprAST> value_, bool is_mut_,
                             size_t pos_) : StmtAST(pos_), name(name_), type(type_), value(std::move(value_)),
                                            is_mut(is_mut_) {
}

void StaticStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "StaticStmtAST: " << name << ' ' << type << '\n';
  value->dump(indent + DumpSpaceNumber);
}

ReturnStmtAST::ReturnStmtAST(size_t pos_, unique_ptr<ExprAST> value_, bool is_implicit_return)
    : StmtAST(pos_), value(std::move(value_)), is_implicit(is_implicit_return) {
}

void ReturnStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ReturnStmt:\n";

  if (value) {
    value->dump(indent + DumpSpaceNumber);
  } else {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}

BreakStmtAST::BreakStmtAST(size_t pos_) : StmtAST(pos_) {
}

BreakStmtAST::BreakStmtAST(size_t pos_, std::unique_ptr<ExprAST> value_) : StmtAST(pos_), value(std::move(value_)) {
}

void BreakStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "BreakStmt:\n";
  if (value) {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Value:\n";
    value->dump(indent + 2 * DumpSpaceNumber);
  }
}

ContinueStmtAST::ContinueStmtAST(size_t pos_) : StmtAST(pos_) {
}

void ContinueStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ContinueStmt:\n";
}

// LoopStmtAST 实现
LoopStmtAST::LoopStmtAST(std::unique_ptr<StmtAST> body_, size_t pos_) : StmtAST(pos_), body(std::move(body_)) {
}

void LoopStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "LoopStmt:\n";
  
  dump_space(indent + DumpSpaceNumber);
  std::cout << "Body:\n";
  if (body) {
    body->dump(indent + 2 * DumpSpaceNumber);
  } else {
    dump_space(indent + 2 * DumpSpaceNumber);
    std::cout << "<null>\n";
  }
}

ExitStmtAST::ExitStmtAST(size_t pos_, std::unique_ptr<ExprAST> value_)
  : StmtAST(pos_), value(std::move(value_)) {
}

void ExitStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "ExitStmt:\n";
  if (value) {
    dump_space(indent + DumpSpaceNumber);
    std::cout << "Value:\n";
    value->dump(indent + DumpSpaceNumber * 2);
  }
}

StructStmtAST::StructStmtAST(const string &name_, std::vector<std::pair<string, string>> fields_, size_t pos_)
  : StmtAST(pos_), name(name_), fields(std::move(fields_)) {
}

void StructStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "StructStmt: " << name << "\n";
  dump_space(indent + DumpSpaceNumber);
  std::cout << "Fields:\n";

  for (const auto &field : fields) {
    dump_space(indent + DumpSpaceNumber * 2);
    std::cout << field.first << ": " << field.second << "\n";
  }
}

// EnumStmtAST 实现
EnumStmtAST::EnumStmtAST(const string &name_, std::vector<std::pair<string, std::unique_ptr<TypeAST>>> variants_, size_t pos_)
  : StmtAST(pos_), name(name_), variants(std::move(variants_)) {
}

void EnumStmtAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "EnumStmt: " << name << "\n";
  dump_space(indent + DumpSpaceNumber);
  std::cout << "Variants:\n";

  for (const auto &variant : variants) {
    dump_space(indent + DumpSpaceNumber * 2);
    std::cout << variant.first;
    if (variant.second) {
      std::cout << "(" << variant.second->toString() << ")";
    }
    std::cout << "\n";
  }
}

ImplStmtAST::ImplStmtAST(const string& type_name_, const string& trait_name_, 
                         std::vector<std::unique_ptr<FnStmtAST>> methods_, size_t pos_)
    : StmtAST(pos_), type_name(type_name_), trait_name(trait_name_), 
      methods(std::move(methods_)) {}

void ImplStmtAST::dump(int indent) const {
  dump_space(indent);
  if (trait_name.empty()) {
    std::cout << "ImplStmt: " << type_name << "\n";
  } else {
    std::cout << "ImplStmt: " << trait_name << " for " << type_name << "\n";
  }
  
  for (const auto& method : methods) {
    method->dump(indent + DumpSpaceNumber);
  }
}

CastExprAST::CastExprAST(unique_ptr<ExprAST> expr_, unique_ptr<TypeAST> type, size_t pos_)
  : ExprAST(pos_), expr(std::move(expr_)), target_type(std::move(type)) {
}

void CastExprAST::dump(int indent) const {
  dump_space(indent);
  std::cout << "Cast to :" << target_type->toString() << "\n";
  expr->dump(indent + DumpSpaceNumber);
}