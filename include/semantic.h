#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

//===----------------------------------------------------------------------===//

enum class BaseType {
  Int,
  Bool,
  Float,
  String,
  Char,
  Void,
  Array,
  Reference,
  Function,
  Struct,
  Enum,
  Custom,
  Unknown
};

struct TypeInfo;
using TypeRef = std::shared_ptr<TypeInfo>;

struct TypeInfo {
  BaseType kind = BaseType::Unknown;
  std::string name; // 结构体、枚举或自定义类型名
  std::vector<TypeRef> parameters; // 函数参数或复合类型成分
  TypeRef returnType; // 函数返回类型
  TypeRef elementType; // 数组元素类型
  bool isMutableRef = false; // 引用是否可变
  bool isUnsigned = false; // 整型是否为无符号
  int bitWidth = 0; // 整型位宽（0 表示未指定）
  bool hasArrayLength = false; // 数组是否携带长度信息
  int64_t arrayLength = -1; // 数组长度

  bool equals(const TypeRef &other) const;

  std::string toString() const;

  bool isNumeric() const;

  bool isBool() const { return kind == BaseType::Bool; }
};

namespace TypeFactory {
  TypeRef getInt();

  TypeRef getSignedInt(const std::string &name);

  TypeRef getUnsignedInt(const std::string &name);

  TypeRef getBool();

  TypeRef getFloat();

  TypeRef getString();

  TypeRef getChar();

  TypeRef getVoid();

  TypeRef getUnknown();

  TypeRef makeArray(const TypeRef &element, int64_t length = -1, bool hasLength = false);

  TypeRef makeReference(const TypeRef &target, bool isMutable);

  TypeRef makeFunction(const std::vector<TypeRef> &params, const TypeRef &ret);

  TypeRef makeStruct(const std::string &name);

  TypeRef makeEnum(const std::string &name);

  TypeRef makeCustom(const std::string &name);
}

//===----------------------------------------------------------------------===//

enum class SymbolKind {
  Variable,
  Function,
  TypeAlias,
  Const
};

struct Symbol {
  std::string name;
  SymbolKind kind;
  TypeRef type;
  bool isMutable = false;
};

class SymbolTable {
public:
  void enterScope();

  void exitScope();

  bool addSymbol(const Symbol &symbol, bool allowShadow = false);

  const Symbol *lookup(const std::string &name) const;

  const Symbol *lookupCurrent(const std::string &name) const;

private:
  std::vector<std::unordered_map<std::string, Symbol> > scopes_;
};

//===----------------------------------------------------------------------===//

struct StructInfo {
  std::string name;
  std::unordered_map<std::string, TypeRef> fields;
  std::vector<std::pair<std::string, TypeRef> > orderedFields;
};

struct EnumVariant {
  std::string name;
  TypeRef payload; // nullptr
};

struct EnumInfo {
  std::string name;
  std::unordered_map<std::string, EnumVariant> variants;
};

struct FunctionInfo {
  std::string name;
  std::vector<TypeRef> params;
  std::vector<bool> paramMut;
  TypeRef returnType;
  bool isMethod = false;
  bool hasSelf = false;
  TypeRef receiverType; // for methods
  bool selfIsReference = false;
  bool selfIsMutable = false;
};

struct SemanticIssue {
  std::string message;
  size_t position = 0;
};

//===----------------------------------------------------------------------===//

class SemanticAnalyzer {
public:
  SemanticAnalyzer();

  bool analyze(BlockStmtAST *program);

  const std::vector<SemanticIssue> &errors() const { return issues; }

  public:
  // 生命周期管理
  void reset();

  void registerBuiltins();

  void collectTypeDeclarations(BlockStmtAST *program);

  void collectConstDeclarations(BlockStmtAST *program);

  void collectFunctionDeclarations(BlockStmtAST *program);

  void registerStruct(StructStmtAST *structStmt);

  void registerEnum(EnumStmtAST *enumStmt);

  void registerLocalFunctionSymbol(FnStmtAST *fn);

  void preRegisterLocalFunctions(BlockStmtAST *block);

  // 注册函数/方法
  void registerFunction(FnStmtAST *fn, const std::string &ownerType = "");

  void registerImpl(ImplStmtAST *impl);

  // 分析入口
  void analyzeTopLevel(BlockStmtAST *program);

  void analyzeFunctionBody(FnStmtAST *fn, const std::string &ownerType = "");

  // 语句/表达式
  void analyzeStatement(StmtAST *stmt);

  void analyzeLet(LetStmtAST *stmt, bool isConst);

  void analyzeConst(ConstStmtAST *stmt);

  void analyzeStatic(StaticStmtAST *stmt);

  void analyzeAssign(AssignStmtAST *stmt);

  void analyzeIfStmt(IfStmtAST *stmt);

  void analyzeWhileStmt(WhileStmtAST *stmt);

  void analyzeLoopStmt(LoopStmtAST *stmt);

  void analyzeReturn(ReturnStmtAST *stmt);

  void analyzeLocalFunction(FnStmtAST *stmt);

  void analyzeBreak(BreakStmtAST *stmt);

  void analyzeContinue(ContinueStmtAST *stmt);

  void analyzeBlock(BlockStmtAST *stmt, bool createScope = true);

  TypeRef analyzeExpr(ExprAST *expr);

  TypeRef analyzeIfExpr(IfExprAST *expr);

  TypeRef analyzeBlockExpr(BlockExprAST *expr);

  TypeRef analyzeLoopExpr(LoopExprAST *expr);

  TypeRef analyzeBinaryExpr(BinaryExprAST *expr);

  TypeRef analyzeUnaryExpr(UnaryExprAST *expr);

  TypeRef analyzeCallExpr(CallExprAST *expr);

  TypeRef analyzeStaticCall(StaticCallExprAST *expr);

  TypeRef analyzeStructExpr(StructExprAST *expr);

  TypeRef analyzeMemberAccess(MemberAccessExprAST *expr);

  TypeRef analyzeArrayExpr(ArrayExprAST *expr);

  TypeRef analyzeArrayIndex(ArrayIndexExprAST *expr);

  TypeRef analyzeCastExpr(CastExprAST *expr);

  TypeRef analyzeReturnExpr(ReturnExprAST *expr);

  // 辅助
  TypeRef resolveType(TypeAST *typeAst);

  TypeRef resolveTypeName(const std::string &name, const std::string &selfType = "");

  void reportError(size_t position, const std::string &msg);

  bool ensureAssignable(const TypeRef &from, const TypeRef &to, size_t position, ExprAST *originExpr = nullptr);

  bool isSameType(const TypeRef &lhs, const TypeRef &rhs) const;

  TypeRef stripReference(const TypeRef &type) const;

  TypeRef processReturnValue(ExprAST *valueExpr, size_t position);

  bool blockGuaranteesReturn(BlockStmtAST *block) const;

  bool statementGuaranteesReturn(StmtAST *stmt) const;

  bool tryEvaluateConstInt(ExprAST *expr, int64_t &value) const;

  // Evaluate a constant integer expression provided as a raw string (used for array lengths in type annotations).
  bool evaluateConstLengthExpr(const std::string &expr, int64_t &value) const;

  bool isNumericLiteral(ExprAST *expr) const;

  bool exprGuaranteesReturn(ExprAST *expr) const;

  const Symbol *lookupAssignmentTarget(ExprAST *expr) const;

  bool isMutableBindingExpr(ExprAST *expr) const;

  void validateTypeConstraints(const TypeRef &type, size_t position);

  void ensureIntLiteralFits(ExprAST *expr, const TypeRef &targetType, size_t position);

  TypeRef getCachedExprType(ExprAST *expr);

  // lookup helpers
  FunctionInfo *findFunction(const std::string &name);

  FunctionInfo *findMethod(const std::string &typeName, const std::string &methodName);

  const StructInfo *getStructInfo(const std::string &name) const;

  const std::unordered_map<std::string, StructInfo> &getStructTable() const { return structs; }

private:
  SymbolTable symbols;
  std::vector<SemanticIssue> issues;
  std::unordered_map<std::string, StructInfo> structs;
  std::unordered_map<std::string, EnumInfo> enums;
  std::unordered_map<std::string, FunctionInfo> functions;
  std::unordered_map<std::string, std::unordered_map<std::string, FunctionInfo> > methods;
  TypeRef currentReturn;
  std::string currentImplType;
  std::string currentFunctionName;
  int loopDepth = 0;
  std::unordered_map<std::string, int64_t> constIntValues;
  std::unordered_map<ExprAST *, TypeRef> exprTypes;
};

#endif // SEMANTIC_H
