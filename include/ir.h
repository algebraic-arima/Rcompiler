#ifndef IR_H
#define IR_H

#include <string>
#include "ast.h"
#include "semantic.h"

// Entry point for IR generation; produces textual LLVM IR (no LLVM libs required).
// On failure (unsupported AST/type or IO error), an exception is thrown.
bool generate_ir(BlockStmtAST *program, SemanticAnalyzer &analyzer, const std::string &inputPath, bool emitLLVM);

#endif // IR_H
