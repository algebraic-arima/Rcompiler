#include "ir.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <optional>
#include <sstream>
#include <algorithm>

namespace {
namespace fs = std::filesystem;

struct Value {
	std::string name;
	std::string type; // "i64", "i1", or "ptr"
	bool arrayAlloca = false; // true if the pointer comes from alloca [N x i64]
	size_t slots = 1; // total slots when arrayAlloca is true or aggregate pointer
};

struct TypeLayout {
	size_t slots = 1; // number of i64 slots occupied
	bool aggregate = false; // true for structs/arrays
	bool arrayLike = false; // true for arrays (including array fields)
};

struct FunctionCtx {
	std::string name;
	bool returnsVoid = false;
	bool aggregateReturn = false;
	TypeLayout retLayout;
	std::string retPtr;
	int tempId = 0;
	int labelId = 0;
	std::ostringstream body;
	std::vector<std::string> entryAllocas;
	struct VarInfo {
		TypeRef type;
		TypeLayout layout;
		std::string ptr;
		bool arrayAlloca = false;
	};
	std::unordered_map<std::string, VarInfo> vars;
	std::string breakLabel;
	std::string continueLabel;
	bool terminated = false;
};

fs::path deriveLlPath(const std::string &inputPath) {
	fs::path in(inputPath);
	if (in.has_extension()) {
		in.replace_extension(".ll");
	} else {
		in += ".ll";
	}
	return in;
}

std::string freshTemp(FunctionCtx &fn) {
	return "%t" + std::to_string(++fn.tempId);
}

std::string freshLabel(FunctionCtx &fn, const std::string &prefix) {
	return prefix + std::to_string(++fn.labelId);
}

Value toI64(FunctionCtx &fn, const Value &v);
void copySlots(FunctionCtx &fn, const Value &src, const Value &dst, size_t count);

Value ensureBool(FunctionCtx &fn, const Value &v) {
	if (v.type == "i1") return v;
	Value asInt = v;
	if (asInt.type != "i64") {
		asInt = toI64(fn, v);
	}
	std::string tmp = freshTemp(fn);
	fn.body << "  " << tmp << " = icmp ne i64 " << asInt.name << ", 0\n";
	return {tmp, "i1"};
}

Value toI64(FunctionCtx &fn, const Value &v) {
	if (v.type == "i64") return v;
	std::string tmp = freshTemp(fn);
	if (v.type == "ptr") {
		fn.body << "  " << tmp << " = ptrtoint ptr " << v.name << " to i64\n";
	} else {
		fn.body << "  " << tmp << " = zext i1 " << v.name << " to i64\n";
	}
	return {tmp, "i64"};
}

void startBlock(FunctionCtx &fn, const std::string &label) {
	if (!label.empty()) {
		fn.body << label << ":\n";
	}
	fn.terminated = false;
}

std::string typeOf(TypeAST *t) {
	if (!t) return "i64";
	if (auto *p = dynamic_cast<PrimitiveTypeAST *>(t)) {
		const std::string n = p->name;
		if (n == "bool") return "i1";
		if (n == "void") return "void";
	}
	return "i64";
}
SemanticAnalyzer *g_analyzer = nullptr;

TypeRef stripRef(const TypeRef &t) {
	if (!g_analyzer) return t;
	return g_analyzer->stripReference(t);
}

std::unordered_map<std::string, std::vector<std::tuple<std::string, size_t, size_t, TypeRef> > > g_structLayouts;

TypeLayout layoutOf(const TypeRef &t);

bool isRefType(const TypeRef &t) {
	return t && t->kind == BaseType::Reference;
}

const std::vector<std::tuple<std::string, size_t, size_t, TypeRef> > &getStructLayout(const std::string &name) {
	auto it = g_structLayouts.find(name);
	if (it != g_structLayouts.end()) return it->second;
	if (!g_analyzer) {
		static std::vector<std::tuple<std::string, size_t, size_t, TypeRef> > empty;
		return empty;
	}
	const StructInfo *info = g_analyzer->getStructInfo(name);
	if (!info) {
		static std::vector<std::tuple<std::string, size_t, size_t, TypeRef> > empty;
		return empty;
	}
	std::vector<std::tuple<std::string, size_t, size_t, TypeRef> > fields;
	size_t offset = 0;
	for (const auto &p : info->orderedFields) {
		auto fieldLayout = layoutOf(p.second);
		fields.emplace_back(p.first, offset, fieldLayout.slots, p.second);
		offset += fieldLayout.slots;
	}
	g_structLayouts[name] = fields;
	return g_structLayouts[name];
}

TypeLayout layoutOf(const TypeRef &t) {
	TypeLayout result;
	if (!t) return result;
	auto base = stripRef(t);
	if (!base) return result;
	switch (base->kind) {
		case BaseType::Bool:
		case BaseType::Int:
		case BaseType::Float:
		case BaseType::Char:
			return result;
		case BaseType::Array: {
			TypeLayout elem = layoutOf(base->elementType);
			size_t len = 1;
			if (base->hasArrayLength && base->arrayLength > 0) {
				len = static_cast<size_t>(base->arrayLength);
			}
			result.slots = std::max<size_t>(1, elem.slots * len);
			result.aggregate = true;
			result.arrayLike = true;
			return result;
		}
		case BaseType::Struct: {
			auto &fields = getStructLayout(base->name);
			size_t total = 0;
			for (auto &f : fields) total += std::get<2>(f);
			result.slots = std::max<size_t>(1, total);
			result.aggregate = true;
			return result;
		}
		case BaseType::Enum: {
			// 枚举类型在LLVM中表示为整数
			return result;
		}
		case BaseType::String: {
			// 字符串表示为指针
			result.slots = 1;
			return result;
		}
		case BaseType::Function: {
			// 函数类型表示为指针
			result.slots = 1;
			return result;
		}
		default:
			return result;
	}
}

// 获取类型的LLVM类型表示
std::string llvmTypeOf(const TypeRef &t) {
	if (!t) return "i64";
	auto base = stripRef(t);
	if (!base) return "i64";
	switch (base->kind) {
		case BaseType::Bool:
			return "i1";
		case BaseType::Int:
			return "i64";
		case BaseType::Float:
			return "double";
		case BaseType::Char:
			return "i8";
		case BaseType::String:
		case BaseType::Function:
		case BaseType::Array:
		case BaseType::Struct:
		case BaseType::Enum:
			return "ptr";
		case BaseType::Void:
			return "void";
		default:
			return "i64";
	}
}

// 检查类型是否需要按引用传递
bool needsByValue(const TypeRef &t) {
	auto layout = layoutOf(t);
	return !layout.aggregate && layout.slots == 1;
}

// 检查类型是否需要按引用传递
bool needsByRef(const TypeRef &t) {
	return !needsByValue(t);
}

// 生成类型转换代码
Value emitCast(FunctionCtx &fn, const Value &v, const std::string &targetType) {
	if (v.type == targetType) return v;
	
	std::string tmp = freshTemp(fn);
	if (v.type == "i1" && targetType == "i64") {
		// bool to int
		fn.body << "  " << tmp << " = zext i1 " << v.name << " to i64\n";
		return {tmp, "i64"};
	} else if (v.type == "i64" && targetType == "i1") {
		// int to bool
		fn.body << "  " << tmp << " = icmp ne i64 " << v.name << ", 0\n";
		return {tmp, "i1"};
	} else if (v.type == "i64" && targetType == "ptr") {
		// int to ptr
		fn.body << "  " << tmp << " = inttoptr i64 " << v.name << " to ptr\n";
		return {tmp, "ptr"};
	} else if (v.type == "ptr" && targetType == "i64") {
		// ptr to int
		fn.body << "  " << tmp << " = ptrtoint ptr " << v.name << " to i64\n";
		return {tmp, "i64"};
	}
	
	// 默认返回原值
	return v;
}

// 生成内存分配代码
Value emitAlloca(FunctionCtx &fn, const TypeRef &t, const std::string &name = "") {
	auto layout = layoutOf(t);
	std::string varName = name.empty() ? freshTemp(fn) : ("%" + name);
	
	if (layout.aggregate || layout.slots > 1) {
		fn.body << "  " << varName << " = alloca [" << layout.slots << " x i64]\n";
		return {varName, "ptr", true, layout.slots};
	} else {
		fn.body << "  " << varName << " = alloca i64\n";
		fn.body << "  store i64 0, ptr " << varName << "\n";
		return {varName, "ptr", false, 1};
	}
}

// 生成内存加载代码
Value emitLoad(FunctionCtx &fn, const Value &ptr, const TypeRef &type) {
	auto layout = layoutOf(type);
	
	if (layout.aggregate || layout.slots > 1) {
		// 聚合类型直接返回指针
		return {ptr.name, "ptr", ptr.arrayAlloca, layout.slots};
	}
	
	// 标量类型加载值
	std::string tmp = freshTemp(fn);
	fn.body << "  " << tmp << " = load i64, ptr " << ptr.name << "\n";
	return {tmp, "i64"};
}

// 生成内存存储代码
void emitStore(FunctionCtx &fn, const Value &value, const Value &ptr, const TypeRef &type) {
	auto layout = layoutOf(type);
	
	if (layout.aggregate || layout.slots > 1) {
		// 聚合类型需要复制
		copySlots(fn, value, ptr, layout.slots);
	} else {
		// 标量类型直接存储
		auto val = value.type == "i64" ? value : emitCast(fn, value, "i64");
		fn.body << "  store i64 " << val.name << ", ptr " << ptr.name << "\n";
	}
}

Value asPtr(Value v) {
	if (v.type == "ptr") return v;
	Value out;
	out.type = "ptr";
	out.name = v.name;
	return out;
}

void copySlots(FunctionCtx &fn, const Value &src, const Value &dst, size_t count) {
	if (count == 0) return;
	Value srcPtr = src;
	Value dstPtr = dst;
	if (srcPtr.type != "ptr") {
		srcPtr = {freshTemp(fn), "ptr"};
		fn.body << "  " << srcPtr.name << " = inttoptr i64 " << src.name << " to ptr\n";
	}
	if (dstPtr.type != "ptr") {
		dstPtr = {freshTemp(fn), "ptr"};
		fn.body << "  " << dstPtr.name << " = inttoptr i64 " << dst.name << " to ptr\n";
	}
	for (size_t i = 0; i < count; ++i) {
		std::string sPtr = srcPtr.arrayAlloca ? freshTemp(fn) : freshTemp(fn);
		std::string dPtr = dstPtr.arrayAlloca ? freshTemp(fn) : freshTemp(fn);
		if (srcPtr.arrayAlloca) {
			fn.body << "  " << sPtr << " = getelementptr [" << srcPtr.slots << " x i64], ptr " << srcPtr.name << ", i64 0, i64 " << i << "\n";
		} else {
			fn.body << "  " << sPtr << " = getelementptr i64, ptr " << srcPtr.name << ", i64 " << i << "\n";
		}
		if (dstPtr.arrayAlloca) {
			fn.body << "  " << dPtr << " = getelementptr [" << dstPtr.slots << " x i64], ptr " << dstPtr.name << ", i64 0, i64 " << i << "\n";
		} else {
			fn.body << "  " << dPtr << " = getelementptr i64, ptr " << dstPtr.name << ", i64 " << i << "\n";
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = load i64, ptr " << sPtr << "\n";
		fn.body << "  store i64 " << tmp << ", ptr " << dPtr << "\n";
	}
}

std::string gepSlot(FunctionCtx &fn, const Value &base, size_t idx) {
	std::string tmp = freshTemp(fn);
	if (base.arrayAlloca) {
		fn.body << "  " << tmp << " = getelementptr [" << base.slots << " x i64], ptr " << base.name << ", i64 0, i64 " << idx << "\n";
	} else {
		fn.body << "  " << tmp << " = getelementptr i64, ptr " << base.name << ", i64 " << idx << "\n";
	}
	return tmp;
}

Value emitExpr(FunctionCtx &fn, ExprAST *expr);
void emitStmt(FunctionCtx &fn, StmtAST *stmt);
std::unordered_map<std::string, size_t> g_declArity;
std::unordered_set<std::string> g_definedFuncs;

TypeRef exprType(ExprAST *expr) {
	if (!g_analyzer || !expr) return nullptr;
	return g_analyzer->getCachedExprType(expr);
}

std::optional<int64_t> constInt(ExprAST *e) {
	if (auto *n = dynamic_cast<NumberExprAST *>(e)) return n->value;
	return std::nullopt;
}

Value fallbackValue() {
	return {"0", "i64"};
}

Value emitNumber(int64_t v) {
	return {std::to_string(v), "i64"};
}

Value emitBool(bool v) {
	return {v ? "1" : "0", "i1"};
}

FunctionCtx::VarInfo makeAlloca(FunctionCtx &fn, const std::string &name, const TypeLayout &layout) {
	FunctionCtx::VarInfo info;
	info.layout = layout;
	info.arrayAlloca = layout.aggregate || layout.slots > 1;
	info.ptr = "%" + name;
	if (info.arrayAlloca) {
		fn.entryAllocas.push_back("  " + info.ptr + " = alloca [" + std::to_string(layout.slots) + " x i64]\n");
	} else {
		fn.entryAllocas.push_back("  " + info.ptr + " = alloca i64\n");
		fn.entryAllocas.push_back("  store i64 0, ptr " + info.ptr + "\n");
	}
	return info;
}

FunctionCtx::VarInfo &ensureVar(FunctionCtx &fn, const std::string &name, const TypeRef &typeHint = nullptr) {
	auto it = fn.vars.find(name);
	if (it != fn.vars.end()) return it->second;
	TypeLayout layout;
	if (typeHint) layout = layoutOf(typeHint);
	auto info = makeAlloca(fn, name, layout);
	info.type = typeHint;
	return fn.vars[name] = info;
}

Value getLValuePtr(FunctionCtx &fn, ExprAST *expr, TypeRef expectedType = nullptr) {
	if (!expr) return {"0", "ptr"};
	TypeRef exprTy = expectedType ? expectedType : exprType(expr);
	TypeLayout layout = layoutOf(exprTy);
	if (auto *v = dynamic_cast<VariableExprAST *>(expr)) {
		auto &info = ensureVar(fn, v->name, exprTy);
		Value out{info.ptr, "ptr", info.arrayAlloca, layout.slots};
		if (layout.aggregate || layout.slots > 1) out.arrayAlloca = true;
		return out;
	}
	if (auto *idx = dynamic_cast<ArrayIndexExprAST *>(expr)) {
		Value base = emitExpr(fn, idx->array_expr.get());
		Value basePtr = base.type == "ptr" ? base : Value{freshTemp(fn), "ptr"};
		if (basePtr.name != base.name || basePtr.type != base.type) {
			fn.body << "  " << basePtr.name << " = inttoptr i64 " << base.name << " to ptr\n";
		}
		TypeRef arrType = exprType(idx->array_expr.get());
		TypeRef stripped = stripRef(arrType);
		TypeRef elemType = (stripped && stripped->kind == BaseType::Array) ? stripped->elementType : nullptr;
		TypeLayout elemLayout = layoutOf(elemType);
		size_t elemSlots = std::max<size_t>(1, elemLayout.slots);
		auto index = toI64(fn, emitExpr(fn, idx->index_expr.get()));
		std::string scaled = freshTemp(fn);
		fn.body << "  " << scaled << " = mul i64 " << index.name << ", " << elemSlots << "\n";
		std::string elemPtr = freshTemp(fn);
		if (basePtr.arrayAlloca) {
			fn.body << "  " << elemPtr << " = getelementptr [" << basePtr.slots << " x i64], ptr " << basePtr.name << ", i64 0, i64 " << scaled << "\n";
		} else {
			fn.body << "  " << elemPtr << " = getelementptr i64, ptr " << basePtr.name << ", i64 " << scaled << "\n";
		}
		return {elemPtr, "ptr", false, elemSlots};
	}
	if (auto *mem = dynamic_cast<MemberAccessExprAST *>(expr)) {
		Value base = emitExpr(fn, mem->struct_expr.get());
		if (base.type != "ptr") {
			Value tmp{freshTemp(fn), "ptr"};
			fn.body << "  " << tmp.name << " = inttoptr i64 " << base.name << " to ptr\n";
			tmp.arrayAlloca = base.arrayAlloca;
			tmp.slots = base.slots;
			base = tmp;
		}
		TypeRef baseType = exprType(mem->struct_expr.get());
		TypeRef stripped = stripRef(baseType);
		auto &fields = getStructLayout(stripped ? stripped->name : "");
		auto it = std::find_if(fields.begin(), fields.end(), [&](auto &t) { return std::get<0>(t) == mem->member_name; });
		if (it == fields.end()) return {"0", "ptr"};
		size_t offset = std::get<1>(*it);
		size_t slots = std::get<2>(*it);
		std::string ptr = gepSlot(fn, base, offset);
		return {ptr, "ptr", false, slots};
	}
	// fallback: if expression already yields a pointer, reuse it; otherwise materialize a temporary
	Value val = emitExpr(fn, expr);
	if (val.type == "ptr") {
		val.slots = layout.slots;
		val.arrayAlloca = val.arrayAlloca || (layout.aggregate || layout.slots > 1);
		return val;
	}
	std::string tmpAlloc = freshTemp(fn);
	fn.body << "  " << tmpAlloc << " = alloca [" << std::max<size_t>(1, layout.slots) << " x i64]\n";
	Value dst{tmpAlloc, "ptr", true, std::max<size_t>(1, layout.slots)};
	copySlots(fn, val, dst, std::max<size_t>(1, layout.slots));
	return dst;
}

Value emitExpr(FunctionCtx &fn, ExprAST *expr) {
	if (!expr) return emitNumber(0);

	int64_t constVal = 0;
	if (g_analyzer && g_analyzer->tryEvaluateConstInt(expr, constVal)) {
		return emitNumber(constVal);
	}

	// 不支持的特性：字符串、枚举、元组
	if (dynamic_cast<StringExprAST *>(expr)) {
		throw std::runtime_error("IR: string literals are not supported");
	}
	if (dynamic_cast<EnumValueExprAST *>(expr) || dynamic_cast<EnumExprAST *>(expr)) {
		throw std::runtime_error("IR: enums are not supported");
	}

	if (auto *n = dynamic_cast<NumberExprAST *>(expr)) {
		return emitNumber(n->value);
	}
	if (auto *b = dynamic_cast<BoolExprAST *>(expr)) {
		return emitBool(b->value);
	}
	if (auto *v = dynamic_cast<VariableExprAST *>(expr)) {
		auto &info = ensureVar(fn, v->name, exprType(expr));
		if (isRefType(exprType(expr))) {
			return {info.ptr, "ptr", info.arrayAlloca, info.layout.slots};
		}
		if (info.layout.aggregate || info.layout.slots > 1) {
			return {info.ptr, "ptr", info.arrayAlloca, info.layout.slots};
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = load i64, ptr " << info.ptr << "\n";
		return {tmp, "i64"};
	}
	if (auto *u = dynamic_cast<UnaryExprAST *>(expr)) {
		auto val = emitExpr(fn, u->expr.get());
		if (u->op == "&" || u->op == "&mut") {
			return getLValuePtr(fn, u->expr.get(), exprType(u->expr.get()));
		}
		if (u->op == "*") {
			if (val.type != "ptr") {
				std::string tmpPtr = freshTemp(fn);
				fn.body << "  " << tmpPtr << " = inttoptr i64 " << val.name << " to ptr\n";
				val = {tmpPtr, "ptr", val.arrayAlloca, val.slots};
			}
			TypeLayout lay = layoutOf(exprType(u->expr.get()));
			if (lay.aggregate || lay.slots > 1) {
				return {val.name, "ptr", val.arrayAlloca, lay.slots};
			}
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = load i64, ptr " << val.name << "\n";
			return {tmp, "i64"};
		}
		if (u->op == "-") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = sub i64 0, " << val.name << "\n";
			return {tmp, "i64"};
		}
		if (u->op == "!") {
			val = ensureBool(fn, val);
			std::string boolTmp = freshTemp(fn);
			fn.body << "  " << boolTmp << " = xor i1 " << val.name << ", 1\n";
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = zext i1 " << boolTmp << " to i64\n";
			return {tmp, "i64"};
		}
		return fallbackValue();
	}
	if (auto *bin = dynamic_cast<BinaryExprAST *>(expr)) {
		auto lhs = emitExpr(fn, bin->left_expr.get());
		auto rhs = emitExpr(fn, bin->right_expr.get());
		const std::string &op = bin->op;
		if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			const char *opcode = (op == "+") ? "add" : (op == "-") ? "sub" : (op == "*") ? "mul" : (op == "/") ? "sdiv" : "srem";
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = " << opcode << " i64 " << lhs.name << ", " << rhs.name << "\n";
			return {tmp, "i64"};
		}
		if (op == "^") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = xor i64 " << lhs.name << ", " << rhs.name << "\n";
			return {tmp, "i64"};
		}
		if (op == "^") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = xor i64 " << lhs.name << ", " << rhs.name << "\n";
			return {tmp, "i64"};
		}
		if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			const char *pred = (op == "==") ? "eq" : (op == "!=") ? "ne" : (op == "<") ? "slt" : (op == "<=") ? "sle" : (op == ">") ? "sgt" : "sge";
			std::string cmp = freshTemp(fn);
			fn.body << "  " << cmp << " = icmp " << pred << " i64 " << lhs.name << ", " << rhs.name << "\n";
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = zext i1 " << cmp << " to i64\n";
			return {tmp, "i64"};
		}
		if (op == "&&" || op == "||") {
			lhs = ensureBool(fn, lhs);
			rhs = ensureBool(fn, rhs);
			std::string boolTmp = freshTemp(fn);
			fn.body << "  " << boolTmp << " = " << ((op == "&&") ? "and" : "or") << " i1 " << lhs.name << ", " << rhs.name << "\n";
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = zext i1 " << boolTmp << " to i64\n";
			return {tmp, "i64"};
		}
		if (op == "&" || op == "|") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = " << ((op == "&") ? "and" : "or") << " i64 " << lhs.name << ", " << rhs.name << "\n";
			return {tmp, "i64"};
		}
		if (op == "<<" || op == ">>") {
			lhs = toI64(fn, lhs);
			rhs = toI64(fn, rhs);
			std::string tmp = freshTemp(fn);
			const char *opcode = (op == "<<") ? "shl" : "lshr"; // logical right shift to avoid stuck negative values
			fn.body << "  " << tmp << " = " << opcode << " i64 " << lhs.name << ", " << rhs.name << "\n";
			return {tmp, "i64"};
		}
		return fallbackValue();
	}
	if (auto *idx = dynamic_cast<ArrayIndexExprAST *>(expr)) {
		auto base = emitExpr(fn, idx->array_expr.get());
		auto index = toI64(fn, emitExpr(fn, idx->index_expr.get()));
		Value basePtr = base.type == "ptr" ? base : Value{freshTemp(fn), "ptr"};
		if (basePtr.name != base.name || basePtr.type != base.type) {
			fn.body << "  " << basePtr.name << " = inttoptr i64 " << base.name << " to ptr\n";
		}
		TypeRef arrType = exprType(idx->array_expr.get());
		auto arrLayout = layoutOf(arrType);
		TypeRef elemType = nullptr;
		auto stripped = stripRef(arrType);
		if (stripped && stripped->kind == BaseType::Array) {
			elemType = stripped->elementType;
		}
		auto elemLayout = layoutOf(elemType);
		size_t elemSlots = std::max<size_t>(1, elemLayout.slots);
		std::string scaled = freshTemp(fn);
		fn.body << "  " << scaled << " = mul i64 " << index.name << ", " << elemSlots << "\n";
		std::string elemPtr = basePtr.arrayAlloca ? freshTemp(fn) : freshTemp(fn);
		if (basePtr.arrayAlloca) {
			fn.body << "  " << elemPtr << " = getelementptr [" << basePtr.slots << " x i64], ptr " << basePtr.name << ", i64 0, i64 " << scaled << "\n";
		} else {
			fn.body << "  " << elemPtr << " = getelementptr i64, ptr " << basePtr.name << ", i64 " << scaled << "\n";
		}
		if (elemLayout.aggregate || elemLayout.slots > 1) {
			return {elemPtr, "ptr", false, elemLayout.slots};
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = load i64, ptr " << elemPtr << "\n";
		return {tmp, "i64"};
	}
	if (auto *call = dynamic_cast<CallExprAST *>(expr)) {
		if (call->object_expr) {
			// Method call lowering: mangle to Struct__method and pass receiver first (by pointer).
			TypeRef objType = exprType(call->object_expr.get());
			TypeRef strippedObj = stripRef(objType);
			std::string receiverName = strippedObj ? strippedObj->name : "";
			FunctionInfo *minfo = g_analyzer ? g_analyzer->findMethod(receiverName, call->call) : nullptr;
			std::string mangled = receiverName.empty() ? call->call : receiverName + "__" + call->call;
			TypeLayout retLayout = layoutOf(minfo ? minfo->returnType : nullptr);
			bool aggRet = retLayout.aggregate || retLayout.slots > 1;
			Value retDest;
			if (aggRet) {
				retDest = {freshTemp(fn), "ptr", true, retLayout.slots};
				fn.body << "  " << retDest.name << " = alloca [" << retLayout.slots << " x i64]\n";
			}
			std::vector<Value> args;
			TypeLayout recvLayout = layoutOf(objType);
			bool recvByRef = minfo && minfo->selfIsReference;
			Value recv = recvByRef ? getLValuePtr(fn, call->object_expr.get(), objType) : emitExpr(fn, call->object_expr.get());
			if (!recvByRef && (recvLayout.aggregate || recvLayout.slots > 1)) {
				size_t copySlotsCount = std::max<size_t>(recvLayout.slots, std::max<size_t>(1, recv.slots));
				std::string tmpAlloc = freshTemp(fn);
				fn.body << "  " << tmpAlloc << " = alloca [" << copySlotsCount << " x i64]\n";
				Value tmp{tmpAlloc, "ptr", true, copySlotsCount};
				copySlots(fn, recv, tmp, copySlotsCount);
				recv = tmp;
				recv.type = "ptr";
				recv.arrayAlloca = true;
				recv.slots = copySlotsCount;
			} else if (recvByRef) {
				recv.type = "ptr";
				recv.arrayAlloca = recvLayout.aggregate || recvLayout.slots > 1;
				recv.slots = recvLayout.slots;
			}
			args.push_back(recv);
			for (size_t i = 0; i < call->args.size(); ++i) {
				TypeRef paramType = (minfo && i < minfo->params.size()) ? minfo->params[i] : nullptr;
				TypeLayout pLayout = layoutOf(paramType);
				bool paramByRef = isRefType(paramType);
				bool paramMutable = (minfo && i < minfo->paramMut.size()) ? minfo->paramMut[i] : false;
				Value argV = paramByRef ? getLValuePtr(fn, call->args[i].get(), paramType) : emitExpr(fn, call->args[i].get());
				TypeLayout argLayout = layoutOf(exprType(call->args[i].get()));
				size_t argSlots = std::max<size_t>(1, std::max<size_t>(argV.slots, argLayout.slots));
				bool wantsAggregate = pLayout.aggregate || pLayout.slots > 1 || argLayout.aggregate || argSlots > 1 || argV.arrayAlloca;
				if (paramByRef) {
					argV.type = "ptr";
					argV.arrayAlloca = argLayout.aggregate || argLayout.slots > 1 || argV.arrayAlloca;
					argV.slots = std::max<size_t>(pLayout.slots, argSlots);
					args.push_back(argV);
					continue;
				}
				if (wantsAggregate) {
					size_t copySlotsCount = std::max<size_t>(pLayout.slots, argSlots);
					auto forceCopy = [&](Value &v) {
						std::string tmpAlloc = freshTemp(fn);
						fn.body << "  " << tmpAlloc << " = alloca [" << copySlotsCount << " x i64]\n";
						Value tmp{tmpAlloc, "ptr", true, copySlotsCount};
						copySlots(fn, v, tmp, copySlotsCount);
						v = tmp;
						v.type = "ptr";
						v.slots = copySlotsCount;
						v.arrayAlloca = true;
					};
					if (paramMutable) {
						forceCopy(argV);
						args.push_back(argV);
					} else {
						if (argV.type != "ptr" || argV.slots < copySlotsCount || !argV.arrayAlloca) {
							forceCopy(argV);
						} else {
							argV.type = "ptr";
							argV.slots = copySlotsCount;
							argV.arrayAlloca = true;
						}
						args.push_back(argV);
					}
				} else {
					args.push_back(toI64(fn, argV));
				}
			}
			std::ostringstream argss;
			if (aggRet) {
				argss << "ptr " << retDest.name;
				if (!args.empty()) argss << ", ";
			}
			for (size_t i = 0; i < args.size(); ++i) {
				if (i) argss << ", ";
				argss << (args[i].type == "ptr" ? "ptr " : "i64 ") << args[i].name;
			}
			if (aggRet) {
				fn.body << "  call void @" << mangled << "(" << argss.str() << ")\n";
				g_declArity[mangled] = std::max<size_t>(g_declArity[mangled], args.size() + 1);
				return retDest;
			}
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i64 @" << mangled << "(" << argss.str() << ")\n";
			g_declArity[mangled] = std::max<size_t>(g_declArity[mangled], args.size());
			return {tmp, "i64"};
		}
		std::vector<Value> args;
		FunctionInfo *info = g_analyzer ? g_analyzer->findFunction(call->call) : nullptr;
		TypeLayout retLayout = layoutOf(info ? info->returnType : nullptr);
		bool aggRet = retLayout.aggregate || retLayout.slots > 1;
		Value retDest;
		if (aggRet) {
			retDest = {freshTemp(fn), "ptr", true, retLayout.slots};
			fn.body << "  " << retDest.name << " = alloca [" << retLayout.slots << " x i64]\n";
		}
		for (size_t i = 0; i < call->args.size(); ++i) {
			TypeRef paramType = (info && i < info->params.size()) ? info->params[i] : nullptr;
			TypeLayout layout = layoutOf(paramType);
			bool paramByRef = isRefType(paramType);
			bool paramMutable = (info && i < info->paramMut.size()) ? info->paramMut[i] : false;
			Value argV = paramByRef ? getLValuePtr(fn, call->args[i].get(), paramType) : emitExpr(fn, call->args[i].get());
			TypeLayout argLayout = layoutOf(exprType(call->args[i].get()));
			size_t argSlots = std::max<size_t>(1, std::max<size_t>(argV.slots, argLayout.slots));
			bool wantsAggregate = layout.aggregate || layout.slots > 1 || argLayout.aggregate || argSlots > 1 || argV.arrayAlloca;
			if (paramByRef) {
				argV.type = "ptr";
				argV.arrayAlloca = argLayout.aggregate || argLayout.slots > 1 || argV.arrayAlloca;
				argV.slots = std::max<size_t>(layout.slots, argSlots);
				args.push_back(argV);
				continue;
			}
			if (wantsAggregate) {
				size_t copySlotsCount = std::max<size_t>(layout.slots, argSlots);
				auto forceCopy = [&](Value &v) {
					std::string tmpAlloca = freshTemp(fn);
					fn.body << "  " << tmpAlloca << " = alloca [" << copySlotsCount << " x i64]\n";
					Value dst{tmpAlloca, "ptr", true, copySlotsCount};
					copySlots(fn, v, dst, copySlotsCount);
					v = dst;
					v.type = "ptr";
					v.slots = copySlotsCount;
					v.arrayAlloca = true;
				};
				if (paramMutable) {
					forceCopy(argV);
					args.push_back(argV);
				} else {
					if (argV.type != "ptr" || argV.slots < copySlotsCount || !argV.arrayAlloca) {
						forceCopy(argV);
					} else {
						argV.type = "ptr";
						argV.slots = copySlotsCount;
						argV.arrayAlloca = true;
					}
					args.push_back(argV);
				}
			} else {
				args.push_back(toI64(fn, argV));
			}
		}
		std::ostringstream argss;
		if (aggRet) {
			argss << "ptr " << retDest.name;
			if (!args.empty()) argss << ", ";
		}
		for (size_t i = 0; i < args.size(); ++i) {
			if (i) argss << ", ";
			argss << (args[i].type == "ptr" ? "ptr " : "i64 ") << args[i].name;
		}
		const std::string &name = call->call;
		if (name == "printlnInt") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i64 @printlnInt(i64 " << args[0].name << ")\n";
			return {tmp, "i64"};
		}
		if (name == "printlnStr") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i64 @printlnStr(ptr " << args[0].name << ")\n";
			return {tmp, "i64"};
		}
		if (name == "stringLength") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i64 @stringLength(ptr " << args[0].name << ")\n";
			return {tmp, "i64"};
		}
		if (name == "stringEquals") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i1 @stringEquals(ptr " << args[0].name << ", ptr " << args[1].name << ")\n";
			std::string tmp2 = freshTemp(fn);
			fn.body << "  " << tmp2 << " = zext i1 " << tmp << " to i64\n";
			return {tmp2, "i64"};
		}
		if (name == "stringConcat") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call ptr @stringConcat(ptr " << args[0].name << ", ptr " << args[1].name << ")\n";
			return {tmp, "ptr"};
		}
		if (name == "getInt") {
			std::string tmp = freshTemp(fn);
			fn.body << "  " << tmp << " = call i64 @getInt()\n";
			return {tmp, "i64"};
		}
		if (name == "exit") {
			fn.body << "  call void @exit_rt(i64 " << (args.empty() ? std::string("0") : args[0].name) << ")\n";
			fn.terminated = true;
			return emitNumber(0);
		}
		if (aggRet) {
			fn.body << "  call void @" << name << "(" << argss.str() << ")\n";
			auto it = g_declArity.find(name);
			if (it == g_declArity.end() || it->second < args.size() + 1) {
				g_declArity[name] = args.size() + 1;
			}
			return retDest;
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = call i64 @" << name << "(" << argss.str() << ")\n";
		auto it = g_declArity.find(name);
		if (it == g_declArity.end() || it->second < args.size()) {
			g_declArity[name] = args.size();
		}
		return {tmp, "i64"};
	}
	if (auto *structLit = dynamic_cast<StructExprAST *>(expr)) {
		TypeRef stType = exprType(expr);
		auto layout = layoutOf(stType);
		size_t totalSlots = layout.slots;
		std::string allocaName = freshTemp(fn);
		fn.body << "  " << allocaName << " = alloca [" << totalSlots << " x i64]\n";
		Value dst{allocaName, "ptr", true, totalSlots};
		auto structName = stType ? stripRef(stType)->name : structLit->name;
		auto &fields = getStructLayout(structName);
		for (auto &field : structLit->fields) {
			auto it = std::find_if(fields.begin(), fields.end(), [&](auto &t) { return std::get<0>(t) == field.first; });
			if (it == fields.end()) continue;
			size_t offset = std::get<1>(*it);
			size_t slots = std::get<2>(*it);
			TypeLayout fldLayout = layoutOf(std::get<3>(*it));
			Value val = emitExpr(fn, field.second.get());
			if (fldLayout.aggregate || fldLayout.slots > 1) {
				if (val.type != "ptr") {
					std::string tmpAlloc = freshTemp(fn);
					fn.body << "  " << tmpAlloc << " = alloca [" << slots << " x i64]\n";
					Value tmp{tmpAlloc, "ptr", true, slots};
					copySlots(fn, val, tmp, slots);
					val = tmp;
				}
				Value dstField = dst;
				dstField.arrayAlloca = true;
				std::string ptr = gepSlot(fn, dstField, offset);
				Value dstPtr{ptr, "ptr", false, slots};
				copySlots(fn, val, dstPtr, slots);
			} else {
				val = toI64(fn, val);
				std::string ptr = gepSlot(fn, dst, offset);
				fn.body << "  store i64 " << val.name << ", ptr " << ptr << "\n";
			}
		}
		return dst;
	}
	if (auto *staticCall = dynamic_cast<StaticCallExprAST *>(expr)) {
		FunctionInfo *info = g_analyzer ? g_analyzer->findMethod(staticCall->type_name, staticCall->method_name) : nullptr;
		std::string mangled = staticCall->type_name + "__" + staticCall->method_name;
		TypeLayout retLayout = layoutOf(info ? info->returnType : nullptr);
		bool aggRet = retLayout.aggregate || retLayout.slots > 1;
		Value retDest;
		if (aggRet) {
			retDest = {freshTemp(fn), "ptr", true, retLayout.slots};
			fn.body << "  " << retDest.name << " = alloca [" << retLayout.slots << " x i64]\n";
		}
		std::vector<Value> args;
		for (size_t i = 0; i < staticCall->args.size(); ++i) {
			TypeRef paramType = (info && i < info->params.size()) ? info->params[i] : nullptr;
			TypeLayout pLayout = layoutOf(paramType);
			bool paramByRef = isRefType(paramType);
			bool paramMutable = (info && i < info->paramMut.size()) ? info->paramMut[i] : false;
			Value argV = paramByRef ? getLValuePtr(fn, staticCall->args[i].get(), paramType) : emitExpr(fn, staticCall->args[i].get());
			TypeLayout argLayout = layoutOf(exprType(staticCall->args[i].get()));
			size_t argSlots = std::max<size_t>(1, std::max<size_t>(argV.slots, argLayout.slots));
			bool wantsAggregate = pLayout.aggregate || pLayout.slots > 1 || argLayout.aggregate || argSlots > 1 || argV.arrayAlloca;
			if (paramByRef) {
				argV.type = "ptr";
				argV.arrayAlloca = argLayout.aggregate || argLayout.slots > 1 || argV.arrayAlloca;
				argV.slots = std::max<size_t>(pLayout.slots, argSlots);
				args.push_back(argV);
				continue;
			}
			if (wantsAggregate) {
				size_t copySlotsCount = std::max<size_t>(pLayout.slots, argSlots);
				auto forceCopy = [&](Value &v) {
					std::string tmpAlloc = freshTemp(fn);
					fn.body << "  " << tmpAlloc << " = alloca [" << copySlotsCount << " x i64]\n";
					Value tmp{tmpAlloc, "ptr", true, copySlotsCount};
					copySlots(fn, v, tmp, copySlotsCount);
					v = tmp;
					v.type = "ptr";
					v.slots = copySlotsCount;
					v.arrayAlloca = true;
				};
				if (paramMutable) {
					forceCopy(argV);
					args.push_back(argV);
				} else {
					if (argV.type != "ptr" || argV.slots < copySlotsCount || !argV.arrayAlloca) {
						forceCopy(argV);
					} else {
						argV.type = "ptr";
						argV.slots = copySlotsCount;
						argV.arrayAlloca = true;
					}
					args.push_back(argV);
				}
			} else {
				args.push_back(toI64(fn, argV));
			}
		}
		std::ostringstream argss;
		if (aggRet) {
			argss << "ptr " << retDest.name;
			if (!args.empty()) argss << ", ";
		}
		for (size_t i = 0; i < args.size(); ++i) {
			if (i) argss << ", ";
			argss << (args[i].type == "ptr" ? "ptr " : "i64 ") << args[i].name;
		}
		if (aggRet) {
			fn.body << "  call void @" << mangled << "(" << argss.str() << ")\n";
			g_declArity[mangled] = std::max<size_t>(g_declArity[mangled], args.size() + 1);
			return retDest;
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = call i64 @" << mangled << "(" << argss.str() << ")\n";
		g_declArity[mangled] = std::max<size_t>(g_declArity[mangled], args.size());
		return {tmp, "i64"};
	}
	if (auto *mem = dynamic_cast<MemberAccessExprAST *>(expr)) {
		Value base = emitExpr(fn, mem->struct_expr.get());
		if (base.type != "ptr") {
			Value tmp{freshTemp(fn), "ptr"};
			fn.body << "  " << tmp.name << " = inttoptr i64 " << base.name << " to ptr\n";
			tmp.arrayAlloca = base.arrayAlloca;
			tmp.slots = base.slots;
			base = tmp;
		}
		TypeRef baseType = exprType(mem->struct_expr.get());
		TypeRef stripped = stripRef(baseType);
		auto &fields = getStructLayout(stripped ? stripped->name : "");
		auto it = std::find_if(fields.begin(), fields.end(), [&](auto &t) { return std::get<0>(t) == mem->member_name; });
		if (it == fields.end()) return fallbackValue();
		size_t offset = std::get<1>(*it);
		size_t slots = std::get<2>(*it);
		TypeLayout fldLayout = layoutOf(std::get<3>(*it));
		std::string ptr = gepSlot(fn, base, offset);
		if (fldLayout.aggregate || fldLayout.slots > 1) {
			return {ptr, "ptr", false, slots};
		}
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = load i64, ptr " << ptr << "\n";
		return {tmp, "i64"};
	}
	if (auto *cast = dynamic_cast<CastExprAST *>(expr)) {
		Value v = emitExpr(fn, cast->expr.get());
		std::string tgt = typeOf(cast->target_type.get());
		if (tgt == "i1") return ensureBool(fn, v);
		return toI64(fn, v);
	}
	if (auto *arr = dynamic_cast<ArrayExprAST *>(expr)) {
		TypeRef arrType = exprType(expr);
		TypeLayout arrLayout = layoutOf(arrType);
		size_t totalSlots = std::max<size_t>(1, arrLayout.slots);
		Value dst{freshTemp(fn), "ptr", true, totalSlots};
		fn.body << "  " << dst.name << " = alloca [" << totalSlots << " x i64]\n";
		TypeRef stripped = stripRef(arrType);
		TypeRef elemType = (stripped && stripped->kind == BaseType::Array) ? stripped->elementType : nullptr;
		TypeLayout elemLayout = layoutOf(elemType);
		size_t elemSlots = std::max<size_t>(1, elemLayout.slots);
		size_t elemCount = elemSlots ? totalSlots / elemSlots : 0;
		if (arr->is_repeated) {
			Value val = emitExpr(fn, arr->element.get());
			if (elemLayout.aggregate || elemLayout.slots > 1) {
				if (val.type != "ptr") {
					std::string tmpAlloc = freshTemp(fn);
					fn.body << "  " << tmpAlloc << " = alloca [" << elemSlots << " x i64]\n";
					Value tmp{tmpAlloc, "ptr", true, elemSlots};
					copySlots(fn, val, tmp, elemSlots);
					val = tmp;
				}
				for (size_t i = 0; i < elemCount; ++i) {
					Value slotBase = dst;
					slotBase.arrayAlloca = true;
					std::string ptr = gepSlot(fn, slotBase, i * elemSlots);
					Value dstPtr{ptr, "ptr", false, elemSlots};
					copySlots(fn, val, dstPtr, elemSlots);
				}
			} else {
				val = toI64(fn, val);
				for (size_t i = 0; i < elemCount; ++i) {
					std::string ptr = gepSlot(fn, dst, i);
					fn.body << "  store i64 " << val.name << ", ptr " << ptr << "\n";
				}
			}
		} else {
			for (size_t i = 0; i < arr->elements.size() && i < elemCount; ++i) {
				Value val = emitExpr(fn, arr->elements[i].get());
				if (elemLayout.aggregate || elemLayout.slots > 1) {
					if (val.type != "ptr") {
						std::string tmpAlloc = freshTemp(fn);
						fn.body << "  " << tmpAlloc << " = alloca [" << elemSlots << " x i64]\n";
						Value tmp{tmpAlloc, "ptr", true, elemSlots};
						copySlots(fn, val, tmp, elemSlots);
						val = tmp;
					}
					std::string ptr = gepSlot(fn, dst, i * elemSlots);
					Value dstPtr{ptr, "ptr", false, elemSlots};
					copySlots(fn, val, dstPtr, elemSlots);
				} else {
					val = toI64(fn, val);
					std::string ptr = gepSlot(fn, dst, i);
					fn.body << "  store i64 " << val.name << ", ptr " << ptr << "\n";
				}
			}
		}
		return dst;
	}
	if (auto *ifexpr = dynamic_cast<IfExprAST *>(expr)) {
		auto cond = emitExpr(fn, ifexpr->cond.get());
		cond = ensureBool(fn, cond);
		std::string thenL = freshLabel(fn, "then");
		std::string elseL = freshLabel(fn, "else");
		std::string mergeL = freshLabel(fn, "ifend");
		TypeLayout resLayout = layoutOf(exprType(ifexpr));
		bool aggResult = resLayout.aggregate || resLayout.slots > 1;
		Value aggDest;
		if (aggResult) {
			aggDest = {freshTemp(fn), "ptr", true, std::max<size_t>(1, resLayout.slots)};
			fn.body << "  " << aggDest.name << " = alloca [" << aggDest.slots << " x i64]\n";
		}
		auto copyToAgg = [&](const Value &src) {
			Value dst{aggDest.name, "ptr", true, aggDest.slots};
			Value val = src;
			if (val.type != "ptr") {
				std::string tmpAlloc = freshTemp(fn);
				fn.body << "  " << tmpAlloc << " = alloca [" << aggDest.slots << " x i64]\n";
				Value tmp{tmpAlloc, "ptr", true, aggDest.slots};
				copySlots(fn, val, tmp, aggDest.slots);
				val = tmp;
			}
			copySlots(fn, val, dst, aggDest.slots);
		};
		fn.body << "  br i1 " << cond.name << ", label %" << thenL << ", label %" << elseL << "\n";
		fn.terminated = true;

		startBlock(fn, thenL);
		auto thenV = emitExpr(fn, ifexpr->then_branch.get());
		bool thenFlows = !fn.terminated;
		if (thenFlows) {
			if (aggResult) copyToAgg(thenV);
			fn.body << "  br label %" << mergeL << "\n";
			fn.terminated = true;
		}

		startBlock(fn, elseL);
		auto elseV = emitExpr(fn, ifexpr->else_branch.get());
		bool elseFlows = !fn.terminated;
		if (elseFlows) {
			if (aggResult) copyToAgg(elseV);
			fn.body << "  br label %" << mergeL << "\n";
			fn.terminated = true;
		}

		startBlock(fn, mergeL);
		if (!thenFlows && !elseFlows) {
			fn.terminated = true;
			return fallbackValue();
		}
		if (aggResult) {
			return aggDest;
		}
		if (thenFlows && !elseFlows) {
			return toI64(fn, thenV);
		}
		if (!thenFlows && elseFlows) {
			return toI64(fn, elseV);
		}
		thenV = toI64(fn, thenV);
		elseV = toI64(fn, elseV);
		std::string tmp = freshTemp(fn);
		fn.body << "  " << tmp << " = phi i64 [ " << thenV.name << ", %" << thenL << " ], [ " << elseV.name << ", %" << elseL << " ]\n";
		return {tmp, "i64"};
	}

	if (auto *block = dynamic_cast<BlockExprAST *>(expr)) {
		Value last = emitNumber(0);
		for (auto &st : block->statements) {
			emitStmt(fn, st.get());
			if (fn.terminated) return last;
		}
		if (block->value && !fn.terminated) {
			last = emitExpr(fn, block->value.get());
		}
		return last;
	}
	if (auto *loop = dynamic_cast<LoopExprAST *>(expr)) {
		std::string header = freshLabel(fn, "loop");
		std::string bodyL = freshLabel(fn, "loopbody");
		std::string exitL = freshLabel(fn, "loopexit");
		fn.body << "  br label %" << header << "\n";
		fn.terminated = true;
		startBlock(fn, header);
		fn.body << "  br label %" << bodyL << "\n";
		fn.terminated = true;
		startBlock(fn, bodyL);
		std::string savedBreak = fn.breakLabel;
		std::string savedCont = fn.continueLabel;
		fn.breakLabel = exitL;
		fn.continueLabel = header;
		emitStmt(fn, loop->body.get());
		fn.breakLabel = savedBreak;
		fn.continueLabel = savedCont;
		if (!fn.terminated) {
			fn.body << "  br label %" << header << "\n";
			fn.terminated = true;
		}
		startBlock(fn, exitL);
		return emitNumber(0);
	}

	return fallbackValue();
}

void emitStmt(FunctionCtx &fn, StmtAST *stmt) {
	if (!stmt) return;
	if (auto *exprs = dynamic_cast<ExprStmtAST *>(stmt)) {
		(void) emitExpr(fn, exprs->expr.get());
		return;
	}
	if (auto *block = dynamic_cast<BlockStmtAST *>(stmt)) {
		for (auto &s : block->statements) {
			emitStmt(fn, s.get());
			if (fn.terminated) break;
		}
		return;
	}
	if (auto *let = dynamic_cast<LetStmtAST *>(stmt)) {
		auto *ident = dynamic_cast<IdentPatternAST *>(let->pattern.get());
		if (!ident) return;
		TypeRef varType = exprType(let->value.get());
		TypeLayout layout = layoutOf(varType);
		auto &info = ensureVar(fn, ident->name, varType);
		info.layout = layout;
		info.arrayAlloca = layout.aggregate || layout.slots > 1;
		info.type = varType;
		if (info.ptr.empty()) {
			info = makeAlloca(fn, ident->name, layout);
			fn.vars[ident->name] = info;
		}
		if (let->value) {
			Value rhs = emitExpr(fn, let->value.get());
			if (layout.aggregate || layout.slots > 1) {
				if (rhs.type != "ptr") {
					std::string tmpAlloc = freshTemp(fn);
					fn.body << "  " << tmpAlloc << " = alloca [" << layout.slots << " x i64]\n";
					Value tmp{tmpAlloc, "ptr", true, layout.slots};
					copySlots(fn, rhs, tmp, layout.slots);
					rhs = tmp;
				}
				Value dst{info.ptr, "ptr", info.arrayAlloca, layout.slots};
				copySlots(fn, rhs, dst, layout.slots);
			} else {
				rhs = toI64(fn, rhs);
				fn.body << "  store i64 " << rhs.name << ", ptr " << info.ptr << "\n";
			}
		}
		return;
	}
	if (auto *asn = dynamic_cast<AssignStmtAST *>(stmt)) {
		TypeRef lhsType = exprType(asn->lhs_expr.get());
		auto lhsLayout = layoutOf(lhsType);
		Value rhs = emitExpr(fn, asn->value.get());
		if (lhsLayout.aggregate || lhsLayout.slots > 1) {
			if (rhs.type != "ptr") {
				std::string tmpAlloc = freshTemp(fn);
				fn.body << "  " << tmpAlloc << " = alloca [" << lhsLayout.slots << " x i64]\n";
				Value tmp{tmpAlloc, "ptr", true, lhsLayout.slots};
				copySlots(fn, rhs, tmp, lhsLayout.slots);
				rhs = tmp;
			}
		}
		auto combineScalar = [&](const std::string &ptr, Value rhsVal) {
			rhsVal = toI64(fn, rhsVal);
			if (asn->op == "=") return rhsVal;
			std::string cur = freshTemp(fn);
			fn.body << "  " << cur << " = load i64, ptr " << ptr << "\n";
			std::string tmp = freshTemp(fn);
			std::string opcode;
			if (asn->op == "+=") opcode = "add";
			else if (asn->op == "-=") opcode = "sub";
			else if (asn->op == "*=") opcode = "mul";
			else if (asn->op == "/=") opcode = "sdiv";
			else if (asn->op == "%=") opcode = "srem";
			else if (asn->op == "&=") opcode = "and";
			else if (asn->op == "|=") opcode = "or";
			else if (asn->op == "^=") opcode = "xor";
			else if (asn->op == "<<=") opcode = "shl";
			else if (asn->op == ">>=") opcode = "lshr"; // logical shift to converge
			else opcode = "add";
			fn.body << "  " << tmp << " = " << opcode << " i64 " << cur << ", " << rhsVal.name << "\n";
			return Value{tmp, "i64"};
		};
		if (auto *lhsVar = dynamic_cast<VariableExprAST *>(asn->lhs_expr.get())) {
			auto &info = ensureVar(fn, lhsVar->name, lhsType);
			info.layout = lhsLayout;
			if (lhsLayout.aggregate || lhsLayout.slots > 1) {
				Value dst{info.ptr, "ptr", info.arrayAlloca, lhsLayout.slots};
				copySlots(fn, rhs, dst, lhsLayout.slots);
			} else {
				auto v = combineScalar(info.ptr, rhs);
				fn.body << "  store i64 " << v.name << ", ptr " << info.ptr << "\n";
			}
			return;
		}
		if (auto *lhsIdx = dynamic_cast<ArrayIndexExprAST *>(asn->lhs_expr.get())) {
			auto base = emitExpr(fn, lhsIdx->array_expr.get());
			auto index = toI64(fn, emitExpr(fn, lhsIdx->index_expr.get()));
			Value basePtr = base.type == "ptr" ? base : Value{freshTemp(fn), "ptr"};
			if (basePtr.name != base.name || basePtr.type != base.type) {
				fn.body << "  " << basePtr.name << " = inttoptr i64 " << base.name << " to ptr\n";
			}
			TypeRef arrType = exprType(lhsIdx->array_expr.get());
			TypeRef stripped = stripRef(arrType);
			TypeRef elemType = (stripped && stripped->kind == BaseType::Array) ? stripped->elementType : nullptr;
			auto elemLayout = layoutOf(elemType);
			size_t elemSlots = std::max<size_t>(1, elemLayout.slots);
			std::string scaled = freshTemp(fn);
			fn.body << "  " << scaled << " = mul i64 " << index.name << ", " << elemSlots << "\n";
			std::string elemPtr = basePtr.arrayAlloca ? freshTemp(fn) : freshTemp(fn);
			if (basePtr.arrayAlloca) {
				fn.body << "  " << elemPtr << " = getelementptr [" << basePtr.slots << " x i64], ptr " << basePtr.name << ", i64 0, i64 " << scaled << "\n";
			} else {
				fn.body << "  " << elemPtr << " = getelementptr i64, ptr " << basePtr.name << ", i64 " << scaled << "\n";
			}
			if (elemLayout.aggregate || elemLayout.slots > 1) {
				Value dst{elemPtr, "ptr", false, elemLayout.slots};
				copySlots(fn, rhs, dst, elemLayout.slots);
			} else {
				auto v = combineScalar(elemPtr, rhs);
				fn.body << "  store i64 " << v.name << ", ptr " << elemPtr << "\n";
			}
			return;
		}
		if (auto *lhsMem = dynamic_cast<MemberAccessExprAST *>(asn->lhs_expr.get())) {
			Value base = emitExpr(fn, lhsMem->struct_expr.get());
			if (base.type != "ptr") {
				Value tmp{freshTemp(fn), "ptr"};
				fn.body << "  " << tmp.name << " = inttoptr i64 " << base.name << " to ptr\n";
				tmp.arrayAlloca = base.arrayAlloca;
				tmp.slots = base.slots;
				base = tmp;
			}
			TypeRef baseType = exprType(lhsMem->struct_expr.get());
			TypeRef stripped = stripRef(baseType);
			auto &fields = getStructLayout(stripped ? stripped->name : "");
			auto it = std::find_if(fields.begin(), fields.end(), [&](auto &t) { return std::get<0>(t) == lhsMem->member_name; });
			if (it == fields.end()) return;
			size_t offset = std::get<1>(*it);
			size_t slots = std::get<2>(*it);
			TypeLayout fldLayout = layoutOf(std::get<3>(*it));
			std::string ptr = gepSlot(fn, base, offset);
			if (fldLayout.aggregate || fldLayout.slots > 1) {
				Value dst{ptr, "ptr", false, slots};
				copySlots(fn, rhs, dst, slots);
			} else {
				auto v = combineScalar(ptr, rhs);
				fn.body << "  store i64 " << v.name << ", ptr " << ptr << "\n";
			}
			return;
		}
		if (auto *lhsDeref = dynamic_cast<UnaryExprAST *>(asn->lhs_expr.get())) {
			if (lhsDeref->op == "*") {
				Value base = emitExpr(fn, lhsDeref->expr.get());
				if (base.type != "ptr") {
					std::string tmpPtr = freshTemp(fn);
					fn.body << "  " << tmpPtr << " = inttoptr i64 " << base.name << " to ptr\n";
					base = {tmpPtr, "ptr", base.arrayAlloca, base.slots};
				}
				if (lhsLayout.aggregate || lhsLayout.slots > 1) {
					Value dst{base.name, "ptr", base.arrayAlloca, lhsLayout.slots};
					copySlots(fn, rhs, dst, lhsLayout.slots);
				} else {
					auto v = combineScalar(base.name, rhs);
					fn.body << "  store i64 " << v.name << ", ptr " << base.name << "\n";
				}
				return;
			}
		}
		return;
	}
	if (auto *ifs = dynamic_cast<IfStmtAST *>(stmt)) {
		auto cond = ensureBool(fn, emitExpr(fn, ifs->cond.get()));
		std::string thenL = freshLabel(fn, "then");
		std::string elseL = freshLabel(fn, "else");
		std::string endL = freshLabel(fn, "ifend");
		fn.body << "  br i1 " << cond.name << ", label %" << thenL << ", label %" << elseL << "\n";
		fn.terminated = true;
		startBlock(fn, thenL);
		emitStmt(fn, ifs->then_branch.get());
		if (!fn.terminated) {
			fn.body << "  br label %" << endL << "\n";
			fn.terminated = true;
		}
		startBlock(fn, elseL);
		emitStmt(fn, ifs->else_branch.get());
		if (!fn.terminated) {
			fn.body << "  br label %" << endL << "\n";
			fn.terminated = true;
		}
		startBlock(fn, endL);
		return;
	}
	if (auto *wh = dynamic_cast<WhileStmtAST *>(stmt)) {
		std::string head = freshLabel(fn, "while");
		std::string bodyL = freshLabel(fn, "whilebody");
		std::string exitL = freshLabel(fn, "whileexit");
		fn.body << "  br label %" << head << "\n";
		fn.terminated = true;
		startBlock(fn, head);
		auto cond = ensureBool(fn, emitExpr(fn, wh->cond.get()));
		fn.body << "  br i1 " << cond.name << ", label %" << bodyL << ", label %" << exitL << "\n";
		fn.terminated = true;
		startBlock(fn, bodyL);
		std::string savedBreak = fn.breakLabel;
		std::string savedCont = fn.continueLabel;
		fn.breakLabel = exitL;
		fn.continueLabel = head;
		emitStmt(fn, wh->body.get());
		fn.breakLabel = savedBreak;
		fn.continueLabel = savedCont;
		if (!fn.terminated) {
			fn.body << "  br label %" << head << "\n";
			fn.terminated = true;
		}
		startBlock(fn, exitL);
		return;
	}
	if (auto *lp = dynamic_cast<LoopStmtAST *>(stmt)) {
		std::string head = freshLabel(fn, "loop");
		std::string bodyL = freshLabel(fn, "loopbody");
		std::string exitL = freshLabel(fn, "loopexit");
		fn.body << "  br label %" << head << "\n";
		fn.terminated = true;
		startBlock(fn, head);
		fn.body << "  br label %" << bodyL << "\n";
		fn.terminated = true;
		startBlock(fn, bodyL);
		std::string savedBreak = fn.breakLabel;
		std::string savedCont = fn.continueLabel;
		fn.breakLabel = exitL;
		fn.continueLabel = head;
		emitStmt(fn, lp->body.get());
		fn.breakLabel = savedBreak;
		fn.continueLabel = savedCont;
		if (!fn.terminated) {
			fn.body << "  br label %" << head << "\n";
			fn.terminated = true;
		}
		startBlock(fn, exitL);
		return;
	}
	if (auto *br = dynamic_cast<BreakStmtAST *>(stmt)) {
		if (fn.breakLabel.empty()) return;
		fn.body << "  br label %" << fn.breakLabel << "\n";
		fn.terminated = true;
		return;
	}
	if (auto *cont = dynamic_cast<ContinueStmtAST *>(stmt)) {
		if (fn.continueLabel.empty()) return;
		fn.body << "  br label %" << fn.continueLabel << "\n";
		fn.terminated = true;
		return;
	}
	if (auto *ret = dynamic_cast<ReturnStmtAST *>(stmt)) {
		if (fn.aggregateReturn) {
			Value rhs = emitExpr(fn, ret->value.get());
			if (rhs.type != "ptr") {
				std::string tmpAlloc = freshTemp(fn);
				fn.body << "  " << tmpAlloc << " = alloca [" << fn.retLayout.slots << " x i64]\n";
				Value tmp{tmpAlloc, "ptr", true, fn.retLayout.slots};
				copySlots(fn, rhs, tmp, fn.retLayout.slots);
				rhs = tmp;
			}
			Value dst{fn.retPtr, "ptr", true, fn.retLayout.slots};
			copySlots(fn, rhs, dst, fn.retLayout.slots);
			fn.body << "  ret void\n";
		} else if (fn.returnsVoid) {
			fn.body << "  ret void\n";
		} else {
			auto v = toI64(fn, emitExpr(fn, ret->value.get()));
			fn.body << "  ret i64 " << v.name << "\n";
		}
		fn.terminated = true;
		return;
	}
}

void emitFunction(std::ostringstream &mod, FnStmtAST *fnAst, const std::string &ownerType = "") {
	FunctionCtx fn;
	FunctionInfo *finfo = nullptr;
	if (g_analyzer) {
		finfo = ownerType.empty() ? g_analyzer->findFunction(fnAst->name) : g_analyzer->findMethod(ownerType, fnAst->name);
	}
	fn.name = ownerType.empty() ? fnAst->name : (ownerType + "__" + fnAst->name);
	fn.retLayout = layoutOf(finfo ? finfo->returnType : nullptr);
	fn.aggregateReturn = fn.retLayout.aggregate || fn.retLayout.slots > 1;
	fn.retPtr = fn.aggregateReturn ? "%ret" : "";
	std::string retTy = fn.aggregateReturn ? std::string("void") : typeOf(fnAst->return_type.get());
	fn.returnsVoid = fn.aggregateReturn || (retTy == "void");

	std::ostringstream params;
	size_t paramIndex = 0;
	if (fn.aggregateReturn) {
		params << "ptr %ret";
	}
	if (finfo && finfo->isMethod && finfo->hasSelf) {
		if (paramIndex || fn.aggregateReturn) params << ", ";
		TypeLayout recvLayout = layoutOf(finfo->receiverType);
		bool recvByRef = finfo->selfIsReference;
		bool recvPtr = recvByRef || recvLayout.aggregate || recvLayout.slots > 1;
		params << (recvPtr ? "ptr" : "i64") << " %p" << paramIndex++;
	}
	for (size_t i = 0; i < fnAst->params.size(); ++i) {
		if (finfo && finfo->isMethod && finfo->hasSelf && i == 0) {
			continue; // skip self (already added)
		}
		if (i || paramIndex || fn.aggregateReturn) params << ", ";
		size_t semanticIdx = i;
		if (finfo && finfo->isMethod && finfo->hasSelf) {
			semanticIdx = i - 1; // shift because self not stored in finfo->params
		}
		TypeRef paramType = (finfo && semanticIdx < finfo->params.size()) ? finfo->params[semanticIdx] : nullptr;
		TypeLayout pLayout = layoutOf(paramType);
		bool paramByRef = isRefType(paramType);
		bool usePtr = paramByRef || pLayout.aggregate || pLayout.slots > 1;
		params << (usePtr ? "ptr" : "i64") << " %p" << paramIndex++;
	}

	mod << "define " << (fn.returnsVoid ? "void" : "i64") << " @" << fn.name << "(" << params.str() << ") {\n";
	mod << "entry:\n";
	fn.terminated = false;

	// allocate/record params
	paramIndex = 0;
	size_t semanticIdx = 0; // index into finfo->params (excludes self)
	if (finfo && finfo->isMethod && finfo->hasSelf) {
		TypeLayout recvLayout = layoutOf(finfo->receiverType);
		FunctionCtx::VarInfo info;
		info.type = finfo->receiverType;
		info.layout = recvLayout;
		info.ptr = "%p" + std::to_string(paramIndex++);
		info.arrayAlloca = recvLayout.aggregate || recvLayout.slots > 1;
		fn.vars["self"] = info;
	}
	for (size_t i = 0; i < fnAst->params.size(); ++i) {
		if (finfo && finfo->isMethod && finfo->hasSelf && i == 0) continue;
		auto &p = fnAst->params[i];
		auto *id = p.first.get();
		TypeRef paramType = (finfo && semanticIdx < finfo->params.size()) ? finfo->params[semanticIdx] : nullptr;
		TypeLayout pLayout = layoutOf(paramType);
		bool paramMutable = (finfo && semanticIdx < finfo->paramMut.size()) ? finfo->paramMut[semanticIdx] : false;
		FunctionCtx::VarInfo info;
		info.type = paramType;
		info.layout = pLayout;
		info.arrayAlloca = pLayout.aggregate || pLayout.slots > 1;
		std::string paramName = "%p" + std::to_string(paramIndex++);
		bool paramByRef = isRefType(paramType);
		if (paramByRef) {
			info.ptr = paramName;
			info.arrayAlloca = pLayout.aggregate || pLayout.slots > 1;
		} else if (pLayout.aggregate || pLayout.slots > 1) {
			size_t slots = std::max<size_t>(1, pLayout.slots);
			if (paramMutable) {
				info.ptr = "%" + id->name;
				info.arrayAlloca = true;
				fn.entryAllocas.push_back("  " + info.ptr + " = alloca [" + std::to_string(slots) + " x i64]\n");
				Value src{paramName, "ptr", true, slots};
				Value dst{info.ptr, "ptr", true, slots};
				copySlots(fn, src, dst, slots);
			} else {
				info.ptr = paramName;
				info.arrayAlloca = true;
				info.layout.slots = slots;
			}
		} else {
			info.ptr = "%" + id->name;
			fn.entryAllocas.push_back("  " + info.ptr + " = alloca i64\n");
			fn.entryAllocas.push_back("  store i64 " + paramName + ", ptr " + info.ptr + "\n");
		}
		fn.vars[id->name] = info;
		++semanticIdx;
	}

	emitStmt(fn, fnAst->body.get());

	for (auto &line : fn.entryAllocas) {
		mod << line;
	}

	mod << fn.body.str();
	if (!fn.terminated) {
		mod << (fn.returnsVoid ? "  ret void\n" : "  ret i64 0\n");
	}

	mod << "}\n\n";
}

// 生成字符串操作函数
void emitStringFunctions(std::ostringstream &mod) {
	// 字符串长度函数
	mod << "define i64 @stringLength(ptr %str) {\n";
	mod << "entry:\n";
	mod << "  %len = call i64 @strlen(ptr %str)\n";
	mod << "  ret i64 %len\n";
	mod << "}\n\n";
	
	// 字符串比较函数
	mod << "define i1 @stringEquals(ptr %str1, ptr %str2) {\n";
	mod << "entry:\n";
	mod << "  %cmp = call i32 @strcmp(ptr %str1, ptr %str2)\n";
	mod << "  %eq = icmp eq i32 %cmp, 0\n";
	mod << "  ret i1 %eq\n";
	mod << "}\n\n";
	
	// 字符串连接函数
	mod << "define ptr @stringConcat(ptr %str1, ptr %str2) {\n";
	mod << "entry:\n";
	mod << "  %len1 = call i64 @strlen(ptr %str1)\n";
	mod << "  %len2 = call i64 @strlen(ptr %str2)\n";
	mod << "  %totalLen = add i64 %len1, %len2\n";
	mod << "  %allocSize = add i64 %totalLen, 1\n";
	mod << "  %newStr = call ptr @malloc(i64 %allocSize)\n";
	mod << "  call void @memcpy(ptr %newStr, ptr %str1, i64 %len1)\n";
	mod << "  %destPtr = getelementptr i8, ptr %newStr, i64 %len1\n";
	mod << "  call void @memcpy(ptr %destPtr, ptr %str2, i64 %len2)\n";
	mod << "  %endPtr = getelementptr i8, ptr %newStr, i64 %totalLen\n";
	mod << "  store i8 0, ptr %endPtr\n";
	mod << "  ret ptr %newStr\n";
	mod << "}\n\n";
	
	// 声明外部函数
	mod << "declare i64 @strlen(ptr)\n";
	mod << "declare i32 @strcmp(ptr, ptr)\n";
	mod << "declare void @memcpy(ptr, ptr, i64)\n";
	mod << "declare ptr @malloc(i64)\n";
	mod << "declare void @free(ptr)\n\n";
}

bool writeModule(const fs::path &path, BlockStmtAST *program) {
	std::ofstream out(path, std::ios::trunc);
	if (!out.is_open()) return false;
	std::ostringstream mod;
	mod << "; Autogenerated textual LLVM IR\n";
	mod << "source_filename = \"RCompiler\"\n\n";
	
	g_declArity.clear();
	g_definedFuncs.clear();
	// iterate top-level functions and impl methods
	for (auto &stmt : program->statements) {
		if (auto *fn = dynamic_cast<FnStmtAST *>(stmt.get())) {
			g_definedFuncs.insert(fn->name);
			emitFunction(mod, fn);
		} else if (auto *impl = dynamic_cast<ImplStmtAST *>(stmt.get())) {
			for (auto &m : impl->methods) {
				std::string mangled = impl->type_name + "__" + m->name;
				g_definedFuncs.insert(mangled);
				emitFunction(mod, m.get(), impl->type_name);
			}
		}
	}

	// 检查是否需要字符串函数
	bool needsStringFunctions = false;
	for (auto &[name, arity] : g_declArity) {
		if (name == "stringLength" || name == "stringEquals" || name == "stringConcat") {
			needsStringFunctions = true;
			break;
		}
	}
	
	// 如果需要，添加字符串函数
	if (needsStringFunctions) {
		emitStringFunctions(mod);
	}
	
	// emit minimal runtime (printf/scanf wrappers) if referenced
	if (!g_declArity.empty()) {
		mod << "@.fmt_int = private constant [5 x i8] c\"%ld\\0A\\00\"\n";
		mod << "@.fmt_in = private constant [4 x i8] c\"%ld\\00\"\n";
		mod << "@.fmt_str = private constant [4 x i8] c\"%s\\0A\\00\"\n";
		mod << "declare i32 @printf(ptr, ...)\n";
		mod << "declare i32 @scanf(ptr, ...)\n";
		mod << "declare void @exit(i32)\n\n";
		mod << "define i64 @printlnInt(i64 %p0) {\nentry:\n  %fmt = getelementptr [5 x i8], ptr @.fmt_int, i64 0, i64 0\n  %r = call i32 (ptr, ...) @printf(ptr %fmt, i64 %p0)\n  ret i64 %p0\n}\n\n";
		mod << "define i64 @printlnStr(ptr %p0) {\nentry:\n  %fmt = getelementptr [4 x i8], ptr @.fmt_str, i64 0, i64 0\n  %r = call i32 (ptr, ...) @printf(ptr %fmt, ptr %p0)\n  ret i64 0\n}\n\n";
		mod << "define i64 @getInt() {\nentry:\n  %tmp = alloca i64\n  %fmt = getelementptr [4 x i8], ptr @.fmt_in, i64 0, i64 0\n  %r = call i32 (ptr, ...) @scanf(ptr %fmt, ptr %tmp)\n  %v = load i64, ptr %tmp\n  ret i64 %v\n}\n\n";
		mod << "define void @exit_rt(i64 %code) {\nentry:\n  %c = trunc i64 %code to i32\n  call void @exit(i32 %c)\n  ret void\n}\n\n";
		g_definedFuncs.insert("printlnInt");
		g_definedFuncs.insert("printlnStr");
		g_definedFuncs.insert("getInt");
		g_definedFuncs.insert("exit_rt");
		g_definedFuncs.insert("stringLength");
		g_definedFuncs.insert("stringEquals");
		g_definedFuncs.insert("stringConcat");
	}

	// emit stubs for any referenced but undefined functions to satisfy llc/clang
	for (auto &[name, arity] : g_declArity) {
		if (g_definedFuncs.count(name)) continue;
		mod << "define i64 @" << name << "(...) {\nentry:\n  ret i64 0\n}\n\n";
	}

	// If no function emitted, add a dummy main
	if (mod.str().find("define") == std::string::npos) {
		mod << "define i64 @main() {\nentry:\n  ret i64 0\n}\n";
	}

	out << mod.str();
	return true;
}

} // namespace

bool generate_ir(BlockStmtAST *program, SemanticAnalyzer &analyzer, const std::string &inputPath, bool emitLLVM) {
	g_analyzer = &analyzer;
	g_structLayouts.clear();
	if (!emitLLVM) return true;
	if (!program) {
		throw std::runtime_error("IR generation failed: null program");
	}
	const fs::path llPath = deriveLlPath(inputPath);
	if (!writeModule(llPath, program)) {
		throw std::runtime_error("IR generation failed: cannot create " + llPath.string());
	}
	return true;
}
