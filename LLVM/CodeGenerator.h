////
//// Created by Тихонов Александр on 04.01.2025.
////
//
//#ifndef COMPILER_LANGUAGE_CODEGENERATOR_H
//#define COMPILER_LANGUAGE_CODEGENERATOR_H
//
//#include "AST.h"
//#include "SymbolTable.h"
//#include <llvm/IR/LLVMContext.h>
//#include <llvm/IR/IRBuilder.h>
//#include <llvm/IR/Module.h>
//#include <unordered_map>
//#include <memory>
//
//class CodeGenerator {
//public:
//    CodeGenerator(SymbolTable &symTable)
//            : context(), builder(context), module("my_module", context), symbolTable(symTable) {}
//
//    std::unique_ptr<llvm::Module> generate(ASTNode &root);
//
//private:
//    llvm::LLVMContext context;
//    llvm::IRBuilder<> builder;
//    llvm::Module module;
//    SymbolTable &symbolTable;
//
//    // Таблица для хранения переменных текущего блока
//    std::unordered_map<std::string, llvm::Value*> namedValues;
//
//    void generateFunction(ASTNode &funcDecl);
//    llvm::Value* generateExpression(ASTNode &expr);
//    void generateStatement(ASTNode &stmt);
//};
//
//#endif //COMPILER_LANGUAGE_CODEGENERATOR_H
//

#pragma once

#include <memory>
#include <unordered_map>
#include "../Parser_AST/AST.h"
#include "../SemanticAnalyzer/SymbolTable.h"

// Подключаем основные заголовки LLVM
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>

class CodeGenerator {
public:
    explicit CodeGenerator(SymbolTable &symTable);

    // Генерируем LLVM IR для корневого узла (Program)
    std::unique_ptr<llvm::Module> generate(ASTNode &root);

private:
    llvm::LLVMContext context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    // Ссылка на таблицу символов (из вашего SemanticAnalyzer)
    SymbolTable &symbolTable;

    // Локальные переменные (имя -> alloca)
    std::unordered_map<std::string, llvm::Value*> namedValues;

private:
    // --- Вспомогательные функции ---
    llvm::Type *getLLVMType(const std::string &typeName);
    llvm::Function *createFunction(ASTNode &funcNode);
    void generateFunctionBody(ASTNode &funcNode, llvm::Function *function);

    // Генерация кода для разных узлов AST
    void generateTopLevelDecl(ASTNode &node);
    void generateVarDecl(const ASTNode &node);

//    llvm::Value *generateStatement(ASTNode &node);
//    llvm::Value *generateExpression(ASTNode &node);
//    void generateBlock(ASTNode &blockNode);

    uint64_t evaluateArraySize(const ASTNode &sizeNode);
    llvm::Value *generateArrayIndexPointer(const ASTNode &binaryOpNode);
    void generateForStatement(const ASTNode &node);
    void generateWhileStatement(const ASTNode &node);

    llvm::Value *generateStatement(const ASTNode &node);
    llvm::Value *generateExpression(const ASTNode &node);
    void generateBlock(const ASTNode &blockNode);

    // Помощники для alloca и т.п.
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *function,
                                             llvm::Type *type,
                                             const std::string &varName);
};
