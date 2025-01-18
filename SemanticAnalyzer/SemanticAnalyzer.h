//
// Created by Тихонов Александр on 26.12.2024.
//

#ifndef COMPILER_LANGUAGE_SEMANTICANALYZER_H
#define COMPILER_LANGUAGE_SEMANTICANALYZER_H

#include "../Parser_AST/AST.h"
#include "SymbolTable.h"
#include <iostream>
#include <stdexcept>
#include <sstream>
#include <unordered_map>

class SemanticAnalyzer {
public:
    explicit SemanticAnalyzer(ASTNode &root)
            : root(root)
    {
        symbolTable.enterScope();
        addBuiltInSymbols();
    }

    void analyze() {
        visitProgram(root);
    }

    SymbolTable& getSymbolTable(){
        return symbolTable;
    }

private:
    ASTNode &root;
    SymbolTable symbolTable;
    std::unordered_map<std::string, std::string> typeAliases;

    std::string currentFunctionName;
    std::string currentFunctionReturnType;

private:
    void addBuiltInSymbols() {
        Symbol nullptrSymbol("nullptr", "void*", false, {});
        if (!symbolTable.addSymbol(nullptrSymbol)) {
            throw std::runtime_error("Failed to add built-in symbol: nullptr");
        }
    }

    std::string getBaseType(const std::string &type) const {
        std::istringstream iss(type);
        std::string token, result;
        while (iss >> token) {
            if (token != "static" && token != "const") {
                auto it = typeAliases.find(token);
                if (it != typeAliases.end()) {
                    token = it->second;
                }
                result += token + " ";
            }
        }
        if (!result.empty() && result.back() == ' ') {
            result.pop_back();
        }
        return result;
    }

    bool isTypeCompatible(const std::string &expected, const std::string &actual) const
    {
        std::string eBase = getBaseType(expected);
        std::string aBase = getBaseType(actual);

        if (eBase == aBase) {
            return true;
        }

        auto stripArray = [](const std::string &t) {
            if (t.size() >= 2 && t.compare(t.size() - 2, 2, "[]") == 0) {
                return t.substr(0, t.size() - 2);  // отрезаем "[]"
            }
            return t;
        };

        auto stripPtr = [](const std::string &t) {
            if (!t.empty() && t.back() == '*') {
                return t.substr(0, t.size() - 1);  // отрезаем '*'
            }
            return t;
        };

        // expected = "integer[]", actual = "integer*"
        if (stripArray(eBase) == stripPtr(aBase)) {
            return true;
        }

        // expected = "integer*", actual = "integer[]"
        if (stripPtr(eBase) == stripArray(aBase)) {
            return true;
        }

        // Если ничего не совпало — возвращаем false
        return false;
    }

    bool endsWith(const std::string &str, const std::string &suffix) {
        if (str.size() < suffix.size()) return false;
        return (str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0);
    }

    std::string inferLiteralType(const std::string &value) {
        if (value.size() >= 2 && value.front() == '"' && value.back() == '"') {
            return "string";
        }
        if (value == "true" || value == "false") {
            return "bool";
        }
        if (value.find('.') != std::string::npos) {
            return "float";
        }
        return "integer";
    }

private:
    void visitProgram(ASTNode &node) {
        for (auto &child : node.children) {
            if (child.type == ASTNodeType::FunctionDecl) {
                visitFunctionDecl(child);
            }
            else if (child.type == ASTNodeType::VarDecl) {
                visitVarDecl(child);
            }
            else {
                throw std::runtime_error("Unknown top-level declaration");
            }
        }
    }

    void visitFunctionDecl(ASTNode &node) {
        if (node.children.empty()) {
            throw std::runtime_error("Function declaration missing children");
        }

        ASTNode &funcNameNode = node.children[0];
        if (funcNameNode.type != ASTNodeType::Identifier) {
            throw std::runtime_error("Function name must be an identifier");
        }
        std::string funcName = funcNameNode.value;
        std::string returnType = node.value;

        // Собираем типы параметров
        std::vector<std::string> paramTypes;
        for (size_t i = 1; i < node.children.size() - 1; ++i) {
            ASTNode &param = node.children[i];
            if (param.type != ASTNodeType::Parameter) {
                throw std::runtime_error("Expected parameter in function declaration");
            }
            paramTypes.push_back(param.value);
        }

        // Добавляем функцию в таблицу символов
        Symbol funcSymbol(funcName, returnType, true, paramTypes);
        if (!symbolTable.addSymbol(funcSymbol)) {
            throw std::runtime_error("Function '" + funcName + "' already declared");
        }

        // Вход в область видимости функции
        auto oldFuncName = currentFunctionName;
        auto oldReturnType = currentFunctionReturnType;
        currentFunctionName = funcName;
        currentFunctionReturnType = returnType;

        // Входим в область видимости
        symbolTable.enterScope();

        for (size_t i = 1; i < node.children.size() - 1; ++i) {
            ASTNode &param = node.children[i];
            if (param.children.empty()) {
                throw std::runtime_error("Parameter has no identifier child");
            }
            ASTNode &paramNameNode = param.children[0];
            Symbol paramSymbol(paramNameNode.value, param.value);
            if (!symbolTable.addSymbol(paramSymbol)) {
                throw std::runtime_error("Parameter '" + paramNameNode.value
                                         + "' already declared in function '" + funcName + "'");
            }
        }

        ASTNode &bodyNode = node.children.back();
        visitBlock(bodyNode);

        symbolTable.exitScope();

        currentFunctionName = oldFuncName;
        currentFunctionReturnType = oldReturnType;
    }

    void visitBlock(ASTNode &node) {
        symbolTable.enterScope();
        for (auto &child : node.children) {
            visitStatement(child);
        }
        symbolTable.exitScope();
    }

    void visitStatement(ASTNode &node) {
        switch (node.type) {
            case ASTNodeType::IfStmt:
                visitIfStatement(node);
                break;
            case ASTNodeType::WhileStmt:
                visitWhileStatement(node);
                break;
            case ASTNodeType::ForStmt:
                visitForStatement(node);
                break;
            case ASTNodeType::ReturnStmt:
                visitReturnStatement(node);
                break;
            case ASTNodeType::VarDecl:
                visitVarDecl(node);
                break;
            case ASTNodeType::BinaryOp:
            case ASTNodeType::Assignment:
            case ASTNodeType::Expression:
            case ASTNodeType::Identifier:
            case ASTNodeType::Literal:
                visitExpression(node);
                break;
            default:
                throw std::runtime_error("Unknown statement type: " + astNodeTypeToString(node.type));
        }
    }

    void visitVarDecl(ASTNode &node) {
        // 1) Считываем имя, тип
        if (node.children.empty()) {
            throw std::runtime_error("Variable declaration missing children");
        }
        ASTNode &varNameNode = node.children[0];
        if (varNameNode.type != ASTNodeType::Identifier) {
            throw std::runtime_error("Variable name must be an Identifier");
        }
        std::string baseType = getBaseType(node.value);

        // 2) Ищем arrayDims
        int arrayDimsCount = 0;
        std::vector<ASTNode*> dimensionExprs;
        for (size_t i = 1; i < node.children.size(); ++i) {
            ASTNode &child = node.children[i];
            if (child.type == ASTNodeType::BinaryOp && child.value == "arrayDim") {
                arrayDimsCount++;
                // внутри arrayDim лежит 1 ребёнок: выражение
                dimensionExprs.push_back(&child.children[0]);
            }
        }
        // Если arrayDimsCount>0 => baseType += "[]"
        for (int i = 0; i < arrayDimsCount; i++) {
            baseType += "[]";
        }

        // 3) Регистрируем переменную
        Symbol varSymbol(varNameNode.value, baseType);
        if (!symbolTable.addSymbol(varSymbol)) {
            throw std::runtime_error("Variable '" + varNameNode.value
                                     + "' already declared in this scope");
        }

        // 4) Для каждого arrayDim: проверим dimExpr
        for (auto *dimExpr : dimensionExprs) {
            std::string dimType = inferType(*dimExpr);

            // Размер должен быть integer
            if (getBaseType(dimType) != "integer") {
                throw std::runtime_error(
                        "Array dimension must be integer, got " + dimType);
            }

            if (dimExpr->type == ASTNodeType::Literal) {
                // compile-time check
                int val = std::stoi(dimExpr->value);
                if (val <= 0) {
                    throw std::runtime_error(
                            "Array dimension must be > 0, got " + std::to_string(val));
                }
            }
        }

        // 5) Проверяем инициализацию (напр. arr = new int[0])
        for (size_t i = 1; i < node.children.size(); ++i) {
            ASTNode &child = node.children[i];
            if (child.type == ASTNodeType::BinaryOp && child.value != "arrayDim") {
                // Это значит присваивание
                std::string initType = inferType(child);
                if (!isTypeCompatible(baseType, initType)) {
                    throw std::runtime_error(
                            "Type mismatch in variable initialization: expected "
                            + baseType + ", got " + initType);
                }
            }
        }
    }


    // ------------------ if / while / for / return ------------------
    void visitIfStatement(ASTNode &node) {
        if (node.children.size() < 2) {
            throw std::runtime_error("If statement missing condition or body");
        }
        std::string condType = inferType(node.children[0]);
        if (condType != "bool") {
            throw std::runtime_error("Condition in if must be bool, got " + condType);
        }
        visitBlock(node.children[1]);
        if (node.children.size() > 2) {
            visitBlock(node.children[2]);
        }
    }

    void visitWhileStatement(ASTNode &node) {
        if (node.children.size() < 2) {
            throw std::runtime_error("While missing condition or body");
        }
        std::string condType = inferType(node.children[0]);
        if (condType != "bool") {
            throw std::runtime_error("While condition must be bool, got " + condType);
        }
        visitBlock(node.children[1]);
    }

    void visitForStatement(ASTNode &node) {
        // for(init; cond; step) { body }
        symbolTable.enterScope();

        ASTNode &init = node.children[0];
        if (!(init.type == ASTNodeType::Literal && init.value == "no_init")) {
            visitStatement(init);
        }

        ASTNode &cond = node.children[1];
        if (!(cond.type == ASTNodeType::Literal && cond.value == "true")) {
            std::string ctype = inferType(cond);
            if (ctype != "bool") {
                throw std::runtime_error("For condition must be bool, got " + ctype);
            }
        }

        ASTNode &step = node.children[2];
        if (!(step.type == ASTNodeType::Literal && step.value == "no_step")) {
            visitExpression(step);
        }
        visitBlock(node.children[3]);

        symbolTable.exitScope();
    }

    void visitReturnStatement(ASTNode &node) {
        // return (expr?)
        if (node.children.empty()) {
            // Если функция не void, ошибка
            if (getBaseType(currentFunctionReturnType) != "void") {
                throw std::runtime_error(
                        "Function '" + currentFunctionName
                        + "' must return a value of type " + currentFunctionReturnType
                );
            }
            return;
        }
        // Иначе есть выражение
        std::string exprType = inferType(node.children[0]);
        std::string expected = getBaseType(currentFunctionReturnType);
        std::string actual   = getBaseType(exprType);

        if (expected == "void") {
            throw std::runtime_error(
                    "Void function '" + currentFunctionName + "' cannot return a value"
            );
        }
        if (!isTypeCompatible(expected, actual)) {
            throw std::runtime_error(
                    "Return type mismatch in function '" + currentFunctionName
                    + "': expected '" + expected + "', got '" + actual + "'"
            );
        }
    }

    void visitExpression(ASTNode &node) {
        inferType(node);
    }

    std::string inferType(ASTNode &node) {
        switch (node.type) {
            case ASTNodeType::Literal:
                return inferLiteralType(node.value);

            case ASTNodeType::Identifier: {
                auto sym = symbolTable.lookup(node.value);
                if (!sym.has_value()) {
                    throw std::runtime_error("Undefined identifier: " + node.value);
                }
                return getBaseType(sym->type);
            }

            case ASTNodeType::BinaryOp: {
                // 1) Присваивание '='
                if (node.value == "=") {
                    if (node.children.size() != 2) {
                        throw std::runtime_error("Assignment must have 2 children");
                    }
                    std::string leftType = inferType(node.children[0]);
                    std::string rightType = inferType(node.children[1]);
                    if (!isTypeCompatible(leftType, rightType)) {
                        throw std::runtime_error(
                                "Type mismatch in assignment: cannot assign "
                                + rightType + " to " + leftType
                        );
                    }
                    node.inferredType = leftType;
                    return leftType;
                }
                else if (node.value == "+=" || node.value == "-=" || node.value == "*=" || node.value == "/=") {
                    if (node.children.size() != 2) {
                        throw std::runtime_error("Assignment operator must have 2 children");
                    }
                    std::string leftType = inferType(node.children[0]);
                    std::string rightType = inferType(node.children[1]);

                    if (!isTypeCompatible(leftType, rightType)) {
                        throw std::runtime_error(
                                "Type mismatch in assignment operation '" + node.value + "': " +
                                leftType + " vs " + rightType
                        );
                    }
                    // Тип результата совпадает с левым операндом
                    node.inferredType = leftType;
                    return leftType;
                }
                // 2) call
                if (node.value == "call") {
                    ASTNode &funcNode = node.children[0];
                    if (funcNode.type != ASTNodeType::Identifier) {
                        throw std::runtime_error("Function call must be an identifier");
                    }
                    std::string funcName = funcNode.value;

                    auto symbol = symbolTable.lookup(funcName);
                    if (!symbol.has_value()) {
                        throw std::runtime_error("Undefined function: " + funcName);
                    }
                    if (!symbol->isFunction) {
                        throw std::runtime_error(funcName + " is not a function");
                    }

                    // Проверяем аргументы
                    size_t expectedArgs = symbol->parameterTypes.size();
                    size_t providedArgs = node.children.size() - 1;
                    if (providedArgs != expectedArgs) {
                        throw std::runtime_error(
                                "Function '" + funcName + "' expects " + std::to_string(expectedArgs) +
                                " arguments, but got " + std::to_string(providedArgs)
                        );
                    }
                    for (size_t i = 0; i < expectedArgs; ++i) {
                        std::string expectedType = getBaseType(symbol->parameterTypes[i]);
                        std::string providedType = inferType(node.children[i + 1]);
                        if (!isTypeCompatible(expectedType, providedType)) {
                            throw std::runtime_error(
                                    "Argument " + std::to_string(i + 1) + " of function '" + funcName +
                                    "' expects type " + expectedType + ", but got " + providedType
                            );
                        }
                    }
                    // Тип результата вызова = тип, который объявлен у функции
                    std::string returnType = getBaseType(symbol->type);
                    node.inferredType = returnType;
                    return returnType;
                }
                // 3) []
                // Индексация массива '[]'
                if (node.value == "[]") {
                    std::string arrayType = inferType(node.children[0]);
                    if (!endsWith(arrayType, "[]")) {
                        throw std::runtime_error("Attempting to index a non-array type: " + arrayType);
                    }
                    // Убираем одну пару []
                    arrayType = arrayType.substr(0, arrayType.size() - 2);

                    // Проверяем, что индекс — integer
                    std::string indexType = inferType(node.children[1]);
                    if (indexType != "integer") {
                        throw std::runtime_error("Index must be integer, got " + indexType);
                    }
                    node.inferredType = arrayType;
                    return arrayType;
                }
                    // 4) new
                else if (node.value == "new") {
                    // node.children[0] => Literal("int" или "int*")
                    // node.children[1] => BinaryOp("arrayDim"), внутри [0]=Literal("23")
                    if (node.children.size() != 2) {
                        throw std::runtime_error("Invalid 'new' node: expected 2 children");
                    }
                    ASTNode &typeNode = node.children[0];
                    ASTNode &arrDim = node.children[1];
                    if (typeNode.type != ASTNodeType::Literal) {
                        throw std::runtime_error("'new': child[0] must be a Literal (type)");
                    }
                    // Проверим, что arrDim.value == "arrayDim"
                    if (arrDim.value != "arrayDim") {
                        throw std::runtime_error("'new': child[1] must be arrayDim node");
                    }
                    // Проверим тип размера
                    if (arrDim.children.size() != 1) {
                        throw std::runtime_error("'arrayDim' node must have exactly 1 child");
                    }
                    std::string sizeT = inferType(arrDim.children[0]);
                    if (sizeT != "integer") {
                        throw std::runtime_error(
                                "Array size must be integer, got " + sizeT
                        );
                    }
                    if (arrDim.children[0].type == ASTNodeType::Literal) {
                        int val = std::stoi(arrDim.children[0].value);
                        if (val <= 0) {
                            throw std::runtime_error(
                                    "Array size in 'new' must be > 0, got " + std::to_string(val)
                            );
                        }
                    }
                    std::string base = getBaseType(typeNode.value);
                    if (!endsWith(base, "*")) {
                        base += "*";
                    }
                    node.inferredType = base;
                    return base;
                }
                    // 5) массивы
                else if (node.value == "arrayDim") {
                    if (node.children.size() != 1) {
                        throw std::runtime_error(
                                "Invalid 'arrayDim': must have exactly 1 child"
                        );
                    }
                    std::string dimExprType = inferType(node.children[0]);
                    node.inferredType = "integer";
                    return "integer";
                }
                // 6) Остальные операторы (+, -, *, /, <, >, &&, ||, ==, != ...)
                else {
                    size_t nc = node.children.size();
                    if (nc == 1) {
                        // Унарный оператор
                        if (node.value == "-") {
                            std::string cT = inferType(node.children[0]);
                            node.inferredType = cT;
                            return cT;
                        } else if (node.value == "!") {
                            std::string cT = inferType(node.children[0]);
                            if (cT != "bool") {
                                throw std::runtime_error("Unary '!' on non-bool type");
                            }
                            node.inferredType = "bool";
                            return "bool";
                        } else {
                            throw std::runtime_error("Unknown unary operator: " + node.value);
                        }
                    } else if (nc == 2) {
                        // Бинарные операторы: +, -, *, /, <, >, ...
                        std::string leftType = inferType(node.children[0]);
                        std::string rightType = inferType(node.children[1]);

                        // Список бинарных операторов
                        if (node.value == "+" || node.value == "-" || node.value == "*" || node.value == "/") {
                            // Проверка совместимости
                            if (!isTypeCompatible(leftType, rightType)) {
                                throw std::runtime_error(
                                        "Type mismatch in binary operation '" + node.value + "': " +
                                        leftType + " vs " + rightType
                                );
                            }
                            // результат = leftType
                            node.inferredType = leftType;
                            return leftType;
                        } else if (node.value == "<" || node.value == ">" ||
                                   node.value == "<=" || node.value == ">=" ||
                                   node.value == "==" || node.value == "!=" ||
                                   node.value == "&&" || node.value == "||") {
                            // Проверка совместимости
                            if (!isTypeCompatible(leftType, rightType)) {
                                throw std::runtime_error(
                                        "Type mismatch in binary operation '" + node.value + "': " +
                                        leftType + " vs " + rightType
                                );
                            }
                            // Результат таких операций — bool
                            node.inferredType = "bool";
                            return "bool";
                        } else {
                            throw std::runtime_error("Unknown binary operator: " + node.value);
                        }
                    } else {
                        throw std::runtime_error(
                                "Invalid number of children (" + std::to_string(nc) +
                                ") for operator: " + node.value
                        );
                    }
                }
            }

            // 4) Параметр, Объявление функции, Объявление переменной, Блок, Программа
            case ASTNodeType::Parameter:
            case ASTNodeType::FunctionDecl:
            case ASTNodeType::VarDecl:
            case ASTNodeType::Block:
            case ASTNodeType::Program:
                return "";

            default:
                throw std::runtime_error(
                        "Unknown AST node type for type inference: "
                        + astNodeTypeToString(node.type)
                );
        }
    }
};

#endif //COMPILER_LANGUAGE_SEMANTICANALYZER_H
