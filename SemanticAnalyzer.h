//
// Created by Тихонов Александр on 26.12.2024.
//

#ifndef COMPILER_LANGUAGE_SEMANTICANALYZER_H
#define COMPILER_LANGUAGE_SEMANTICANALYZER_H

#include "AST.h"
#include "SymbolTable.h"
#include <iostream>
#include <stdexcept>
#include <sstream>

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

private:
    ASTNode &root;
    SymbolTable symbolTable;
    std::unordered_map<std::string, std::string> typeAliases;

    void addBuiltInSymbols() {
        Symbol nullptrSymbol("nullptr", "void*", false, {});
        if (!symbolTable.addSymbol(nullptrSymbol)) {
            throw std::runtime_error("Failed to add built-in symbol: nullptr");
        }
    }


    // Функция для разрешения типа с учётом псевдонимов
    // пока еще костыльно работает, тупо удаляя static/const... и забирая "важную часть", надо будет разобраться
    std::string resolveType(const std::string &type) const {
        auto it = typeAliases.find(type);
        if (it != typeAliases.end()) {
            return it->second;
        }
        return type;
    }

    // Продолжение верхней
    std::string getBaseType(const std::string &type) const {
        std::string baseType;
        std::istringstream iss(type);
        std::string token;
        while (iss >> token) {
            if (token != "static" && token != "const") {
                baseType += resolveType(token);
                baseType += " ";
            }
        }
        if (!baseType.empty() && baseType.back() == ' ') {
            baseType.pop_back();
        }
        return baseType;
    }

    // парсинг узлов дерева
    void visitProgram(ASTNode &node) {
        for (auto &child : node.children) {
            if (child.type == ASTNodeType::FunctionDecl) {
                visitFunctionDecl(child);
            } else if (child.type == ASTNodeType::VarDecl) {
                visitVarDecl(child);
            } else {
                throw std::runtime_error("Unknown top-level declaration");
            }
        }
    }

    void visitFunctionDecl(ASTNode &node) {
        // Первый ребенок - имя функции
        // Остальные дети - параметры и тело

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
        symbolTable.enterScope();

        // Добавляем параметры в таблицу символов
        for (size_t i = 1; i < node.children.size() - 1; ++i) {
            ASTNode &param = node.children[i];
            ASTNode &paramNameNode = param.children[0];
            std::string paramName = paramNameNode.value;
            std::string paramType = param.value;

            Symbol paramSymbol(paramName, paramType);
            if (!symbolTable.addSymbol(paramSymbol)) {
                throw std::runtime_error("Parameter '" + paramName + "' already declared in function '" + funcName + "'");
            }
        }

        // Посещаем тело функции
        ASTNode &bodyNode = node.children.back();
        visitBlock(bodyNode);

        // Выход из области видимости функции
        symbolTable.exitScope();
    }

    void visitVarDecl(ASTNode &node) {
        if (node.children.empty()) {
            throw std::runtime_error("Variable declaration missing children");
        }

        // Первый ребёнок - это Identifier
        ASTNode &varNameNode = node.children[0];
        if (varNameNode.type != ASTNodeType::Identifier) {
            throw std::runtime_error("Variable name must be an identifier");
        }

        // node.value, например "static bool" или "static const integer"
        std::string baseType = getBaseType(node.value);

        // Считаем кол-во измерений массива
        int arrayDimsCount = 0;
        for (size_t i = 1; i < node.children.size(); ++i) {
            ASTNode &child = node.children[i];
            if (child.type == ASTNodeType::BinaryOp && child.value == "arrayDim") {
                arrayDimsCount++;
            }
        }

        // Дописываем [] для каждого измерения
        // Пример: если arrayDimsCount=1, то baseType -> "bool[]"
        //         если arrayDimsCount=2, то baseType -> "bool[][]"
        for (int i = 0; i < arrayDimsCount; i++) {
            baseType += "[]";
        }

        Symbol varSymbol(varNameNode.value, baseType);
        if (!symbolTable.addSymbol(varSymbol)) {
            throw std::runtime_error("Variable '" + varNameNode.value + "' already declared in current scope");
        }

        for (size_t i = 1; i < node.children.size(); ++i) {
            ASTNode &child = node.children[i];
            if (child.type == ASTNodeType::BinaryOp && child.value == "arrayDim") {
                // dimExpr — выражение внутри скобок []
                ASTNode &dimExpr = child.children[0];
                std::string dimType = inferType(dimExpr);
                if (dimType != "integer") {
                    throw std::runtime_error(
                            "Array dimension must be of type integer, got " + dimType
                    );
                }
            }
            else if (child.type == ASTNodeType::BinaryOp) {
                // Это может быть инициализация: is_prime = ...
                std::string initType = inferType(child);
                // Проверяем совместимость initType и baseType
                if (!isTypeCompatible(baseType, initType)) {
                    throw std::runtime_error(
                            "Type mismatch in variable initialization: expected " + baseType +
                            ", got " + initType
                    );
                }
            }
        }
    }



    void visitBlock(ASTNode &node) {
        // Вход в новую область видимости
        symbolTable.enterScope();

        for (auto &child : node.children) {
            visitStatement(child);
        }

        // Выход из области видимости
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

    void visitIfStatement(ASTNode &node) {
        // node.value == "if"
        // Первые два ребенка: условие и then-блок
        // Третий ребенок (необязательный): else-блок

        if (node.children.size() < 2) {
            throw std::runtime_error("If statement missing condition or then-block");
        }

        ASTNode &cond = node.children[0];
        std::string condType = inferType(cond);
        if (condType != "bool") {
            throw std::runtime_error("Condition in if statement must be of type bool, got " + condType);
        }

        ASTNode &thenBlock = node.children[1];
        visitBlock(thenBlock);

        if (node.children.size() > 2) {
            ASTNode &elseBlock = node.children[2];
            visitBlock(elseBlock);
        }
    }

    void visitWhileStatement(ASTNode &node) {
        // node.value == "while"
        // Первые два ребенка: условие и тело цикла

        if (node.children.size() < 2) {
            throw std::runtime_error("While statement missing condition or body");
        }

        ASTNode &cond = node.children[0];
        std::string condType = inferType(cond);
        if (condType != "bool") {
            throw std::runtime_error("Condition in while statement must be of type bool, got " + condType);
        }

        ASTNode &body = node.children[1];
        visitBlock(body);
    }

    void visitForStatement(ASTNode &node) {
        symbolTable.enterScope();

        // 1) init
        ASTNode &init = node.children[0];
        if (init.type != ASTNodeType::Literal || init.value != "no_init") {
            visitStatement(init);
        }

        // 2) condition
        ASTNode &cond = node.children[1];
        if (!(cond.type == ASTNodeType::Literal && cond.value == "true")) {
            std::string condType = inferType(cond);
            if (condType != "bool") {
                throw std::runtime_error("Condition in for statement must be bool, got " + condType);
            }
        }

        // 3) step
        ASTNode &step = node.children[2];
        if (!(step.type == ASTNodeType::Literal && step.value == "no_step")) {
            visitExpression(step);
        }

        // 4) body
        ASTNode &body = node.children[3];
        visitBlock(body);

        symbolTable.exitScope();
    }

    // @TODO надо доделать, пока что тут только заглушка
    void visitReturnStatement(ASTNode &node) {

        if (node.children.empty()) {

            return;
        }

        ASTNode &expr = node.children[0];
        std::string exprType = inferType(expr);
    }

    // @TODO надо доделать, пока что тут только заглушка
    void visitExpression(ASTNode &node) {
        inferType(node);
    }

    bool endsWith(const std::string &str, const std::string &suffix) {
        if (str.size() < suffix.size()) return false;
        return (str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0);
    }

    // Функция для определения типа выражения
    std::string inferType(ASTNode &node) {
        switch (node.type) {
            case ASTNodeType::Literal: {
                return inferLiteralType(node.value);
            }

            case ASTNodeType::Identifier: {
                auto symbol = symbolTable.lookup(node.value);
                if (!symbol.has_value()) {
                    throw std::runtime_error("Undefined identifier: " + node.value);
                }
                // Возвращаем базовый тип: например, "int", "bool[]", и т.п.
                return getBaseType(symbol->type);
            }


            // операторы =, [], +, -, ...
            case ASTNodeType::BinaryOp: {
                // 3.1) Присваивание '='
                if (node.value == "=") {
                    // node.children[0] -- левый операнд (переменная, элемент массива, и т.п.)
                    // node.children[1] -- правый операнд (выражение)
                    ASTNode &left = node.children[0];
                    ASTNode &right = node.children[1];

                    std::string leftType = inferType(left);
                    std::string rightType = inferType(right);

                    if (!isTypeCompatible(leftType, rightType)) {
                        throw std::runtime_error(
                                "Type mismatch in assignment: cannot assign " + rightType + " to " + leftType
                        );
                    }
                    node.inferredType = leftType; // результат выражения '=' обычно тот же, что и у левого операнда
                    return leftType;
                }

                // ----------------- 3.2) Вызов функции 'call' -----------------
                if (node.value == "call") {
                    // node.children[0] -- Identifier(имя_функции)
                    // node.children[1..N] -- аргументы
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

                    // Проверяем кол-во и типы аргументов
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
                    // Тип результата вызова = тип возврата функции (symbol->type)
                    std::string returnType = getBaseType(symbol->type);
                    node.inferredType = returnType;
                    return returnType;
                }

                // 3.3) Индексация массива '[]'
                if (node.value == "[]") {
                    // node.children[0] -- сам массив
                    // node.children[1] -- индекс
                    std::string arrayType = inferType(node.children[0]);

                    // проверяем, действительно ли arrayType оканчивается на "[]"
                    if (!endsWith(arrayType, "[]")) {
                        throw std::runtime_error(
                                "Attempting to index a non-array type: " + arrayType
                        );
                    }

                    // "bool[]" -> "bool", "bool[][]" -> "bool[]"
                    arrayType = arrayType.substr(0, arrayType.size() - 2);

                    // Проверяем, что индекс имеет тип integer
                    std::string indexType = inferType(node.children[1]);
                    if (indexType != "integer") {
                        throw std::runtime_error("Index must be integer, got " + indexType);
                    }

                    node.inferredType = arrayType;
                    return arrayType;
                }

                // 3.4) Все остальные операторы: +, -, *, /, <, >, <=, >=, ==, !=,
                size_t numChildren = node.children.size();
                if (numChildren == 1) {
                    // -------- Унарная операция --------
                    std::string childType = inferType(node.children[0]);

                    if (node.value == "-") {
                        // унарный минус -> тип = тип ребёнка
                        node.inferredType = childType;
                        return childType;
                    }
                    else if (node.value == "!") {
                        // логическое НЕ -> результат "bool"
                        if (childType != "bool") {
                            throw std::runtime_error("Unary '!' applied to non-bool type: " + childType);
                        }
                        node.inferredType = "bool";
                        return "bool";
                    }
                    else {
                        throw std::runtime_error("Unknown unary operator: " + node.value);
                    }
                }
                else if (numChildren == 2) {
                    // -------- Бинарная операция --------
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
                    }
                    else if (node.value == "<" || node.value == ">" ||
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
                    }
                    else {
                        throw std::runtime_error("Unknown binary operator: " + node.value);
                    }
                }
                else {
                    throw std::runtime_error(
                            "Invalid number of children (" + std::to_string(numChildren) +
                            ") for operator: " + node.value
                    );
                }
            }


                // 4) Параметр, Объявление функции, Объявление переменной, Блок, Программа
            case ASTNodeType::Parameter:
                // node.value типа "integer" или "bool", но может быть "static const integer"
                return getBaseType(node.value);

            case ASTNodeType::FunctionDecl:
                // Тип функции для выражения в большинстве языков не нужен
                return "";

            case ASTNodeType::VarDecl:
                // node.value = "bool", "int", "static const integer", ...
                return getBaseType(node.value);

            case ASTNodeType::Block:
                return "";

            case ASTNodeType::Program:
                return "";

            default:
                throw std::runtime_error(
                        "Unknown AST node type for type inference: " + astNodeTypeToString(node.type)
                );
        }
    }


    // Определение типа строкового литерала
    std::string inferLiteralType(const std::string &value) {
        if (value.front() == '"' && value.back() == '"') {
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

    // Проверка совместимости типов
    bool isTypeCompatible(const std::string &expected, const std::string &actual) const {
        std::string baseExpected = getBaseType(expected);
        std::string baseActual = getBaseType(actual);
        return baseExpected == baseActual;
    }


};
#endif //COMPILER_LANGUAGE_SEMANTICANALYZER_H
