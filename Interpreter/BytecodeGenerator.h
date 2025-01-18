//#pragma once
//
//#include "InstructionMy.h"
//#include "InterpreterSymbolTable.h"
//#include "BytecodeProgramMy.h"
//#include "../Parser_AST/AST.h"
//
//#include <stdexcept>
//#include <iostream>
//#include <cstdlib>
//#include <string>
//
///**
// * Класс, который обходит AST (с типами Program, VarDecl, ForStmt, и т.д.)
// * и заполняет BytecodeProgram собственными инструкциями.
// */
//class MyBytecodeGenerator {
//public:
//    MyBytecodeGenerator() = default;
//
//    BytecodeProgramMy generate(const ASTNode &root) {
//        BytecodeProgramMy program;
//
//        if (root.type != ASTNodeType::Program) {
//            throw std::runtime_error("MyBytecodeGenerator: root must be Program");
//        }
//
//        // 1. Создаём функцию 'main' сначала
//        program.functions.emplace_back(BytecodeFunctionMy("main"));
//        BytecodeFunctionMy &mainFn = program.functions.back();
//
//        // 2. Обходим детей (VarDecl или FunctionDecl)
//        for (auto &child : root.children) {
//            switch (child.type) {
//                case ASTNodeType::VarDecl:
//                    // Генерация глобальной переменной и её инициализация в 'main'
//                    generateGlobalVarDecl(child, program, mainFn);
//                    break;
//                case ASTNodeType::FunctionDecl:
//                    // Генерация функции
//                    generateFunctionDecl(child, program);
//                    break;
//                default:
//                    // Что-то ещё на уровне Program?
//                    std::cerr << "WARNING: top-level node not handled: "
//                              << astNodeTypeToString(child.type) << std::endl;
//                    break;
//            }
//        }
//
//        // 3. Добавляем команду HALT в 'main'
//        mainFn.instructions.emplace_back(OpCode::HALT);
//        return program;
//    }
//
//private:
//    // ------------------------------------------------------------
//    // 1) Глобальные переменные
//    // ------------------------------------------------------------
//    void generateGlobalVarDecl(const ASTNode &varNode, BytecodeProgramMy &program, BytecodeFunctionMy &mainFn) {
//        // varNode.value = "integer" или "bool" и т.п.
//        // varNode.children[0] = Identifier(...)
//        // varNode.children[1..] = инициализация или массив, и т.д.
//        if (varNode.children.empty()) {
//            throw std::runtime_error("VarDecl node has no children (missing identifier?).");
//        }
//        std::string varName = varNode.children[0].value;
//        std::cout << "Global VarDecl: " << varNode.value << " " << varName << std::endl;
//
//        if (varName.empty()) {
//            throw std::runtime_error("Variable name is empty.");
//        }
//
//        VarType varType = parseVarType(varNode.value);
//
//        // Проверяем, массив ли это
//        if (varNode.children.size() > 1 &&
//            varNode.children[1].type == ASTNodeType::BinaryOp &&
//            varNode.children[1].value == "arrayDim") {
//            // Обработка массива
//            ASTNode sizeExpr = varNode.children[1].children[0];
//            int arraySize = evaluateConstantExpression(sizeExpr, program.globalSymbolTable);
//
//            // Создаём переменную массива
//            Variable var(varType, varName, varType, arraySize);
//            program.globalSymbolTable.addVariable(var);
//
//            // Инициализируем массив нулями
//            for(int i = 0; i < arraySize; ++i) {
//                mainFn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
//                mainFn.instructions.emplace_back(OpCode::STORE_ARRAY, varName);
//            }
//        }
//        else {
//            // Обработка простой переменной
//            Variable var(varType, varName);
//            int varIndex = program.globalSymbolTable.addVariable(var); // Добавляем переменную и получаем её индекс
//
//            if (varNode.children.size() > 1) {
//                // Инициализация переменной
//                ASTNode initExpr = varNode.children[1];
//                int initVal = evaluateConstantExpression(initExpr, program.globalSymbolTable);
//
//                // Обновляем существующую переменную как константу
//                program.globalSymbolTable.variables[varIndex].setConstant(initVal);
//
//                // Генерируем инструкции для инициализации
//                generateExpression(initExpr, program.globalSymbolTable, mainFn);
//                mainFn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
//            }
//            else {
//                // Без инициализации — устанавливаем 0
//                mainFn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
//                mainFn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
//            }
//        }
//    }
//
//    VarType parseVarType(const std::string &typeStr) const {
//        if (typeStr == "integer") return VarType::INTEGER;
//        if (typeStr == "float") return VarType::FLOAT;
//        if (typeStr == "bool") return VarType::BOOL;
//        if (typeStr == "string") return VarType::STRING;
//        if (typeStr == "void") return VarType::VOID;
//        throw std::runtime_error("Unknown variable type: " + typeStr);
//    }
//
//    // ------------------------------------------------------------
//    // 2) Обработка FunctionDecl
//    // ------------------------------------------------------------
//    void generateFunctionDecl(const ASTNode &funcNode, BytecodeProgramMy &program) {
//        // funcNode.value = "void" или "integer" etc. (тип)
//        // funcNode.children[0] = Identifier(имя_функции)
//        // funcNode.children[1..] = Параметры, а последний — Block.
//
//        if (funcNode.children.empty()) {
//            throw std::runtime_error("FunctionDecl has no children");
//        }
//        std::string funcName = funcNode.children[0].value;
//
//        int fnIndex = program.addFunction(funcName);
//        BytecodeFunctionMy &fn = program.functions[fnIndex];
//
//        // Обработка параметров и тела функции
//        size_t i = 1;
//        // Сначала параметры
//        for (; i < funcNode.children.size(); i++) {
//            if (funcNode.children[i].type == ASTNodeType::Parameter) {
//                generateParameter(funcNode.children[i], fn);
//            }
//            else {
//                // дошли до блока
//                break;
//            }
//        }
//        // В i должен быть Block
//        if (i >= funcNode.children.size()) {
//            throw std::runtime_error("FunctionDecl: no body (Block) found");
//        }
//        if (funcNode.children[i].type != ASTNodeType::Block) {
//            throw std::runtime_error("FunctionDecl: expected Block as last child");
//        }
//        generateBlock(funcNode.children[i], program.globalSymbolTable, fn);
//
//        // В конце функции, если нет RET, добавим
//        if (fn.instructions.empty()
//            || fn.instructions.back().opcode != OpCode::RET) {
//            fn.instructions.emplace_back(OpCode::RET);
//        }
//    }
//
//    void generateParameter(const ASTNode &paramNode, BytecodeFunctionMy &fn) {
//        // paramNode.value = тип (например, "integer")
//        // paramNode.children[0] = Identifier(имя)
//        VarType paramType = parseVarType(paramNode.value);
//        std::string paramName = paramNode.children[0].value;
//
//        // Добавляем параметр в локальную символическую таблицу
//        Variable var(paramType, paramName);
//        fn.symbolTable.addVariable(var);
//
//        std::cout << "Parameter: type=" << paramNode.value
//                  << " name=" << paramName << std::endl;
//    }
//
//    // ------------------------------------------------------------
//    // 3) Обработка блоков и операторов
//    // ------------------------------------------------------------
//    void generateBlock(const ASTNode &blockNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        if (blockNode.type != ASTNodeType::Block) {
//            throw std::runtime_error("generateBlock: node is not Block");
//        }
//        // blockNode.children — это набор statement'ов
//        for (auto &stmt : blockNode.children) {
//            generateStatement(stmt, globalSymbolTable, fn);
//        }
//    }
//
//    void generateStatement(const ASTNode &stmt, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        switch (stmt.type) {
//            case ASTNodeType::VarDecl:
//                generateLocalVarDecl(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::ReturnStmt:
//                generateReturnStmt(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::IfStmt:
//                generateIfStmt(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::WhileStmt:
//                generateWhileStmt(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::ForStmt:
//                generateForStmt(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::Assignment:
//                generateAssignment(stmt, globalSymbolTable, fn);
//                break;
//            case ASTNodeType::Expression:
//            case ASTNodeType::BinaryOp:
//            case ASTNodeType::Identifier:
//            case ASTNodeType::Literal:
//                // Это может быть Expression-Statement, например "a + 1;"
//                generateExpression(stmt, globalSymbolTable, fn);
//                // При необходимости можно добавить POP
//                break;
//            default:
//                std::cerr << "Unhandled statement type: "
//                          << astNodeTypeToString(stmt.type) << std::endl;
//                break;
//        }
//    }
//
//    // ------------------------------------------------------------
//    // 4) Локальные переменные
//    // ------------------------------------------------------------
//    void generateLocalVarDecl(const ASTNode &varNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // Пример: "integer key = global_array[1];"
//        // varNode.value = "integer"
//        // varNode.children[0] = Identifier("key")
//        // varNode.children[1] = BinaryOp("[]")
//
//        if (varNode.children.empty()) {
//            throw std::runtime_error("Local VarDecl has no children");
//        }
//        std::string varName = varNode.children[0].value;
//
//        // Определение типа
//        VarType varType = parseVarType(varNode.value);
//
//        // Проверяем, массив ли это
//        if (varNode.children.size() > 1 &&
//            varNode.children[1].type == ASTNodeType::BinaryOp &&
//            varNode.children[1].value == "arrayDim") {
//            // Обработка массива (аналогично глобальным переменным)
//            ASTNode sizeExpr = varNode.children[1].children[0];
//            int arraySize = evaluateConstantExpression(sizeExpr, globalSymbolTable);
//
//            // Создаём переменную массива
//            Variable var(varType, varName, varType, arraySize);
//            fn.symbolTable.addVariable(var);
//
//            // Инициализируем массив нулями
//            for(int i = 0; i < arraySize; ++i) {
//                fn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
//                fn.instructions.emplace_back(OpCode::STORE_ARRAY, varName);
//            }
//        }
//        else {
//            // Простая переменная
//            Variable var(varType, varName);
//            fn.symbolTable.addVariable(var);
//
//            std::cout << "[local var] " << varNode.value << " " << varName << "\n";
//
//            if (varNode.children.size() > 1) {
//                // Инициализация переменной
//                ASTNode initExpr = varNode.children[1];
//
//                // Генерируем инструкции для инициализации
//                generateExpression(initExpr, globalSymbolTable, fn);
//                fn.instructions.emplace_back(OpCode::STORE_LOCAL, fn.symbolTable.getVariableIndex(varName));
//            }
//            else {
//                // Без инициализации — устанавливаем 0
//                fn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
//                fn.instructions.emplace_back(OpCode::STORE_LOCAL, fn.symbolTable.getVariableIndex(varName));
//            }
//        }
//    }
//
//    // ------------------------------------------------------------
//    // 5) RETURN
//    // ------------------------------------------------------------
//    void generateReturnStmt(const ASTNode &retNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // Если есть выражение в return
//        if (!retNode.children.empty()) {
//            generateExpression(retNode.children[0], globalSymbolTable, fn);
//        }
//        fn.instructions.emplace_back(OpCode::RET);
//    }
//
//    // ------------------------------------------------------------
//    // 6) IF
//    // ------------------------------------------------------------
//    void generateIfStmt(const ASTNode &ifNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // ifNode.children[0] = cond
//        // ifNode.children[1] = thenBlock
//        // ifNode.children[2] = elseBlock (опционально)
//        if (ifNode.children.size() < 2) {
//            throw std::runtime_error("IfStmt has <2 children (missing cond or thenBlock)");
//        }
//        // 1) условие
//        generateExpression(ifNode.children[0], globalSymbolTable, fn);
//
//        // 2) вставляем JMP_IF_FALSE (пока operandInt = -1)
//        int jmpIfFalseIndex = (int)fn.instructions.size();
//        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);
//
//        // 3) thenBlock
//        generateBlock(ifNode.children[1], globalSymbolTable, fn);
//
//        // 4) вставляем JMP, чтобы перепрыгнуть через else
//        int jmpIndex = (int)fn.instructions.size();
//        fn.instructions.emplace_back(OpCode::JMP, -1);
//
//        // 5) fixup JMP_IF_FALSE -> начало else
//        fn.instructions[jmpIfFalseIndex].operandInt = (int64_t)fn.instructions.size();
//
//        // 6) если есть elseBlock
//        if (ifNode.children.size() > 2) {
//            generateBlock(ifNode.children[2], globalSymbolTable, fn);
//        }
//
//        // 7) fixup JMP -> конец if
//        fn.instructions[jmpIndex].operandInt = (int64_t)fn.instructions.size();
//    }
//
//    // ------------------------------------------------------------
//    // 7) WHILE
//    // ------------------------------------------------------------
//    void generateWhileStmt(const ASTNode &whileNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // whileNode.children[0] = cond
//        // whileNode.children[1] = body
//        if (whileNode.children.size() < 2) {
//            throw std::runtime_error("WhileStmt has <2 children");
//        }
//        int loopStart = (int)fn.instructions.size();
//
//        // Условие
//        generateExpression(whileNode.children[0], globalSymbolTable, fn);
//
//        // jmp_if_false => конец цикла
//        int jmpIfFalseIndex = (int)fn.instructions.size();
//        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);
//
//        // Тело цикла
//        generateBlock(whileNode.children[1], globalSymbolTable, fn);
//
//        // Переход назад к началу
//        fn.instructions.emplace_back(OpCode::JMP, loopStart);
//
//        // fixup jmpIfFalse -> конец цикла
//        int endPos = (int)fn.instructions.size();
//        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
//    }
//
//    // ------------------------------------------------------------
//    // 8) FOR
//    // ------------------------------------------------------------
//    void generateForStmt(const ASTNode &forNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // forNode (value="for") имеет 4 children:
//        //   [0] init
//        //   [1] cond
//        //   [2] step
//        //   [3] body
//
//        if (forNode.children.size() < 4) {
//            throw std::runtime_error("ForStmt must have 4 children (init, cond, step, body)");
//        }
//
//        // (1) инициализация
//        generateStatement(forNode.children[0], globalSymbolTable, fn);
//
//        // (2) метка начала цикла
//        int loopStart = (int)fn.instructions.size();
//
//        // (3) условие
//        generateExpression(forNode.children[1], globalSymbolTable, fn);
//
//        // (4) jmp_if_false => конец цикла
//        int jmpIfFalseIndex = (int)fn.instructions.size();
//        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);
//
//        // (5) тело цикла
//        generateBlock(forNode.children[3], globalSymbolTable, fn);
//
//        // (6) шаг
//        generateStatement(forNode.children[2], globalSymbolTable, fn);
//
//        // (7) переход назад к началу
//        fn.instructions.emplace_back(OpCode::JMP, loopStart);
//
//        // (8) fixup jmpIfFalse -> конец цикла
//        int endPos = (int)fn.instructions.size();
//        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
//    }
//
//    // ------------------------------------------------------------
//    // 9) ASSIGNMENT
//    // ------------------------------------------------------------
//    void generateAssignment(const ASTNode &assignNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        // assignNode.children[0] = lhs
//        // assignNode.children[1] = rhs
//        if (assignNode.children.size() < 2) {
//            throw std::runtime_error("Assignment node must have 2 children (lhs, rhs)");
//        }
//
//        // Генерим RHS
//        generateExpression(assignNode.children[1], globalSymbolTable, fn);
//
//        // Определяем LHS
//        const ASTNode &lhs = assignNode.children[0];
//        if (lhs.type == ASTNodeType::Identifier) {
//            std::string varName = lhs.value;
//
//            bool isGlobal = false;
//            bool isLocal = false;
//
//            // Проверяем, существует ли переменная в глобальной таблице
//            if (globalSymbolTable.hasVariable(varName)) {
//                isGlobal = true;
//            }
//            // Проверяем, существует ли переменная в локальной таблице функции
//            if (fn.symbolTable.hasVariable(varName)) {
//                isLocal = true;
//            }
//
//            if (!isGlobal && !isLocal) {
//                throw std::runtime_error("Undefined variable: " + varName);
//            }
//
//            // Проверяем, является ли переменная константой
//            if (isGlobal) {
//                int constVal;
//                if (globalSymbolTable.hasConstant(varName, constVal)) {
//                    throw std::runtime_error("Cannot modify constant variable: " + varName);
//                }
//                // Присваиваем значение глобальной переменной
//                fn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
//            }
//            else if (isLocal) {
//                int constVal;
//                if (fn.symbolTable.hasConstant(varName, constVal)) {
//                    throw std::runtime_error("Cannot modify constant variable: " + varName);
//                }
//                // Присваиваем значение локальной переменной
//                int localIndex = fn.symbolTable.getVariableIndex(varName);
//                fn.instructions.emplace_back(OpCode::STORE_LOCAL, localIndex);
//            }
//        }
//        else if (lhs.type == ASTNodeType::BinaryOp && lhs.value == "[]") {
//            // Обработка присваивания элементу массива
//            if (lhs.children.size() != 2) {
//                throw std::runtime_error("Array assignment requires two children: array name and index");
//            }
//            std::string arrayName = lhs.children[0].value;
//
//            // Проверяем, существует ли массив в глобальной таблице
//            if (!globalSymbolTable.hasVariable(arrayName)) {
//                throw std::runtime_error("Undefined array: " + arrayName);
//            }
//
//            // Генерируем индекс
//            generateExpression(lhs.children[1], globalSymbolTable, fn);
//
//            // Проверяем, является ли массив константой
//            int constVal;
//            if (globalSymbolTable.hasConstant(arrayName, constVal)) {
//                throw std::runtime_error("Cannot modify constant array: " + arrayName);
//            }
//
//            // Присваиваем значение элементу массива
//            fn.instructions.emplace_back(OpCode::STORE_ARRAY, arrayName);
//        }
//        else {
//            throw std::runtime_error("Assignment to complex LHS not implemented.");
//        }
//    }
//
//    // ------------------------------------------------------------
//    // 10) Генерация выражений
//    // ------------------------------------------------------------
//    void generateLiteral(const ASTNode &literalNode, BytecodeFunctionMy &fn) {
//        // Поддержка только целых чисел для простоты
//        // Можно расширить для других типов (float, bool, string и т.д.)
//        long long val = std::stoll(literalNode.value);
//        fn.instructions.emplace_back(OpCode::LOAD_CONST, val);
//    }
//
//
//    void generateExpression(const ASTNode &exprNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        switch (exprNode.type) {
//            case ASTNodeType::Literal:
//                generateLiteral(exprNode, fn);
//                break;
//
//            case ASTNodeType::Identifier:
//                generateIdentifier(exprNode, globalSymbolTable, fn);
//                break;
//
//            case ASTNodeType::BinaryOp:
//                generateBinaryOp(exprNode, globalSymbolTable, fn);
//                break;
//
//            default:
//                std::cerr << "generateExpression: unhandled type: "
//                          << astNodeTypeToString(exprNode.type) << std::endl;
//                break;
//        }
//    }
//
//    void generateIdentifier(const ASTNode &idNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        std::string varName = idNode.value;
//        // Проверяем, глобальная ли переменная
//        if (globalSymbolTable.hasVariable(varName)) {
//            fn.instructions.emplace_back(OpCode::LOAD_GLOBAL, varName);
//        }
//        else {
//            // Локальная переменная
//            int localIndex = fn.symbolTable.getVariableIndex(varName);
//            if (localIndex == -1) {
//                throw std::runtime_error("Undefined variable: " + varName);
//            }
//            fn.instructions.emplace_back(OpCode::LOAD_LOCAL, localIndex);
//        }
//    }
//
//    // ------------------------------------------------------------
//    // 11) Генерация бинарных операций
//    // ------------------------------------------------------------
//    void generateBinaryOp(const ASTNode &binOp, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
//        const std::string &op = binOp.value;
//
//        // Обработка присваивания непосредственно здесь
//        if (op == "=") {
//            // Логика присваивания: a = b + c
//            // Генерим RHS (b + c)
//            generateExpression(binOp.children[1], globalSymbolTable, fn);
//
//            // Определяем LHS
//            const ASTNode &lhs = binOp.children[0];
//            if (lhs.type == ASTNodeType::Identifier) {
//                std::string varName = lhs.value;
//
//                // Проверяем, существует ли переменная
//                if (!globalSymbolTable.hasVariable(varName)) {
//                    throw std::runtime_error("Undefined variable: " + varName);
//                }
//
//                // Проверяем, является ли переменная константой
//                int constVal;
//                if (globalSymbolTable.hasConstant(varName, constVal)) {
//                    throw std::runtime_error("Cannot modify constant variable: " + varName);
//                }
//
//                // Присваиваем значение глобальной переменной
//                fn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
//            }
//            else if (lhs.type == ASTNodeType::BinaryOp && lhs.value == "[]") {
//                // Присваивание элементу массива: array[i] = value
//                if (lhs.children.size() != 2) {
//                    throw std::runtime_error("Array assignment requires two children: array name and index");
//                }
//                std::string arrayName = lhs.children[0].value;
//
//                // Проверяем, существует ли массив
//                if (!globalSymbolTable.hasVariable(arrayName)) {
//                    throw std::runtime_error("Undefined array: " + arrayName);
//                }
//
//                // Генерируем индекс
//                generateExpression(lhs.children[1], globalSymbolTable, fn);
//
//                // Присваиваем значение элементу массива
//                fn.instructions.emplace_back(OpCode::STORE_ARRAY, arrayName);
//            }
//            else {
//                throw std::runtime_error("Assignment to complex LHS not implemented.");
//            }
//            return;
//        }
//
//        // Обычные бинарные операции: +, -, *, /, и т.д.
//        // Генерим левый и правый операнды
//        generateExpression(binOp.children[0], globalSymbolTable, fn);
//        generateExpression(binOp.children[1], globalSymbolTable, fn);
//
//        // Генерим соответствующий опкод
//        if (op == "+") {
//            fn.instructions.emplace_back(OpCode::ADD);
//        }
//        else if (op == "-") {
//            fn.instructions.emplace_back(OpCode::SUB);
//        }
//        else if (op == "*") {
//            fn.instructions.emplace_back(OpCode::MUL);
//        }
//        else if (op == "/") {
//            fn.instructions.emplace_back(OpCode::DIV);
//        }
//        else if (op == "%") {
//            fn.instructions.emplace_back(OpCode::MOD);
//        }
//        else if (op == "<") {
//            fn.instructions.emplace_back(OpCode::CMP_LT);
//        }
//        else if (op == "<=") {
//            fn.instructions.emplace_back(OpCode::CMP_LE);
//        }
//        else if (op == ">") {
//            fn.instructions.emplace_back(OpCode::CMP_GT);
//        }
//        else if (op == ">=") {
//            fn.instructions.emplace_back(OpCode::CMP_GE);
//        }
//        else if (op == "==") {
//            fn.instructions.emplace_back(OpCode::CMP_EQ);
//        }
//        else if (op == "!=") {
//            fn.instructions.emplace_back(OpCode::CMP_NE);
//        }
//        else if (op == "&&") {
//            fn.instructions.emplace_back(OpCode::AND);
//        }
//        else if (op == "||") {
//            fn.instructions.emplace_back(OpCode::OR);
//        }
//        else if (op == "[]") {
//            // Доступ к элементу массива: array[i]
//            // Генерим имя массива и индекс
//            std::string arrayName = binOp.children[0].value;
//            generateExpression(binOp.children[1], globalSymbolTable, fn);
//            fn.instructions.emplace_back(OpCode::LOAD_ARRAY, arrayName);
//        }
//        else if (op == "call") {
//            // Вызов функции: func(args)
//            // binOp.children[0] = Identifier (имя функции)
//            // binOp.children[1..] = аргументы
//            std::string funcName = binOp.children[0].value;
//            fn.instructions.emplace_back(OpCode::CALL, funcName);
//        }
//        else {
//            std::cerr << "Unknown BinaryOp: " << op << std::endl;
//        }
//    }
//
//    // ------------------------------------------------------------
//    // Дополнительные члены класса
//    // ------------------------------------------------------------
//
//    /**
//     * Вспомогательная функция для оценки константных выражений
//     */
//    int evaluateConstantExpression(const ASTNode &exprNode, const InterpreterSymbolTable &symbolTable) const {
//        switch (exprNode.type) {
//            case ASTNodeType::Literal:
//                return std::stoi(exprNode.value);
//
//            case ASTNodeType::Identifier: {
//                int val;
//                if (symbolTable.hasConstant(exprNode.value, val)) {
//                    return val;
//                }
//                throw std::runtime_error("Array size identifier is not a constant: " + exprNode.value);
//            }
//
//            case ASTNodeType::BinaryOp: {
//                if (exprNode.children.size() != 2) {
//                    throw std::runtime_error("BinaryOp must have exactly two children.");
//                }
//                // Рекурсивно вычисляем левый и правый операнды
//                int left = evaluateConstantExpression(exprNode.children[0], symbolTable);
//                int right = evaluateConstantExpression(exprNode.children[1], symbolTable);
//
//                // Выполняем операцию в зависимости от оператора
//                if (exprNode.value == "+") {
//                    return left + right;
//                }
//                else if (exprNode.value == "-") {
//                    return left - right;
//                }
//                else if (exprNode.value == "*") {
//                    return left * right;
//                }
//                else if (exprNode.value == "/") {
//                    if (right == 0) {
//                        throw std::runtime_error("Division by zero in constant expression.");
//                    }
//                    return left / right;
//                }
//                else if (exprNode.value == "%") {
//                    if (right == 0) {
//                        throw std::runtime_error("Modulo by zero in constant expression.");
//                    }
//                    return left % right;
//                }
//                else {
//                    throw std::runtime_error("Unsupported operator in constant expression: " + exprNode.value);
//                }
//            }
//
//            default:
//                throw std::runtime_error("Array size must be a literal, a constant identifier, or a constant expression.");
//        }
//    }
//};

#pragma once

#include "InstructionMy.h"
#include "InterpreterSymbolTable.h"
#include "BytecodeProgramMy.h"
#include "../Parser_AST/AST.h"

#include <stdexcept>
#include <iostream>
#include <cstdlib>
#include <string>

/**
 * Класс, который обходит AST (с типами Program, VarDecl, ForStmt, и т.д.)
 * и заполняет BytecodeProgram собственными инструкциями.
 */
class MyBytecodeGenerator {
public:
    MyBytecodeGenerator() = default;

    BytecodeProgramMy generate(const ASTNode &root) {
        BytecodeProgramMy program;

        if (root.type != ASTNodeType::Program) {
            throw std::runtime_error("MyBytecodeGenerator: root must be Program");
        }

        // 1. Создаём функцию 'main' сначала
        program.functions.emplace_back(BytecodeFunctionMy("main"));
        BytecodeFunctionMy &mainFn = program.functions.back();

        // 2. Обходим детей (VarDecl или FunctionDecl)
        for (auto &child : root.children) {
            switch (child.type) {
                case ASTNodeType::VarDecl:
                    // Генерация глобальной переменной и её инициализация в 'main'
                    generateGlobalVarDecl(child, program, mainFn);
                    break;
                case ASTNodeType::FunctionDecl:
                    // Генерация функции
                    generateFunctionDecl(child, program);
                    break;
                default:
                    // Что-то ещё на уровне Program?
                    std::cerr << "WARNING: top-level node not handled: "
                              << astNodeTypeToString(child.type) << std::endl;
                    break;
            }
        }

        // 3. Добавляем команду HALT в 'main'
        mainFn.instructions.emplace_back(OpCode::HALT);
        return program;
    }

private:
    // ------------------------------------------------------------
    // 1) Глобальные переменные
    // ------------------------------------------------------------
    void generateGlobalVarDecl(const ASTNode &varNode, BytecodeProgramMy &program, BytecodeFunctionMy &mainFn) {
        // varNode.value = "integer" или "bool" и т.п.
        // varNode.children[0] = Identifier(...)
        // varNode.children[1..] = инициализация или массив, и т.д.
        if (varNode.children.empty()) {
            throw std::runtime_error("VarDecl node has no children (missing identifier?).");
        }
        std::string varName = varNode.children[0].value;
        std::cout << "Global VarDecl: " << varNode.value << " " << varName << std::endl;

        if (varName.empty()) {
            throw std::runtime_error("Variable name is empty.");
        }

        VarType varType = parseVarType(varNode.value);

        // Проверяем, массив ли это
        if (varNode.children.size() > 1 &&
            varNode.children[1].type == ASTNodeType::BinaryOp &&
            varNode.children[1].value == "arrayDim") {
            // Обработка массива
            ASTNode sizeExpr = varNode.children[1].children[0];
            int arraySize = evaluateConstantExpression(sizeExpr, program.globalSymbolTable);

            // Создаём переменную массива
            Variable var(varType, varName, varType, arraySize);
            program.globalSymbolTable.addVariable(var);

            // Инициализируем массив нулями
            for(int i = 0; i < arraySize; ++i) {
                mainFn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                mainFn.instructions.emplace_back(OpCode::STORE_ARRAY, varName);
            }
        }
        else {
            // Обработка простой переменной
            Variable var(varType, varName);
            int varIndex = program.globalSymbolTable.addVariable(var); // Добавляем переменную и получаем её индекс

            if (varNode.children.size() > 1) {
                // Инициализация переменной
                ASTNode initExpr = varNode.children[1];
                int initVal = evaluateConstantExpression(initExpr, program.globalSymbolTable);

                // Обновляем существующую переменную как константу
                program.globalSymbolTable.variables[varIndex].isConstant = true;
                program.globalSymbolTable.variables[varIndex].ConstantValue = initVal;

                // Генерируем инструкции для инициализации
                generateExpression(initExpr, program.globalSymbolTable, mainFn);
                mainFn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
            }
            else {
                // Без инициализации — устанавливаем 0
                mainFn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                mainFn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
            }
        }
    }

    VarType parseVarType(const std::string &typeStr) const {
        if (typeStr == "integer") return VarType::INTEGER;
        if (typeStr == "float") return VarType::FLOAT;
        if (typeStr == "bool") return VarType::BOOL;
        if (typeStr == "string") return VarType::STRING;
        if (typeStr == "void") return VarType::VOID;
        throw std::runtime_error("Unknown variable type: " + typeStr);
    }

    // ------------------------------------------------------------
    // 2) Обработка FunctionDecl
    // ------------------------------------------------------------
    void generateFunctionDecl(const ASTNode &funcNode, BytecodeProgramMy &program) {
        // funcNode.value = "void" или "integer" etc. (тип)
        // funcNode.children[0] = Identifier(имя_функции)
        // funcNode.children[1..] = Параметры, а последний — Block.

        if (funcNode.children.empty()) {
            throw std::runtime_error("FunctionDecl has no children");
        }
        std::string funcName = funcNode.children[0].value;

        int fnIndex = program.addFunction(funcName);
        BytecodeFunctionMy &fn = program.functions[fnIndex];

        // Обработка параметров и тела функции
        size_t i = 1;
        // Сначала параметры
        for (; i < funcNode.children.size(); i++) {
            if (funcNode.children[i].type == ASTNodeType::Parameter) {
                generateParameter(funcNode.children[i], fn);
            }
            else {
                // дошли до блока
                break;
            }
        }
        // В i должен быть Block
        if (i >= funcNode.children.size()) {
            throw std::runtime_error("FunctionDecl: no body (Block) found");
        }
        if (funcNode.children[i].type != ASTNodeType::Block) {
            throw std::runtime_error("FunctionDecl: expected Block as last child");
        }
        generateBlock(funcNode.children[i], program.globalSymbolTable, fn);

        // В конце функции, если нет RET, добавим
        if (fn.instructions.empty()
            || fn.instructions.back().opcode != OpCode::RET) {
            fn.instructions.emplace_back(OpCode::RET);
        }
    }

    void generateParameter(const ASTNode &paramNode, BytecodeFunctionMy &fn) {
        // paramNode.value = тип (например, "integer")
        // paramNode.children[0] = Identifier(имя)
        VarType paramType = parseVarType(paramNode.value);
        std::string paramName = paramNode.children[0].value;

        // Добавляем параметр в локальную символическую таблицу
        Variable var(paramType, paramName);
        fn.symbolTable.addVariable(var);

        std::cout << "Parameter: type=" << paramNode.value
                  << " name=" << paramName << std::endl;
    }

    // ------------------------------------------------------------
    // 3) Обработка блоков и операторов
    // ------------------------------------------------------------
    void generateBlock(const ASTNode &blockNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        if (blockNode.type != ASTNodeType::Block) {
            throw std::runtime_error("generateBlock: node is not Block");
        }
        // blockNode.children — это набор statement'ов
        for (auto &stmt : blockNode.children) {
            generateStatement(stmt, globalSymbolTable, fn);
        }
    }

    void generateStatement(const ASTNode &stmt, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        switch (stmt.type) {
            case ASTNodeType::VarDecl:
                generateLocalVarDecl(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::ReturnStmt:
                generateReturnStmt(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::IfStmt:
                generateIfStmt(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::WhileStmt:
                generateWhileStmt(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::ForStmt:
                generateForStmt(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::Assignment:
                generateAssignment(stmt, globalSymbolTable, fn);
                break;
            case ASTNodeType::Expression:
            case ASTNodeType::BinaryOp:
            case ASTNodeType::Identifier:
            case ASTNodeType::Literal:
                // Это может быть Expression-Statement, например "a + 1;"
                generateExpression(stmt, globalSymbolTable, fn);
                // При необходимости можно добавить POP
                break;
            default:
                std::cerr << "Unhandled statement type: "
                          << astNodeTypeToString(stmt.type) << std::endl;
                break;
        }
    }

    // ------------------------------------------------------------
    // 4) Локальные переменные
    // ------------------------------------------------------------
    void generateLocalVarDecl(const ASTNode &varNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // Пример: "integer key = global_array[1];"
        // varNode.value = "integer"
        // varNode.children[0] = Identifier("key")
        // varNode.children[1] = BinaryOp("[]")

        if (varNode.children.empty()) {
            throw std::runtime_error("Local VarDecl has no children");
        }
        std::string varName = varNode.children[0].value;

        // Определение типа
        VarType varType = parseVarType(varNode.value);

        // Проверяем, массив ли это
        if (varNode.children.size() > 1 &&
            varNode.children[1].type == ASTNodeType::BinaryOp &&
            varNode.children[1].value == "arrayDim") {
            // Обработка массива (аналогично глобальным переменным)
            ASTNode sizeExpr = varNode.children[1].children[0];
            int arraySize = evaluateConstantExpression(sizeExpr, globalSymbolTable);

            // Создаём переменную массива
            Variable var(varType, varName, varType, arraySize);
            fn.symbolTable.addVariable(var);
            int varIndex = fn.symbolTable.getVariableIndex(varName);
            std::cout << "Added local array variable: " << varName << " (Size: " << arraySize << ") at index " << varIndex << std::endl;

            // Инициализируем массив нулями
            for(int i = 0; i < arraySize; ++i) {
                fn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                fn.instructions.emplace_back(OpCode::STORE_ARRAY, varName);
            }
        }
        else {
            // Простая переменная
            Variable var(varType, varName);
            int varIndex = fn.symbolTable.addVariable(var);
            std::cout << "Added local variable: " << varName << " (Index: " << varIndex << ")" << std::endl;

            if (varNode.children.size() > 1) {
                // Инициализация переменной
                ASTNode initExpr = varNode.children[1];

                // Генерируем инструкции для инициализации
                generateExpression(initExpr, globalSymbolTable, fn);
                fn.instructions.emplace_back(OpCode::STORE_LOCAL, varIndex);
                std::cout << "Initialized local variable: " << varName << std::endl;
            }
            else {
                // Без инициализации — устанавливаем 0
                fn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                fn.instructions.emplace_back(OpCode::STORE_LOCAL, varIndex);
                std::cout << "Initialized local variable to 0: " << varName << std::endl;
            }
        }
    }

    // ------------------------------------------------------------
    // 5) RETURN
    // ------------------------------------------------------------
    void generateReturnStmt(const ASTNode &retNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // Если есть выражение в return
        if (!retNode.children.empty()) {
            generateExpression(retNode.children[0], globalSymbolTable, fn);
        }
        fn.instructions.emplace_back(OpCode::RET);
    }

    // ------------------------------------------------------------
    // 6) IF
    // ------------------------------------------------------------
    void generateIfStmt(const ASTNode &ifNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // ifNode.children[0] = cond
        // ifNode.children[1] = thenBlock
        // ifNode.children[2] = elseBlock (опционально)
        if (ifNode.children.size() < 2) {
            throw std::runtime_error("IfStmt has <2 children (missing cond or thenBlock)");
        }
        // 1) условие
        generateExpression(ifNode.children[0], globalSymbolTable, fn);

        // 2) вставляем JMP_IF_FALSE (пока operandInt = -1)
        int jmpIfFalseIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);

        // 3) thenBlock
        generateBlock(ifNode.children[1], globalSymbolTable, fn);

        // 4) вставляем JMP, чтобы перепрыгнуть через else
        int jmpIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP, -1);

        // 5) fixup JMP_IF_FALSE -> начало else
        fn.instructions[jmpIfFalseIndex].operandInt = (int)fn.instructions.size();

        // 6) если есть elseBlock
        if (ifNode.children.size() > 2) {
            generateBlock(ifNode.children[2], globalSymbolTable, fn);
        }

        // 7) fixup JMP -> конец if
        fn.instructions[jmpIndex].operandInt = (int)fn.instructions.size();
    }

    // ------------------------------------------------------------
    // 7) WHILE
    // ------------------------------------------------------------
    void generateWhileStmt(const ASTNode &whileNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // whileNode.children[0] = cond
        // whileNode.children[1] = body
        if (whileNode.children.size() < 2) {
            throw std::runtime_error("WhileStmt has <2 children");
        }
        int loopStart = (int)fn.instructions.size();

        // Условие
        generateExpression(whileNode.children[0], globalSymbolTable, fn);

        // jmp_if_false => конец цикла
        int jmpIfFalseIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);

        // Тело цикла
        generateBlock(whileNode.children[1], globalSymbolTable, fn);

        // Переход назад к началу
        fn.instructions.emplace_back(OpCode::JMP, loopStart);

        // fixup jmpIfFalse -> конец цикла
        int endPos = (int)fn.instructions.size();
        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
    }

    // ------------------------------------------------------------
    // 8) FOR
    // ------------------------------------------------------------
    void generateForStmt(const ASTNode &forNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // forNode (value="for") имеет 4 children:
        //   [0] init
        //   [1] cond
        //   [2] step
        //   [3] body

        if (forNode.children.size() < 4) {
            throw std::runtime_error("ForStmt must have 4 children (init, cond, step, body)");
        }

        // (1) инициализация
        generateStatement(forNode.children[0], globalSymbolTable, fn);

        // (2) метка начала цикла
        int loopStart = (int)fn.instructions.size();

        // (3) условие
        generateExpression(forNode.children[1], globalSymbolTable, fn);

        // (4) jmp_if_false => конец цикла
        int jmpIfFalseIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);

        // (5) тело цикла
        generateBlock(forNode.children[3], globalSymbolTable, fn);

        // (6) шаг
        generateStatement(forNode.children[2], globalSymbolTable, fn);

        // (7) переход назад к началу
        fn.instructions.emplace_back(OpCode::JMP, loopStart);

        // (8) fixup jmpIfFalse -> конец цикла
        int endPos = (int)fn.instructions.size();
        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
    }

    // ------------------------------------------------------------
    // 9) ASSIGNMENT
    // ------------------------------------------------------------
    void generateAssignment(const ASTNode &assignNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // assignNode.children[0] = lhs
        // assignNode.children[1] = rhs
        if (assignNode.children.size() < 2) {
            throw std::runtime_error("Assignment node must have 2 children (lhs, rhs)");
        }

        // Генерим RHS
        generateExpression(assignNode.children[1], globalSymbolTable, fn);

        // Определяем LHS
        const ASTNode &lhs = assignNode.children[0];
        if (lhs.type == ASTNodeType::Identifier) {
            std::string varName = lhs.value;

            bool isGlobal = globalSymbolTable.hasVariable(varName);
            bool isLocal = fn.symbolTable.hasVariable(varName);

            std::cout << "Assignment to variable: " << varName
                      << ", isGlobal: " << isGlobal
                      << ", isLocal: " << isLocal << std::endl;

            if (!isGlobal && !isLocal) {
                throw std::runtime_error("Undefined variable: " + varName);
            }

            if (isGlobal) {
                int constVal;
                if (globalSymbolTable.hasConstant(varName, constVal)) {
                    throw std::runtime_error("Cannot modify constant variable: " + varName);
                }
                // Присваиваем значение глобальной переменной
                fn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
                std::cout << "Assigned to global variable: " << varName << std::endl;
            }
            else if (isLocal) {
                int constVal;
                if (fn.symbolTable.hasConstant(varName, constVal)) {
                    throw std::runtime_error("Cannot modify constant variable: " + varName);
                }
                // Присваиваем значение локальной переменной
                int localIndex = fn.symbolTable.getVariableIndex(varName);
                std::cout << "Assigned to local variable: " << varName << " (Index: " << localIndex << ")" << std::endl;
                fn.instructions.emplace_back(OpCode::STORE_LOCAL, localIndex);
            }
        }
        else if (lhs.type == ASTNodeType::BinaryOp && lhs.value == "[]") {
            // Обработка присваивания элементу массива
            if (lhs.children.size() != 2) {
                throw std::runtime_error("Array assignment requires two children: array name and index");
            }
            std::string arrayName = lhs.children[0].value;

            // Проверяем, существует ли массив в глобальной таблице
            if (!globalSymbolTable.hasVariable(arrayName)) {
                throw std::runtime_error("Undefined array: " + arrayName);
            }

            // Генерируем индекс
            generateExpression(lhs.children[1], globalSymbolTable, fn);

            // Проверяем, является ли массив константой
            int constVal;
            if (globalSymbolTable.hasConstant(arrayName, constVal)) {
                throw std::runtime_error("Cannot modify constant array: " + arrayName);
            }

            // Присваиваем значение элементу массива
            fn.instructions.emplace_back(OpCode::STORE_ARRAY, arrayName);
            std::cout << "Assigned to array: " << arrayName << std::endl;
        }
        else {
            throw std::runtime_error("Assignment to complex LHS not implemented.");
        }
    }

    // ------------------------------------------------------------
    // 10) Генерация выражений
    // ------------------------------------------------------------
    void generateLiteral(const ASTNode &literalNode, BytecodeFunctionMy &fn) {
        // Поддержка только целых чисел для простоты
        // Можно расширить для других типов (float, bool, string и т.д.)
        long long val = std::stoll(literalNode.value);
        fn.instructions.emplace_back(OpCode::LOAD_CONST, val);
        std::cout << "Generated LOAD_CONST " << val << std::endl;
    }

    void generateExpression(const ASTNode &exprNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        switch (exprNode.type) {
            case ASTNodeType::Literal:
                generateLiteral(exprNode, fn);
                break;

            case ASTNodeType::Identifier:
                generateIdentifier(exprNode, globalSymbolTable, fn);
                break;

            case ASTNodeType::BinaryOp:
                generateBinaryOp(exprNode, globalSymbolTable, fn);
                break;

            default:
                std::cerr << "generateExpression: unhandled type: "
                          << astNodeTypeToString(exprNode.type) << std::endl;
                break;
        }
    }

    void generateIdentifier(const ASTNode &idNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        std::string varName = idNode.value;
        // Проверяем, глобальная ли переменная
        if (globalSymbolTable.hasVariable(varName)) {
            fn.instructions.emplace_back(OpCode::LOAD_GLOBAL, varName);
            std::cout << "Generated LOAD_GLOBAL " << varName << std::endl;
        }
        else {
            // Локальная переменная
            int localIndex = fn.symbolTable.getVariableIndex(varName);
            if (localIndex == -1) {
                throw std::runtime_error("Undefined variable: " + varName);
            }
            fn.instructions.emplace_back(OpCode::LOAD_LOCAL, localIndex);
            std::cout << "Generated LOAD_LOCAL " << varName << " (Index: " << localIndex << ")" << std::endl;
        }
    }

    // ------------------------------------------------------------
    // 11) Генерация бинарных операций
    // ------------------------------------------------------------
    void generateBinaryOp(const ASTNode &binOp, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        const std::string &op = binOp.value;

        // Обработка присваивания непосредственно здесь
        if (op == "=") {
            // Логика присваивания: a = b + c
            // Генерим RHS (b + c)
            generateExpression(binOp.children[1], globalSymbolTable, fn);

            // Определяем LHS
            const ASTNode &lhs = binOp.children[0];
            if (lhs.type == ASTNodeType::Identifier) {
                std::string varName = lhs.value;

                bool isGlobal = globalSymbolTable.hasVariable(varName);
                bool isLocal = fn.symbolTable.hasVariable(varName);

                std::cout << "Assignment to variable: " << varName
                          << ", isGlobal: " << isGlobal
                          << ", isLocal: " << isLocal << std::endl;

                if (!isGlobal && !isLocal) {
                    throw std::runtime_error("Undefined variable: " + varName);
                }

                if (isGlobal) {
                    int constVal;
                    if (globalSymbolTable.hasConstant(varName, constVal)) {
                        throw std::runtime_error("Cannot modify constant variable: " + varName);
                    }
                    // Присваиваем значение глобальной переменной
                    fn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);
                    std::cout << "Assigned to global variable: " << varName << std::endl;
                }
                else if (isLocal) {
                    int constVal;
                    if (fn.symbolTable.hasConstant(varName, constVal)) {
                        throw std::runtime_error("Cannot modify constant variable: " + varName);
                    }
                    // Присваиваем значение локальной переменной
                    int localIndex = fn.symbolTable.getVariableIndex(varName);
                    std::cout << "Assigned to local variable: " << varName << " (Index: " << localIndex << ")" << std::endl;
                    fn.instructions.emplace_back(OpCode::STORE_LOCAL, localIndex);
                }
            }
            else if (lhs.type == ASTNodeType::BinaryOp && lhs.value == "[]") {
                // Присваивание элементу массива: array[i] = value
                if (lhs.children.size() != 2) {
                    throw std::runtime_error("Array assignment requires two children: array name and index");
                }
                std::string arrayName = lhs.children[0].value;

                // Проверяем, существует ли массив в глобальной таблице
                if (!globalSymbolTable.hasVariable(arrayName)) {
                    throw std::runtime_error("Undefined array: " + arrayName);
                }

                // Генерируем индекс
                generateExpression(lhs.children[1], globalSymbolTable, fn);

                // Проверяем, является ли массив константой
                int constVal;
                if (globalSymbolTable.hasConstant(arrayName, constVal)) {
                    throw std::runtime_error("Cannot modify constant array: " + arrayName);
                }

                // Присваиваем значение элементу массива
                fn.instructions.emplace_back(OpCode::STORE_ARRAY, arrayName);
                std::cout << "Assigned to array: " << arrayName << std::endl;
            }
            else {
                throw std::runtime_error("Assignment to complex LHS not implemented.");
            }
            return;
        }

        // Обычные бинарные операции: +, -, *, /, и т.д.
        // Генерим левый и правый операнды
        generateExpression(binOp.children[0], globalSymbolTable, fn);
        generateExpression(binOp.children[1], globalSymbolTable, fn);

        // Генерим соответствующий опкод
        if (op == "+") {
            fn.instructions.emplace_back(OpCode::ADD);
            std::cout << "Generated ADD" << std::endl;
        }
        else if (op == "-") {
            fn.instructions.emplace_back(OpCode::SUB);
            std::cout << "Generated SUB" << std::endl;
        }
        else if (op == "*") {
            fn.instructions.emplace_back(OpCode::MUL);
            std::cout << "Generated MUL" << std::endl;
        }
        else if (op == "/") {
            fn.instructions.emplace_back(OpCode::DIV);
            std::cout << "Generated DIV" << std::endl;
        }
        else if (op == "%") {
            fn.instructions.emplace_back(OpCode::MOD);
            std::cout << "Generated MOD" << std::endl;
        }
        else if (op == "<") {
            fn.instructions.emplace_back(OpCode::CMP_LT);
            std::cout << "Generated CMP_LT" << std::endl;
        }
        else if (op == "<=") {
            fn.instructions.emplace_back(OpCode::CMP_LE);
            std::cout << "Generated CMP_LE" << std::endl;
        }
        else if (op == ">") {
            fn.instructions.emplace_back(OpCode::CMP_GT);
            std::cout << "Generated CMP_GT" << std::endl;
        }
        else if (op == ">=") {
            fn.instructions.emplace_back(OpCode::CMP_GE);
            std::cout << "Generated CMP_GE" << std::endl;
        }
        else if (op == "==") {
            fn.instructions.emplace_back(OpCode::CMP_EQ);
            std::cout << "Generated CMP_EQ" << std::endl;
        }
        else if (op == "!=") {
            fn.instructions.emplace_back(OpCode::CMP_NE);
            std::cout << "Generated CMP_NE" << std::endl;
        }
        else if (op == "&&") {
            fn.instructions.emplace_back(OpCode::AND);
            std::cout << "Generated AND" << std::endl;
        }
        else if (op == "||") {
            fn.instructions.emplace_back(OpCode::OR);
            std::cout << "Generated OR" << std::endl;
        }
        else if (op == "[]") {
            // Доступ к элементу массива: array[i]
            // Генерим имя массива и индекс
            std::string arrayName = binOp.children[0].value;
            generateExpression(binOp.children[1], globalSymbolTable, fn);
            fn.instructions.emplace_back(OpCode::LOAD_ARRAY, arrayName);
            std::cout << "Generated LOAD_ARRAY " << arrayName << std::endl;
        }
        else if (op == "call") {
            // Вызов функции: func(args)
            // binOp.children[0] = Identifier (имя функции)
            // binOp.children[1..] = аргументы
            std::string funcName = binOp.children[0].value;
            fn.instructions.emplace_back(OpCode::CALL, funcName);
            std::cout << "Generated CALL " << funcName << std::endl;
        }
        else {
            std::cerr << "Unknown BinaryOp: " << op << std::endl;
        }
    }

    // ------------------------------------------------------------
    // Дополнительные члены класса
    // ------------------------------------------------------------

    /**
     * Вспомогательная функция для оценки константных выражений
     */
    int evaluateConstantExpression(const ASTNode &exprNode, const InterpreterSymbolTable &symbolTable) const {
        switch (exprNode.type) {
            case ASTNodeType::Literal:
                return std::stoi(exprNode.value);

            case ASTNodeType::Identifier: {
                int val;
                if (symbolTable.hasConstant(exprNode.value, val)) {
                    return val;
                }
                throw std::runtime_error("Array size identifier is not a constant: " + exprNode.value);
            }

            case ASTNodeType::BinaryOp: {
                if (exprNode.children.size() != 2) {
                    throw std::runtime_error("BinaryOp must have exactly two children.");
                }
                // Рекурсивно вычисляем левый и правый операнды
                int left = evaluateConstantExpression(exprNode.children[0], symbolTable);
                int right = evaluateConstantExpression(exprNode.children[1], symbolTable);

                // Выполняем операцию в зависимости от оператора
                if (exprNode.value == "+") {
                    return left + right;
                }
                else if (exprNode.value == "-") {
                    return left - right;
                }
                else if (exprNode.value == "*") {
                    return left * right;
                }
                else if (exprNode.value == "/") {
                    if (right == 0) {
                        throw std::runtime_error("Division by zero in constant expression.");
                    }
                    return left / right;
                }
                else if (exprNode.value == "%") {
                    if (right == 0) {
                        throw std::runtime_error("Modulo by zero in constant expression.");
                    }
                    return left % right;
                }
                else {
                    throw std::runtime_error("Unsupported operator in constant expression: " + exprNode.value);
                }
            }

            default:
                throw std::runtime_error("Array size must be a literal, a constant identifier, or a constant expression.");
        }
    }
};
