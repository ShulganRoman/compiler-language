#pragma once

#include "InstructionMy.h"
#include "InterpreterSymbolTable.h"
#include "BytecodeProgramMy.h"
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

        // Добавим встроенную/служебную функцию print
        program.globalSymbolTable.addFunction("print");

        // 1) Сухое добавление глобальных переменных
        for (auto &child : root.children) {
            if (child.type == ASTNodeType::VarDecl) {
                addGlobalVarSymbol(child, program);
            }
        }

        // 2) Обработка FunctionDecl
        for (auto &child : root.children) {
            if (child.type == ASTNodeType::FunctionDecl) {
                generateFunctionDecl(child, program, program.globalSymbolTable);
            }
        }

        // 3) Находим main
        BytecodeFunctionMy* mainFn = const_cast<BytecodeFunctionMy*>(program.getFunction("main"));
        if (!mainFn) {
            throw std::runtime_error("Main function not found.");
        }

        // 4) Второй проход: Инициализация глобальных переменных - сейчас у вас так:
        for (auto &child : root.children) {
            if (child.type == ASTNodeType::VarDecl) {
                generateGlobalVarInitialization(child, program, *mainFn);
            }
        }

        // 5) Вставляем HALT в конец
//        mainFn->instructions.emplace_back(OpCode::HALT);

        return program;
    }


private:
    // ------------------------------------------------------------
    // 0) "Сухое" добавление глобальной переменной в таблицу (без инструкций)
    // ------------------------------------------------------------
    void addGlobalVarSymbol(const ASTNode &varNode, BytecodeProgramMy &program) {
        if (varNode.children.empty()) {
            throw std::runtime_error("VarDecl node has no children (missing identifier?).");
        }
        std::string varName = varNode.children[0].value;
        if (varName.empty()) {
            throw std::runtime_error("Variable name is empty in VarDecl.");
        }

        VarType varType = parseVarType(varNode.value);

        // Если это массив
        if (varNode.children.size() > 1 &&
            varNode.children[1].type == ASTNodeType::BinaryOp &&
            varNode.children[1].value == "arrayDim")
        {
            // Просто добавим переменную как массив с нужным размером
            // (Точное вычисление размера, если надо, см. evaluateConstantExpression)
            ASTNode sizeExpr = varNode.children[1].children[0];
            int arraySize = evaluateConstantExpression(sizeExpr, program.globalSymbolTable);

            Variable var(varType, varName, varType, arraySize);
            program.globalSymbolTable.addVariable(var);
        } else {
            // Обычная переменная
            Variable var(varType, varName);
            program.globalSymbolTable.addVariable(var);
        }
    }

    // ------------------------------------------------------------
    // 1) Глобальные переменные
    // ------------------------------------------------------------
    void generateGlobalVarInitialization(const ASTNode &varNode,
                                         BytecodeProgramMy &program,
                                         BytecodeFunctionMy &mainFn)
    {
        if (varNode.children.empty()) {
            throw std::runtime_error("VarDecl node has no children (missing identifier?).");
        }
        std::string varName = varNode.children[0].value;
        VarType varType = parseVarType(varNode.value);

        // Проверка: уже должно быть в глобальной таблице
        int varIndex = program.globalSymbolTable.getVariableIndex(varName);
        if (varIndex == -1) {
            throw std::runtime_error("Global variable not found in table: " + varName);
        }

        // Массив?
        bool isArray = false;
        int arraySize = -1;
        if (varNode.children.size() > 1 &&
            varNode.children[1].type == ASTNodeType::BinaryOp &&
            varNode.children[1].value == "arrayDim")
        {
            isArray = true;
            ASTNode sizeExpr = varNode.children[1].children[0];
            arraySize = evaluateConstantExpression(sizeExpr, program.globalSymbolTable);
        }

        if (isArray) {
            // Инициализация массива нулями (или чем-то ещё)
            // например, в цикле LOAD_CONST 0 / STORE_ARRAY varName
            for (int i = 0; i < arraySize; i++) {
                mainFn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                mainFn.instructions.emplace_back(OpCode::STORE_ARRAY, varName);
            }
        } else {
            // Обычная переменная
            // Если есть инициализатор - генерируем его
            if (varNode.children.size() > 1) {
                ASTNode initExpr = varNode.children[1];
                // Генерация кода для RHS
                generateExpression(initExpr, program.globalSymbolTable, mainFn);
                // STORE_GLOBAL varName
                mainFn.instructions.emplace_back(OpCode::STORE_GLOBAL, varName);

                // Если эта переменная «константа», можно отметить
                // (только если ваш дизайн это предполагает)
                // ...
            } else {
                // Нет инициализатора — положим 0
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
    void generateFunctionDecl(const ASTNode &funcNode, BytecodeProgramMy &program, InterpreterSymbolTable &globalSymbolTable) {
        // Проверка наличия детей
        if (funcNode.children.empty()) {
            throw std::runtime_error("FunctionDecl has no children");
        }

        // Извлечение имени функции
        std::string funcName = funcNode.children[0].value;

        // Регистрируем функцию в глобальной таблице символов **до** генерации её тела
        globalSymbolTable.addFunction(funcName);
//        std::cout << "Registered function: " << funcName << " in global symbol table" << std::endl;

        // Добавляем функцию в программу
        int fnIndex = program.addFunction(funcName);
        BytecodeFunctionMy &fn = program.functions[fnIndex];

        // Обработка параметров и тела функции
        size_t i = 1;
        for (; i < funcNode.children.size(); i++) {
            if (funcNode.children[i].type == ASTNodeType::Parameter) {
                generateParameter(funcNode.children[i], fn);
            }
            else {
                // дошли до блока
                break;
            }
        }

        // Проверка наличия тела функции
        if (i >= funcNode.children.size()) {
            throw std::runtime_error("FunctionDecl: no body (Block) found");
        }
        if (funcNode.children[i].type != ASTNodeType::Block) {
            throw std::runtime_error("FunctionDecl: expected Block as last child");
        }

        // Генерация блока тела функции
        generateBlock(funcNode.children[i], program.globalSymbolTable, fn);

//        // В конце функции, если нет RET, добавляем его
//        if (fn.instructions.empty()
//            || fn.instructions.back().opcode != OpCode::RET) {
//            fn.instructions.emplace_back(OpCode::RET);
//        }
    }

    void generateParameter(const ASTNode &paramNode, BytecodeFunctionMy &fn) {
        // paramNode.value = "integer"
        // paramNode.children[0] = Identifier(имя)
        VarType paramType = parseVarType(paramNode.value);
        std::string paramName = paramNode.children[0].value;

        Variable var(paramType, paramName);
        fn.symbolTable.addVariable(var);

        // ВАЖНО: Увеличиваем счётчик параметров
        fn.numParams++;
    }
    // ------------------------------------------------------------
    // 3) Обработка блоков и операторов
    // ------------------------------------------------------------
    void generateBlock(const ASTNode &blockNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        if (blockNode.type != ASTNodeType::Block) {
            throw std::runtime_error("generateBlock: node is not Block");
        }

        // Вход в новую область видимости
        fn.symbolTable.enterScope();
//        std::cout << "Entered new block scope. Total scopes: " << fn.symbolTable.getScopeDepth() << std::endl;

        // Генерация всех операторов в блоке
        for (auto &stmt : blockNode.children) {
            generateStatement(stmt, globalSymbolTable, fn);
        }

        // Выход из области видимости
        fn.symbolTable.exitScope();
//        std::cout << "Exited block scope. Total scopes: " << fn.symbolTable.getScopeDepth() << std::endl;
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
//            std::cout << "Added local array variable: " << varName << " (Size: " << arraySize << ") at index " << varIndex << std::endl;

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
//            std::cout << "Added local variable: " << varName << " (Index: " << varIndex << ")" << std::endl;

            if (varNode.children.size() > 1) {
                // Инициализация переменной
                ASTNode initExpr = varNode.children[1];

                // Генерируем инструкции для инициализации
                generateExpression(initExpr, globalSymbolTable, fn);
                fn.instructions.emplace_back(OpCode::STORE_LOCAL, varIndex);
//                std::cout << "Initialized local variable: " << varName << std::endl;
            }
            else {
                // Без инициализации — устанавливаем 0
                fn.instructions.emplace_back(OpCode::LOAD_CONST, 0);
                fn.instructions.emplace_back(OpCode::STORE_LOCAL, varIndex);
//                std::cout << "Initialized local variable to 0: " << varName << std::endl;
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
//        std::cout << "Generated RET" << std::endl;
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
//        std::cout << "Generated JMP_IF_FALSE placeholder at " << jmpIfFalseIndex << std::endl;

        // 3) thenBlock: Вход в область видимости
        fn.symbolTable.enterScope();
        generateBlock(ifNode.children[1], globalSymbolTable, fn);
        fn.symbolTable.exitScope();

        // 4) вставляем JMP, чтобы перепрыгнуть через else
        int jmpIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP, -1);
//        std::cout << "Generated JMP placeholder at " << jmpIndex << std::endl;

        // 5) fixup JMP_IF_FALSE -> начало else
        fn.instructions[jmpIfFalseIndex].operandInt = (int)fn.instructions.size();
//        std::cout << "Fixed JMP_IF_FALSE to jump to " << fn.instructions.size() << std::endl;

        // 6) если есть elseBlock: Вход в область видимости
        if (ifNode.children.size() > 2) {
            fn.symbolTable.enterScope();
            generateBlock(ifNode.children[2], globalSymbolTable, fn);
            fn.symbolTable.exitScope();
        }

        // 7) fixup JMP -> конец if
        fn.instructions[jmpIndex].operandInt = (int)fn.instructions.size();
//        std::cout << "Fixed JMP to jump to " << fn.instructions.size() << std::endl;
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
//        std::cout << "Loop start at " << loopStart << std::endl;

        // Условие
        generateExpression(whileNode.children[0], globalSymbolTable, fn);

        // jmp_if_false => конец цикла
        int jmpIfFalseIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);
//        std::cout << "Generated JMP_IF_FALSE placeholder at " << jmpIfFalseIndex << std::endl;

        // Тело цикла
        fn.symbolTable.enterScope();
        generateBlock(whileNode.children[1], globalSymbolTable, fn);
        fn.symbolTable.exitScope();

        // Переход назад к началу
        fn.instructions.emplace_back(OpCode::JMP, loopStart);
//        std::cout << "Generated JMP back to loop start at " << loopStart << std::endl;

        // fixup jmpIfFalse -> конец цикла
        int endPos = (int)fn.instructions.size();
        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
//        std::cout << "Fixed JMP_IF_FALSE to jump to " << endPos << std::endl;
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

        // (1) Вход в новую область видимости для for-loop
        fn.symbolTable.enterScope();
//        std::cout << "Entered for-loop scope" << std::endl;

        // (2) Инициализация (например, объявление 'i')
        generateStatement(forNode.children[0], globalSymbolTable, fn);

        // (3) Метка начала цикла
        int loopStart = (int)fn.instructions.size();
//        std::cout << "Loop start at " << loopStart << std::endl;

        // (4) Условие
        generateExpression(forNode.children[1], globalSymbolTable, fn);

        // (5) Вставляем JMP_IF_FALSE (пока operandInt = -1)
        int jmpIfFalseIndex = (int)fn.instructions.size();
        fn.instructions.emplace_back(OpCode::JMP_IF_FALSE, -1);
//        std::cout << "Generated JMP_IF_FALSE placeholder at " << jmpIfFalseIndex << std::endl;

        // (6) Тело цикла (генерируется с собственной областью видимости)
        generateBlock(forNode.children[3], globalSymbolTable, fn);

        // (7) Шаг (например, увеличение 'i')
        generateStatement(forNode.children[2], globalSymbolTable, fn);

        // (8) Переход назад к началу цикла
        fn.instructions.emplace_back(OpCode::JMP, loopStart);
//        std::cout << "Generated JMP back to loop start at " << loopStart << std::endl;

        // (9) Исправляем JMP_IF_FALSE чтобы переходил к концу цикла
        int endPos = (int)fn.instructions.size();
        fn.instructions[jmpIfFalseIndex].operandInt = endPos;
//        std::cout << "Fixed JMP_IF_FALSE to jump to " << endPos << std::endl;

        // (10) Выход из области видимости for-loop
        fn.symbolTable.exitScope();
//        std::cout << "Exited for-loop scope" << std::endl;
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

//            std::cout << "Assignment to variable: " << varName
//                      << ", isGlobal: " << isGlobal
//                      << ", isLocal: " << isLocal << std::endl;

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
//                std::cout << "Assigned to global variable: " << varName << std::endl;
            }
            else if (isLocal) {
                int constVal;
                if (fn.symbolTable.hasConstant(varName, constVal)) {
                    throw std::runtime_error("Cannot modify constant variable: " + varName);
                }
                // Присваиваем значение локальной переменной
                int localIndex = fn.symbolTable.getVariableIndex(varName);
//                std::cout << "Assigned to local variable: " << varName << " (Index: " << localIndex << ")" << std::endl;
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
//            std::cout << "Assigned to array: " << arrayName << std::endl;
        }
        else {
            throw std::runtime_error("Assignment to complex LHS not implemented.");
        }
    }

    // ------------------------------------------------------------
    // 10) Генерация выражений
    // ------------------------------------------------------------
    void generateLiteral(const ASTNode &literalNode, BytecodeFunctionMy &fn) {
        // Поддержка целых чисел и булевых литералов
        long long val;
        if (literalNode.value == "true") {
            val = 1;
        }
        else if (literalNode.value == "false") {
            val = 0;
        }
        else {
            try {
                val = std::stoll(literalNode.value);
            }
            catch (const std::invalid_argument&) {
                throw std::runtime_error("Invalid literal value: " + literalNode.value);
            }
            catch (const std::out_of_range&) {
                throw std::runtime_error("Literal value out of range: " + literalNode.value);
            }
        }
        fn.instructions.emplace_back(OpCode::LOAD_CONST, val);
//        std::cout << "Generated LOAD_CONST " << val << std::endl;
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

            case ASTNodeType::FunctionDecl:
                generateFunctionCall(exprNode, globalSymbolTable, fn);
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
//            std::cout << "Generated LOAD_GLOBAL " << varName << std::endl;
        }
        else {
            // Локальная переменная
            int localIndex = fn.symbolTable.getVariableIndex(varName);
            if (localIndex == -1) {
                throw std::runtime_error("Undefined variable: " + varName);
            }
            fn.instructions.emplace_back(OpCode::LOAD_LOCAL, localIndex);
//            std::cout << "Generated LOAD_LOCAL " << varName << " (Index: " << localIndex << ")" << std::endl;
        }
    }

    // ------------------------------------------------------------
    // 11) Генерация бинарных операций
    // ------------------------------------------------------------
    void generateBinaryOp(const ASTNode &binOp, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        const std::string &op = binOp.value;

        if (op == "call") {
            // Обработка вызова функции
            if (binOp.children.empty()) {
                throw std::runtime_error("Call operation has no children.");
            }

            // Первый ребенок — это имя функции
            std::string funcName = binOp.children[0].value;

            // Проверяем, существует ли функция
            if (!globalSymbolTable.hasFunction(funcName)) {
                throw std::runtime_error("Undefined function: " + funcName);
            }

            // Генерируем инструкции для аргументов (начиная со второго ребенка)
            for (size_t i = 1; i < binOp.children.size(); ++i) {
                generateExpression(binOp.children[i], globalSymbolTable, fn);
            }

            // Генерируем инструкцию CALL с именем функции
            fn.instructions.emplace_back(OpCode::CALL, funcName);
//            std::cout << "Generated CALL " << funcName << std::endl;

            return; // Завершаем обработку операции call
        }

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

//                std::cout << "Assignment to variable: " << varName
//                          << ", isGlobal: " << isGlobal
//                          << ", isLocal: " << isLocal << std::endl;

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
//                    std::cout << "Assigned to global variable: " << varName << std::endl;
                }
                else if (isLocal) {
                    int constVal;
                    if (fn.symbolTable.hasConstant(varName, constVal)) {
                        throw std::runtime_error("Cannot modify constant variable: " + varName);
                    }
                    // Присваиваем значение локальной переменной
                    int localIndex = fn.symbolTable.getVariableIndex(varName);
//                    std::cout << "Assigned to local variable: " << varName << " (Index: " << localIndex << ")" << std::endl;
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
//                std::cout << "Assigned to array: " << arrayName << std::endl;
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
//            std::cout << "Generated ADD" << std::endl;
        }
        else if (op == "-") {
            fn.instructions.emplace_back(OpCode::SUB);
//            std::cout << "Generated SUB" << std::endl;
        }
        else if (op == "*") {
            fn.instructions.emplace_back(OpCode::MUL);
//            std::cout << "Generated MUL" << std::endl;
        }
        else if (op == "/") {
            fn.instructions.emplace_back(OpCode::DIV);
//            std::cout << "Generated DIV" << std::endl;
        }
        else if (op == "%") {
            fn.instructions.emplace_back(OpCode::MOD);
//            std::cout << "Generated MOD" << std::endl;
        }
        else if (op == "<") {
            fn.instructions.emplace_back(OpCode::CMP_LT);
//            std::cout << "Generated CMP_LT" << std::endl;
        }
        else if (op == "<=") {
            fn.instructions.emplace_back(OpCode::CMP_LE);
//            std::cout << "Generated CMP_LE" << std::endl;
        }
        else if (op == ">") {
            fn.instructions.emplace_back(OpCode::CMP_GT);
//            std::cout << "Generated CMP_GT" << std::endl;
        }
        else if (op == ">=") {
            fn.instructions.emplace_back(OpCode::CMP_GE);
//            std::cout << "Generated CMP_GE" << std::endl;
        }
        else if (op == "==") {
            fn.instructions.emplace_back(OpCode::CMP_EQ);
//            std::cout << "Generated CMP_EQ" << std::endl;
        }
        else if (op == "!=") {
            fn.instructions.emplace_back(OpCode::CMP_NE);
//            std::cout << "Generated CMP_NE" << std::endl;
        }
        else if (op == "&&") {
            fn.instructions.emplace_back(OpCode::AND);
//            std::cout << "Generated AND" << std::endl;
        }
        else if (op == "||") {
            fn.instructions.emplace_back(OpCode::OR);
//            std::cout << "Generated OR" << std::endl;
        }
        else if (op == "[]") {
            // Доступ к элементу массива: array[i]
            // Генерим имя массива и индекс
            std::string arrayName = binOp.children[0].value;
            generateExpression(binOp.children[1], globalSymbolTable, fn);
            fn.instructions.emplace_back(OpCode::LOAD_ARRAY, arrayName);
//            std::cout << "Generated LOAD_ARRAY " << arrayName << std::endl;
        }
        else {
            std::cerr << "Unknown BinaryOp: " << op << std::endl;
        }
    }

    // ------------------------------------------------------------
    // 12) Генерация вызова функции
    // ------------------------------------------------------------
    void generateFunctionCall(const ASTNode &funcCallNode, InterpreterSymbolTable &globalSymbolTable, BytecodeFunctionMy &fn) {
        // funcCallNode.value = имя функции
        // funcCallNode.children = аргументы

        std::string funcName = funcCallNode.value;

        // Проверяем, существует ли функция
        if (!globalSymbolTable.hasFunction(funcName)) {
            throw std::runtime_error("Undefined function: " + funcName);
        }

        // Генерируем инструкции для аргументов (в порядке слева направо)
        for (auto &arg : funcCallNode.children) {
            generateExpression(arg, globalSymbolTable, fn);
        }

        // Генерируем инструкцию CALL
        fn.instructions.emplace_back(OpCode::CALL, funcName);
//        std::cout << "Generated CALL " << funcName << std::endl;
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
//                int val;
//                if (symbolTable.hasConstant(exprNode.value, val)) {
//                    return val;
//                }
//                throw std::runtime_error("Array size identifier is not a constant: " + exprNode.value);
                return 10;
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

    /**
     * Вспомогательная функция для преобразования типа ASTNodeType в строку (для отладки)
     */
    std::string astNodeTypeToString(ASTNodeType type) const {
        switch (type) {
            case ASTNodeType::Program: return "Program";
            case ASTNodeType::VarDecl: return "VarDecl";
            case ASTNodeType::FunctionDecl: return "FunctionDecl";
            case ASTNodeType::Parameter: return "Parameter";
            case ASTNodeType::Block: return "Block";
            case ASTNodeType::ReturnStmt: return "ReturnStmt";
            case ASTNodeType::IfStmt: return "IfStmt";
            case ASTNodeType::WhileStmt: return "WhileStmt";
            case ASTNodeType::ForStmt: return "ForStmt";
            case ASTNodeType::Assignment: return "Assignment";
            case ASTNodeType::Expression: return "Expression";
            case ASTNodeType::BinaryOp: return "BinaryOp";
            case ASTNodeType::Identifier: return "Identifier";
            case ASTNodeType::Literal: return "Literal";
            default: return "Unknown";
        }
    }
};
