// VirtualMachine.h
#pragma once

#include "Interpreter/InstructionMy.h"
#include "Interpreter/BytecodeProgramMy.h"
#include "Interpreter/InterpreterSymbolTable.h"

#include <stack>
#include <unordered_map>
#include <vector>
#include <string>
#include <stdexcept>
#include <iostream>

struct CallFrame {
    const BytecodeFunctionMy* function; // Указатель на текущую функцию
    size_t ip;                          // Указатель на текущую инструкцию
    std::vector<int> locals;            // Локальные переменные

    CallFrame(const BytecodeFunctionMy* func)
            : function(func), ip(0), locals(func->symbolTable.variables.size(), 0) {}
};

class VirtualMachine {
public:
    int lastReturnValue;
    VirtualMachine(const BytecodeProgramMy& program)
            : program(program), halted(false) {}

    void execute() {
        // Инициализация фрейма вызова для функции main
        const BytecodeFunctionMy* mainFn = program.getFunction("main");
        if (!mainFn) {
            throw std::runtime_error("Main function not found.");
        }

        // Инициализируем фрейм вызова для main
        CallFrame mainFrame(mainFn);
        callStack.push(mainFrame);

        while (!callStack.empty() && !halted) {
            CallFrame& currentFrame = callStack.top();

            if (currentFrame.ip >= currentFrame.function->instructions.size()) {
                // Если достигнут конец функции без RET, автоматически возвращаемся
                callStack.pop();
                continue;
            }

            InstructionMy instr = currentFrame.function->instructions[currentFrame.ip];
            currentFrame.ip++;

            try {
                executeInstruction(instr, currentFrame);
            }
            catch (const std::exception& e) {
                std::cerr << "Runtime Error: " << e.what() << std::endl;
                halted = true;
            }
        }
        printStack();
        printGlobalVariables();
    }

private:
    const BytecodeProgramMy& program;
    std::stack<CallFrame> callStack;
    std::stack<int> operandStack;
    std::unordered_map<std::string, int> globalVariables;
    bool halted;

    void executeInstruction(const InstructionMy& instr, CallFrame& frame) {
        // Отладочный вывод текущей инструкции
        std::cout << "Executing " << opcodeToString(instr.opcode);
        if (!instr.operandStr.empty()) {
            std::cout << " " << instr.operandStr;
        }
        if (instr.operandInt != 0) {
            std::cout << " " << instr.operandInt;
        }
        std::cout << std::endl;
        switch (instr.opcode) {
            // Работа с памятью
            case OpCode::LOAD_GLOBAL:
                loadGlobal(instr.operandStr);
                break;
            case OpCode::STORE_GLOBAL:
                storeGlobal(instr.operandStr);
                break;
            case OpCode::LOAD_LOCAL:
                loadLocal(instr.operandInt, frame);
                break;
            case OpCode::STORE_LOCAL:
                storeLocal(instr.operandInt, frame);
                break;
            case OpCode::LOAD_CONST:
                loadConst(instr.operandInt);
                break;

                // Арифметика
            case OpCode::ADD:
                add();
                break;
            case OpCode::SUB:
                sub();
                break;
            case OpCode::MUL:
                mul();
                break;
            case OpCode::DIV:
                divInstr();
                break;
            case OpCode::MOD:
                mod();
                break;

                // Сравнения
            case OpCode::CMP_EQ:
                cmp_eq();
                break;
            case OpCode::CMP_NE:
                cmp_ne();
                break;
            case OpCode::CMP_LT:
                cmp_lt();
                break;
            case OpCode::CMP_GT:
                cmp_gt();
                break;
            case OpCode::CMP_LE:
                cmp_le();
                break;
            case OpCode::CMP_GE:
                cmp_ge();
                break;

                // Логические
            case OpCode::AND:
                logical_and();
                break;
            case OpCode::OR:
                logical_or();
                break;
            case OpCode::NOT:
                logical_not();
                break;

                // Работа с массивами
            case OpCode::LOAD_ARRAY:
                loadArray(instr.operandStr);
                break;
            case OpCode::STORE_ARRAY:
                storeArray(instr.operandStr);
                break;

                // Управление потоком
            case OpCode::JMP:
                jmp(instr.operandInt, frame);
                break;
            case OpCode::JMP_IF_FALSE:
                jmp_if_false(instr.operandInt, frame);
                break;
            case OpCode::CALL:
                callFunction(instr.operandStr);
                break;
            case OpCode::RET:
                ret(frame);
                break;
            case OpCode::HALT:
                halt();
                break;

            default:
                throw std::runtime_error("Unknown opcode encountered.");
        }
    }

    void printStack() const {
        std::stack<int> tempStack = operandStack;
        std::vector<int> stackElements;
        while (!tempStack.empty()) {
            stackElements.push_back(tempStack.top());
            tempStack.pop();
        }
        std::cout << "Operand Stack (top to bottom): ";
        for (auto it = stackElements.begin(); it != stackElements.end(); ++it) {
            std::cout << *it << " ";
        }
        std::cout << std::endl;
    }

    void printGlobalVariables() const {
        std::cout << "Global Variables:" << std::endl;
        for (const auto& [name, value] : globalVariables) {
            std::cout << "  " << name << " = " << value << std::endl;
        }
    }

    // Методы обработки опкодов

    // Работа с памятью
    void loadGlobal(const std::string& varName) {
        if (globalVariables.find(varName) == globalVariables.end()) {
            throw std::runtime_error("Undefined global variable: " + varName);
        }
        operandStack.push(globalVariables[varName]);
    }

    void storeGlobal(const std::string& varName) {
        if (globalVariables.find(varName) == globalVariables.end()) {
            throw std::runtime_error("Undefined global variable: " + varName);
        }
        if (operandStack.empty()) {
            throw std::runtime_error("Operand stack underflow on STORE_GLOBAL.");
        }
        int value = operandStack.top();
        operandStack.pop();
        globalVariables[varName] = value;
    }

    void loadLocal(int varIndex, CallFrame& frame) {
        if (varIndex < 0 || varIndex >= frame.locals.size()) {
            throw std::runtime_error("Invalid local variable index: " + std::to_string(varIndex));
        }
        operandStack.push(frame.locals[varIndex]);
    }

    void storeLocal(int varIndex, CallFrame& frame) {
        if (varIndex < 0 || varIndex >= frame.locals.size()) {
            throw std::runtime_error("Invalid local variable index: " + std::to_string(varIndex));
        }
        if (operandStack.empty()) {
            throw std::runtime_error("Operand stack underflow on STORE_LOCAL.");
        }
        int value = operandStack.top();
        operandStack.pop();
        frame.locals[varIndex] = value;
    }

    void loadConst(int value) {
        operandStack.push(value);
    }

    // Арифметические операции
    void add() {
        binaryOp([](int a, int b) -> int { return a + b; }, "ADD");
    }

    void sub() {
        binaryOp([](int a, int b) -> int { return a - b; }, "SUB");
    }

    void mul() {
        binaryOp([](int a, int b) -> int { return a * b; }, "MUL");
    }

    void divInstr() {
        binaryOp([](int a, int b) -> int {
            if (b == 0) throw std::runtime_error("Division by zero.");
            return a / b;
        }, "DIV");
    }

    void mod() {
        binaryOp([](int a, int b) -> int {
            if (b == 0) throw std::runtime_error("Modulo by zero.");
            return a % b;
        }, "MOD");
    }

    // Сравнительные операции
    void cmp_eq() {
        binaryCompare([](int a, int b) -> int { return (a == b) ? 1 : 0; }, "CMP_EQ");
    }

    void cmp_ne() {
        binaryCompare([](int a, int b) -> int { return (a != b) ? 1 : 0; }, "CMP_NE");
    }

    void cmp_lt() {
        binaryCompare([](int a, int b) -> int { return (a < b) ? 1 : 0; }, "CMP_LT");
    }

    void cmp_gt() {
        binaryCompare([](int a, int b) -> int { return (a > b) ? 1 : 0; }, "CMP_GT");
    }

    void cmp_le() {
        binaryCompare([](int a, int b) -> int { return (a <= b) ? 1 : 0; }, "CMP_LE");
    }

    void cmp_ge() {
        binaryCompare([](int a, int b) -> int { return (a >= b) ? 1 : 0; }, "CMP_GE");
    }

    // Логические операции
    void logical_and() {
        binaryOp([](int a, int b) -> int { return (a && b) ? 1 : 0; }, "AND");
    }

    void logical_or() {
        binaryOp([](int a, int b) -> int { return (a || b) ? 1 : 0; }, "OR");
    }

    void logical_not() {
        if (operandStack.empty()) {
            throw std::runtime_error("Operand stack underflow on NOT.");
        }
        int a = operandStack.top();
        operandStack.pop();
        operandStack.push((!a) ? 1 : 0);
    }

    // Работа с массивами
    void loadArray(const std::string& arrayName) {
        if (globalVariables.find(arrayName) == globalVariables.end()) {
            throw std::runtime_error("Undefined array: " + arrayName);
        }
        if (operandStack.empty()) {
            throw std::runtime_error("Operand stack underflow on LOAD_ARRAY.");
        }
        int index = operandStack.top();
        operandStack.pop();

        // Здесь предполагается, что массивы хранятся как линейные списки в глобальных переменных
        // Вы можете изменить этот механизм в зависимости от вашей реализации массивов
        // Например, использовать отдельную таблицу для массивов

        // Для простоты, предполагаем, что массив хранится в глобVariables как базовый индекс
        // Необходимо расширить глобVariables для поддержки массивов
        // Ниже приведен пример простой реализации с массивами, хранящимися как std::vector<int>

        throw std::runtime_error("LOAD_ARRAY not implemented.");
    }

    void storeArray(const std::string& arrayName) {
        if (globalVariables.find(arrayName) == globalVariables.end()) {
            throw std::runtime_error("Undefined array: " + arrayName);
        }
        if (operandStack.size() < 2) {
            throw std::runtime_error("Operand stack underflow on STORE_ARRAY.");
        }
        int value = operandStack.top(); operandStack.pop();
        int index = operandStack.top(); operandStack.pop();

        // Аналогично LOAD_ARRAY, здесь необходимо реализовать хранение и доступ к массивам
        throw std::runtime_error("STORE_ARRAY not implemented.");
    }

    // Управление потоком
    void jmp(int target, CallFrame& frame) {
        if (target < 0 || target >= frame.function->instructions.size()) {
            throw std::runtime_error("Invalid JMP target: " + std::to_string(target));
        }
        frame.ip = target;
    }

    void jmp_if_false(int target, CallFrame& frame) {
        if (operandStack.empty()) {
            throw std::runtime_error("Operand stack underflow on JMP_IF_FALSE.");
        }
        int condition = operandStack.top();
        operandStack.pop();
        if (condition == 0) {
            if (target < 0 || target >= frame.function->instructions.size()) {
                throw std::runtime_error("Invalid JMP_IF_FALSE target: " + std::to_string(target));
            }
            frame.ip = target;
        }
    }

    void callFunction(const std::string& funcName) {
        const BytecodeFunctionMy* func = program.getFunction(funcName);
        if (!func) {
            throw std::runtime_error("Undefined function: " + funcName);
        }

        // Получение количества параметров
        size_t paramCount = func->symbolTable.variables.size(); // Предполагается, что все переменные в symbolTable — это параметры

        // Извлечение параметров из стека
        std::vector<int> params;
        for (size_t i = 0; i < paramCount; ++i) {
            if (operandStack.empty()) {
                throw std::runtime_error("Operand stack underflow on CALL.");
            }
            params.push_back(operandStack.top());
            operandStack.pop();
        }
        std::reverse(params.begin(), params.end()); // Параметры были добавлены в обратном порядке

        // Создание нового фрейма вызова
        CallFrame newFrame(func);
        for (size_t i = 0; i < params.size(); ++i) {
            if (i < newFrame.locals.size()) {
                newFrame.locals[i] = params[i];
            }
            else {
                throw std::runtime_error("Too many parameters provided to function: " + funcName);
            }
        }

        callStack.push(newFrame);
    }

    void ret(CallFrame& frame) {
        if (callStack.empty()) {
            throw std::runtime_error("Call stack underflow on RET.");
        }

        // Получение возвращаемого значения, если есть
        int retValue = 0;
        if (!operandStack.empty()) {
            retValue = operandStack.top();
            operandStack.pop();
        }

        callStack.pop(); // Удаление текущего фрейма

        if (callStack.empty()) {
            // Если стек вызовов пуст, сохраняем возвращённое значение
            lastReturnValue = retValue;
        }
        else {
            operandStack.push(retValue);
        }
    }


    void halt() {
        halted = true;
    }

    // Вспомогательные методы для арифметических и сравнительных операций
    void binaryOp(std::function<int(int, int)> opFunc, const std::string& opName) {
        if (operandStack.size() < 2) {
            throw std::runtime_error("Operand stack underflow on " + opName + ".");
        }
        int b = operandStack.top(); operandStack.pop();
        int a = operandStack.top(); operandStack.pop();
        int result = opFunc(a, b);
        operandStack.push(result);
    }

    void binaryCompare(std::function<int(int, int)> cmpFunc, const std::string& cmpName) {
        binaryOp(cmpFunc, cmpName);
    }
};
