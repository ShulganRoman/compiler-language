#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>
#include <stdexcept>
#include <iostream>

enum class OpCode : uint8_t {
    // Работа с памятью
    LOAD_GLOBAL,     // Загрузить значение глобальной переменной в стек (operandStr = имя)
    STORE_GLOBAL,    // Сохранить значение из вершины стека в глобальную переменную
    LOAD_LOCAL,      // Загрузить из локальной переменной по индексу
    STORE_LOCAL,     // Сохранить в локальную переменную по индексу
    LOAD_CONST,      // Загрузить константу в стек

    // Арифметика
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,

    // Сравнения
    CMP_EQ,
    CMP_NE,
    CMP_LT,
    CMP_GT,
    CMP_LE,
    CMP_GE,

    // Логические
    AND,
    OR,
    NOT,

    // Работа с массивами
    LOAD_ARRAY,      // Загрузить элемент массива (operandStr = имя массива)
    STORE_ARRAY,     // Сохранить элемент в массив (operandStr = имя массива)

    // Управление потоком
    JMP,            // Безусловный переход (operandInt = индекс инструкции)
    JMP_IF_FALSE,   // Переход, если вершина стека == false
    CALL,           // Вызов функции (operandStr = имя)
    RET,            // Возврат из функции
    HALT            // Конец программы
};

struct InstructionMy {
    OpCode opcode {OpCode::LOAD_CONST};
    int64_t operandInt {0};        // Для индексов, значений
    std::string operandStr;        // Для имен переменных, функций и т.п.

    InstructionMy() = default;
    InstructionMy(OpCode op) : opcode(op) {}
    InstructionMy(OpCode op, int64_t i) : opcode(op), operandInt(i) {}
    InstructionMy(OpCode op, const std::string &s) : opcode(op), operandStr(s) {}
    InstructionMy(OpCode op, int64_t i, const std::string &s)
            : opcode(op), operandInt(i), operandStr(s) {}
};

std::string opcodeToString(OpCode opcode) {
    static const std::unordered_map<OpCode, std::string> opcodeNames = {
            // Работа с памятью
            {OpCode::LOAD_GLOBAL, "LOAD_GLOBAL"},
            {OpCode::STORE_GLOBAL, "STORE_GLOBAL"},
            {OpCode::LOAD_LOCAL, "LOAD_LOCAL"},
            {OpCode::STORE_LOCAL, "STORE_LOCAL"},
            {OpCode::LOAD_CONST, "LOAD_CONST"},

            // Арифметика
            {OpCode::ADD, "ADD"},
            {OpCode::SUB, "SUB"},
            {OpCode::MUL, "MUL"},
            {OpCode::DIV, "DIV"},
            {OpCode::MOD, "MOD"},

            // Сравнения
            {OpCode::CMP_EQ, "CMP_EQ"},
            {OpCode::CMP_NE, "CMP_NE"},
            {OpCode::CMP_LT, "CMP_LT"},
            {OpCode::CMP_GT, "CMP_GT"},
            {OpCode::CMP_LE, "CMP_LE"},
            {OpCode::CMP_GE, "CMP_GE"},

            // Логические
            {OpCode::AND, "AND"},
            {OpCode::OR, "OR"},
            {OpCode::NOT, "NOT"},

            // Работа с массивами
            {OpCode::LOAD_ARRAY, "LOAD_ARRAY"},
            {OpCode::STORE_ARRAY, "STORE_ARRAY"},

            // Управление потоком
            {OpCode::JMP, "JMP"},
            {OpCode::JMP_IF_FALSE, "JMP_IF_FALSE"},
            {OpCode::CALL, "CALL"},
            {OpCode::RET, "RET"},
            {OpCode::HALT, "HALT"},
    };

    auto it = opcodeNames.find(opcode);
    if (it != opcodeNames.end()) {
        return it->second;
    }
    return "UNKNOWN";
}