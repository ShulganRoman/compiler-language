//
// Created by Тихонов Александр on 18.01.2025.
//

#pragma once

#include <unordered_map>
#include <string>
#include <vector>
#include <stdexcept>

/**
 * Типы переменных
 */
enum class VarType {
    INTEGER,
    FLOAT,
    BOOL,
    STRING,
    VOID,
    ARRAY
};

/**
 * Класс для описания переменной
 */
class Variable {
public:
    std::string name;
    VarType type;
    bool isConstant;
    int ConstantValue; // Только для констант
    int arraySize; // Только для массивов, -1 если не массив

    // Конструктор для обычной переменной
    Variable(VarType type, const std::string &name)
            : type(type), name(name), isConstant(false), ConstantValue(0), arraySize(-1) {}

    // Конструктор для массива
    Variable(VarType type, const std::string &name, VarType varType, int arraySize)
            : type(type), name(name), isConstant(false), ConstantValue(0), arraySize(arraySize) {}

    // Метод для установки переменной как константы
    void setConstant(int val) {
        isConstant = true;
        ConstantValue = val;
    }
};

/**
 * Символическая таблица
 */
class InterpreterSymbolTable {
public:
    // Карта имени переменной к её индексу
    std::unordered_map<std::string, int> symbols;
    // Список переменных
    std::vector<Variable> variables;

    // Добавить переменную, вернуть её индекс
    int addVariable(const Variable &var) {
        if (symbols.find(var.name) != symbols.end()) {
            throw std::runtime_error("Duplicate variable name: " + var.name);
        }
        int index = variables.size();
        variables.push_back(var);
        symbols[var.name] = index;
        return index;
    }

    // Получить индекс переменной по имени
    int getVariableIndex(const std::string &name) const {
        auto it = symbols.find(name);
        if (it == symbols.end()) {
            return -1;
        }
        return it->second;
    }

    // Проверить наличие переменной
    bool hasVariable(const std::string &name) const {
        return symbols.find(name) != symbols.end();
    }
    /**
     * Проверить, является ли переменная константой и получить её значение.
     * @param name Имя переменной.
     * @param val Ссылка для записи значения переменной, если она константа.
     * @return true, если переменная существует и является константой, иначе false.
     */
    bool hasConstant(const std::string &name, int &val) const {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            const Variable &var = variables[it->second];
            if (var.isConstant) {
                val = var.ConstantValue;
                return true;
            }
        }
        return false;
    }
};