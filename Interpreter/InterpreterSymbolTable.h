#pragma once

#include <unordered_map>
#include <vector>
#include <string>
#include <stdexcept>
#include <iostream>
#include <stack>

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
 * Символическая таблица с поддержкой областей видимости
 */
class InterpreterSymbolTable {
public:
    InterpreterSymbolTable() {
        // Инициализируем глобальную область видимости
        enterScope();
    }

    // Вход в новую область видимости
    void enterScope() {
        scopes.emplace_back();
//        std::cout << "Entered new scope. Total scopes: " << scopes.size() << std::endl;
    }

    // Выход из текущей области видимости
    void exitScope() {
        if (scopes.empty()) {
            throw std::runtime_error("No scope to exit.");
        }
        scopes.pop_back();
        std::cout << "Exited scope. Total scopes: " << scopes.size() << std::endl;
    }

    // Добавляет переменную и возвращает её индекс
    int addVariable(const Variable &var) {
        if (scopes.empty()) {
            throw std::runtime_error("No scope available to add variable.");
        }
        auto &currentScope = scopes.back();
        if (currentScope.find(var.name) != currentScope.end()) {
            throw std::runtime_error("Duplicate variable name: " + var.name);
        }
        int index = variables.size();
        variables.push_back(var);
        currentScope[var.name] = index;
        std::cout << "Added variable: " << var.name << " (Index: " << index << ")" << std::endl;
        return index;
    }

    // Добавляет функцию и возвращает её индекс
    int addFunction(const std::string &funcName) {
        if (functions.find(funcName) != functions.end()) {
            throw std::runtime_error("Duplicate function name: " + funcName);
        }
        int index = functionList.size();
        functionList.push_back(funcName);
        functions[funcName] = index;
        std::cout << "Added function: " << funcName << " (Index: " << index << ")" << std::endl;
        return index;
    }

    // Проверяет наличие переменной (по всем областям видимости)
    bool hasVariable(const std::string &varName) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            if (it->find(varName) != it->end()) {
                return true;
            }
        }
        return false;
    }

    int getScopeDepth() const {
        return scopes.size();
    }

    // Получает индекс переменной, если существует
    int getVariableIndex(const std::string &varName) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(varName);
            if (found != it->end()) {
                return found->second;
            }
        }
        return -1;
    }

    // Проверяет наличие функции
    bool hasFunction(const std::string &funcName) const {
        return functions.find(funcName) != functions.end();
    }

    // Возвращает индекс функции или -1, если не найдена
    int getFunctionIndex(const std::string &funcName) const {
        auto it = functions.find(funcName);
        if (it != functions.end()) {
            return it->second;
        }
        return -1;
    }

    // Проверяет, является ли переменная константой
    bool hasConstant(const std::string &varName, int &value) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(varName);
            if (found != it->end()) {
                const Variable &var = variables[found->second];
                if (var.isConstant) {
                    value = var.ConstantValue;
                    return true;
                }
            }
        }
        return false;
    }

    std::vector<Variable> variables;

private:
    // Стек областей видимости: каждая область видимости — это map <имя переменной, индекс в variables>
    std::vector<std::unordered_map<std::string, int>> scopes;

    // Переменные
    std::unordered_map<std::string, int> functions; // Имя функции -> индекс
    std::vector<std::string> functionList; // Список функций
};
