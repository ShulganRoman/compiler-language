#pragma once

#include "../Interpreter/InterpreterSymbolTable.h"
#include "InstructionMy.h"
#include <vector>
#include <unordered_map>
#include <string>

struct BytecodeFunctionMy {
    std::string name;
    std::vector<InstructionMy> instructions;
    InterpreterSymbolTable symbolTable;
    int numParams {0};

    BytecodeFunctionMy() = default;
    BytecodeFunctionMy(const std::string &fname)
            : name(fname) {}
};

class BytecodeProgramMy {
public:
    InterpreterSymbolTable globalSymbolTable;
    std::vector<BytecodeFunctionMy> functions;
    std::unordered_map<std::string, int> funcIndexByName;

    // Добавить функцию
    int addFunction(const std::string &fname) {
        if (funcIndexByName.find(fname) != funcIndexByName.end()) {
            throw std::runtime_error("Duplicate function name: " + fname);
        }
        int idx = (int)functions.size();
        functions.emplace_back(fname);
        funcIndexByName[fname] = idx;
        return idx;
    }

    // Получить индекс функции по имени
    int getFunctionIndex(const std::string &fname) const {
        auto it = funcIndexByName.find(fname);
        if (it == funcIndexByName.end()) {
            return -1;
        }
        return it->second;
    }
    const BytecodeFunctionMy* getFunction(const std::string &fname) const {
        int index = getFunctionIndex(fname);
        if (index == -1) {
            return nullptr;
        }
        return &functions[index];
    }
};
