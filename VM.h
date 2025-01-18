// VirtualMachine.h

#pragma once

#include "Interpreter/BytecodeGenerator.h"

#include <vector>
#include <unordered_map>
#include <stack>
#include <cstdint>
#include <iostream>

class VirtualMachine {
public:
    VirtualMachine(const BytecodeProgramMy &prog)
            : program(prog), globals(prog.nextGlobalVarIndex, 0) {
        initializeArrays();
    }

    // Выполнить функцию по имени
    int64_t executeFunction(const std::string &funcName) {
        int funcIndex = program.getFunctionIndex(funcName);
        if (funcIndex == -1) {
            throw std::runtime_error("Function not found: " + funcName);
        }

        const BytecodeFunctionMy &func = program.functions[funcIndex];
        std::vector<int64_t> locals(func.numLocals, 0);

        std::stack<int64_t> vmStack;
        size_t ip = 0; // Instruction Pointer

        while (ip < func.instructions.size()) {
            const InstructionMy &instr = func.instructions[ip];
            switch (instr.opcode) {
                case OpCode::LOAD_CONST:
                    vmStack.push(instr.operandInt);
                    break;

                case OpCode::STORE_GLOBAL:
                    if (instr.operandInt < 0 || instr.operandInt >= globals.size()) {
                        throw std::runtime_error("Invalid global index.");
                    }
                    globals[instr.operandInt] = vmStack.top();
                    vmStack.pop();
                    break;

                case OpCode::LOAD_GLOBAL:
                    if (instr.operandInt < 0 || instr.operandInt >= globals.size()) {
                        throw std::runtime_error("Invalid global index.");
                    }
                    vmStack.push(globals[instr.operandInt]);
                    break;

                case OpCode::STORE_LOCAL:
                    if (instr.operandInt < 0 || instr.operandInt >= locals.size()) {
                        throw std::runtime_error("Invalid local index.");
                    }
                    locals[instr.operandInt] = vmStack.top();
                    vmStack.pop();
                    break;

                case OpCode::LOAD_LOCAL:
                    if (instr.operandInt < 0 || instr.operandInt >= locals.size()) {
                        throw std::runtime_error("Invalid local index.");
                    }
                    vmStack.push(locals[instr.operandInt]);
                    break;

                case OpCode::ADD: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on ADD.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a + b);
                    break;
                }

                case OpCode::SUB: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on SUB.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a - b);
                    break;
                }

                case OpCode::MUL: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on MUL.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a * b);
                    break;
                }

                case OpCode::DIV: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on DIV.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    if (b == 0) throw std::runtime_error("Division by zero.");
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a / b);
                    break;
                }

                case OpCode::CMP_EQ: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on CMP_EQ.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a == b ? 1 : 0);
                    break;
                }

                case OpCode::CMP_NE: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on CMP_NE.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a != b ? 1 : 0);
                    break;
                }

                case OpCode::CMP_LT: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on CMP_LT.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a < b ? 1 : 0);
                    break;
                }

                case OpCode::CMP_GT: {
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on CMP_GT.");
                    int64_t b = vmStack.top(); vmStack.pop();
                    int64_t a = vmStack.top(); vmStack.pop();
                    vmStack.push(a > b ? 1 : 0);
                    break;
                }

                case OpCode::JMP_IF_FALSE: {
                    if (vmStack.empty()) throw std::runtime_error("Stack underflow on JMP_IF_FALSE.");
                    int64_t condition = vmStack.top(); vmStack.pop();
                    if (condition == 0) {
                        ip = static_cast<size_t>(instr.operandInt);
                        continue; // Пропускаем инкремент ip
                    }
                    break;
                }

                case OpCode::JMP: {
                    ip = static_cast<size_t>(instr.operandInt);
                    continue; // Пропускаем инкремент ip
                }

                case OpCode::RET: {
                    if (vmStack.empty()) return 0; // Если нет значения, возвращаем 0
                    return vmStack.top();
                }

                case OpCode::HALT: {
                    return 0; // Завершаем выполнение
                }

                case OpCode::LOAD_ARRAY: {
                    // operandInt = индекс массива в program.globalArrays
                    if (vmStack.empty()) throw std::runtime_error("Stack underflow on LOAD_ARRAY.");
                    int64_t index = vmStack.top(); vmStack.pop();
                    if (index < 0 || static_cast<size_t>(index) >= arrays[instr.operandInt].size()) {
                        throw std::runtime_error("Array index out of bounds.");
                    }
                    vmStack.push(arrays[instr.operandInt][index]);
                    break;
                }

                case OpCode::STORE_ARRAY: {
                    // operandInt = индекс массива в program.globalArrays
                    if (vmStack.size() < 2) throw std::runtime_error("Stack underflow on STORE_ARRAY.");
                    int64_t value = vmStack.top(); vmStack.pop();
                    int64_t index = vmStack.top(); vmStack.pop();
                    if (index < 0 || static_cast<size_t>(index) >= arrays[instr.operandInt].size()) {
                        throw std::runtime_error("Array index out of bounds.");
                    }
                    arrays[instr.operandInt][index] = value;
                    break;
                }

                default:
                    throw std::runtime_error("Unknown OpCode encountered.");
            }
            ip++;
        }

        return 0;
    }

private:
    const BytecodeProgramMy &program;
    std::vector<int64_t> globals; // Объявление globals
    std::vector<std::vector<int64_t>> arrays;

    // Инициализация массивов
    void initializeArrays() {
        arrays.resize(program.nextGlobalArrayIndex, std::vector<int64_t>());
        for (const auto &pair: program.globalArrays) {
            int index = pair.second;
            // Здесь можно хранить размеры массивов, например, в отдельной таблице
            // Для примера инициализируем массив размером 10000
            arrays[index].resize(10000, 0);
        }
    }
};
