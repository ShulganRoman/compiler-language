#include <iostream>
#include "Lexer/Lexer.h"
#include "Parser_AST/AST.h"
#include "SemanticAnalyzer/SemanticAnalyzer.h"
#include "LLVM/CodeGenerator.h"
#include "Interpreter/InstructionMy.h"
#include "Interpreter/BytecodeGenerator.h"
#include "VM.h"
#include <llvm/Support/raw_ostream.h>

void printAST(const ASTNode &node, int indent=0) {
    std::string prefix(indent, ' ');

    // Печатаем сам узел
    std::cout << prefix
              << astNodeTypeToString(node.type)
              << " (value='" << node.value << "')\n";

    // Рекурсивно печатаем детей
    for (auto &child : node.children) {
        printAST(child, indent + 2);
    }
}

int main() {

    std::string code = R"(
integer global_array[10];

integer main(){
    integer num = 10;
    integer MAX_SIZE = 10;
    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        global_array[i] = num;
        num = num-1;
    }
    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        print(global_array[i]);
    }
    print(12121212121);
    for (integer i = 1; i < MAX_SIZE; i = i + 1) {
        integer key = global_array[i];
        integer j = i - 1;
        while (j > 0 && global_array[j] > key) {
            global_array[j + 1] = global_array[j];
            j = j - 1;
        }
        global_array[j + 1] = key;
    }

    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        print(global_array[i]);
    }
    return 0;
}
)";

    // 1. Лексер
    Lexer lexer(code);
    auto tokens = lexer.getTokens();

    // 2. Вывод токенов
    std::cout << "=== TOKENS ===\n";
    for (auto &t : tokens) {
        std::cout << "[" << t.name << "] "
                  << "Type: " << typeOfVarToString(t.type)
                  << " (line=" << t.line << ", col=" << t.col << ")"
                  << std::endl;
    }

    // 3. Парсер
    Parser parser(tokens);
    ASTNode root;
    try {
        root = parser.parseProgram();

        // 4. Выводим AST
        std::cout << "\n=== AST ===\n";
        printAST(root, 0);

    } catch (const std::exception &ex) {
        std::cerr << "Parse error: " << ex.what() << std::endl;
        return 1;
    }

    // 5. Семантический анализ
    SemanticAnalyzer analyzer(root);
    try {
        analyzer.analyze();
        std::cout << "\n=== Semantic Analysis Passed ===\n";
    } catch (const std::exception &ex) {
        std::cerr << "Semantic error: " << ex.what() << std::endl;
        return 1;
    }

//    // 6. Генерация кода с помощью LLVM
//    try {
//        CodeGenerator codeGen(analyzer.getSymbolTable());
//        std::unique_ptr<llvm::Module> module = codeGen.generate(root);
//        std::cout << "\n=== Generated LLVM IR ===\n";
//        module->print(llvm::outs(), nullptr);
//    } catch (const std::exception &ex) {
//        std::cerr << "LLVM generation error: " << ex.what() << std::endl;
//    }


    // 7. Генерация собственным интерпретатором
    try {
        MyBytecodeGenerator generator;
        BytecodeProgramMy program = generator.generate(root);
        std::cout << "\n=== BYTECODE PROGRAM ===\n";

        for (const auto &fn: program.functions) {
            std::cout << "Function " << fn.name << ":\n";
            for (size_t i = 0; i < fn.instructions.size(); i++) {
                const auto &ins = fn.instructions[i];
                std::cout << "  " << i << ": " << opcodeToString(ins.opcode)
                          << " int=" << ins.operandInt
                          << " str='" << ins.operandStr << "'\n";
            }
        }

        // Создание и запуск VM
        VirtualMachine vm(program);
        std::cout << "\n=== VM EXECUTION ===\n";
        vm.execute();

        // Вывод последнего возвращённого значения
        std::cout << "Program returned: " << vm.lastReturnValue << std::endl;

    } catch (const std::exception &ex){
        std::cerr << "Bytecode/VM program error: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}