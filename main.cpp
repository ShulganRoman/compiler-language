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
    static const int MAX_SIZE = 10000;
    static bool is_prime[MAX_SIZE];
    bool test2 = false;
    is_prime[1]=test2;


    std::string code = R"(
integer factorial_calc(integer num){
    if (num == 0){
        return 1;
    } else { return num * factorial_calc(num - 1);}
}

integer main(){
    integer x=43;
    print(x);
    return factorial_calc(6);
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
    // 7. Генерация собственным интерпретатором и выполнение байткода
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
        std::cerr << "Bytecode program error: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}