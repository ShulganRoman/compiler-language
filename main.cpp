#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include "Lexer/Lexer.h"
#include "Parser_AST/AST.h"
#include "SemanticAnalyzer/SemanticAnalyzer.h"
#include "LLVM/CodeGenerator.h"
#include "Interpreter/InstructionMy.h"
#include "Interpreter/BytecodeGenerator.h"
#include "VM.h"
#include "Parser_AST/AST_Optimizer.h"
#include <llvm/Support/raw_ostream.h>
#include <memory>

// Функция для красивой печати AST
void printAST(const ASTNode &node, int indent=0) {
    std::string prefix(indent, ' ');

    std::cout << prefix
              << astNodeTypeToString(node.type)
              << " (value='" << node.value << "')\n";

    for (auto &child : node.children) {
        printAST(child, indent + 2);
    }
}

int main(int argc, char** argv) {
    // -----------------------------------------------------------------
    // 1) Разбор аргументов командной строки
    // -----------------------------------------------------------------

    if (argc < 2) {
        std::cerr << "Usage: " << argv[0]
                  << " input.lcpp [--run] [--tokens] [--ast] [--llvm] [--O2] [-o out.bc]\n";
        return 1;
    }

    std::string inputFile;
    std::string outputFile;
    bool doRun   = false;
    bool showAst = false;
    bool showTokens = false;
    bool showLLVM = false;
    bool optimizeO2 = false;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];

        if (arg == "--run") {
            doRun = true;
        }
        else if (arg == "--ast") {
            showAst = true;
        }
        else if (arg == "--tokens") {
            showTokens = true;
        }
        else if (arg == "--llvm") {
            showLLVM = true;
        }
        else if (arg == "--O2") {
            optimizeO2 = true;
        }
        else if (arg == "-o") {
            if (i+1 < argc) {
                outputFile = argv[++i]; // берём следующий аргумент как имя файла
            } else {
                std::cerr << "Error: -o requires filename\n";
                return 1;
            }
        }
        else {
            // Считаем, что это inputFile
            if (inputFile.empty()) {
                inputFile = arg;
            } else {
                std::cerr << "Warning: multiple input files specified: "
                          << arg << "\n";
            }
        }
    }

    if (inputFile.empty()) {
        std::cerr << "Error: no input file specified!\n";
        return 1;
    }

    // -----------------------------------------------------------------
    // 2) Читаем исходный файл .lcpp в строку "code"
    // -----------------------------------------------------------------
    std::ifstream ifs(inputFile);
    if (!ifs.is_open()) {
        std::cerr << "Error: cannot open file " << inputFile << "\n";
        return 1;
    }
    std::string code((std::istreambuf_iterator<char>(ifs)),
                     std::istreambuf_iterator<char>());

    // -----------------------------------------------------------------
    // 3) Лексер
    // -----------------------------------------------------------------
    Lexer lexer(code);
    auto tokens = lexer.getTokens();

    if (showTokens) {
        std::cout << "=== TOKENS ===\n";
        for (auto &t : tokens) {
            std::cout << "[" << t.name << "] "
                      << "Type: " << typeOfVarToString(t.type)
                      << " (line=" << t.line << ", col=" << t.col << ")"
                      << std::endl;
        }
    }

    // -----------------------------------------------------------------
    // 4) Парсер
    // -----------------------------------------------------------------
    Parser parser(tokens);
    ASTNode root;
    try {
        root = parser.parseProgram();
    }
    catch (const std::exception &ex) {
        std::cerr << "Parse error: " << ex.what() << std::endl;
        return 1;
    }

    if (showAst) {
        std::cout << "\n=== AST ===\n";
        printAST(root, 0);
    }

    // -----------------------------------------------------------------
    // 5) Семантический анализ
    // -----------------------------------------------------------------
    SemanticAnalyzer analyzer(root);
    try {
        analyzer.analyze();
        std::cout << "\n=== Semantic Analysis Passed ===\n";
    } catch (const std::exception &ex) {
        std::cerr << "Semantic error: " << ex.what() << std::endl;
        return 1;
    }

    // -----------------------------------------------------------------
    // 6) Оптимизации на AST
    // -----------------------------------------------------------------
    // Если хотите implement optimizeO2 = true → constant folding, etc.
    if (optimizeO2) {
        SemanticAnalyzer analyzer(root);
        analyzer.analyze();

        // 6. Запускаем оптимизации
        ASTOptimizer optimizer;
        optimizer.optimize(root);
    }

    // -----------------------------------------------------------------
    // 7) Генерация LLVM IR
    // -----------------------------------------------------------------

//    if (showLLVM) {
//        try {
//            CodeGenerator codeGen(analyzer.getSymbolTable());
//            std::unique_ptr<llvm::Module> module = codeGen.generate(root);
//            std::cout << "\n=== Generated LLVM IR ===\n";
//            module->print(llvm::outs(), nullptr);
//        } catch (const std::exception &ex) {
//            std::cerr << "LLVM generation error: " << ex.what() << std::endl;
//        }
//    }


    // -----------------------------------------------------------------
    // 8) Генерация байткода
    // -----------------------------------------------------------------
    MyBytecodeGenerator generator;
    BytecodeProgramMy program;
    try {
        program = generator.generate(root);
    }
    catch (const std::exception &ex) {
        std::cerr << "Bytecode generation error: " << ex.what() << std::endl;
        return 1;
    }

    // Если хотим куда-то сохранить байткод: -o out.txt
    if (!outputFile.empty()) {
        std::ofstream ofs(outputFile);
        if (!ofs.is_open()) {
            std::cerr << "Error: cannot open output file: " << outputFile << "\n";
            return 1;
        }
        // Пишем туда инструкции всех функций
        for (const auto &fn : program.functions) {
            ofs << "Function " << fn.name << ":\n";
            for (size_t i = 0; i < fn.instructions.size(); i++) {
                const auto &ins = fn.instructions[i];
                ofs << "  " << i << ": "
                    << opcodeToString(ins.opcode)
                    << " int=" << ins.operandInt
                    << " str='" << ins.operandStr << "'\n";
            }
        }
        ofs.close();
        std::cout << "Bytecode saved to " << outputFile << "\n";
    }

    // -----------------------------------------------------------------
    // 9) Если флаг --run, то запускаем нашу VM
    // -----------------------------------------------------------------
    if (doRun) {
        std::cout << "\n=== VM EXECUTION ===\n";
        VirtualMachine vm(program);
        try {
            vm.execute();
            std::cout << "Program returned: "
                      << vm.lastReturnValue << std::endl;
        } catch (const std::exception &ex){
            std::cerr << "Runtime error: " << ex.what() << std::endl;
            return 1;
        }
    }

    return 0;
}
