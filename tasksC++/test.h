//#include <iostream>
//#include "Lexer/Lexer.h"
//#include "Parser_AST/AST.h"
//#include "SemanticAnalyzer/SemanticAnalyzer.h"
//#include "LLVM/CodeGenerator.h"
//#include "Interpreter/InstructionMy.h"
//#include "Interpreter/BytecodeGenerator.h"
//#include "VM.h"
//#include <llvm/Support/raw_ostream.h>
//
//void printAST(const ASTNode &node, int indent=0) {
//    std::string prefix(indent, ' ');
//
//    // Печатаем сам узел
//    std::cout << prefix
//              << astNodeTypeToString(node.type)
//              << " (value='" << node.value << "')\n";
//
//    // Рекурсивно печатаем детей
//    for (auto &child : node.children) {
//        printAST(child, indent + 2);
//    }
//}
//
//int main() {
//    std::string code = R"(
//bool is_prime[10000];
//integer primes[10000];
//
//integer main(){
//    integer n = 9999;
//    for (integer i = 0; i < n; i = i + 1) {
//        is_prime[i] = true;
//    }
//
//    is_prime[0] = false;
//    is_prime[1] = false;
//
//    for (integer i = 2; i * i < n; i = i + 1) {
//        if (is_prime[i] == true) {
//            for (integer j = i * i; j < n; j = j + i) {
//                is_prime[j] = false;
//            }
//        }
//    }
//
//    integer prime_count = 0;
//    for (integer i = 2; i <= n; i = i + 1) {
//        if (is_prime[i]) {
//            primes[prime_count] = i;
//            prime_count = prime_count + 1;
//        }
//    }
//    integer fin = 1-2;
//    primes[prime_count] = fin;
//
//    integer i = 0;
//    while (primes[i] != fin){
//        print(primes[i]);
//        i=i+1;
//    }
//    return 0;
//}
//)";
//
//    // 1. Лексер
//    Lexer lexer(code);
//    auto tokens = lexer.getTokens();
//
//    // 2. Вывод токенов
//    std::cout << "=== TOKENS ===\n";
//    for (auto &t : tokens) {
//        std::cout << "[" << t.name << "] "
//                  << "Type: " << typeOfVarToString(t.type)
//                  << " (line=" << t.line << ", col=" << t.col << ")"
//                  << std::endl;
//    }
//
//    // 3. Парсер
//    Parser parser(tokens);
//    ASTNode root;
//    try {
//        root = parser.parseProgram();
//
//        // 4. Выводим AST
//        std::cout << "\n=== AST ===\n";
//        printAST(root, 0);
//
//    } catch (const std::exception &ex) {
//        std::cerr << "Parse error: " << ex.what() << std::endl;
//        return 1;
//    }
//
//    // 5. Семантический анализ
//    SemanticAnalyzer analyzer(root);
//    try {
//        analyzer.analyze();
//        std::cout << "\n=== Semantic Analysis Passed ===\n";
//    } catch (const std::exception &ex) {
//        std::cerr << "Semantic error: " << ex.what() << std::endl;
//        return 1;
//    }
//
////    // 6. Генерация кода с помощью LLVM
////    try {
////        CodeGenerator codeGen(analyzer.getSymbolTable());
////        std::unique_ptr<llvm::Module> module = codeGen.generate(root);
////        std::cout << "\n=== Generated LLVM IR ===\n";
////        module->print(llvm::outs(), nullptr);
////    } catch (const std::exception &ex) {
////        std::cerr << "LLVM generation error: " << ex.what() << std::endl;
////    }
//
//
//    // 7. Генерация собственным интерпретатором
//    try {
//        MyBytecodeGenerator generator;
//        BytecodeProgramMy program = generator.generate(root);
//        std::cout << "\n=== BYTECODE PROGRAM ===\n";
//
//        for (const auto &fn: program.functions) {
//            std::cout << "Function " << fn.name << ":\n";
//            for (size_t i = 0; i < fn.instructions.size(); i++) {
//                const auto &ins = fn.instructions[i];
//                std::cout << "  " << i << ": " << opcodeToString(ins.opcode)
//                          << " int=" << ins.operandInt
//                          << " str='" << ins.operandStr << "'\n";
//            }
//        }
//
//        // Создание и запуск VM
//        VirtualMachine vm(program);
//        std::cout << "\n=== VM EXECUTION ===\n";
//        vm.execute();
//
//        // Вывод последнего возвращённого значения
//        std::cout << "Program returned: " << vm.lastReturnValue << std::endl;
//
//    } catch (const std::exception &ex){
//        std::cerr << "Bytecode/VM program error: " << ex.what() << std::endl;
//        return 1;
//    }
//
//    return 0;
//}

