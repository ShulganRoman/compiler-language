#include <iostream>
#include "Lexer.h"
#include "AST.h"
#include "SemanticAnalyzer.h"

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
const integer MAX_SIZE = 10000;
bool is_prime[MAX_SIZE];
integer primes[MAX_SIZE];

void eratosthenes_sieve(integer n) {
     for (integer i = 0; i <= n; i = i + 1) {
        is_prime[i] = true;
    }

    is_prime[0] = is_prime[1] = false;

    for (integer i = 2; i * i <= n; i = i + 1) {
        if (is_prime[i]) {
            for (integer j = i * i; j <= n; j = j + i) {
                is_prime[j] = false;
            }
        }
    }

    integer prime_count = 0;
    for (integer i = 2; i <= n; i = i + 1) {
        if (is_prime[i]) {
            primes[prime_count] = i;
            prime_count = prime_count + 1;
        }
    }

    primes[prime_count] = -1;
}
    )";

    // 1. Лексер
    Lexer lexer(code);
    auto tokens = lexer.getTokens();

    // 2. Посмотрим, что за токены он выдаёт
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
    try {
        SemanticAnalyzer analyzer(root);
        analyzer.analyze();
        std::cout << "\n=== Semantic Analysis Passed ===\n";
    } catch (const std::exception &ex) {
        std::cerr << "Semantic error: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}


//#include "tasksC++/erastiophene.cpp"
//#include "tasksC++/sort.cpp"
//#include "tasksC++/factorial.cpp"
//#include <iostream>
//
//int main() {
//    integer n = 50;
//    integer* primes = eratosthenes_sieve(n);
//    std::cout << "Primes up to " << n << ": ";
//    for (integer i = 0; primes[i] != -1; i++) {
//        std::cout << primes[i] << " ";
//    }
//    std::cout << std::endl;
//
//
//    integer array[] = {7, 3, 8, 1, 9, 5};
//    integer len = 6;
//    sort(array, len);
//    std::cout << "Sorted array: ";
//    for (integer i = 0; i < len; i++) {
//        std::cout << array[i] << " ";
//    }
//    std::cout << std::endl;
//
//
//    integer num = 5;
//    integer result = factorial_calc(num);
//    std::cout << "Factorial of " << num << ": " << result << std::endl;
//
//    return 0;
//}

