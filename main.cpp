#include <iostream>
#include "Lexer.h"
#include "AST.h"

// Небольшая функция для отладки: печать AST
void printAST(const ASTNode &node, int indent = 0) {
    std::string prefix(indent, ' ');
    std::cout << prefix
              << astNodeTypeToString(node.type)
              << " (value='" << node.value << "')" << std::endl;
    for (auto &child : node.children) {
        printAST(child, indent + 2);
    }
}

int main() {
    std::string code = R"(
integer* eratosthenes_sieve(integer n) {
    static const integer MAX_SIZE = 10000;
    static bool is_prime[MAX_SIZE];
    static integer primes[MAX_SIZE];
    integer SIZE_OF_BOOL = 1;

    if (n >= MAX_SIZE) {
        return nullptr;
    }

    memset(is_prime, true, (n + 1) * SIZE_OF_BOOL);
    is_prime[0] = is_prime[1] = false;

    for (integer i = 2; i * i <= n; i+=1) {
        if (is_prime[i]) {
            for (integer j = i * i; j <= n; j =j+ i) {
                is_prime[j] = false;
            }
        }
    }

    integer prime_count = 0;
    for (integer i = 2; i <= n; i=i+1) {
        if (is_prime[i]) {
            primes[prime_count=prime_count+1] = i;
        }
    }

    primes[prime_count] = -1;
    return primes;
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
    try {
        ASTNode root = parser.parseProgram();

        // 4. Выводим AST
        std::cout << "\n=== AST ===\n";
        printAST(root, 0);

    } catch (const std::exception &ex) {
        std::cerr << "Parse error: " << ex.what() << std::endl;
    }

    return 0;
}
