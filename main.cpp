#include "lexer.h"
#include <iostream>

int main() {
    std::string code = "integer arr[3]; float func(integer x) { return x + 1; }";
    Lexer lexer(code);

    for (const auto &token : lexer.tokens) {
        std::cout << "Token: " << token.name << ", Type: ";
        switch (token.type) {
            case TypeOfVar::_integer: std::cout << "Integer"; break;
            case TypeOfVar::_float: std::cout << "Float"; break;
            case TypeOfVar::_array: std::cout << "Array"; break;
            case TypeOfVar::_function: std::cout << "Function"; break;
            case TypeOfVar::_keyword: std::cout << "Keyword"; break;
            case TypeOfVar::_operator: std::cout << "Operator"; break;
            case TypeOfVar::_number: std::cout << "Number"; break;
            case TypeOfVar::_identifier: std::cout << "Identifier"; break;
            case TypeOfVar::_separator: std::cout << "Separator"; break;
            case TypeOfVar::_unknown: std::cout << "Unknown"; break;
        }
        std::cout << std::endl;
    }

    return 0;
}
