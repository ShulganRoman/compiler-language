#ifndef COMPILER_LANGUAGE_LEXER_H
#define COMPILER_LANGUAGE_LEXER_H

#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <cctype>
#include <set>


enum class TypeOfVar {
    _integer,
    _float,
    _boolean,
    _string,
    _keyword,    // "integer", "float", "bool", "string", "if", "else", "while", "for", "return", "void"
    _operator,   // +, -, *, /, %, =, ==, !=, <, >, <=, >=, &&, ||, ...
    _number,     // числовые литералы
    _identifier, // имена переменных, функций, ...
    _separator,  // ; { } ( ) [ ] , ...
    _unknown
};


inline std::string typeOfVarToString(TypeOfVar t) {
    switch(t) {
        case TypeOfVar::_integer:   return "Integer";
        case TypeOfVar::_float:     return "Float";
        case TypeOfVar::_boolean:   return "Boolean";
        case TypeOfVar::_string:    return "String";
        case TypeOfVar::_keyword:   return "Keyword";
        case TypeOfVar::_operator:  return "Operator";
        case TypeOfVar::_number:    return "Number";
        case TypeOfVar::_identifier:return "Identifier";
        case TypeOfVar::_separator: return "Separator";
        case TypeOfVar::_unknown:   return "Unknown";
    }
    return "Unknown";
}

struct Token {
public:
    std::string name;               // сам текст токена
    TypeOfVar type;                 // тип токена (keyword, operator, ... )
    std::optional<TypeOfVar> number_type; // если число, может уточнять int/float
    int line;                       // для отладки
    int col;                        // для отладки

    Token(std::string name = "none",
          TypeOfVar type = TypeOfVar::_unknown,
          std::optional<TypeOfVar> number_type = std::nullopt,
          int line = 0,
          int col  = 0)
            : name(std::move(name)),
              type(type),
              number_type(number_type),
              line(line),
              col(col)
    {}
};

class Lexer {
public:
    explicit Lexer(std::string text)
            : text(std::move(text)), position(0), line(1), col(1) {
        tokens = parse();
    }

    std::vector<Token> getTokens() const { return tokens; }

    ~Lexer() = default;

private:
    // Исходный текст
    std::string text;
    // Текущая позиция (индекс) в строке
    size_t position;
    // Номер текущей строки/столбца (для отладки)
    int line;
    int col;

    // Результирующие токены
    std::vector<Token> tokens;

    // Ключевые слова — добавим `void`
    std::set<std::string> keywords = {
            "integer", "float", "bool", "string", "void",
            "if", "else", "while", "for", "return",
            "static", "const"
    };

    // Набор булевых значений
    std::set<std::string> boolean_values = {
            "true", "false"
    };

    // Многосимвольные операторы
    std::vector<std::string> multiCharOps = {
            "==", "!=", "<=", ">=", "&&", "||", "+=", "-=", "*=", "/="
    };

    // Односимвольные операторы
    std::set<char> singleCharOps = {
            '+', '-', '*', '/', '%', '=', '<', '>', '!',
    };

    // Разделители
    std::set<char> separators = {
            ';', '{', '}', '(', ')', '[', ']', ','
    };

private:
    std::vector<Token> parse() {
        std::vector<Token> result;

        while (!isEOF()) {
            if (std::isspace(peek())) {
                advance();
                continue;
            }

            // Строковый литерал
            if (peek() == '"') {
                result.push_back(parseString());
                continue;
            }

            // Комментарий // ...
            if (peek() == '/' && lookAhead(1) == '/') {
                skipLineComment();
                continue;
            }
            // Комментарий /* ... */
            if (peek() == '/' && lookAhead(1) == '*') {
                skipBlockComment();
                continue;
            }

            // Многосимвольный оператор (==, !=, <=, >=, &&, ||, +=, ...)
            if (auto maybeOp = parseMultiCharOperator()) {
                result.push_back(*maybeOp);
                continue;
            }

            // Односимвольный оператор (+, -, *, /, =, ...)
            if (isOperator(peek())) {
                result.push_back(makeToken(std::string(1, advance()), TypeOfVar::_operator));
                continue;
            }

            // Разделитель (; { } ( ) [ ] ,)
            if (isSeparator(peek())) {
                char c = advance();
                result.push_back(makeToken(std::string(1, c), TypeOfVar::_separator));
                continue;
            }

            // Число (целое или с точкой)
            if (std::isdigit(peek()) || (peek() == '.' && std::isdigit(lookAhead(1)))) {
                result.push_back(parseNumber());
                continue;
            }

            // Идентификатор / ключевое слово / boolean
            if (std::isalpha(peek()) || peek() == '_') {
                result.push_back(parseIdentifierOrKeyword());
                continue;
            }

            // Если ничего не подошло, считаем unknown
            char unknownChar = advance();
            result.push_back(makeToken(std::string(1, unknownChar), TypeOfVar::_unknown));
        }

        return result;
    }

    Token parseString() {
        int startLine = line;
        int startCol  = col;

        // Пропускаем начальную кавычку
        advance();
        std::string value;
        while (!isEOF() && peek() != '"') {
            value.push_back(advance());
        }
        // Пропускаем закрывающую кавычку
        if (!isEOF()) {
            advance();
        }

        return Token("\"" + value + "\"", TypeOfVar::_string, std::nullopt, startLine, startCol);
    }

    void skipLineComment() {
        // съедаем '//'
        advance();
        advance();
        while (!isEOF() && peek() != '\n') {
            advance();
        }
    }

    void skipBlockComment() {
        // съедаем '/*'
        advance();
        advance();
        while (!isEOF()) {
            if (peek() == '*' && lookAhead(1) == '/') {
                advance();
                advance();
                break;
            }
            advance();
        }
    }

    std::optional<Token> parseMultiCharOperator() {
        int startLine = line;
        int startCol  = col;
        if (isEOF()) return std::nullopt;

        std::string twoChars(1, peek());
        if (position + 1 < text.size()) {
            twoChars.push_back(text[position+1]);
        }

        for (auto &op : multiCharOps) {
            if (twoChars == op) {
                advance();
                advance();
                return Token(op, TypeOfVar::_operator, std::nullopt, startLine, startCol);
            }
        }
        return std::nullopt;
    }

    bool isOperator(char c) const {
        return (singleCharOps.count(c) > 0);
    }

    bool isSeparator(char c) const {
        return (separators.count(c) > 0);
    }

    std::string parseSingleCharOperator() {
        return std::string(1, advance());
    }

    Token parseNumber() {
        int startLine = line;
        int startCol  = col;

        std::string value;
        bool hasDot = false;

        while (!isEOF() && (std::isdigit(peek()) || peek() == '.')) {
            if (peek() == '.') {
                if (hasDot) {
                    break;
                }
                hasDot = true;
            }
            value.push_back(advance());
        }

        TypeOfVar subtype = hasDot ? TypeOfVar::_float : TypeOfVar::_integer;
        return Token(value, TypeOfVar::_number, subtype, startLine, startCol);
    }

    Token parseIdentifierOrKeyword() {
        int startLine = line;
        int startCol  = col;

        std::string value;
        while (!isEOF() && (std::isalnum(peek()) || peek() == '_')) {
            value.push_back(advance());
        }

        // boolean?
        if (boolean_values.count(value)) {
            return Token(value, TypeOfVar::_boolean, std::nullopt, startLine, startCol);
        }

        // keyword?
        if (keywords.count(value)) {
            return Token(value, TypeOfVar::_keyword, std::nullopt, startLine, startCol);
        }

        // иначе идентификатор
        return Token(value, TypeOfVar::_identifier, std::nullopt, startLine, startCol);
    }

    char peek() const {
        if (position >= text.size()) return '\0';
        return text[position];
    }

    char lookAhead(int offset) const {
        if (position + offset >= text.size()) return '\0';
        return text[position + offset];
    }

    char advance() {
        if (isEOF()) return '\0';
        char c = text[position++];
        if (c == '\n') {
            line++;
            col = 1;
        } else {
            col++;
        }
        return c;
    }

    bool isEOF() const {
        return position >= text.size();
    }

    Token makeToken(const std::string &name, TypeOfVar type) {
        return Token(name, type, std::nullopt, line, col - (int)name.size());
    }
};

#endif // COMPILER_LANGUAGE_LEXER_H