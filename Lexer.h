//
// Created by Роман Шульган on 14.12.2024.
//

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
    _array,
    _function,
    _keyword,
    _operator,
    _number,
    _identifier,
    _separator,
    _unknown
};

struct Token {
public:
    std::string name;
    TypeOfVar type;
    std::optional<TypeOfVar> number_type;

    explicit Token(std::string name = "none", TypeOfVar type = TypeOfVar::_unknown,
                   std::optional<TypeOfVar> number_type = std::nullopt)
            : name(std::move(name)), type(type), number_type(number_type) {}
};

class Lexer {
public:
    std::vector<Token> tokens;

    explicit Lexer(std::string text) : text(std::move(text)) {
        tokens = parse(this->text);
    }

    bool is_operator(const std::string &str) {
        return str == "+" || str == "-" || str == "*" || str == "/" || str == "=" ||
               str == "==" || str == "!=" || str == "<" || str == "<=" || str == ">" || str == ">=";
    }

    bool is_separator(char c) {
        return c == ';' || c == '{' || c == '}' || c == '(' || c == ')' || c == '[' || c == ']';
    }

    std::optional<TypeOfVar> get_number_type(const std::string &str) {
        bool has_dot = false;
        for (char c : str) {
            if (!isdigit(c) && c != '.') return std::nullopt;
            if (c == '.') {
                if (has_dot) return std::nullopt;
                has_dot = true;
            }
        }
        return has_dot ? TypeOfVar::_float : TypeOfVar::_integer;
    }

    std::vector<Token> parse(std::string &str) {
        std::vector<Token> resultOfParse;
        strip(str);
        std::vector<std::string> words = split_tokens(str);

        const std::set<std::string> keywords = {"integer", "float", "if", "else", "while", "for", "return"};

        for (size_t i = 0; i < words.size(); i++) {
            if (keywords.count(words[i])) {
                resultOfParse.emplace_back(words[i], TypeOfVar::_keyword);
            } else if (is_operator(words[i])) {
                resultOfParse.emplace_back(words[i], TypeOfVar::_operator);
            } else if (words[i] == "[" && i > 0 && resultOfParse.back().type == TypeOfVar::_identifier) {
                resultOfParse.back().type = TypeOfVar::_array;
                resultOfParse.emplace_back(words[i], TypeOfVar::_separator);
            } else if (words[i] == "(" && i > 0 && resultOfParse.back().type == TypeOfVar::_identifier) {
                resultOfParse.back().type = TypeOfVar::_function;
                resultOfParse.emplace_back(words[i], TypeOfVar::_separator);
            } else if (auto number_type = get_number_type(words[i])) {
                resultOfParse.emplace_back(words[i], TypeOfVar::_number, *number_type);
            } else if (is_valid_identifier(words[i])) {
                resultOfParse.emplace_back(words[i], TypeOfVar::_identifier);
            } else if (words[i].length() == 1 && is_separator(words[i][0])) {
                resultOfParse.emplace_back(words[i], TypeOfVar::_separator);
            } else {
                resultOfParse.emplace_back(words[i], TypeOfVar::_unknown);
            }
        }

        return resultOfParse;
    }


    ~Lexer() = default;

private:
    std::string text;

    std::vector<std::string> split_tokens(const std::string &str) {
        std::vector<std::string> tokens;
        std::string current;
        for (char c : str) {
            if (isspace(c)) {
                if (!current.empty()) {
                    tokens.push_back(current);
                    current.clear();
                }
            } else if (is_separator(c)) {
                if (!current.empty()) {
                    tokens.push_back(current);
                    current.clear();
                }
                tokens.emplace_back(1, c);
            } else if (is_operator(std::string(1, c))) {
                if (!current.empty()) {
                    tokens.push_back(current);
                    current.clear();
                }
                tokens.emplace_back(1, c);
            } else {
                current += c;
            }
        }
        if (!current.empty()) {
            tokens.push_back(current);
        }
        return tokens;
    }

    void strip(std::string &str) {
        str.erase(str.begin(), std::find_if(str.begin(), str.end(), [](unsigned char c) {
            return !std::isspace(c);
        }));
        str.erase(std::find_if(str.rbegin(), str.rend(), [](unsigned char c) {
            return !std::isspace(c);
        }).base(), str.end());
    }

    bool is_valid_identifier(const std::string &str) {
        if (str.empty() || !isalpha(str[0])) return false;
        for (char c : str) {
            if (!isalnum(c) && c != '_') return false;
        }
        return true;
    }
};

#endif //COMPILER_LANGUAGE_LEXER_H
