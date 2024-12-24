//
// Created by Роман Шульган on 14.12.2024.
//

#ifndef COMPILER_LANGUAGE_LEXER_H
#define COMPILER_LANGUAGE_LEXER_H

#include<iostream>

enum class TypeOfVar {
    _integer,
    _float,
};


struct Token {
public:
    std::string name;
    TypeOfVar type;

    explicit Token(std::string name = "none", TypeOfVar type = TypeOfVar::_integer) : name(std::move(name)),
                                                                                      type(type) {}

};


class Lexer {
public:
    std::vector<Token> tokens;

    explicit Lexer(std::string &text) : text(std::move(text)) {
        tokens = parse(this->text);
    }

    [[maybe_unused]] std::vector<Token> parse(std::string &str) {
        std::vector<Token> resultOfParse;
        strip(str);
        std::vector<std::string> words = split(str, ' ');

        for (int i = 0; i < words.size(); i++) {
            if (words[i] == "integer" && is_word(words[i + 1])) {
                resultOfParse.emplace_back(words[i + 1], TypeOfVar::_integer);
            }
        }

        return resultOfParse;
    }

    ~Lexer() = default;

private:
    std::string text;

    std::vector<std::string> split(const std::string &text, char separate);

    void strip(std::string &str);

    bool is_word(const std::string &str) {
        for (char c: str)
            if (!isalpha(c))
                return false;

        return true;
    }
};


void Lexer::strip(std::string &str) {
    for (int i = 0, j = 0; i <= str.size(); i++) {
        if (isspace(str[i]) && i != str.size()) j++;
        else {
            if (j > 1) str.replace(i - j, j, " ");
            i -= j;
            j = 0;
        }
    }
    if (str[0] == ' ') str.erase(0, 1);
    if (str[str.size() - 1] == ' ') str.erase(str.size() - 1, 1);
}

std::vector<std::string> Lexer::split(const std::string &text, char separate) {
    std::vector<std::string> vec;

    for (int i = 0, pos = 0; i <= text.size(); i++) {
        if (text[i] == separate or i == text.size()) {
            vec.push_back(text.substr(pos, i - pos));
            pos = ++i;
        }
    }

    return vec;
}


#endif //COMPILER_LANGUAGE_LEXER_H
