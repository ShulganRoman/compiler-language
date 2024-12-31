#ifndef COMPILER_LANGUAGE_SYMBOLTABLE_H
#define COMPILER_LANGUAGE_SYMBOLTABLE_H

#include <string>
#include <unordered_map>
#include <vector>
#include <optional>

struct Symbol {
public:
    std::string name;
    std::string type;
    bool isFunction;
    std::vector<std::string> parameterTypes;

    Symbol(const std::string &n, const std::string &t, bool func = false, const std::vector<std::string> &params = {})
            : name(n), type(t), isFunction(func), parameterTypes(params) {}
};

class SymbolTable {
public:
    SymbolTable() {
        enterScope(); // Вход в глобальную область видимости
    }

    // Вход в новую область видимости
    void enterScope() {
        scopes.emplace_back();
    }

    // Выход из текущей области видимости
    void exitScope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    // Добавление символа в текущую область видимости
    bool addSymbol(const Symbol &symbol) {
        if (scopes.empty()) return false;
        auto &currentScope = scopes.back();
        if (currentScope.find(symbol.name) != currentScope.end()) {
            return false; // Символ уже существует в текущей области
        }
        currentScope.emplace(symbol.name, symbol);
        return true;
    }

    // Поиск символа в таблице (от текущей области до глобальной)
    std::optional<Symbol> lookup(const std::string &name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            const auto &scope = *it;
            auto found = scope.find(name);
            if (found != scope.end()) {
                return found->second;
            }
        }
        return std::nullopt;
    }

private:
    std::vector<std::unordered_map<std::string, Symbol>> scopes;
};

#endif // COMPILER_LANGUAGE_SYMBOLTABLE_H

