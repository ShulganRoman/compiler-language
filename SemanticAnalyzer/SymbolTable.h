#pragma once

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
        enterScope();
    }

    void enterScope() {
        scopes.emplace_back();
    }

    void exitScope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    bool addSymbol(const Symbol &symbol) {
        if (scopes.empty()) return false;
        auto &currentScope = scopes.back();
        if (currentScope.find(symbol.name) != currentScope.end()) {
            return false;
        }
        currentScope.emplace(symbol.name, symbol);
        return true;
    }

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
