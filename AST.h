#ifndef COMPILER_LANGUAGE_PARSER_H
#define COMPILER_LANGUAGE_PARSER_H

#include "Lexer.h"
#include <stdexcept>
#include <vector>
#include <string>


enum class ASTNodeType {
    Program,
    VarDecl,
    FunctionDecl,
    Block,
    Assignment,
    ReturnStmt,
    IfStmt,
    WhileStmt,
    ForStmt,
    Expression,
    BinaryOp,
    Literal,
    Identifier,
    Parameter
};

inline std::string astNodeTypeToString(ASTNodeType t) {
    switch(t) {
        case ASTNodeType::Program:       return "Program";
        case ASTNodeType::VarDecl:       return "VarDecl";
        case ASTNodeType::FunctionDecl:  return "FunctionDecl";
        case ASTNodeType::Block:         return "Block";
        case ASTNodeType::Assignment:    return "Assignment";
        case ASTNodeType::ReturnStmt:    return "ReturnStmt";
        case ASTNodeType::IfStmt:        return "IfStmt";
        case ASTNodeType::WhileStmt:     return "WhileStmt";
        case ASTNodeType::ForStmt:       return "ForStmt";
        case ASTNodeType::Expression:    return "Expression";
        case ASTNodeType::BinaryOp:      return "BinaryOp";
        case ASTNodeType::Literal:       return "Literal";
        case ASTNodeType::Identifier:    return "Identifier";
        case ASTNodeType::Parameter:     return "Parameter";
    }
    return "Unknown";
}

struct ASTNode {
    ASTNodeType type;
    std::string value;
    std::vector<ASTNode> children;

    ASTNode(ASTNodeType t, std::string v)
            : type(t), value(std::move(v)) {}


    ASTNode()
            : type(ASTNodeType::Literal),
              value("empty_default") {}

    void addChild(const ASTNode &child) {
        children.push_back(child);
    }
};

class Parser {
public:
    explicit Parser(std::vector<Token> tokens)
            : tokens(std::move(tokens)), position(0) {}

    ASTNode parseProgram() {
        ASTNode root(ASTNodeType::Program, "program");
        while (!isAtEnd()) {
            root.addChild(parseTopLevelDecl());
        }
        return root;
    }

private:
    std::vector<Token> tokens;
    size_t position;

    //=== Методы доступа к токенам ===//
    Token currentToken() const {
        if (position < tokens.size()) {
            return tokens[position];
        }
        return Token("EOF", TypeOfVar::_unknown, std::nullopt, -1, -1);
    }
    bool isAtEnd() const {
        return position >= tokens.size();
    }
    Token advance() {
        if (!isAtEnd()) {
            return tokens[position++];
        }
        return Token("EOF", TypeOfVar::_unknown, std::nullopt, -1, -1);
    }
    bool match(TypeOfVar t) const {
        if (isAtEnd()) return false;
        return (currentToken().type == t);
    }
    bool match(TypeOfVar t, const std::string &name) const {
        if (isAtEnd()) return false;
        return (currentToken().type == t && currentToken().name == name);
    }
    bool check(TypeOfVar t) const {
        return match(t);
    }

    bool isTypeKeyword() const {
        if (!match(TypeOfVar::_keyword)) return false;
        std::string kw = currentToken().name;
        return (kw == "integer" || kw == "float" ||
                kw == "bool"    || kw == "string" ||
                kw == "void");
    }

    ASTNode parseTopLevelDecl() {
        if (isTypeKeyword()) {
            Token typeKw = advance();
            bool isPointer = false;
            if (match(TypeOfVar::_operator, "*")) {
                advance();
                isPointer = true;
            }
            if (!match(TypeOfVar::_identifier)) {
                throw std::runtime_error("Expected identifier after type keyword");
            }
            Token ident = advance();

            if (match(TypeOfVar::_separator, "(")) {
                // functionDecl
                return parseFunctionDecl(typeKw, isPointer, ident);
            } else {
                // varDecl
                return parseVarDecl(typeKw, isPointer, ident);
            }
        }
        throw std::runtime_error("Expected type keyword at top-level, got: " + currentToken().name);
    }

    //=== function_decl -> type ('*')? ident '(' param_list? ')' block ===//
    ASTNode parseFunctionDecl(const Token &typeKw, bool isPointer, const Token &funcIdent) {
        // уже видим '('
        advance(); // '('

        ASTNode funcNode(ASTNodeType::FunctionDecl, typeKw.name + (isPointer ? "*" : ""));
        ASTNode funcName(ASTNodeType::Identifier, funcIdent.name);
        funcNode.addChild(funcName);

        if (!match(TypeOfVar::_separator, ")")) {
            parseParameterList(funcNode);
        }
        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' in function declaration");
        }
        advance(); // ')'

        // block
        ASTNode blockNode = parseBlock();
        funcNode.addChild(blockNode);

        return funcNode;
    }

    //=== param_list -> param (',' param)* ===//
    void parseParameterList(ASTNode &funcNode) {
        parseOneParameter(funcNode);
        while (match(TypeOfVar::_separator, ",")) {
            advance(); // ','
            parseOneParameter(funcNode);
        }
    }
    //=== param -> type ('*')? ident ===//
    void parseOneParameter(ASTNode &funcNode) {
        if (!isTypeKeyword()) {
            throw std::runtime_error("Expected type keyword in parameter list");
        }
        Token paramType = advance();
        bool isPtr = false;
        if (match(TypeOfVar::_operator, "*")) {
            advance();
            isPtr = true;
        }
        if (!match(TypeOfVar::_identifier)) {
            throw std::runtime_error("Expected identifier after type keyword in param");
        }
        Token paramIdent = advance();

        ASTNode paramNode(ASTNodeType::Parameter, paramType.name + (isPtr ? "*" : ""));
        ASTNode paramName(ASTNodeType::Identifier, paramIdent.name);
        paramNode.addChild(paramName);

        funcNode.addChild(paramNode);
    }

    //=== var_decl -> type(' *')? ident ('=' assignmentExpr)? ';' ===//
    ASTNode parseVarDecl(const Token &typeKw, bool isPointer, const Token &ident) {
        ASTNode varDecl(ASTNodeType::VarDecl, typeKw.name + (isPointer ? "*" : ""));
        ASTNode varName(ASTNodeType::Identifier, ident.name);
        varDecl.addChild(varName);

        if (match(TypeOfVar::_operator, "=")) {
            advance(); // '='
            ASTNode expr = parseAssignmentExpr();
            varDecl.addChild(expr);
        }

        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after var_decl");
        }
        advance(); // ';'

        return varDecl;
    }

    //=== block -> '{' statement* '}' ===//
    ASTNode parseBlock() {
        if (!match(TypeOfVar::_separator, "{")) {
            throw std::runtime_error("Expected '{' to start block");
        }
        advance(); // '{'

        ASTNode blockNode(ASTNodeType::Block, "{}");
        while (!isAtEnd() && !match(TypeOfVar::_separator, "}")) {
            blockNode.addChild(parseStatement());
        }
        if (!match(TypeOfVar::_separator, "}")) {
            throw std::runtime_error("Expected '}' at end of block");
        }
        advance(); // '}'

        return blockNode;
    }

    //=== statement -> if | while | for | return | var_decl | expr-stmt ===//
    ASTNode parseStatement() {
        // if
        if (match(TypeOfVar::_keyword, "if")) {
            return parseIfStatement();
        }
        // while
        if (match(TypeOfVar::_keyword, "while")) {
            return parseWhileStatement();
        }
        // for
        if (match(TypeOfVar::_keyword, "for")) {
            return parseForStatement();
        }
        // return
        if (match(TypeOfVar::_keyword, "return")) {
            return parseReturnStatement();
        }
        // var_decl
        if (isTypeKeyword()) {
            Token typeKw = advance();
            bool isPtr = false;
            if (match(TypeOfVar::_operator, "*")) {
                advance();
                isPtr = true;
            }
            if (!match(TypeOfVar::_identifier)) {
                throw std::runtime_error("Expected identifier after type keyword in statement");
            }
            Token ident = advance();
            if (match(TypeOfVar::_separator, "(")) {
                throw std::runtime_error("FunctionDecl not allowed inside block");
            }
            return parseVarDecl(typeKw, isPtr, ident);
        }
        // expression-statement
        // (любое выражение, заканчивающееся ';')
        return parseExprStatement();
    }

    //=== expression-statement -> assignmentExpr ';' ===//
    // (или развернуто: expr -> assignment -> logicalOr …)
    ASTNode parseExprStatement() {
        ASTNode expr = parseAssignmentExpr();
        // ожидаем ';'
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after expression statement");
        }
        advance(); // ';'

        // Для AST можно либо создать узел Statement, либо вернуть сам expr
        // Если хотим различать assignment vs. expr, смотрим корень expr.
        return expr;
    }

    //=== for -> 'for' '(' for_init ';' (assignmentExpr?) ';' (assignmentExpr?) ')' block ===//
    ASTNode parseForStatement() {
        advance(); // 'for'
        if (!match(TypeOfVar::_separator, "(")) {
            throw std::runtime_error("Expected '(' after 'for'");
        }
        advance(); // '('

        // parse for_init
        ASTNode initNode = parseForInitNoSemicolon();
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after for-init");
        }
        advance(); // ';'

        // condition
        ASTNode condNode;
        if (!match(TypeOfVar::_separator, ";")) {
            condNode = parseAssignmentExpr();
        } else {
            condNode = ASTNode(ASTNodeType::Literal, "true");
        }
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after for-condition");
        }
        advance(); // ';'

        // step
        ASTNode stepNode;
        if (!match(TypeOfVar::_separator, ")")) {
            stepNode = parseAssignmentExpr();
        } else {
            stepNode = ASTNode(ASTNodeType::Literal, "no_step");
        }
        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' after for-step");
        }
        advance(); // ')'

        ASTNode body = parseBlock();

        ASTNode forNode(ASTNodeType::ForStmt, "for");
        forNode.addChild(initNode);
        forNode.addChild(condNode);
        forNode.addChild(stepNode);
        forNode.addChild(body);
        return forNode;
    }

    //=== for_initNoSemicolon -> var_decl_no_semicolon | assignmentExpr | пусто ===//
    ASTNode parseForInitNoSemicolon() {
        // если ';' => пустое
        if (match(TypeOfVar::_separator, ";")) {
            return ASTNode(ASTNodeType::Literal, "no_init");
        }
        // если тип => varDeclNoSemicolon
        if (isTypeKeyword()) {
            Token typeKw = advance();
            bool isPtr = false;
            if (match(TypeOfVar::_operator, "*")) {
                advance();
                isPtr = true;
            }
            if (!match(TypeOfVar::_identifier)) {
                throw std::runtime_error("Expected identifier in for-init");
            }
            Token ident = advance();
            if (match(TypeOfVar::_separator, "(")) {
                throw std::runtime_error("FuncDecl not allowed in for-init");
            }
            return parseVarDeclNoSemicolon(typeKw, isPtr, ident);
        }
        // иначе parseAssignmentExpr
        // (не требуем ';', потому что ';' прочитаем в parseForStatement)
        if (match(TypeOfVar::_identifier) || match(TypeOfVar::_number) || match(TypeOfVar::_boolean)
            || match(TypeOfVar::_string) || match(TypeOfVar::_separator, "(")
            || match(TypeOfVar::_operator, "!") || match(TypeOfVar::_operator, "-"))
        {
            return parseAssignmentExpr();
        }
        // пусто
        return ASTNode(ASTNodeType::Literal, "no_init");
    }

    //=== var_declNoSemicolon -> type(' *')? ident ('=' assignmentExpr)? (без ';') ===//
    ASTNode parseVarDeclNoSemicolon(const Token &typeKw, bool isPointer, const Token &ident) {
        ASTNode varDecl(ASTNodeType::VarDecl, typeKw.name + (isPointer?"*":""));
        varDecl.addChild(ASTNode(ASTNodeType::Identifier, ident.name));

        if (match(TypeOfVar::_operator, "=")) {
            advance();
            ASTNode expr = parseAssignmentExpr();
            varDecl.addChild(expr);
        }
        return varDecl;
    }

    //=== if -> 'if' '(' assignmentExpr ')' block ( 'else' block )? ===//
    ASTNode parseIfStatement() {
        advance(); // 'if'
        if (!match(TypeOfVar::_separator, "(")) {
            throw std::runtime_error("Expected '(' after 'if'");
        }
        advance(); // '('
        ASTNode cond = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' after if-condition");
        }
        advance(); // ')'

        ASTNode thenBlock = parseBlock();
        ASTNode ifNode(ASTNodeType::IfStmt, "if");
        ifNode.addChild(cond);
        ifNode.addChild(thenBlock);

        if (match(TypeOfVar::_keyword, "else")) {
            advance(); // 'else'
            ASTNode elseBlock = parseBlock();
            ifNode.addChild(elseBlock);
        }
        return ifNode;
    }

    //=== while -> 'while' '(' assignmentExpr ')' block ===//
    ASTNode parseWhileStatement() {
        advance(); // 'while'
        if (!match(TypeOfVar::_separator, "(")) {
            throw std::runtime_error("Expected '(' after 'while'");
        }
        advance(); // '('
        ASTNode cond = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' after while-condition");
        }
        advance(); // ')'

        ASTNode body = parseBlock();
        ASTNode whileNode(ASTNodeType::WhileStmt, "while");
        whileNode.addChild(cond);
        whileNode.addChild(body);
        return whileNode;
    }

    //=== return -> 'return' assignmentExpr? ';' ===//
    ASTNode parseReturnStatement() {
        advance(); // 'return'
        if (match(TypeOfVar::_separator, ";")) {
            advance();
            return ASTNode(ASTNodeType::ReturnStmt, "return");
        }
        // иначе парсим assignmentExpr
        ASTNode expr = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after return");
        }
        advance(); // ';'
        ASTNode ret(ASTNodeType::ReturnStmt, "return");
        ret.addChild(expr);
        return ret;
    }

    //=== parseAssignmentExpr -> parseLogicalOr ( '=' parseAssignmentExpr )? ===//
    ASTNode parseAssignmentExpr() {
        ASTNode left = parseLogicalOr();
        // если видим '=', значит присваивание
        if (!isAtEnd() && match(TypeOfVar::_operator, "=")) {
            Token op = advance(); // '='
            ASTNode right = parseAssignmentExpr();
            ASTNode assignOp(ASTNodeType::BinaryOp, "=");
            assignOp.addChild(left);
            assignOp.addChild(right);
            return assignOp;
        }
        return left;
    }

    //=== parseLogicalOr -> parseLogicalAnd ( '||' parseLogicalAnd )* ===//
    ASTNode parseLogicalOr() {
        ASTNode left = parseLogicalAnd();
        while (!isAtEnd() && match(TypeOfVar::_operator) && currentToken().name == "||") {
            Token op = advance();
            ASTNode right = parseLogicalAnd();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseLogicalAnd -> parseEquality ( '&&' parseEquality )* ===//
    ASTNode parseLogicalAnd() {
        ASTNode left = parseEquality();
        while (!isAtEnd() && match(TypeOfVar::_operator) && currentToken().name == "&&") {
            Token op = advance();
            ASTNode right = parseEquality();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseEquality -> parseComparison ( ('=='|'!=') parseComparison )* ===//
    ASTNode parseEquality() {
        ASTNode left = parseComparison();
        while (!isAtEnd() && match(TypeOfVar::_operator) &&
               (currentToken().name == "==" || currentToken().name == "!=")) {
            Token op = advance();
            ASTNode right = parseComparison();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseComparison -> parseTerm ( ('<'|'>'|'<='|'>=') parseTerm )* ===//
    ASTNode parseComparison() {
        ASTNode left = parseTerm();
        while (!isAtEnd() && match(TypeOfVar::_operator) &&
               (currentToken().name == "<"  || currentToken().name == ">"  ||
                currentToken().name == "<=" || currentToken().name == ">=")) {
            Token op = advance();
            ASTNode right = parseTerm();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseTerm -> parseFactor ( ('+'|'-') parseFactor )* ===//
    ASTNode parseTerm() {
        ASTNode left = parseFactor();
        while (!isAtEnd() && match(TypeOfVar::_operator) &&
               (currentToken().name == "+" || currentToken().name == "-")) {
            Token op = advance();
            ASTNode right = parseFactor();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseFactor -> parseUnary ( ('*'|'/') parseUnary )* ===//
    ASTNode parseFactor() {
        ASTNode left = parseUnary();
        while (!isAtEnd() && match(TypeOfVar::_operator) &&
               (currentToken().name == "*" || currentToken().name == "/")) {
            Token op = advance();
            ASTNode right = parseUnary();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    //=== parseUnary -> (('!'|'-') parseUnary) | parsePrimary ===//
    ASTNode parseUnary() {
        if (!isAtEnd() && match(TypeOfVar::_operator) &&
            (currentToken().name == "!" || currentToken().name == "-")) {
            Token op = advance();
            ASTNode right = parseUnary();
            ASTNode unOp(ASTNodeType::BinaryOp, op.name);
            unOp.addChild(right);
            return unOp;
        }
        return parsePrimary();
    }

    //=== parsePrimary -> '(' assignmentExpr ')' | number | bool | string | identifier(...) [...]* ===//
    ASTNode parsePrimary() {
        if (match(TypeOfVar::_separator, "(")) {
            advance(); // '('
            ASTNode sub = parseAssignmentExpr();
            if (!match(TypeOfVar::_separator, ")")) {
                throw std::runtime_error("Expected ')' after subexpression");
            }
            advance(); // ')'
            return sub;
        }
        // number
        if (match(TypeOfVar::_number)) {
            Token numTok = advance();
            return ASTNode(ASTNodeType::Literal, numTok.name);
        }
        // boolean
        if (match(TypeOfVar::_boolean)) {
            Token boolTok = advance();
            return ASTNode(ASTNodeType::Literal, boolTok.name);
        }
        // string
        if (match(TypeOfVar::_string)) {
            Token strTok = advance();
            return ASTNode(ASTNodeType::Literal, strTok.name);
        }
        // identifier
        if (match(TypeOfVar::_identifier)) {
            Token idTok = advance();
            ASTNode baseNode(ASTNodeType::Identifier, idTok.name);

            // Проверим, не идёт ли '(' => вызов функции
            if (match(TypeOfVar::_separator, "(")) {
                baseNode = parseFunctionCall(baseNode);
            }
            // Проверим, не идёт ли '[' => индекс
            while (match(TypeOfVar::_separator, "[")) {
                baseNode = parseIndexAccess(baseNode);
            }
            return baseNode;
        }
        throw std::runtime_error("Expected primary expression, got: " + currentToken().name);
    }

    //=== parseFunctionCall(baseIdent) -> '(' (assignmentExpr (',' assignmentExpr)*)? ')' ===//
    ASTNode parseFunctionCall(ASTNode baseIdent) {
        advance(); // '('

        ASTNode callNode(ASTNodeType::BinaryOp, "call");
        callNode.addChild(baseIdent);

        if (!match(TypeOfVar::_separator, ")")) {
            // значит есть аргумент(ы)
            ASTNode arg = parseAssignmentExpr();
            callNode.addChild(arg);

            while (match(TypeOfVar::_separator, ",")) {
                advance();
                ASTNode nextArg = parseAssignmentExpr();
                callNode.addChild(nextArg);
            }
        }

        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' after function call");
        }
        advance(); // ')'

        return callNode;
    }

    //=== parseIndexAccess(base) -> '[' assignmentExpr ']' ===//
    ASTNode parseIndexAccess(ASTNode base) {
        advance(); // '['
        ASTNode idx = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, "]")) {
            throw std::runtime_error("Expected ']' after index");
        }
        advance(); // ']'

        ASTNode node(ASTNodeType::BinaryOp, "[]");
        node.addChild(base);
        node.addChild(idx);
        return node;
    }
};

#endif // COMPILER_LANGUAGE_PARSER_H
