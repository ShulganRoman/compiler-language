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
        default:                         return "Unknown";
    }
}

// Узел AST
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

// Парсер
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

    bool isRealTypeKeyword(const std::string &kw) const {
        return (kw == "integer" || kw == "float" || kw == "bool" || kw == "string" || kw == "void");
    }

    // parseModifiers() — считывает подряд static/const возвращая строку
    std::string parseModifiers() {
        std::string mods;
        while (!isAtEnd() && match(TypeOfVar::_keyword)) {
            std::string kw = currentToken().name;
            if (kw == "static" || kw == "const") {
                if (!mods.empty()) mods += " ";
                mods += kw;
                advance();
            } else {
                break;
            }
        }
        return mods;
    }

    // _keyword, который == integer/float/bool/...
    bool isTypeKeyword() const {
        if (isAtEnd()) return false;
        if (currentToken().type != TypeOfVar::_keyword) return false;
        std::string name = currentToken().name;
        return isRealTypeKeyword(name);
    }

    // program -> topLevelDecl*
    ASTNode parseTopLevelDecl() {
        std::string mods = parseModifiers();

        if (!isTypeKeyword()) {
            throw std::runtime_error(
                    "Expected type keyword at top-level, got: " + currentToken().name
            );
        }

        Token typeKw = advance();

        bool isPointer = false;
        if (match(TypeOfVar::_operator, "*")) {
            advance();
            isPointer = true;
        }

        if (!match(TypeOfVar::_identifier)) {
            throw std::runtime_error("Expected identifier after type keyword");
        }
        Token ident = advance(); // e.g. "eratosthenes_sieve"

        // Если дальше '(' => это функция
        if (match(TypeOfVar::_separator, "(")) {
            return parseFunctionDecl(mods, typeKw, isPointer, ident);
        } else {
            return parseVarDecl(mods, typeKw, isPointer, ident);
        }
    }


    // function_decl -> (mods?) type(' *')? ident '(' param_list? ')' block
    ASTNode parseFunctionDecl(const std::string &mods,
                              const Token &typeKw,
                              bool isPointer,
                              const Token &funcIdent)
    {
        advance();

        std::string fullType;
        if (!mods.empty()) {
            fullType = mods + " ";
        }
        fullType += typeKw.name;
        if (isPointer) fullType += "*";

        ASTNode funcNode(ASTNodeType::FunctionDecl, fullType);

        ASTNode funcName(ASTNodeType::Identifier, funcIdent.name);
        funcNode.addChild(funcName);

        if (!match(TypeOfVar::_separator, ")")) {
            parseParameterList(funcNode);
        }

        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' in function declaration");
        }
        advance(); // ')'

        ASTNode blockNode = parseBlock();
        funcNode.addChild(blockNode);

        return funcNode;
    }

    // param_list -> param (',' param)*
    // param -> (mods?) type(' *')? ident
    void parseParameterList(ASTNode &funcNode) {

        parseOneParameter(funcNode);

        while (match(TypeOfVar::_separator, ",")) {
            advance(); // ','
            parseOneParameter(funcNode);
        }
    }

    void parseOneParameter(ASTNode &funcNode) {
        if (!isTypeKeyword()) {
            throw std::runtime_error("Expected type keyword in parameter list");
        }
        Token paramType = advance(); // e.g. "integer"

        // Проверяем '*'
        bool isPtr = false;
        if (match(TypeOfVar::_operator, "*")) {
            advance();
            isPtr = true;
        }

        // Ожидаем идентификатор
        if (!match(TypeOfVar::_identifier)) {
            throw std::runtime_error("Expected identifier after type in parameter");
        }
        Token paramIdent = advance();

        std::string typeName = paramType.name + (isPtr?"*":"");
        ASTNode paramNode(ASTNodeType::Parameter, typeName);

        ASTNode paramName(ASTNodeType::Identifier, paramIdent.name);
        paramNode.addChild(paramName);

        funcNode.addChild(paramNode);
    }

    // var_decl -> (mods?) type(' *')? ident arrayDims? ('=' assignmentExpr)? ';'
    ASTNode parseVarDecl(const std::string &mods,
                         const Token &typeKw,
                         bool isPointer,
                         const Token &ident)
    {
        // e.g. fullType = "static const integer*"
        std::string fullType = (mods.empty() ? "" : (mods + " "))
                               + typeKw.name
                               + (isPointer ? "*" : "");
        ASTNode varDecl(ASTNodeType::VarDecl, fullType);

        ASTNode varName(ASTNodeType::Identifier, ident.name);
        varDecl.addChild(varName);

        parseArrayDims(varName);

        // Если '=', инициализация
        if (match(TypeOfVar::_operator, "=")) {
            advance(); // '='
            ASTNode initExpr = parseAssignmentExpr();
            varDecl.addChild(initExpr);
        }

        // ожидаем ';'
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after var_decl");
        }
        advance(); // ';'
        return varDecl;
    }


    // parseArrayDims( ASTNode varName ) — считываем 0+ раз '[ <expr> ]'
    void parseArrayDims(ASTNode &varName) {
        while (match(TypeOfVar::_separator, "[")) {
            advance(); // '['
            ASTNode dimExpr = parseAssignmentExpr();
            if (!match(TypeOfVar::_separator, "]")) {
                throw std::runtime_error("Expected ']' after array dimension");
            }
            advance(); // ']'

            ASTNode arrDim(ASTNodeType::BinaryOp, "arrayDim");
            arrDim.addChild(dimExpr);

            varName.addChild(arrDim);
        }
    }

    // block -> '{' statement* '}'
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

    // statement -> if | while | for | return | var_decl | exprStatement
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

        // Если isTypeKeyword => varDecl
        {
            size_t oldPos = position;
            std::string mods = parseModifiers(); // считываем, если есть static,const

            if (isTypeKeyword()) {
                // да, значит var_decl
                Token typeKw = advance();
                bool isPtr = false;
                if (match(TypeOfVar::_operator, "*")) {
                    advance();
                    isPtr = true;
                }
                if (!match(TypeOfVar::_identifier)) {
                    throw std::runtime_error("Expected identifier after type in statement");
                }
                Token ident = advance();
                // запретим '(' => function decl in block
                if (match(TypeOfVar::_separator, "(")) {
                    throw std::runtime_error("Function declaration not allowed inside block");
                }
                return parseVarDecl(mods, typeKw, isPtr, ident);
            } else {
                // откат, если mods были, но не было типа
                position = oldPos;
            }
        }
        // если не var_decl, тогда expression-statement
        return parseExpressionStatement();
    }

    // expressionStatement -> assignmentExpr ';'
    ASTNode parseExpressionStatement() {
        ASTNode expr = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after expression statement");
        }
        advance(); // ';'
        return expr;
    }


    // for -> 'for' '(' for_init ';' condition ';' step ')' block
    ASTNode parseForStatement() {
        advance(); // съедаем "for"

        if (!match(TypeOfVar::_separator, "(")) {
            throw std::runtime_error("Expected '(' after 'for'");
        }
        advance(); // '('

        ASTNode initNode = parseForInitNoSemicolon();

        // ожидаем ';'
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after for-init");
        }
        advance(); // ';'

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

    // for_initNoSemicolon -> varDeclNoSemicolon | assignmentExpr | пусто
    ASTNode parseForInitNoSemicolon() {
        // если сразу ';' => пусто
        if (match(TypeOfVar::_separator, ";")) {
            return ASTNode(ASTNodeType::Literal, "no_init");
        }

        // попробуем парсить modifiers + type => var_decl_noSemicolon
        {
            size_t oldPos = position;
            std::string mods = parseModifiers();
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
                // не допускаем '(' => function
                if (match(TypeOfVar::_separator, "(")) {
                    throw std::runtime_error("Function decl not allowed in for-init");
                }
                // varDeclNoSemicolon
                return parseVarDeclNoSemicolon(mods, typeKw, isPtr, ident);
            } else {
                // откат, если не было типа
                position = oldPos;
            }
        }

        // иначе parseAssignmentExpr
        if (!isAtEnd()) {
            return parseAssignmentExpr();
        }
        return ASTNode(ASTNodeType::Literal, "no_init");
    }


    // varDeclNoSemicolon -> (mods?) type(' *')? ident arrayDims? ('=' assignmentExpr)?
    ASTNode parseVarDeclNoSemicolon(const std::string &mods,
                                    const Token &typeKw,
                                    bool isPointer,
                                    const Token &ident)
    {
        std::string fullType = (mods.empty()? "" : mods + " ")
                               + typeKw.name + (isPointer?"*":"");
        ASTNode varDecl(ASTNodeType::VarDecl, fullType);

        ASTNode varName(ASTNodeType::Identifier, ident.name);
        varDecl.addChild(varName);

        parseArrayDims(varName);

        if (match(TypeOfVar::_operator, "=")) {
            advance();
            ASTNode initExpr = parseAssignmentExpr();
            varDecl.addChild(initExpr);
        }
        return varDecl;
    }


    // if -> 'if' '(' assignmentExpr ')' block ( 'else' block )?
    ASTNode parseIfStatement() {
        advance(); // "if"
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
            advance();
            ASTNode elseBlock = parseBlock();
            ifNode.addChild(elseBlock);
        }
        return ifNode;
    }

    // while -> 'while' '(' assignmentExpr ')' block
    ASTNode parseWhileStatement() {
        advance(); // "while"
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


    // return -> 'return' assignmentExpr? ';'
    ASTNode parseReturnStatement() {
        advance(); // 'return'
        if (match(TypeOfVar::_separator, ";")) {
            // пустой return
            advance();
            return ASTNode(ASTNodeType::ReturnStmt, "return");
        }
        // иначе парсим expr
        ASTNode expr = parseAssignmentExpr();
        if (!match(TypeOfVar::_separator, ";")) {
            throw std::runtime_error("Expected ';' after return");
        }
        advance(); // ';'
        ASTNode ret(ASTNodeType::ReturnStmt, "return");
        ret.addChild(expr);
        return ret;
    }


    // assignmentExpr -> logicalOr ( assignmentOp assignmentExpr )?
    // assignmentOp -> '=' | '+=' | '-=' | '*=' | '/=' ...
    ASTNode parseAssignmentExpr() {
        ASTNode left = parseLogicalOr();
        if (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string opStr = currentToken().name;
            if (opStr == "=" || opStr == "+=" || opStr == "-=" || opStr == "*=" || opStr == "/=") {
                advance(); // съедаем оператор
                ASTNode right = parseAssignmentExpr();
                ASTNode assignOp(ASTNodeType::BinaryOp, opStr);
                assignOp.addChild(left);
                assignOp.addChild(right);
                return assignOp;
            }
        }
        return left;
    }


    // logicalOr -> logicalAnd ( '||' logicalAnd )*
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


    // logicalAnd -> equality ( '&&' equality )*
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


    // equality -> comparison ( ('==' | '!=') comparison )*
    ASTNode parseEquality() {
        ASTNode left = parseComparison();
        while (!isAtEnd() && match(TypeOfVar::_operator)
               && (currentToken().name == "==" || currentToken().name == "!=")) {
            Token op = advance();
            ASTNode right = parseComparison();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    // comparison -> term ( ('<' | '>' | '<=' | '>=') term )*
    ASTNode parseComparison() {
        ASTNode left = parseTerm();
        while (!isAtEnd() && match(TypeOfVar::_operator)
               && (currentToken().name == "<" || currentToken().name == ">"
                   || currentToken().name == "<=" || currentToken().name == ">="))
        {
            Token op = advance();
            ASTNode right = parseTerm();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    // term -> factor ( ('+' | '-') factor )*
    ASTNode parseTerm() {
        ASTNode left = parseFactor();
        while (!isAtEnd() && match(TypeOfVar::_operator)
               && (currentToken().name == "+" || currentToken().name == "-"))
        {
            Token op = advance();
            ASTNode right = parseFactor();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }

    // factor -> unary ( ('*' | '/') unary )*
    ASTNode parseFactor() {
        ASTNode left = parseUnary();
        while (!isAtEnd() && match(TypeOfVar::_operator)
               && (currentToken().name == "*" || currentToken().name == "/"))
        {
            Token op = advance();
            ASTNode right = parseUnary();
            ASTNode binOp(ASTNodeType::BinaryOp, op.name);
            binOp.addChild(left);
            binOp.addChild(right);
            left = binOp;
        }
        return left;
    }


    // unary -> ( '!' | '-' ) unary | primary
    ASTNode parseUnary() {
        if (!isAtEnd() && match(TypeOfVar::_operator)
            && (currentToken().name == "!" || currentToken().name == "-"))
        {
            Token op = advance();
            ASTNode right = parseUnary();
            ASTNode unOp(ASTNodeType::BinaryOp, op.name);
            unOp.addChild(right);
            return unOp;
        }
        return parsePrimary();
    }


    // primary -> '(' assignmentExpr ')'
    //          | number | bool | string
    //          | identifier ( '('args' ) )? ( '[' expr ']' )*
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

        if (match(TypeOfVar::_number)) {
            Token numTok = advance();
            return ASTNode(ASTNodeType::Literal, numTok.name);
        }
        if (match(TypeOfVar::_boolean)) {
            Token boolTok = advance();
            return ASTNode(ASTNodeType::Literal, boolTok.name);
        }
        if (match(TypeOfVar::_string)) {
            Token strTok = advance();
            return ASTNode(ASTNodeType::Literal, strTok.name);
        }

        if (match(TypeOfVar::_identifier)) {
            Token idTok = advance();
            ASTNode baseNode(ASTNodeType::Identifier, idTok.name);

            if (match(TypeOfVar::_separator, "(")) {
                baseNode = parseFunctionCall(baseNode);
            }

            while (match(TypeOfVar::_separator, "[")) {
                baseNode = parseIndexAccess(baseNode);
            }

            return baseNode;
        }

        throw std::runtime_error("Expected primary expression, got: " + currentToken().name);
    }


    // parseFunctionCall( baseIdent ) -> '(' (assignmentExpr (',' assignmentExpr)*)? ')'
    ASTNode parseFunctionCall(ASTNode baseIdent) {
        advance(); // '('
        ASTNode callNode(ASTNodeType::BinaryOp, "call");
        callNode.addChild(baseIdent);

        if (!match(TypeOfVar::_separator, ")")) {
            // значит есть аргумент
            ASTNode arg = parseAssignmentExpr();
            callNode.addChild(arg);

            while (match(TypeOfVar::_separator, ",")) {
                advance(); // ','
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

    // parseIndexAccess( base ) -> '[' assignmentExpr ']'
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
