#pragma once

#include "../Lexer/Lexer.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <optional>

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

// Узел AST
struct ASTNode {
    ASTNodeType type;
    std::string value;
    std::vector<ASTNode> children;
    std::optional<std::string> inferredType;

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

    [[nodiscard]] Token currentToken() const {
        if (position < tokens.size()) {
            return tokens[position];
        }
        return Token("EOF", TypeOfVar::_unknown, std::nullopt, -1, -1);
    }

    [[nodiscard]] bool isAtEnd() const {
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
        return isRealTypeKeyword(currentToken().name);
    }

    // program -> topLevelDecl*
    ASTNode parseTopLevelDecl() {
        std::string mods = parseModifiers();

        if (!isTypeKeyword()) {
            throw std::runtime_error(
                    "Expected type keyword at top-level, got: " + currentToken().name
            );
        }

        Token typeKw = advance();  // integer / bool / ...
        bool isPointer = false;
        if (match(TypeOfVar::_operator, "*")) {
            advance(); // '*'
            isPointer = true;
        }

        // теперь должен идти идентификатор
        if (!match(TypeOfVar::_identifier)) {
            throw std::runtime_error("Expected identifier after type keyword");
        }
        Token ident = advance(); // например, "is_prime"

        // Если дальше '(' => function
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
                              const Token &funcIdent) {
        advance(); // съедаем '('

        std::string fullType;
        if (!mods.empty()) {
            fullType = mods + " ";
        }
        fullType += typeKw.name;  // "static bool"
        if (isPointer) fullType += "*"; // "static bool*"

        ASTNode funcNode(ASTNodeType::FunctionDecl, fullType);

        ASTNode funcName(ASTNodeType::Identifier, funcIdent.name);
        funcNode.addChild(funcName);

        // парсим параметры, если не сразу ')'
        if (!match(TypeOfVar::_separator, ")")) {
            parseParameterList(funcNode);
        }

        if (!match(TypeOfVar::_separator, ")")) {
            throw std::runtime_error("Expected ')' in function declaration");
        }
        advance(); // съедаем ')'

        // потом парсим тело (block)
        ASTNode blockNode = parseBlock();
        funcNode.addChild(blockNode);

        return funcNode;
    }

    // param_list -> param (',' param)*
    void parseParameterList(ASTNode &funcNode) {
        parseOneParameter(funcNode);

        while (match(TypeOfVar::_separator, ",")) {
            advance(); // ','
            parseOneParameter(funcNode);
        }
    }

    // param -> (mods?) type(' *')? ident
    void parseOneParameter(ASTNode &funcNode) {
        // возможно, static/const
        std::string mods = parseModifiers();
        if (!isTypeKeyword()) {
            throw std::runtime_error("Expected type keyword in parameter list");
        }
        Token typeKw = advance(); // bool / integer / float / ...
        bool isPtr = false;
        if (match(TypeOfVar::_operator, "*")) {
            advance();
            isPtr = true;
        }

        if (!match(TypeOfVar::_identifier)) {
            throw std::runtime_error("Expected identifier after type in parameter");
        }
        Token paramIdent = advance();

        // склеиваем
        std::string fullType = (mods.empty()? "" : mods+" ") + typeKw.name;
        if (isPtr) fullType += "*";

        ASTNode paramNode(ASTNodeType::Parameter, fullType);

        ASTNode paramName(ASTNodeType::Identifier, paramIdent.name);
        paramNode.addChild(paramName);

        funcNode.addChild(paramNode);
    }

    // var_decl -> (mods?) type(' *')? ident arrayDims? ('=' assignmentExpr)? ';'
    ASTNode parseVarDecl(const std::string &mods,
                         const Token &typeKw,
                         bool isPointer,
                         const Token &ident) {
        // например, fullType = "static bool"
        std::string fullType = (mods.empty() ? "" : (mods + " "))
                               + typeKw.name
                               + (isPointer ? "*" : "");

        ASTNode varDecl(ASTNodeType::VarDecl, fullType);

        ASTNode varName(ASTNodeType::Identifier, ident.name);

        varDecl.addChild(varName);


        // Сразу после чтения ident мы смотрим arrayDims
        parseArrayDims(varDecl);

        // теперь смотрим, есть ли '='
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
    //        std::cout << "parseVarDecl found varName='" << ident.name << "', type='" << typeKw.name << "', isArray=" << "..." << "\n";
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

        // Попытка парсить var_decl
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
                    throw std::runtime_error("Expected identifier after type in statement");
                }
                Token ident = advance();

                // запретим '(' => function decl in block
                if (match(TypeOfVar::_separator, "(")) {
                    throw std::runtime_error("Function declaration not allowed inside block");
                }

                return parseVarDecl(mods, typeKw, isPtr, ident);
            } else {
                // если не тип, откатываемся
                position = oldPos;
            }
        }

        // если не var_decl, значит это expression-statement
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
        if (match(TypeOfVar::_separator, ";")) {
            return ASTNode(ASTNodeType::Literal, "no_init");
        }

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
                return parseVarDeclNoSemicolon(mods, typeKw, isPtr, ident);
            } else {
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
                                    const Token &ident) {
        std::string fullType = (mods.empty()? "" : mods + " ")
                               + typeKw.name
                               + (isPointer?"*":"");

        ASTNode varDecl(ASTNodeType::VarDecl, fullType);

        ASTNode varName(ASTNodeType::Identifier, ident.name);
        varDecl.addChild(varName);

        // опять же, проверяем размерность массива
        parseArrayDims(varName);

        if (match(TypeOfVar::_operator, "=")) {
            advance(); // '='
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
            advance(); // ';'
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
                advance(); // оператор
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
        while (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string op = currentToken().name;
            if (op == "==" || op == "!=") {
                Token opTok = advance();
                ASTNode right = parseComparison();
                ASTNode binOp(ASTNodeType::BinaryOp, opTok.name);
                binOp.addChild(left);
                binOp.addChild(right);
                left = binOp;
            } else {
                break;
            }
        }
        return left;
    }

    // comparison -> term ( ('<' | '>' | '<=' | '>=') term )*
    ASTNode parseComparison() {
        ASTNode left = parseTerm();
        while (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string op = currentToken().name;
            if (op == "<" || op == ">" || op == "<=" || op == ">=") {
                Token opTok = advance();
                ASTNode right = parseTerm();
                ASTNode binOp(ASTNodeType::BinaryOp, opTok.name);
                binOp.addChild(left);
                binOp.addChild(right);
                left = binOp;
            } else {
                break;
            }
        }
        return left;
    }

    // term -> factor ( ('+' | '-') factor )*
    ASTNode parseTerm() {
        ASTNode left = parseFactor();
        while (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string op = currentToken().name;
            if (op == "+" || op == "-") {
                Token opTok = advance();
                ASTNode right = parseFactor();
                ASTNode binOp(ASTNodeType::BinaryOp, opTok.name);
                binOp.addChild(left);
                binOp.addChild(right);
                left = binOp;
            } else {
                break;
            }
        }
        return left;
    }

    // factor -> unary ( ('*' | '/') unary )*
    ASTNode parseFactor() {
        ASTNode left = parseUnary();
        while (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string op = currentToken().name;
            if (op == "*" || op == "/") {
                Token opTok = advance();
                ASTNode right = parseUnary();
                ASTNode binOp(ASTNodeType::BinaryOp, opTok.name);
                binOp.addChild(left);
                binOp.addChild(right);
                left = binOp;
            } else {
                break;
            }
        }
        return left;
    }

    // unary -> ( '!' | '-' ) unary | primary
    ASTNode parseUnary() {
        if (!isAtEnd() && match(TypeOfVar::_operator)) {
            std::string op = currentToken().name;
            if (op == "!" || op == "-") {
                Token opTok = advance();
                ASTNode right = parseUnary();
                ASTNode unOp(ASTNodeType::BinaryOp, opTok.name);
                unOp.addChild(right);
                return unOp;
            }
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

            // возможно, вызов функции
            if (match(TypeOfVar::_separator, "(")) {
                baseNode = parseFunctionCall(baseNode);
            }

            // возможно, индексирование is_prime[i][j]...
            while (match(TypeOfVar::_separator, "[")) {
                baseNode = parseIndexAccess(baseNode);
            }

            return baseNode;
        }
        // Добавим проверку:
        // primary -> 'new' type(' *')? '[' assignmentExpr ']'
        if (match(TypeOfVar::_keyword, "new")) {
            advance(); // съедаем "new"

            // теперь ждём ключевое слово типа (integer/float/bool/и т.п.)
            if (!isTypeKeyword()) {
                throw std::runtime_error("Expected type after 'new'");
            }
            Token typeKw = advance();  // например, "integer"

            bool isPtr = false;
            if (match(TypeOfVar::_operator, "*")) {
                advance();
                isPtr = true;
            }

            // теперь обязательно '['
            if (!match(TypeOfVar::_separator, "[")) {
                throw std::runtime_error("Expected '[' after 'new <type>'");
            }
            advance(); // '['

            // парсим выражение внутри скобок
            ASTNode sizeExpr = parseAssignmentExpr();

            // и ']'
            if (!match(TypeOfVar::_separator, "]")) {
                throw std::runtime_error("Expected ']' after new <type>[expr]");
            }
            advance(); // ']'

            // Собираем конечный узел: BinaryOp(value="new")

            // Ребёнок 0 — тип (например, Literal("integer") или "integer*")
            std::string fullType = typeKw.name;
            if (isPtr) {
                fullType += "*";
            }

            ASTNode newNode(ASTNodeType::BinaryOp, "new");

            ASTNode typeNode(ASTNodeType::Literal, fullType);
            newNode.addChild(typeNode);

            // Теперь делаем узел BinaryOp(value="arrayDim") как "пакет" для sizeExpr,
            // чтобы семантика массивов (размерность) была одинаковой, как при статических объявлениях.
            ASTNode arrDim(ASTNodeType::BinaryOp, "arrayDim");
            arrDim.addChild(sizeExpr);

            newNode.addChild(arrDim);

            return newNode;
        }


        throw std::runtime_error("Expected primary expression, got: " + currentToken().name);
    }

    // parseFunctionCall( baseIdent ) -> '(' (assignmentExpr (',' assignmentExpr)*)? ')'
    ASTNode parseFunctionCall(ASTNode baseIdent) {
        advance(); // '('
        ASTNode callNode(ASTNodeType::BinaryOp, "call");
        callNode.addChild(baseIdent);

        if (!match(TypeOfVar::_separator, ")")) {
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
