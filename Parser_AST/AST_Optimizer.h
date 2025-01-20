#pragma once
#include "AST.h"
#include <optional>
#include <string>
#include <iostream>

/**
 * Класс оптимизатора, который обходит AST и выполняет
 * 1) Constant Folding
 * 2) Constant Propagation (упрощённую)
 * 3) Dead Code Elimination (упрощённую)
 */
class ASTOptimizer {
public:
    /**
     * Запускает оптимизацию: рекурсивно обходим всё дерево.
     */
    void optimize(ASTNode &root) {
        optimizeNode(root);
    }

private:

    /**
     * Рекурсивно обходим узел и его детей:
     *  - Сначала обходим детей, чтобы там можно было
     *    уже всё упростить,
     *  - Затем пытаемся упростить сам узел (constant folding и т.д.)
     *  - Пытаемся вырезать "мёртвые" ветки if (false) {...}
     */
    void optimizeNode(ASTNode &node) {
        // 1) Сначала обходим детей рекурсивно
        for (auto &child : node.children) {
            optimizeNode(child);
        }

        // 2) Пытаемся сделать локальные оптимизации
        switch (node.type) {
            case ASTNodeType::BinaryOp:
                tryConstantFolding(node);
                break;

            case ASTNodeType::IfStmt:
                tryDeadCodeElimination(node);
                break;

            default:
                break;
        }
    }

    /**
     * Пример Constant Folding (простейший):
     * Если узел - BinaryOp("+"/"-"/"*" "/"), и оба ребёнка Literal,
     * вычислим результат на этапе компиляции и заменим этот узел
     * на Literal(result).
     */
    void tryConstantFolding(ASTNode &node) {
        if (node.type != ASTNodeType::BinaryOp) return;
        if (node.children.size() < 2) return; // унарный минус? пропустим

        const std::string &op = node.value;
        ASTNode &left  = node.children[0];
        ASTNode &right = node.children[1];

        // Проверяем, Literal ли левый и правый
        if (left.type == ASTNodeType::Literal && right.type == ASTNodeType::Literal) {
            // Пытаемся преобразовать в число
            // (здесь считаем, что это integer;
            //  если это float, нужно аналогично, но аккуратнее)
            try {
                int leftVal  = std::stoi(left.value);
                int rightVal = std::stoi(right.value);
                int result = 0;
                bool canFold = true;

                if (op == "+") {
                    result = leftVal + rightVal;
                } else if (op == "-") {
                    result = leftVal - rightVal;
                } else if (op == "*") {
                    result = leftVal * rightVal;
                } else if (op == "/") {
                    if (rightVal == 0) {
                        canFold = false; // нельзя делить на 0
                    } else {
                        result = leftVal / rightVal;
                    }
                } else {
                    // можно добавить % и т.д.
                    canFold = false;
                }

                if (canFold) {
                    // Заменяем node -> Literal(result)
                    node.type = ASTNodeType::Literal;
                    node.value = std::to_string(result);
                    // Удаляем детей
                    node.children.clear();
                }
            }
            catch (const std::exception &) {
                // Если std::stoi не получилось, игнорируем
            }
        }
        // если унарный минус (node.children.size() == 1),
        // можно тоже обработать.
    }

    /**
     * Пример Dead Code Elimination:
     * Если ifStmt -> children[0] = cond, children[1] = thenBlock, (children[2] = elseBlock?)
     * и cond - Literal("0") => false,
     *    тогда убираем "thenBlock", заменяем ifStmt -> elseBlock
     * и cond - Literal("1") => true,
     *    тогда убираем elseBlock, заменяем ifStmt -> thenBlock
     */
    void tryDeadCodeElimination(ASTNode &node) {
        if (node.type != ASTNodeType::IfStmt) return;
        if (node.children.empty()) return; // must have cond + thenBlock
        ASTNode &condNode = node.children[0];
        // "then" = node.children[1], "else" = (optional) node.children[2]

        if (condNode.type == ASTNodeType::Literal) {
            // Пытаемся понять, 0 или 1
            // (если у вас bool true/false, можно это тоже проверять)
            int condVal = 0;
            try {
                condVal = std::stoi(condNode.value); // 1,0
            } catch (...) {
                // ignore
                return;
            }
            // condVal==0 => if(false)
            if (condVal == 0) {
                // => выполнить elseBlock, если есть, иначе ничего
                if (node.children.size() >= 3) {
                    // заменяем ifStmt нодом elseBlock
                    ASTNode elseBlock = node.children[2];
                    node = elseBlock;
                    // (просто присваиваем node = elseBlock,
                    //  но нужно быть аккуратным:
                    //  тогда теряется node.type=IfStmt.
                    //  В реальном проекте иногда надо
                    //  copyBlock(node, elseBlock),
                    //  но для простоты можно так)
                } else {
                    // нет elseBlock => удаляем весь ifStmt,
                    //  заменяем на пустой?
                    node.type = ASTNodeType::Block;
                    node.value = "{}";
                    node.children.clear();
                }
            }
                // condVal!=0 => if(true)
            else {
                // => выполнить thenBlock
                if (node.children.size() >= 2) {
                    ASTNode thenBlock = node.children[1];
                    node = thenBlock;
                } else {
                    // нет thenBlock?!
                    node.type = ASTNodeType::Block;
                    node.value = "{}";
                    node.children.clear();
                }
            }
        }
    }
};

