#pragma once
#include "AST.h"
#include <optional>
#include <string>
#include <iostream>

class ASTOptimizer {
public:
    void optimize(ASTNode &root) {
        optimizeNode(root);
    }

private:
    void optimizeNode(ASTNode &node) {
        for (auto &child : node.children) {
            optimizeNode(child);
        }
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

    void tryConstantFolding(ASTNode &node) {
        if (node.type != ASTNodeType::BinaryOp) return;
        if (node.children.size() < 2) return;

        const std::string &op = node.value;
        ASTNode &left  = node.children[0];
        ASTNode &right = node.children[1];

        if (left.type == ASTNodeType::Literal && right.type == ASTNodeType::Literal) {
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
                    canFold = false;
                }

                if (canFold) {
                    // Заменяем node -> Literal(result)
                    node.type = ASTNodeType::Literal;
                    node.value = std::to_string(result);
                    node.children.clear();
                }
            }
            catch (const std::exception &) {
                // Если std::stoi не получилось, игнорируем
            }
        }
    }

    void tryDeadCodeElimination(ASTNode &node) {
        if (node.type != ASTNodeType::IfStmt) return;
        if (node.children.empty()) return;
        ASTNode &condNode = node.children[0];
        // "then" = node.children[1], "else" = (optional) node.children[2]

        if (condNode.type == ASTNodeType::Literal) {
            int condVal = 0;
            try {
                condVal = std::stoi(condNode.value);
            } catch (...) {
                return;
            }
            if (condVal == 0) {
                if (node.children.size() >= 3) {
                    ASTNode elseBlock = node.children[2];
                    node = elseBlock;
                } else {
                    // нет elseBlock => удаляем весь ifStmt,
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
                    node.type = ASTNodeType::Block;
                    node.value = "{}";
                    node.children.clear();
                }
            }
        }
    }
};

