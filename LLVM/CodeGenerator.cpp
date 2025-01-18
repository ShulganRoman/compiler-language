#include "CodeGenerator.h"
#include <stdexcept>
#include <iostream>

// Для отладки (проверка IR)
#include <llvm/IR/Verifier.h>


CodeGenerator::CodeGenerator(SymbolTable &symTable)
        : context(), module(std::make_unique<llvm::Module>("my_module", context)),
          builder(context), symbolTable(symTable)
{
}

std::unique_ptr<llvm::Module> CodeGenerator::generate(ASTNode &root) {
    // Предположим, что root.type == Program
    if (root.type != ASTNodeType::Program) {
        throw std::runtime_error("Top-level AST node is not Program!");
    }

    // Обходим всех детей (VarDecl, FunctionDecl ...)
    for (auto &child : root.children) {
        generateTopLevelDecl(child);
    }

    // Дополнительно можно проверить модуль на корректность
    if (llvm::verifyModule(*module, &llvm::errs())) {
        throw std::runtime_error("Generated LLVM IR is invalid!");
    }

    return std::move(module);
}

void CodeGenerator::generateTopLevelDecl(ASTNode &node) {
    switch (node.type) {
        case ASTNodeType::FunctionDecl:
            createFunction(node);
            break;
        case ASTNodeType::VarDecl:
            // Глобальная переменная
            generateVarDecl(node);
            break;
        default:
            throw std::runtime_error("Unknown top-level declaration in code generator");
    }
}

llvm::Function *CodeGenerator::createFunction(ASTNode &funcNode) {
    // funcNode.value = "integer" (return type) или "void" и т.д.
    // funcNode.children[0] = Identifier (имя функции)
    // funcNode.children[1..n-1] = Parameters
    // funcNode.children[n-1] = body (Block)

    if (funcNode.children.size() < 2) {
        throw std::runtime_error("FunctionDecl node has too few children");
    }

    ASTNode &funcNameNode = funcNode.children[0];
    std::string funcName = funcNameNode.value;
    std::string returnTypeName = funcNode.value; // например "integer", "float", "void" ...

    // Тип возврата
    llvm::Type *returnType = getLLVMType(returnTypeName);
    if (!returnType) {
        throw std::runtime_error("Unsupported return type: " + returnTypeName);
    }

    // Собираем типы параметров
    std::vector<llvm::Type*> paramTypes;
    for (size_t i = 1; i < funcNode.children.size() - 1; ++i) {
        ASTNode &param = funcNode.children[i];
        // param.value = "integer" / "float" / ...
        llvm::Type *pType = getLLVMType(param.value);
        if (!pType) {
            throw std::runtime_error("Unsupported param type: " + param.value);
        }
        paramTypes.push_back(pType);
    }

    // Создаём функцию
    llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, paramTypes, /*isVarArg=*/false);
    llvm::Function *function = llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            funcName,
            module.get()
    );

    // Именуем параметры (необязательно, но удобно)
    size_t idx = 0;
    for (auto &arg : function->args()) {
        ASTNode &param = funcNode.children[idx + 1];
        ASTNode &paramNameNode = param.children[0]; // Identifier
        arg.setName(paramNameNode.value);
        idx++;
    }

    // Создаём тело функции
    generateFunctionBody(funcNode, function);

    return function;
}

void CodeGenerator::generateFunctionBody(ASTNode &funcNode, llvm::Function *function) {
    // Создаём базовый блок для тела
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(bb);

    // Очищаем namedValues для локальных переменных
    namedValues.clear();

    // Создаём alloca для каждого параметра и делаем store
    {
        size_t idx = 0;
        for (auto &arg : function->args()) {
            ASTNode &param = funcNode.children[idx + 1]; // Param node
            std::string paramName = arg.getName().str();

            llvm::AllocaInst* alloc = createEntryBlockAlloca(function, arg.getType(), paramName);
            builder.CreateStore(&arg, alloc);

            // Сохраняем в map
            namedValues[paramName] = alloc;
            idx++;
        }
    }

    // Последний child у функции — это Block
    ASTNode &bodyNode = funcNode.children.back();
    if (bodyNode.type != ASTNodeType::Block) {
        throw std::runtime_error("Function body is not a Block node");
    }

    // Генерируем код блока
    generateBlock(bodyNode);

    // Если мы не встретили return, и функция не void — проблема
    llvm::Type* retTy = function->getReturnType();
    if (!retTy->isVoidTy()) {
        // Можно вставить return 0 (или return undef) для int-функций, в учебных целях
        if (!builder.GetInsertBlock()->getTerminator()) {
            // вставим return
            if (retTy->isIntegerTy()) {
                builder.CreateRet(llvm::ConstantInt::get(retTy, 0));
            } else if (retTy->isFloatTy()) {
                builder.CreateRet(llvm::ConstantFP::get(retTy, 0.0));
            } else {
                // или ret void (но это не совсем корректно)
                builder.CreateRet(llvm::UndefValue::get(retTy));
            }
        }
    } else {
        // Если функция void, а return не вставлен, вставим ret void
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateRetVoid();
        }
    }
}

void CodeGenerator::generateBlock(const ASTNode &blockNode) {
    // blockNode.children — это список statement
    for (auto &stmt : blockNode.children) {
        generateStatement(stmt);
    }
}

llvm::Value *CodeGenerator::generateStatement(const ASTNode &node) {
    switch (node.type) {
        case ASTNodeType::VarDecl:
            generateVarDecl(node);
            return nullptr; // statement не возвращает значение
        case ASTNodeType::ReturnStmt: {
            // return (expr?)
            if (node.children.empty()) {
                // return void
                builder.CreateRetVoid();
            } else {
                llvm::Value *retVal = generateExpression(node.children[0]);
                builder.CreateRet(retVal);
            }
            // После return обычно код дальше не генерируется
            return nullptr;
        }
        case ASTNodeType::IfStmt:
            // node.children[0] = condition expr
            // node.children[1] = then block
            // node.children[2] = else block (optional)
        {
            llvm::Value *condVal = generateExpression(node.children[0]);
            // condVal в LLVM обычно i1 (bool)
            // Если ваше bool = i8 или i32, надо привести (CreateICmpNE ...)
            if (!condVal->getType()->isIntegerTy(1)) {
                // Пытаемся привести. Например, если это i32:
                if (condVal->getType()->isIntegerTy()) {
                    condVal = builder.CreateICmpNE(condVal,
                                                   llvm::ConstantInt::get(condVal->getType(), 0), "boolcast");
                } else {
                    throw std::runtime_error("If condition not i1 and can't cast easily");
                }
            }
            llvm::Function *func = builder.GetInsertBlock()->getParent();

            llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", func);
            llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");

            // Проверка
            if (node.children.size() < 2) {
                throw std::runtime_error("IfStmt missing then-block");
            }
            bool hasElse = (node.children.size() > 2);

            builder.CreateCondBr(condVal, thenBB, hasElse ? elseBB : mergeBB);

            // then block
            builder.SetInsertPoint(thenBB);
            generateBlock(node.children[1]);
            if (!builder.GetInsertBlock()->getTerminator()) {
                builder.CreateBr(mergeBB);
            }

            if (hasElse) {
                elseBB->insertInto(func);
                builder.SetInsertPoint(elseBB);
                generateBlock(node.children[2]);
                if (!builder.GetInsertBlock()->getTerminator()) {
                    builder.CreateBr(mergeBB);
                }
            }

            mergeBB->insertInto(func);
            builder.SetInsertPoint(mergeBB);
        }
            return nullptr;
        case ASTNodeType::WhileStmt:
            generateWhileStatement(node);
            return nullptr;

        case ASTNodeType::ForStmt:
            generateForStatement(node);
            return nullptr;
        default:
            // Иначе считаем это expression statement
            return generateExpression(node);
    }
}

//llvm::Value* CodeGenerator::generateArrayIndexPointer(const ASTNode &binaryOpNode) {
//    // 1) Проверяем, что это действительно node.value == "[]"
//    if (binaryOpNode.type != ASTNodeType::BinaryOp || binaryOpNode.value != "[]") {
//        throw std::runtime_error("generateArrayIndexPointer called on non-'[]' node");
//    }
//    // Должно быть 2 ребёнка: [0] = base (Identifier или что-то ещё), [1] = index
//    if (binaryOpNode.children.size() != 2) {
//        throw std::runtime_error("'[]' node must have exactly 2 children: base and index");
//    }
//
//    // 2) Генерируем код для base (может вернуть глобальную переменную, alloca, ...
//    llvm::Value *baseVal = generateExpression(binaryOpNode.children[0]);
//    // 3) Генерируем код для index (должно быть i32)
//    llvm::Value *idxVal  = generateExpression(binaryOpNode.children[1]);
//
//    // Проверим, что idxVal — i32 (для упрощения)
//    if (!idxVal->getType()->isIntegerTy(32)) {
//        throw std::runtime_error("Array index must be i32 in this simplified example.");
//    }
//
//    // 4) Если baseVal — это GlobalVariable (array i32 [N])
//    if (auto *gvar = llvm::dyn_cast<llvm::GlobalVariable>(baseVal)) {
//        // gvar->getValueType() должно быть i32 [N]
//        llvm::Type *arrTy = gvar->getValueType();
//        if (!arrTy->isArrayTy()) {
//            throw std::runtime_error("Global variable is not an array type (expected i32[]).");
//        }
//
//        llvm::Value *zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
//
//        // Создаём GEP: GEP arrTy, gvar, {0, idxVal}
//        //  - Первый индекс 0, чтобы сначала перейти к элементу 0 массива
//        //  - Второй индекс — собственно idxVal
//        llvm::Value *elemPtr = builder.CreateGEP(
//                arrTy,  // i32 [N]
//                gvar,
//                { zero, idxVal },
//                "arrayidx"
//        );
//        return elemPtr;  // pointer to i32
//    }
//
//    // 5) Если baseVal — это alloca i32 [N] (локальный массив)
//    if (auto *allocInst = llvm::dyn_cast<llvm::AllocaInst>(baseVal)) {
//        // allocaInst->getAllocatedType() должно быть i32 [N]
//        llvm::Type *allocatedTy = allocInst->getAllocatedType();
//        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(allocatedTy)) {
//            llvm::Value *zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
//
//            llvm::Value *elemPtr = builder.CreateGEP(
//                    arrTy,       // i32 [N]
//                    allocInst,   // the alloca
//                    { zero, idxVal },
//                    "arrayidx"
//            );
//            return elemPtr;
//        }
//        // Если же это alloca pointer i32*, вам нужно другой GEP, типа:
//        // builder.CreateGEP(i32, allocInst, {idxVal})  ... Но это надо отдельно реализовать.
//        throw std::runtime_error("Local array is not array type or pointer type in this example.");
//    }
//    baseVal->getType()->print(llvm::errs());
//    std::cout<<"\n";
//
//    // 6) Иначе baseVal не является ни GlobalVariable, ни alloca array
//    throw std::runtime_error("generateArrayIndexPointer: base is not recognized as global/local array.");
//}

llvm::Value* CodeGenerator::generateArrayIndexPointer(const ASTNode &binaryOpNode) {
    // 1) Проверяем, что это действительно "[]"
    if (binaryOpNode.type != ASTNodeType::BinaryOp || binaryOpNode.value != "[]") {
        throw std::runtime_error("generateArrayIndexPointer called on non-'[]' node");
    }
    // Нужно 2 ребёнка: base, index
    if (binaryOpNode.children.size() != 2) {
        throw std::runtime_error("'[]' node must have exactly 2 children: base, index");
    }

    // 2) Генерируем код для base
    llvm::Value *baseVal = generateExpression(binaryOpNode.children[0]);
    // 3) Генерируем код для index (i32)
    llvm::Value *idxVal  = generateExpression(binaryOpNode.children[1]);
    if (!idxVal->getType()->isIntegerTy(32)) {
        throw std::runtime_error("Array index must be i32 in this simplified example.");
    }

    // Проверим, это глоб. переменная массива, или локальная alloca массива
    // (не "загруженная" value, а сам pointer)
    if (auto *gvar = llvm::dyn_cast<llvm::GlobalVariable>(baseVal)) {
        // gvar->getValueType() => [N x T]
        llvm::Type *arrTy = gvar->getValueType();
        if (!arrTy->isArrayTy()) {
            throw std::runtime_error(
                    "Global variable is not an array type."
            );
        }
        llvm::Value *zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
        return builder.CreateGEP(
                arrTy,  // [N x T]
                gvar,
                { zero, idxVal },
                "arrayidx"
        );
    }

    if (auto *allocInst = llvm::dyn_cast<llvm::AllocaInst>(baseVal)) {
        // allocaInst->getAllocatedType() => [N x T]
        llvm::Type *allocatedTy = allocInst->getAllocatedType();
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(allocatedTy)) {
            llvm::Value *zero = llvm::ConstantInt::get(builder.getInt32Ty(), 0);
            return builder.CreateGEP(
                    arrTy,       // [N x T]
                    allocInst,
                    { zero, idxVal },
                    "arrayidx"
            );
        }
        throw std::runtime_error(
                "Local array is not array type or pointer type in this example."
        );
    }

    // Если дошли сюда — значит baseVal не globalVar, не alloca (т.е. мы случайно "загрузили" массив,
    // или это что-то ещё)

    throw std::runtime_error(
            "generateArrayIndexPointer: base is not recognized as global/local array."
    );
}

uint64_t CodeGenerator::evaluateArraySize(const ASTNode &sizeNode) {
    if (sizeNode.type == ASTNodeType::Literal) {
        // Например, "10000"
        return std::stoull(sizeNode.value);
    }
    else if (sizeNode.type == ASTNodeType::Identifier) {
        // Например, "MAX_SIZE". Нужно найти, есть ли в SymbolTable символ?
        // Или, если у вас тоже VarDecl (value='integer') => Literal(10000) =>
        // на этапе codegen надо уметь найти константу.
        // Либо, если пока упрощённо => assume "MAX_SIZE" = 10000
        if (sizeNode.value == "MAX_SIZE") {
            return 10000; // жёстко
        }
        throw std::runtime_error("Can't resolve array size from identifier: " + sizeNode.value);
    }
    else {
        throw std::runtime_error("Array dimension must be literal or known identifier");
    }
}


void CodeGenerator::generateVarDecl(const ASTNode &node) {
    // node.value = "integer"
    // node.children[0] = Identifier
    // возможно, есть инициализация (BinaryOp('='))
    // Либо это глобальная переменная, если вызывается из top-level
    // Либо локальная, если вызывается из function-body.

    std::string varName = node.children[0].value; // "global_array"
    std::string typeName = node.value;           // "integer"

    // Проверим, нет ли "arrayDim" в детях
    // Например, children[1] = BinaryOp("arrayDim") -> внутри child.children[0] = Identifier("MAX_SIZE") или Literal("10000")
    bool isArray = false;
    size_t arraySize = 0;

    for (size_t i = 1; i < node.children.size(); i++) {
        const ASTNode &c = node.children[i];
        if (c.type == ASTNodeType::BinaryOp && c.value == "arrayDim") {
            // Значит у нас массив
            isArray = true;
            // c.children[0] — это размер (либо Literal("10000"), либо Identifier("MAX_SIZE"))
            // Нужно получить compile-time число
            arraySize = evaluateArraySize(c.children[0]);
            // Можно прервать цикл, если только одна размерность
            break;
        }
    }

    // Определяем, глобальная ли это переменная (top-level)
    // Признак: если builder.GetInsertBlock() == nullptr => значит мы вне функции
    if (!builder.GetInsertBlock()) {
        // Мы в глобальной области
        if (isArray) {
            // Создаём ArrayType i32 [arraySize]
            auto *arrTy = llvm::ArrayType::get(llvm::Type::getInt32Ty(context), arraySize);
            // Инициализируем нулями
            auto *initVal = llvm::Constant::getNullValue(arrTy);

            auto *gvar = new llvm::GlobalVariable(
                    *module,
                    arrTy,
                    false,
                    llvm::GlobalValue::ExternalLinkage,
                    initVal,
                    varName
            );
            // global_array теперь типа i32 [N]
        } else {
            // Обычная переменная
            llvm::Type *llvmTy = getLLVMType(typeName);
            auto *initVal = llvm::ConstantInt::get(llvmTy, 0);
            auto *gvar = new llvm::GlobalVariable(*module, llvmTy, false, llvm::GlobalValue::ExternalLinkage, initVal,
                                                  varName);
        }
    } else {
        // Локальная переменная: создаём alloca
        llvm::Type *llvmTy = getLLVMType(typeName); // правильно ли тут так сделать? (ранее ошибка "llvmTy не найдено (который ниже)")
        llvm::Function *func = builder.GetInsertBlock()->getParent();
        llvm::AllocaInst *alloc = createEntryBlockAlloca(func, llvmTy, varName);

        // Если есть инициализация
        for (size_t i = 1; i < node.children.size(); ++i) {
            const ASTNode &child = node.children[i];
            // Это может быть BinaryOp("=") для инициализации
            if (child.type == ASTNodeType::BinaryOp && child.value == "=") {
                // child.children[0] = Identifier (тот же varName?)
                // child.children[1] = expr
                llvm::Value *rhs = generateExpression(child.children[1]);
                builder.CreateStore(rhs, alloc);
            }
        }
        namedValues[varName] = alloc;
    }
}

llvm::Value *CodeGenerator::generateExpression(const ASTNode &node) {
    switch (node.type) {

        // --------------------------------------------------------------------------
        // 1) Литералы (int, float, bool, ...)
        // --------------------------------------------------------------------------
        case ASTNodeType::Literal: {
            const std::string &val = node.value;
            if (val == "true") {
                return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1);
            } else if (val == "false") {
                return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0);
            } else if (val.find('.') != std::string::npos) {
                // float/double
                double d = std::stod(val);
                return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), d);
            } else {
                // int
                int n = std::stoi(val);
                return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), n);
            }
        }

            // --------------------------------------------------------------------------
            // 2) Идентификатор (переменная) — может быть глобальный/локальный массив или скаляр
            // --------------------------------------------------------------------------
        case ASTNodeType::Identifier: {
            std::string varName = node.value;

            // 2.1) Ищем среди локальных переменных (alloca)
            llvm::Value *alloc = namedValues[varName];
            if (!alloc) {
                // 2.2) Если нет в локальных — пробуем глобальную
                llvm::GlobalVariable *gvar = module->getGlobalVariable(varName);
                if (!gvar) {
                    throw std::runtime_error("Unknown variable: " + varName);
                }
                alloc = gvar; // пусть alloc укажет на глобальную переменную
            }

            // Проверяем, является ли alloc глобалкой:
            if (auto *globalVar = llvm::dyn_cast<llvm::GlobalVariable>(alloc)) {
                llvm::Type *gvarTy = globalVar->getValueType(); // [N x T] или скаляр (i32, i1, etc.)
                if (gvarTy->isArrayTy()) {
                    // Глобальный массив => возвращаем сам globalVar (указатель на [N x T]), без Load
                    return globalVar;
                } else {
                    // Глобальная «обычная» переменная => делаем Load (получаем её значение)
                    return builder.CreateLoad(gvarTy, globalVar, varName + "_val");
                }
            }

            // Если это локальная (alloca)
            if (auto *allocInst = llvm::dyn_cast<llvm::AllocaInst>(alloc)) {
                llvm::Type *allTy = allocInst->getAllocatedType();
                if (allTy->isArrayTy()) {
                    // Локальный массив => возвращаем сам alloca (указатель на [N x T])
                    return allocInst;
                } else {
                    // Локальная скалярная переменная => Load
                    return builder.CreateLoad(allTy, allocInst, varName + "_val");
                }
            }

            throw std::runtime_error("Expected global/local variable for identifier: " + varName);
        }

            // --------------------------------------------------------------------------
            // 3) Бинарные операторы: =, +, -, *, /, <, >, [] и т.д.
            // --------------------------------------------------------------------------
        case ASTNodeType::BinaryOp: {
            const std::string &op = node.value;

            // ---------------- 3.1) Присваивание ( = ) ----------------
            if (op == "=") {
                // LHS: Identifier или '[]'
                // RHS: expression
                const ASTNode &lhsNode = node.children[0];
                llvm::Value *lhsPtr = nullptr; // pointer, куда store

                // (A) Присваиваем простому идентификатору?
                if (lhsNode.type == ASTNodeType::Identifier) {
                    lhsPtr = namedValues[lhsNode.value];
                    if (!lhsPtr) {
                        lhsPtr = module->getGlobalVariable(lhsNode.value);
                    }
                    if (!lhsPtr) {
                        throw std::runtime_error(
                                "Unknown variable in assignment: " + lhsNode.value
                        );
                    }
                }
                    // (B) Присваиваем в array[index]?
                else if (lhsNode.type == ASTNodeType::BinaryOp && lhsNode.value == "[]") {
                    llvm::Value *elemPtr = generateArrayIndexPointer(lhsNode); // pointer to element
                    lhsPtr = elemPtr;
                } else {
                    throw std::runtime_error(
                            "Assignment lhs must be identifier or array[index]"
                    );
                }

                // RHS
                llvm::Value *rhsVal = generateExpression(node.children[1]);

                // Store
                if (auto *gvar = llvm::dyn_cast<llvm::GlobalVariable>(lhsPtr)) {
                    // если lhsPtr — глобальная скалярная переменная
                    builder.CreateStore(rhsVal, gvar);
                } else {
                    // иначе pointer to element, или локальная alloca
                    builder.CreateStore(rhsVal, lhsPtr);
                }
                return rhsVal;
            }

            // ---------------- 3.2) Арифметика (+, -, *, /) (упрощённая i32) ----------------
            if (op == "+" || op == "-" || op == "*" || op == "/") {
                llvm::Value *lhsVal = generateExpression(node.children[0]);
                llvm::Value *rhsVal = generateExpression(node.children[1]);
                if (!lhsVal->getType()->isIntegerTy(32) ||
                    !rhsVal->getType()->isIntegerTy(32)) {
                    throw std::runtime_error("BinaryOp: only i32 supported for + - * / in this example");
                }
                if (op == "+")  return builder.CreateAdd(lhsVal, rhsVal, "addtmp");
                if (op == "-")  return builder.CreateSub(lhsVal, rhsVal, "subtmp");
                if (op == "*")  return builder.CreateMul(lhsVal, rhsVal, "multmp");
                // "/"
                return builder.CreateSDiv(lhsVal, rhsVal, "divtmp");
            }

            // ---------------- 3.3) Сравнения (==, !=, <, >, <=, >=) (упрощённо i32) ----------------
            if (op == "==" || op == "!=" ||
                op == "<"  || op == ">"  ||
                op == "<=" || op == ">=") {
                llvm::Value *lhsVal = generateExpression(node.children[0]);
                llvm::Value *rhsVal = generateExpression(node.children[1]);
                if (!lhsVal->getType()->isIntegerTy(32) ||
                    !rhsVal->getType()->isIntegerTy(32)) {
                    throw std::runtime_error("Comparison: only i32 supported in this example");
                }
                if (op == "==")  return builder.CreateICmpEQ(lhsVal, rhsVal,  "eqtmp");
                if (op == "!=")  return builder.CreateICmpNE(lhsVal, rhsVal,  "netmp");
                if (op == "<")   return builder.CreateICmpSLT(lhsVal, rhsVal, "lttmp");
                if (op == ">")   return builder.CreateICmpSGT(lhsVal, rhsVal, "gttmp");
                if (op == "<=")  return builder.CreateICmpSLE(lhsVal, rhsVal, "letmp");
                /* op == ">=" */ return builder.CreateICmpSGE(lhsVal, rhsVal, "getmp");
            }

            // ---------------- 3.4) Логические (&&, ||) (упрощённо) ----------------
            if (op == "&&" || op == "||") {
                llvm::Value *lhsVal = generateExpression(node.children[0]);
                llvm::Value *rhsVal = generateExpression(node.children[1]);
                auto toBool = [&](llvm::Value *v) {
                    if (v->getType()->isIntegerTy(1)) {
                        return v;
                    }
                    if (v->getType()->isIntegerTy(32)) {
                        return builder.CreateICmpNE(
                                v, llvm::ConstantInt::get(v->getType(), 0),
                                "boolcast"
                        );
                    }
                    throw std::runtime_error("Unsupported type for '&&'/'||', need i1 or i32");
                };
                lhsVal = toBool(lhsVal);
                rhsVal = toBool(rhsVal);
                if (op == "&&") {
                    return builder.CreateAnd(lhsVal, rhsVal, "andtmp");
                } else {
                    return builder.CreateOr(lhsVal, rhsVal, "ortmp");
                }
            }

            // ---------------- 3.5) Вызов функции (call) ----------------
            if (op == "call") {
                const ASTNode &funcNameNode = node.children[0];
                std::string funcName = funcNameNode.value;
                llvm::Function *calleeF = module->getFunction(funcName);
                if (!calleeF) {
                    throw std::runtime_error("Unknown function referenced: " + funcName);
                }
                std::vector<llvm::Value*> argsV;
                for (size_t i = 1; i < node.children.size(); i++) {
                    argsV.push_back(generateExpression(node.children[i]));
                }
                return builder.CreateCall(calleeF, argsV, "calltmp");
            }

            // ---------------- 3.6) Оператор индексирования массива: [] (rvalue) ----------------
            if (op == "[]") {
                // array[index] => GEP => load
                llvm::Value *elemPtr = generateArrayIndexPointer(node);

                auto *int32Ty = llvm::Type::getInt32Ty(context);
                return builder.CreateLoad(int32Ty, elemPtr, false, "elemload");

            }

            throw std::runtime_error("Unsupported binary op: " + op);
        }

        default:
            throw std::runtime_error(
                    "generateExpression: unknown AST node type: "
                    + astNodeTypeToString(node.type)
            );
    }
}


void CodeGenerator::generateWhileStatement(const ASTNode &node) {
    // node.children[0] = условие (expr)
    // node.children[1] = тело (Block)

    // Создаём нужные блоки
    llvm::Function *func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "while.cond", func);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "while.body");
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(context, "while.end");

    // Переход из текущего места (где мы сейчас) в condBB
    builder.CreateBr(condBB);

    // (1) condBB
    builder.SetInsertPoint(condBB);
    // Генерируем условие
    llvm::Value *condVal = generateExpression(node.children[0]);
    // Приводим к i1, если это i32
    if (!condVal->getType()->isIntegerTy(1)) {
        if (condVal->getType()->isIntegerTy(32)) {
            condVal = builder.CreateICmpNE(
                    condVal,
                    llvm::ConstantInt::get(condVal->getType(), 0),
                    "whilecond"
            );
        } else {
            throw std::runtime_error("While condition must be bool/i1 or i32 in this example");
        }
    }
    // Создаём condBr => если condVal == true -> bodyBB, иначе -> endBB
    builder.CreateCondBr(condVal, bodyBB, endBB);

    // (2) bodyBB
    bodyBB->insertInto(func); // вставляем bodyBB в функцию
    builder.SetInsertPoint(bodyBB);
    // генерируем тело (Block)
    generateBlock(node.children[1]);
    // в конце body делаем переход назад в condBB
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(condBB);
    }

    // (3) endBB
    endBB->insertInto(func);
    builder.SetInsertPoint(endBB);
    // далее исполнение пойдёт после цикла
}

void CodeGenerator::generateForStatement(const ASTNode &node) {
    // Похоже, что в вашем AST:
    // node.children[0] = init (statement или 'no_init')
    // node.children[1] = cond (expr или 'true')
    // node.children[2] = step (expr или 'no_step')
    // node.children[3] = body (Block)

    llvm::Function *func = builder.GetInsertBlock()->getParent();

    // создаём блоки
    llvm::BasicBlock *initBB = builder.GetInsertBlock();
    // (инит можно делать сразу в "текущем блоке" - если он пуст,
    //  или создать отдельный bb "for.init", но обычно это не строго нужно)

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(context, "for.cond", func);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(context, "for.body");
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(context, "for.step");
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(context, "for.end");

    // (0) init
    // генерируем statement (например, int i=0)
    if (!(node.children[0].type == ASTNodeType::Literal && node.children[0].value == "no_init")) {
        // если не "no_init", значит нужно реально сгенерировать
        node.children[0];
        generateStatement(node.children[0]);
    }
    // после init делаем br condBB
    builder.CreateBr(condBB);

    // (1) condBB
    builder.SetInsertPoint(condBB);
    // генерируем условие, если не 'true'
    if (!(node.children[1].type == ASTNodeType::Literal && node.children[1].value == "true")) {
        llvm::Value *condVal = generateExpression(node.children[1]);
        // приводим к i1
        if (!condVal->getType()->isIntegerTy(1)) {
            if (condVal->getType()->isIntegerTy(32)) {
                condVal = builder.CreateICmpNE(
                        condVal,
                        llvm::ConstantInt::get(condVal->getType(), 0),
                        "forcond"
                );
            } else {
                throw std::runtime_error("For condition must be bool/i1 or i32 in this example");
            }
        }
        // condBr => if true -> bodyBB, else -> endBB
        builder.CreateCondBr(condVal, bodyBB, endBB);
    } else {
        // если условие == "true", значит всегда идём в body (пока не break)
        builder.CreateBr(bodyBB);
    }

    // (2) bodyBB
    bodyBB->insertInto(func);
    builder.SetInsertPoint(bodyBB);
    // генерируем тело
    generateBlock(node.children[3]);  // block
    // после body => br stepBB (если не был сделан return/break)
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(stepBB);
    }

    // (3) stepBB
    stepBB->insertInto(func);
    builder.SetInsertPoint(stepBB);
    if (!(node.children[2].type == ASTNodeType::Literal && node.children[2].value == "no_step")) {
        // генерируем step как expression statement
        generateExpression(node.children[2]);
    }
    // и переходим снова в condBB
    builder.CreateBr(condBB);

    // (4) endBB
    endBB->insertInto(func);
    builder.SetInsertPoint(endBB);
    // дальше идёт выполнение за пределами цикла
}

llvm::AllocaInst* CodeGenerator::createEntryBlockAlloca(llvm::Function *function, llvm::Type *type, const std::string &varName) {
    // Создаём IRBuilder, который ставит инструкции на вход функции
    llvm::IRBuilder<> tmpB(
            &function->getEntryBlock(),
            function->getEntryBlock().begin()
    );
    return tmpB.CreateAlloca(type, nullptr, varName);
}

llvm::Type *CodeGenerator::getLLVMType(const std::string &typeName) {
    // Очень упрощённый маппинг
    // "integer" -> i32
    // "bool" -> i1
    // "float" -> double (предположим)
    // "void" -> void
    if (typeName.find("void") != std::string::npos) {
        return llvm::Type::getVoidTy(context);
    }
    if (typeName.find("bool") != std::string::npos) {
        return llvm::Type::getInt1Ty(context);
    }
    if (typeName.find("float") != std::string::npos) {
        return llvm::Type::getDoubleTy(context);
    }
    if (typeName.find("integer") != std::string::npos) {
        return llvm::Type::getInt32Ty(context);
    }
    // можно добавить логику для массивов / указателей ...
    return nullptr;
}

