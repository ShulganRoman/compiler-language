#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

int main() {
    // Инициализация LLVM
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Создаём LLVM-контекст и модуль
    llvm::LLVMContext Context;
    llvm::Module* Module = new llvm::Module("simple_module", Context);
    llvm::IRBuilder<> Builder(Context);

    // Определяем функцию: int add(int a, int b)
    llvm::FunctionType* AddFuncType = llvm::FunctionType::get(
            Builder.getInt32Ty(), // Возвращаемый тип int
            {Builder.getInt32Ty(), Builder.getInt32Ty()}, // Аргументы: int, int
            false // Не принимает переменное количество аргументов
    );

    llvm::Function* AddFunction = llvm::Function::Create(
            AddFuncType, llvm::Function::ExternalLinkage, "add", Module
    );

    // Создаём блок для тела функции
    llvm::BasicBlock* EntryBlock = llvm::BasicBlock::Create(Context, "entry", AddFunction);
    Builder.SetInsertPoint(EntryBlock);

    // Получаем аргументы функции
    auto Args = AddFunction->args();
    llvm::Function::arg_iterator AI = AddFunction->arg_begin();
    llvm::Value* A = AI++;
    llvm::Value* B = AI;

    // Генерируем код для сложения
    llvm::Value* Sum = Builder.CreateAdd(A, B, "sum");
    Builder.CreateRet(Sum); // Возвращаем результат

    // Компиляция завершена
    Module->print(llvm::errs(), nullptr);

    // Создаём интерпретатор для выполнения
    std::string Error;
    llvm::ExecutionEngine* EE = llvm::EngineBuilder(std::unique_ptr<llvm::Module>(Module))
            .setErrorStr(&Error)
            .setEngineKind(llvm::EngineKind::JIT)
            .create();
    if (!EE) {
        llvm::errs() << "Error creating execution engine: " << Error << "\n";
        return 1;
    }

    // Подготовка аргументов для вызова функции
    std::vector<llvm::GenericValue> ArgsGV(2);
    ArgsGV[0].IntVal = llvm::APInt(32, 7); // Первый аргумент: 7
    ArgsGV[1].IntVal = llvm::APInt(32, 3); // Второй аргумент: 3

    // Выполняем функцию
    llvm::GenericValue Result = EE->runFunction(AddFunction, ArgsGV);
    llvm::outs() << "Result: " << Result.IntVal << "\n";

    return 0;
}
