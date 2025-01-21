cmake_minimum_required(VERSION 3.28)
project(compiler_language)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs
        support
        core
        irreader
        orcjit
        nativecodegen
        # Добавьте другие компоненты по необходимости
)

# Добавить исполняемый файл и исходные файлы проекта
add_executable(compiler_language
        main.cpp
        Lexer/Lexer.h
        Parser_AST/AST.h
        SemanticAnalyzer/SemanticAnalyzer.h
        SemanticAnalyzer/SymbolTable.h
        LLVM/CodeGenerator.h
        LLVM/CodeGenerator.cpp
        BytecodeGen/InstructionMy.h
        BytecodeGen/BytecodeGenerator.h
        BytecodeGen/BytecodeProgramMy.h
        VM.h
        BytecodeGen/BytecodeSymbolTable.h
        Parser_AST/AST_Optimizer.h
        tasksC++/test.h
)

# Связать проект с библиотеками LLVM
target_link_libraries(compiler_language ${llvm_libs})
