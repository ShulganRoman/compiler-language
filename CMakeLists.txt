cmake_minimum_required(VERSION 3.28)
project(compiler_language)

set(CMAKE_CXX_STANDARD 14)

include_directories(.)

add_executable(compiler_language
        main.cpp
        Lexer.h
)
