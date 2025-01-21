язык - like c++, но чуть проще.

Как запустить? Переходим в папку cmake-build-debug, запускаем используя команду (доступно 3 варианта):

# Запустить программу (код в mytest.lcpp) без сохранения байткода
myCompiler mytest.lcpp --run

# Вывести AST, сохранить байткод, но не запускать:
myCompiler mytest.lcpp --ast -o mybytecode.txt

# Показать токены, выполнить, плюс включить оптимизации:
myCompiler mytest.lcpp --tokens --run --O2

Оптимизации - DeadCodeIllumination, ConstantFolding

