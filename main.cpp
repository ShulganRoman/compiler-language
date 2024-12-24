#include <iostream>

template<typename T>
void func(T&& value) {
    std::cout << value << std::endl;
}

template<typename T, typename... Args>
void func(T&& first, Args&&... rest) {
    std::cout << first << ' ';
    func(rest...);
}

int main() {
    func(10, 20, 30, "Hello", 3.14, std::string("hello string"), true, false);
}
