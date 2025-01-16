#include <iostream>
#include <cstring>

typedef int integer;

const integer MAX_SIZE = 10000;
integer global_array[MAX_SIZE];

void sort(integer len) {
    for (integer i = 1; i < len; i = i + 1) {
        integer key = global_array[i];
        integer j = i - 1;
        while (j >= 0 && global_array[j] > key) {
            global_array[j + 1] = global_array[j];
            j = j - 1;
        }
        global_array[j + 1] = key;
    }
}