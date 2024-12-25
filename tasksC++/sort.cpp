#include <iostream>
#include <cstring>

typedef int integer;

void sort(integer* sp, integer le n) {
    for (integer i = 1; i < len; i++) {
        integer key = sp[i];
        integer j = i - 1;
        while (j >= 0 && sp[j] > key) {
            sp[j + 1] = sp[j];
            j--;
        }
        sp[j + 1] = key;
    }
}