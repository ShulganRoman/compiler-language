#include <iostream>
#include <cstring>

typedef int integer;

void sort(integer* sp, integer len) {
    for (integer i = 1; i < len; i=i+1) {
        integer key = sp[i];
        integer j = i - 1;
        while (j >= 0 && sp[j] > key) {
            sp[j + 1] = sp[j];
            j=j-1;
        }
        sp[j + 1] = key;
    }
}