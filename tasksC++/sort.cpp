integer global_array[10000];


integer main(){
    integer MAX_SIZE = 10000;
    for (integer i = 1; i < MAX_SIZE; i = i + 1) {
        integer key = global_array[i];
        integer j = i - 1;
        while (j >= 0 && global_array[j] > key) {
            global_array[j + 1] = global_array[j];
            j = j - 1;
        }
        global_array[j + 1] = key;
    }

    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        global_array[i] = num;
        num = num-1;
    }
    sort();
    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        print(global_array[i]);
    }
    return 0;
}