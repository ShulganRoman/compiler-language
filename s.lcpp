integer global_array[1000];

integer main(){
    integer num = 1000;
    integer MAX_SIZE = 1000;
    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        global_array[i] = num;
        num = num-1;
    }
    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        print(global_array[i]);
    }
    print(121212);
    for (integer i = 1; i < MAX_SIZE; i = i + 1) {
        integer key = global_array[i];
        integer j = i - 1;
        integer val = global_array[j];
        while (j >= 0 && val > key) {
            integer tmp = global_array[j];
            global_array[j + 1] = tmp;
            j = j - 1;
        }
        global_array[j + 1] = key;
    }

    for (integer i = 0; i < MAX_SIZE; i=i+1) {
        print(global_array[i]);
    }
    return 0;
}