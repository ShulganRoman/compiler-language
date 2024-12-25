int factorial_calc(int num){
    if (num == 0){
        return 1;
    } else { return num * factorial_calc(num - 1);}
}
