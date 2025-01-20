integer factorial_calc(integer num){
    if (num == 0){
        return 1;
    } else { return num * factorial_calc(num - 1);}
}

integer main(){
    print(factorial_calc(6));
    return 0;
}