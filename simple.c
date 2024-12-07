int main(void) {
    int a = 0;
    int b = 1;
    a = 2;
    b = 3;
    a = 4;

    loop1:
    if (a >= 20) {
        goto end;
    } else {
        a++;
        goto loop1;
    }
    end:

    for (int i = 0; i < 10; i++) {
    
    }

    return a;
}
