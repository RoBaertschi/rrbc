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
    return a;
}
