int main(void) {
    int a = 0;
    int b = 1;
    a = 2;
    b = 3;
    a = 4;

    if (a)
        return b;
    else
        return a;
}
