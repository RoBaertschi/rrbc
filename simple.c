int main(void) {
    int value = 7;

    int pain = 2;
    while (pain < 3) {
        pain++;
    }

    switch (value) {
    case 4:
        return 4;
    case 5:
        break;
    case 0:
        return 0;
    default:
        return 255;
    }
    return 1;
}
