int main(void) {
    int value = 7;

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
