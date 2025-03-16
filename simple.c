int i = 0;
int j = 0;
int incr_i(void){
    if (i == 1) {
        i++;
        ++i;
    }
    return 0;
}
int main(void) {
    i++ ? 0 : incr_i();
}
