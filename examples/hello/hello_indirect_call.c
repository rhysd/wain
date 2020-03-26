int putchar(int i);

void putc1(char c) {
    putchar(c);
}

void putc2(char c) {
    putchar(c);
}

void putc3(char c) {
    putchar(c);
}

void putc4(char c) {
    putchar(c);
}

void _start() {
    char s[] = "Hello, world\n";
    void (*fs[])(char) = {putc1, putc2, putc3, putc4};

    int i = 0;
    while (s[i] != '\0') {
        fs[i % 4](s[i]);
        i++;
    }
}
