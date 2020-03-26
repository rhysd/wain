int putchar(int i);

void print(char *s) {
    while (*s != '\0') {
        putchar((int) *s);
        s++;
    }
}

void _start()
{
    print("Hello, world\n");
}
