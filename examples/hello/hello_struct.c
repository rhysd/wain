int putchar(int i);

struct HelloWorld {
    char *hello;
    char *world;
};

void print(char *s) {
    while (*s != '\0') {
        putchar(*s);
        s++;
    }
}

void lets_say(struct HelloWorld hw) {
    print(hw.hello);
    print(hw.world);
}

void _start() {
    struct HelloWorld hw;
    hw.hello = "Hello, ";
    hw.world = "world\n";
    lets_say(hw);
}
