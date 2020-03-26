int putchar(int i);

char hw[] = "Hello, world\n";
const unsigned int hw_size = sizeof(hw) / sizeof(char) - 1;

void _start() {
    for (int i = 0; i < hw_size; i++) {
        putchar(hw[i]);
    }
}
