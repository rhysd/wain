// Guessing game for getchar() example
// https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html

int putchar(int);
int getchar(void);

void print_uint(unsigned i)
{
    if (i == 0) return;
    print_uint(i / 10);
    putchar(i % 10 + '0');
}

void print(char const* s)
{
    while (*s != '\0') {
        putchar(*s);
        s++;
    }
}

unsigned get_input(void)
{
    unsigned i = 0;

    print("Please input your guess in 1..100\n");
    for (;;) {
        int const c = getchar();
        if (c < 0 || c == '\n') {
            if (i < 1 || 100 < i) {
                print("\nInput is invalid\n");
                return get_input();
            }
            return i;
        }
        if (c < '0' || '9' < c) {
            print("\nInput is invalid\n");
            return get_input();
        }
        i = i * 10 + (c - '0');
    }
}

void _start(void)
{
    // Chosen by rand(1..100) in Ruby. Guaranteed to be random.
    // https://xkcd.com/221/
    unsigned const secret = 75;
    print("Guess the number!\n\n");

    for (unsigned count = 1;;count++) {
        unsigned const i = get_input();
        if (i < secret) {
            print("Too small!\n\n");
        } else if (i > secret) {
            print("Too big!\n\n");
        } else {
            print("\nYou win! Tries: ");
            print_uint(count);
            putchar('\n');
            return;
        }
    }
}
