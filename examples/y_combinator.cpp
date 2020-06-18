extern "C" {
int putchar(int);
}

void print_uint(int const i)
{
    if (i == 0) {
        return;
    }
    print_uint(i / 10);
    putchar('0' + i % 10);
}

int fib(int i) {
    auto y = [](auto f) {
        return [f](auto x) {
            return f(f, x);
        };
    };

    auto fib = y([](auto f, int n) -> int {
        return n <= 1 ? 1 : f(f, n - 1) + f(f, n - 2);
    });

    return fib(i);
}

extern "C" {
void _start()
{
    print_uint(fib(10));
}
}
