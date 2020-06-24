int putchar(int);

void print_uint(int const i)
{
    if (i == 0) {
        return;
    }
    print_uint(i / 10);
    putchar('0' + i % 10);
}

int fib(int const i)
{
    if (i <= 1) {
        return 1;
    }
    return fib(i - 1) + fib(i - 2);
}

void _start()
{
    print_uint(fib(20));
}
