// https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

int putchar(int);

static int const TRUE = 1;
static int const FALSE = 0;

void print_digits(int const i)
{
    if (i == 0) {
        return;
    }
    print_digits(i / 10);
    putchar('0' + i % 10);
}

void _start()
{
    int nums[101];
    int const size = 100;
    for (int i = 0; i < size; i++) {
        nums[i] = TRUE;
    }

    int const max = (int) __builtin_sqrt((double) size);
    for (int i = 2; i < max; i++) {
        if (nums[i] == FALSE) {
            continue;
        }
        for (int p = i * 2; p < size; p += i) {
            nums[p] = FALSE;
        }
    }

    for (int i = 2; i <= size; i++) {
        if (nums[i] == TRUE) {
            print_digits(i);
            putchar('\n');
        }
    }
}
