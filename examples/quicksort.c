int putchar(int);

int partition(int *const arr, int lo, int hi)
{
    int const pivot = arr[(lo + hi) / 2];
    for (;;) {
        while (arr[lo] < pivot) {
            lo++;
        }

        while (arr[hi] > pivot) {
            hi--;
        }

        if (lo >= hi) {
            return hi;
        }

        int const tmp = arr[lo];
        arr[lo] = arr[hi];
        arr[hi] = tmp;

        lo++;
        hi--;
    }
}

void quicksort(int *const arr, int lo, int hi)
{
    if (lo < hi) {
        int const i = partition(arr, lo, hi);
        quicksort(arr, lo, i);
        quicksort(arr, i+1, hi);
    }
}

void print_digits(int const i)
{
    if (i == 0) {
        return;
    }
    print_digits(i / 10);
    putchar('0' + i % 10);
}

void puts_int(int i)
{
    if (i < 0) {
        putchar('-');
        i = -i;
    }
    if (i == 0) {
        putchar('0');
    } else {
        print_digits(i);
    }
    putchar('\n');
}

void _start()
{
    int arr[] = { 8, 1, 3, -7, 4, -2, 4, 5, 7, -3, 1, 2, 6, 8, 0, 9, -4, -2, 3 };
    int const size = sizeof(arr) / sizeof(int);
    quicksort(arr, 0, size-1);
    for (int i = 0; i < size; i++) {
        puts_int(arr[i]);
    }
}
