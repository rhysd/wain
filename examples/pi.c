// original: https://crypto.stanford.edu/pbc/notes/pi/code.html

int putchar(int);

void print_4d(int i) {
    int d = 10000;
    while (d > 1) {
        int j = (i % d) / (d / 10);
        putchar('0' + j);
        d /= 10;
    }
}

void _start() {
    int r[2800 + 1];
    int i, k;
    int b, d;
    int c = 0;

    for (i = 0; i < 2800; i++) {
        r[i] = 2000;
    }

    for (k = 2800; k > 0; k -= 14) {
        d = 0;

        i = k;
        for (;;) {
            d += r[i] * 10000;
            b = 2 * i - 1;

            r[i] = d % b;
            d /= b;
            i--;
            if (i == 0)
                break;
            d *= i;
        }
        print_4d(c + d / 10000); // %.4d
        c = d % 10000;
    }
}
