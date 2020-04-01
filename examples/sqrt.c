int putchar(int);
double fabs(double);

void print_digits(unsigned i) {
    if (i == 0) {
        return;
    }
    print_digits(i / 10);
    putchar('0' + i % 10);
}

void print_uint(unsigned i)
{
    if (i == 0) {
        putchar('0');
    } else {
        print_digits(i);
    }
}

void print_float(double f)
{
    if (f < 0) {
        putchar('-');
        f = -f;
    }

    print_uint((unsigned) f);
    putchar('.');

    f = f - (int) f;
    if (f == 0.0) {
        putchar('0');
        return;
    }

    while (f > 0.0000001) {
        f *= 10;
        putchar('0' + (int) f);
        f = f - (int) f;
    }
}

void print_str(char const* s)
{
    while (*s != '\0') {
        putchar(*s);
        s++;
    }
}

double my_sqrt(double x)
{
    double const diff = 0.000000001;
    double guess = 1.0;
    while(fabs(guess * guess - x) >= diff){
        guess = (x / guess + guess) / 2;
    }
    return guess;
}

void _start()
{
    print_str("sqrt(2)=");
    print_float(my_sqrt(2.0));
    putchar('\n');

    print_str("sqrt(3)=");
    print_float(my_sqrt(3.0));
    putchar('\n');

    print_str("sqrt(10)=");
    print_float(my_sqrt(10.0));
    putchar('\n');

    print_str("sqrt(100)=");
    print_float(my_sqrt(100.0));
    putchar('\n');
}
