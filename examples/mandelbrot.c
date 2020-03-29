// C translation of https://github.com/crystal-lang/crystal/blob/master/samples/mandelbrot.cr

int putchar(int);

void print_density(double d)
{
    if (d > 8) {
        putchar(' ');
    } else if (d > 4) {
        putchar('.');
    } else if (d > 2) {
        putchar('*');
    } else {
        putchar('+');
    }
}

unsigned mandelconverge(double real, double imag)
{
    double const creal = real;
    double const cimag = imag;
    unsigned iters = 0;
    while (iters <= 255 && real * real + imag * imag < 4) {
        double r = real * real - imag * imag + creal;
        double i = 2 * real * imag + cimag;
        real = r;
        imag = i;
        iters++;
    }
    return iters;
}

void mandel(double realstart, double imagstart, double realmag, double imagmag)
{
    double const xmin = realstart;
    double const xmax = realstart + realmag * 78;
    double const xstep = realmag;
    double const ymin = imagstart;
    double const ymax = imagstart + imagmag * 40;
    double const ystep = imagmag;

    for (double y = ymin; y < ymax; y += ystep) {
        for (double x = xmin; x < xmax; x += xstep) {
            print_density(mandelconverge(x, y));
        }
        putchar('\n');
    }
}

void _start()
{
    mandel(-2.3, -1.3, 0.05, 0.07);
}
