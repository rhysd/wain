// Translated from https://github.com/crystal-lang/crystal/blob/master/samples/nbodies.cr

int putchar(int);

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

static double const PI = 3.1415926535897932384626433;
static double const SOLAR_MASS = 4 * PI * PI;
static double const DAYS_PER_YEAR = 365.24;

struct planet {
    double x;
    double y;
    double z;
    double vx;
    double vy;
    double vz;
    double mass;
};

struct planet init_planet(double x, double y, double z, double vx, double vy, double vz, double mass)
{
    struct planet p = {
        x,
        y,
        z,
        vx * DAYS_PER_YEAR,
        vy * DAYS_PER_YEAR,
        vz * DAYS_PER_YEAR,
        mass * SOLAR_MASS
    };
    return p;
}

void planet_move_from_i(
    struct planet *const b,
    struct planet *const bodies,
    unsigned const nbodies,
    double const dt,
    unsigned i
) {
    while (i < nbodies) {
        struct planet *const b2 = bodies + i;
        double const dx = b->x - b2->x;
        double const dy = b->y - b2->y;
        double const dz = b->z - b2->z;

        // Clang emits f64.sqrt on __builtin_sqrt() call
        double const distance = __builtin_sqrt(dx * dx + dy * dy + dz * dz);
        double const mag = dt / (distance * distance * distance);
        double const b_mass_mag = b->mass * mag;
        double const b2_mass_mag = b2->mass * mag;

        b->vx -= dx * b2_mass_mag;
        b->vy -= dy * b2_mass_mag;
        b->vz -= dz * b2_mass_mag;
        b2->vx += dx * b_mass_mag;
        b2->vy += dy * b_mass_mag;
        b2->vz += dz * b_mass_mag;

        i++;
    }

    b->x += dt * b->vx;
    b->y += dt * b->vy;
    b->z += dt * b->vz;
}

double energy(struct planet const* const bodies, unsigned const nbodies)
{
    double e = 0.0;

    for (unsigned i = 0; i < nbodies; i++) {
        struct planet const* const b = bodies + i;
        e += 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
        for (unsigned j = i + 1; j < nbodies; j++) {
            struct planet const* const b2 = bodies + j;
            double const dx = b->x - b2->x;
            double const dy = b->y - b2->y;
            double const dz = b->z - b2->z;
            double const distance = __builtin_sqrt(dx * dx + dy * dy + dz * dz);
            e -= (b->mass * b2->mass) / distance;
        }
    }

    return e;
}

void offset_momentum(struct planet *const bodies, unsigned const nbodies)
{
    double px = 0.0;
    double py = 0.0;
    double pz = 0.0;

    for (unsigned i = 0; i < nbodies; i++) {
        struct planet *const b = bodies + i;
        double const m = b->mass;
        px += b->vx * m;
        py += b->vy * m;
        pz += b->vz * m;
    }

    struct planet *const b = bodies + 0;
    b->vx = -px / SOLAR_MASS;
    b->vy = -py / SOLAR_MASS;
    b->vz = -pz / SOLAR_MASS;
}

void _start()
{
    struct planet bodies[] = {
        // sun
        init_planet(
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0
        ),
        // jupiter
        init_planet(
            4.84143144246472090e+0,
            -1.16032004402742839e+0,
            -1.03622044471123109e-1,
            1.66007664274403694e-3,
            7.69901118419740425e-3,
            -6.90460016972063023e-5,
            9.54791938424326609e-4
        ),
        // saturn
        init_planet(
            8.34336671824457987e+0,
            4.12479856412430479e+0,
            -4.03523417114321381e-1,
            -2.76742510726862411e-3,
            4.99852801234917238e-3,
            2.30417297573763929e-5,
            2.85885980666130812e-4
        ),
        // uranus
        init_planet(
            1.28943695621391310e+1,
            -1.51111514016986312e+1,
            -2.23307578892655734e-1,
            2.96460137564761618e-3,
            2.37847173959480950e-3,
            -2.96589568540237556e-5,
            4.36624404335156298e-5
        ),
        // neptune
        init_planet(
            1.53796971148509165e+1,
            -2.59193146099879641e+1,
            1.79258772950371181e-1,
            2.68067772490389322e-3,
            1.62824170038242295e-3,
            -9.51592254519715870e-5,
            5.15138902046611451e-5
        ),
    };
    unsigned const nbodies = sizeof(bodies) / sizeof(struct planet);

    unsigned const n = 2000;

    offset_momentum(bodies, nbodies);
    print_float(energy(bodies, nbodies));
    putchar('\n');

    double const dt = 0.01;

    for (unsigned i = 0; i < n; i++) {
        for (unsigned j = 0; j < nbodies; j++) {
            struct planet *b = bodies + j;
            planet_move_from_i(b, bodies, nbodies, dt, j+1);
        }
    }

    print_float(energy(bodies, nbodies));
    putchar('\n');
}
