// Based on http://capm-network.com/?tag=C%E8%A8%80%E8%AA%9E%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0-BM%E6%B3%95

int putchar(int);

#define NULL ((void *) 0)
#define BM_TABLE_SIZE 256

// Add _ at end to avoid -Wincompatible-library-redeclaration
int strlen_(char const* p)
{
    int c = 0;
    while (*p++ != '\0') {
        c++;
    }
    return c;
}

void print_str(char const* p)
{
    while (*p != '\0') {
        putchar(*p);
        p++;
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

void print_int(int i)
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
}

static void bm_table_init(int* table, const char* pattern, int ptn_len)
{
    int cnt = 0;

    for (cnt = 0; cnt < BM_TABLE_SIZE; cnt++) {
        table[cnt] = ptn_len;
    }

    for (cnt = 0; cnt < ptn_len; cnt++) {
        table[(int) pattern[cnt]] = ptn_len - cnt - 1;
    }

    print_str("[table]  : default: step=");
    print_int(ptn_len);
    putchar('\n');
    for (cnt = 0; cnt < BM_TABLE_SIZE; cnt++) {
        if (table[cnt] != ptn_len) {
            print_str("         : char=");
            putchar(cnt);
            print_str(": table[");
            print_int(cnt);
            print_str("]: step=");
            print_int((int)table[cnt]);
            putchar('\n');
        }
    }
}

static void print_compare_process(const char* text, const char* pattern, int i, int j)
{
    int cnt = 0;

    print_str("-----------------------------------\n");
    print_str("[compare]:(text i=");
    print_int(i);
    print_str(")(pattern j=");
    print_int(j);
    print_str(")\n");
    print_str(" text    :");
    print_str(text);
    putchar('\n');

    print_str(" pattern :");
    for (cnt = 0; cnt < (i - j); cnt++) {
        putchar(' ');
    }
    print_str(pattern);
    putchar('\n');

    print_str("         :");
    for (cnt = 0; cnt < i; cnt++) {
        putchar(' ');
    }
    print_str("^\n");
}

static int next_step(int* table, char target, int remain)
{
    if (table[(int) target] > remain) {
        return table[(int)target];
    } else {
        return remain;
    }
}

char* bm_search(const char* text, const char* pattern)
{
    int table[BM_TABLE_SIZE];
    int txt_len = 0;
    int ptn_len = 0;
    int i = 0; // Position of text to compare
    int j = 0; // Position of pattern to compare

    ptn_len = strlen_(pattern);
    txt_len = strlen_(text);

    bm_table_init(table, pattern, ptn_len);

    i = j = ptn_len - 1;
    while ((i < txt_len) && (j >= 0)) {
        print_compare_process(text, pattern, i, j);

        if (text[i] != pattern[j]) {
            i += next_step(table, text[i], (ptn_len - j));
            j = ptn_len - 1;
        } else {
            j--;
            i--;
        }
    }

    if (j < 0) {
        return (char*) text + (i + 1);
    }
    return NULL;
}

void _start()
{
    char* text = "GCTCACTGAGCGCTCGT";
    char* pattern = "GCTCG";
    char* cp = NULL;

    print_str("[text]   :");
    print_str(text);
    putchar('\n');
    print_str("[pattern]:");
    print_str(pattern);
    putchar('\n');

    cp = bm_search(text, pattern);
    if (cp == NULL) {
        print_str("[result] : not found\n");
    } else {
        print_str("[result] : found\n");
        print_str("         : text=");
        print_str(cp);
        putchar('\n');
    }
}
