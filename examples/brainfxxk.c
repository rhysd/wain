// https://en.wikipedia.org/wiki/Brainfuck

#include <stdint.h>

int putchar(int);
int getchar();

struct VM {
    uint8_t tape[30000];
    uint8_t *head;
    char const* prog;
    unsigned prog_size;
    unsigned pc;
};

void init_vm(struct VM *const vm, char const* const prog, unsigned const size)
{
    for (unsigned i = 0; i < sizeof(vm->tape); i++) {
        vm->tape[i] = 0;
    }
    vm->head = &vm->tape[0];
    vm->prog = prog;
    vm->prog_size = size;
    vm->pc = 0;
}

void step(struct VM *const vm)
{
    switch (vm->prog[vm->pc]) {
        case '>':
            vm->head++;
            break;
        case '<':
            vm->head--;
            break;
        case '+':
            (*vm->head)++;
            break;
        case '-':
            (*vm->head)--;
            break;
        case '.':
            putchar(*vm->head);
            break;
        case ',':
            *vm->head = getchar();
            break;
        case '[':
            if (*vm->head == 0) {
                unsigned stack = 0;
                for (;;) {
                    vm->pc++;
                    unsigned const op = vm->prog[vm->pc];
                    if (op == '[') {
                        stack++;
                    } else if (op == ']') {
                        if (stack == 0) {
                            break;
                        } else {
                            stack--;
                        }
                    }
                }
            }
            break;
        case ']':
            if (*vm->head != 0) {
                unsigned stack = 0;
                for (;;) {
                    vm->pc--;
                    unsigned const op = vm->prog[vm->pc];
                    if (op == ']') {
                        stack++;
                    } else if (op == '[') {
                        if (stack == 0) {
                            break;
                        } else {
                            stack--;
                        }
                    }
                }
            }
            break;
        default:
            break;
    }
    vm->pc++;
}

int done(struct VM *vm)
{
    return vm->pc >= vm->prog_size;
}

void _start()
{
    struct VM vm;
    char program[] = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    unsigned const size = sizeof(program) / sizeof(char);

    init_vm(&vm, program, size);

    while (!done(&vm)) {
        step(&vm);
    }
}
