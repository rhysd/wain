int putchar(int);

#define BOARD_SIZE 8
typedef int Board[BOARD_SIZE][BOARD_SIZE];

void print_board(Board board, unsigned const size)
{
    for (int row = 0; row < size; row++) {
        for (int col = 0; col < size; col++) {
            putchar(' ');
            putchar(board[row][col] ? 'Q' : '.');
        }
        putchar('\n');
    }
}

int is_safe(Board board, unsigned const size, unsigned const row, unsigned const col)
{
    for (unsigned r = 0; r < row; r++) {
        if (board[r][col]) {
            return 0;
        }
        if (col + r >= row && board[r][col - (row - r)]) {
            return 0;
        }
        if (col + row - r < size && board[r][col + row - r]) {
            return 0;
        }
    }
    return 1;
}

int solve_row(Board board, unsigned const size, unsigned const row, unsigned const queens)
{
    if (queens >= size) {
        return 1;
    }
    if (row >= size) {
        return 0;
    }

    for (unsigned col = 0; col < size; col++) {
        if (!is_safe(board, size, row, col)) {
            continue;
        }

        board[row][col] = 1;

        if (solve_row(board, size, row+1, queens+1)) {
            return 1;
        }

        board[row][col] = 0; // backtrack
    }

    return 0;
}

int solve(Board board, unsigned const size)
{
    return solve_row(board, size, 0, 0);
}

void put_str(char const* s)
{
    while (*s != '\0') {
        putchar(*s);
        s++;
    }
    putchar('\n');
}

void _start()
{
    Board board;
    unsigned const size = (unsigned) __builtin_sqrt(sizeof(board) / sizeof(int));

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            board[i][j] = 0;
        }
    }

    if (!solve(board, size)) {
        put_str("No answer");
        return;
    }

    print_board(board, size);
}
