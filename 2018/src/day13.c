#include "aoc.h"

#define SIZE 150
#define CARTS 20

// Set to '1' to enable printing of the grid every step
#define DEBUG 0


// Macros implementing transformations to and from characters '><v^' and '-|/\+' to enums
#define TO_DIR(c) (c == '>' ? East : c == '<' ? West : c == 'v' ? South : North)
#define TO_CART(c) (c == East ? '>' : c == West ? '<' : c == North ? '^' : 'v')
#define TO_TRACK(c) (c == '|' || c == '-' ? Straight : c == '/' ? Forward : c == '\\' ? Back : Cross)

// Macros implementing cart movement, along with the static 'TRACK_DIRS'
// These are ordered in the order of which applies first
#define GET_DX(dir) ((dir) == West ? -1 : (dir) == East ? 1 : 0)
#define GET_DY(dir) ((dir) == North ? -1 : (dir) == South ? 1 : 0)
#define GET_DIR(track, dir, turn) ((track) == Cross ? (((dir) + (turn) + NumDirs) % NumDirs) : TRACK_DIRS[(dir) + NumDirs * (track)])
#define GET_TURN(track, turn) ((track) == Cross ? ((turn) == Right ? Left : (turn) + 1) : (turn));


typedef enum { Left = -1, Straight = 0, Right = 1, NumTurns } turn_t;
typedef enum { North, East, South, West, NumDirs } dir_t;
typedef enum { Line, Forward, Back, Cross, NumSimpleTracks = Cross } track_t;

typedef struct {
    int x, y;
    bool alive;
    dir_t dir;
    turn_t turn;
} cart_t;


static dir_t TRACK_DIRS[NumDirs * NumSimpleTracks] = {
    North, East, South, West, // '-' or '|'
    East, North, West, South, // '/'
    West, South, East, North, // '\'
};

void cart_init(cart_t* cart, int* carts, int x, int y, dir_t dir);
int cart_order(const void* a_ptr, const void* b_ptr);

void print(char** grid, cart_t* carts, int iter);


main {
    char** grid = (char**) malloc(sizeof(char*) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        grid[i] = (char*) malloc(sizeof(char) * (SIZE + 1)); // +1 for '\0'
        grid[0][SIZE] = '\0';
        
        fgets(grid[i], SIZE + 1, stdin);
        
        assert('\n' == getc(stdin) || i == SIZE - 1, "reading input line %d", i);
    }

    // Extract cart locations
    cart_t* carts = (cart_t*) malloc(sizeof(cart_t) * CARTS);
    int cart = 0;

    memset(carts, 0, sizeof(cart_t) * CARTS);

    for (int x = 0; x < SIZE; x++) {
        for (int y = 0; y < SIZE; y++) {
            char* at = &grid[y][x];
            switch (*at) {
                case '>':
                case '<':
                    cart_init(&carts[cart], &cart, x, y, *at == '>' ? East : West);
                    *at = '-';
                    break;
                case 'v':
                case '^':
                    cart_init(&carts[cart], &cart, x, y, *at == 'v' ? South : North);
                    *at = '|';
                    break;
            }
        }
    }

    bool part1 = false;
    for (int t = 0;; t++) {

        // Carts need to be sorted deterministically
        qsort(carts, CARTS, sizeof(cart_t), cart_order);

        for (int i = 0; i < CARTS; i++) {
            cart_t* cart = &carts[i];

            if (!cart->alive) continue;

            // Move the cart based on it's last direction first
            cart->x += GET_DX(cart->dir);
            cart->y += GET_DY(cart->dir);

            // Then compute the track under the cart, and based on that, it's previous direction, and turn, compute the new direction and turn
            char track_c = grid[cart->y][cart->x];
            track_t track = TO_TRACK(track_c);

            cart->dir = GET_DIR(track, cart->dir, cart->turn);
            cart->turn = GET_TURN(track, cart->turn);

            // "instantly remove the two crashing carts the moment any crash occurs."
            // This means multi-crashes are not possible, and a three way may occur, which keeps one cart going
            // So after moving, we compare with any previous carts, and invalidate them
            for (int j = 0; j < CARTS; j++) {
                cart_t* prev = &carts[j];
                if (i != j && prev->alive && prev->x == cart->x && prev->y == cart->y) { // Explosion
                    if (!part1) {
                        part1 = true;
                        println("Part 1: %d,%d", cart->x, cart->y);
                    }
                    
                    prev->alive = false;
                    cart->alive = false;
                }
            }
        }

        cart_t* solo = NULL;
        for (int i = 0; i < CARTS; i++) {
            if (carts[i].alive) {
                if (solo != NULL) {
                    solo = NULL;
                    break;
                }
                else solo = &carts[i];
            }
        }

        if (solo != NULL) {
            println("Part 2: %d,%d", solo->x, solo->y);
            break;
        }

        if (DEBUG) {
            print(grid, carts, t);
        }
    }
}

void cart_init(cart_t* cart, int* carts, int x, int y, dir_t dir) {
    cart->x = x;
    cart->y = y;
    cart->dir = dir;
    cart->turn = Left;
    cart->alive = true;
    assert(++*carts < CARTS, "Too many carts: %d >= CARTS", *carts);
}


int cart_order(const void* a_ptr, const void* b_ptr) {
    cart_t* a = (cart_t*) a_ptr;
    cart_t* b = (cart_t*) b_ptr;

    return a->y != b->y
        ? a->y - b->y
        : a->x - b->x;
}

void print(char** grid, cart_t* carts, int iter) {
    println("\n\n=== Iter %d ===\n\n", iter);

    char printable_grid[SIZE][SIZE + 1];

    for (int i = 0; i < SIZE; i++) {
        memcpy(printable_grid[i], grid[i], SIZE + 1);
        printable_grid[i][SIZE] = '\0';
    }
    for (int i = 0; i < CARTS; i++) {
        cart_t* cart = &carts[i];
        if (cart->alive) {
            printable_grid[cart->y][cart->x] = TO_CART(cart->dir);
        }
    }
    for (int i = 0; i < SIZE; i++) {
        printf("%03d : ", i);
        println("%s", printable_grid[i]);
    }
}