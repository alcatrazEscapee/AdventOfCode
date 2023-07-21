#include "aoc.h"

#define SIZE 1685

// Set to 1 to enable printing the full flow map
// Warning: this is very big (~500 chars wide, ~1800 chars tall) and does not view well in an 80 char wide terminal
// Output to a file instead.
#define DEBUG 0

#define MAX(max, value) if ((value) > (max)) max = (value)
#define MIN(min, value) if ((value) < (min)) min = (value) 


typedef struct {
    char axis;
    int x, y0, y1;
} line_t;

typedef struct {
    char** data;
    int minX;
    int maxX;
    int maxY;
} grid_t;

void flow(grid_t* grid, int x, int y);


main {
    line_t* lines = (line_t*) malloc(sizeof(line_t) * SIZE);
    
    int minX = INT_MAX, maxX = INT_MIN, minY = INT_MAX, maxY = INT_MIN;
    
    for (int i = 0; i < SIZE; i++) {
        char c;
        line_t* line = &lines[i];
        
        assert(scanf("%c=%d, %c=%d..%d\n", &line->axis, &line->x, &c, &line->y0, &line->y1) == 5, "reading input line %d", i);
        assert(line->y1 > line->y0, "all lines are positive");

        if (line->axis == 'x') {
            MIN(minX, line->x);
            MAX(maxX, line->x);
            MIN(minY, line->y0);
            MAX(maxY, line->y1);
        } else {
            MIN(minX, line->y0);
            MAX(maxX, line->y1);
            MIN(minY, line->x);
            MAX(maxY, line->x);
        }
    }

    // Need a little bit of room on the sides for water to flow outside the border
    minX -= 1;
    maxX += 1;
    maxY += 1;

    char** data = (char**) malloc(sizeof(char*) * maxY);
    int width = maxX - minX + 1;
    for (int y = 0; y < maxY; y++) {
        data[y] = (char*) malloc(sizeof(char) * (width + 1));
        data[y][width] = '\0';

        memset(data[y], '.', sizeof(char) * width);
    }

    for (int i = 0; i < SIZE; i++) {
        line_t* line = &lines[i];
        for (int y = line->y0; y <= line->y1; y++) {
            if (line->axis == 'x') {
                data[y][line->x - minX] = '#';
            } else {
                data[line->x][y - minX] = '#';
            }
        }
    }

    data[0][500 - minX] = '+';

    grid_t grid = { .data = data, .minX = minX, .maxX = maxX, .maxY = maxY };
    flow(&grid, 500, 0);

    if (DEBUG) {
        for (int y = 0; y < maxY; y++) {
            println("%04d : %s", y, data[y]);
        }
    }

    int flowing = 0, still = 0;
    for (int x = 0; x < width; x++){
        for (int y = minY; y < maxY; y++) {
            if (data[y][x] == '~') still++;
            if (data[y][x] == '|') flowing++;
        }
    }

    println("Part 1: %d", still + flowing);
    println("Part 2: %d", still);
}


// Called at an (x, y) that is a 'source', and can flow immediately downwards
// Cascades down, fills the next basin, and recursively cascades once overflowing
void flow(grid_t* grid, int x, int y) {

#define AT(x, y) (grid->data[(y)][(x) - grid->minX])

#define IS_WALL(x, y) (AT(x, y) == '#')
#define IS_EMPTY(x, y) (AT(x, y) == '.')
#define IS_STILL(x, y) (AT(x, y) == '~')
#define IS_STABLE(x, y) (IS_STILL(x, y) || IS_WALL(x, y))

    for (y++; y < grid->maxY && !IS_WALL(x, y); y++) {
        if (!IS_STILL(x, y)) {
            AT(x, y) = '|';
        }
    }

    if (y == grid->maxY) return; // Cascaded all the way to the bottom

    // Fill the basin until we spill over something
    for(y--;; y--) {
        // We must've hit something solid.
        // Expand both left and right until we reach and edge, or a precipice
        int left = x - 1, right = x + 1;

        for (; !IS_WALL(left,  y) && IS_STABLE(left,  y + 1); left --) ;
        for (; !IS_WALL(right, y) && IS_STABLE(right, y + 1); right++) ;

        // Based on what we were stopped by (if both were walls), this is either still or flowing water.
        bool still = IS_WALL(left, y) && IS_WALL(right, y);
        for (int dx = left + 1; dx < right; dx++) {
            AT(dx, y) = still ? '~' : '|';
        }

        if (!still) {
            // We won't keep accumulating water upwards if we have flowing water, so break, and cascade downwards

            if (IS_EMPTY(left, y)) { // Left is flowing
                AT(left, y) = '|';
                flow(grid, left, y);
            }

            if (IS_EMPTY(right, y)) { // Right is flowing
                AT(right, y) = '|';
                flow(grid, right, y);
            }

            break;
        }
    }
}