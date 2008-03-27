#ifndef Agpl__Interfaces__C_H
#define Agpl__Interfaces__C_H

extern "C" {

typedef int    int_array[];
typedef double double_array[];

int at (int row, int col, int width);
// Returns the index in a 1-d array organized row-first
// The array is width cols wide
// ******* 0-based *******

}

#endif
