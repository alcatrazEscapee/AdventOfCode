// A very basic hash-based set
// Used for situations where we need a resizable, dynamically allocated, set.
// Implemented with a dense hash set, with linear probing. Not the most efficient, but good enough.
// No set removals means we don't have to care about 'graveyard' entries (downside of linear probing).
//
// Uses the following optional #define's:
// - `set_value_t`     : The value type of the set. Must be an integer type, or `set_equal`, `set_hash`, and `set_empty` also need to be defined.
// - `set_equal(a, b)` : An equality function for set elements
// - `set_hash(a)`     : A hash function for set elements
// - `set_free(x)`     : A function that frees values in the set, if needed.
//                     : If the set holds a pointer type, this should no-op on `NULL`, and `set_empty` should be `NULL`
// - `set_empty`       : A sentinel 'empty' value for the set

#ifndef SET_H
#define SET_H

#include <stdlib.h>

#define LOAD_FACTOR 0.75
#define INITIAL_SIZE 64

#ifndef set_value_t
#define set_value_t int
#endif

#ifndef set_empty
#define set_empty (-1)
#endif

#ifndef set_equal
#define set_equal(a, b) ((a) == (b))
#endif

#ifndef set_hash
#define set_hash(a) (a)
#endif

#ifndef set_free
#define set_free(a) ;
#endif

#define set_is_empty(a) (!set_equal(a, set_empty))

typedef struct {
    set_value_t* data; 
    int capacity;
    int len;
} set_t;


static void set_rehash(set_t* set);


set_t* set_new() {
    set_t* set = (set_t*) malloc(sizeof(set_t));

    set->data = (set_value_t*) malloc(sizeof(set_value_t) * INITIAL_SIZE);
    set->capacity = INITIAL_SIZE;
    set->len = 0;

    for (int i = 0; i < INITIAL_SIZE; i++)
        set->data[i] = set_empty;

    return set;
}

/// @brief Puts an value into the set.
/// @returns true if the value was already in the set
bool set_insert(set_t* set, set_value_t value) {
    // In order to guarantee proper set function, the set must have always at least one empty entry
    // This means we need to re-hash the set when inserting at specific sizes
    if (set->len + 1 >= LOAD_FACTOR * set->capacity) set_rehash(set);

    int mask = (set->capacity - 1);
    int index = set_hash(value) & mask;
    set_value_t current_value = set->data[index];

    while (set_is_empty(current_value)) {
        if (set_equal(current_value, value)) {  // Test current value
            set_free(set->data[index]); // Delete the existing value, and replace the new value at this index.
            set->data[index] = value; // This is nicer on the caller of set_insert() than deleting the value they insert
            return true;
        }
        index = (index + 1) & mask;
        current_value = set->data[index];
    }
    // Found a empty value position before a match, there was no matching element
    set->data[index] = value;
    set->len++;
    return false;
}

/// @brief // Checks if a value is present in the set.
/// @return true if the value was already in the set.
bool set_contains(set_t* set, set_value_t value) {
    int mask = (set->capacity - 1);
    int index = set_hash(value) & mask;
    set_value_t current_value = set->data[index];
    
    while (set_is_empty(current_value)) { // The set must always have at least one empty spot - so this is guaranteed to terminate
        if (set_equal(current_value, value)) { // Test current value
            return true; // Value match
        }
        index = (index + 1) & mask;
        current_value = set->data[index];
    }
    return false; // No match
}

/// @brief Clears the set, freeing all associated data
void set_clear(set_t* set) {
    for (int i = 0; i < set->capacity; i++) {
        set_free(set->data[i]);
        set->data[i] = set_empty;
    }
    set->len = 0;
}


// Private Methods

static void set_rehash(set_t* set) {
    // Save a reference to the existing values array
    set_value_t* old_data = set->data;
    int old_capacity = set->capacity;
    int new_capacity = old_capacity << 1;

    // Reallocate the new array
    set->data = (set_value_t*) malloc(sizeof(set_value_t) * new_capacity);

    for (int i = 0; i < new_capacity; i++)
        set->data[i] = set_empty;

    // Set the set to empty
    set->capacity = new_capacity;
    set->len = 0;

    // Insert all old values
    for (int index = 0; index < old_capacity; index++) {
        set_value_t old_value = old_data[index];
        if (!set_is_empty(old_value)) {
            set_insert(set, old_value);
        }
    }

    // Free now-unused old values array
    free(old_data);
}

#undef set_value_t
#undef set_empty
#undef set_equal
#undef set_hash

#undef LOAD_FACTOR
#undef INITIAL_SIZE

#endif