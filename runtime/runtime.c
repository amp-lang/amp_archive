#include <stdint.h>
#include <stdio.h>

typedef struct ByteSlice
{
    char *ptr;
    size_t len;
} ByteSlice;

/// @brief Prints a slice to STDOUT.
/// @param slice
void Print(ByteSlice slice)
{
    fwrite(slice.ptr, 1, slice.len, stdout);
}

/// @brief The application entry point.
void Main();

int main()
{
    Main();
    return 0;
}