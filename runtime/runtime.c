#include <stdint.h>
#include <stdio.h>

typedef struct Slice
{
    void *ptr;
    int len;
} Slice;

size_t Len(Slice slice)
{
    return slice.len;
}

/// @brief Prints a slice to STDOUT.
/// @param slice
void Print(Slice slice)
{
    fwrite(slice.ptr, 1, slice.len, stdout);
}

void PrintInt(size_t value)
{
    printf("%zu", value);
}

/// @brief The application entry point.
void Main();

int main()
{
    Main();
    return 0;
}