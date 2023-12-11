
#include <stdio.h>


int main(int argc, char **argv)
{
    int x, y;
    x = 10;
    y = ++x + --x;

    printf("x = %d, y = %d\n", x, y);
}
