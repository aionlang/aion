#include <stdio.h>
#include <stdint.h>

int main(void) {
    int64_t a = 0;
    while (a <= 100000000) {
        a += 1;
    }
    printf("%lld\n", (long long)a);
    return 0;
}
