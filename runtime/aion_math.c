/*
 * Aion Standard Library — math module
 *
 * Functions are prefixed with `aion_math_` and exposed to Aion code
 * as `math.<name>` after `import aion.math;`.
 */

#include <math.h>

double aion_math_sqrt(double x) {
    return sqrt(x);
}

double aion_math_abs(double x) {
    return fabs(x);
}

double aion_math_pow(double base, double exp) {
    return pow(base, exp);
}

double aion_math_sin(double x) {
    return sin(x);
}

double aion_math_cos(double x) {
    return cos(x);
}

double aion_math_floor(double x) {
    return floor(x);
}

double aion_math_ceil(double x) {
    return ceil(x);
}

long long aion_math_max(long long a, long long b) {
    return a > b ? a : b;
}

long long aion_math_min(long long a, long long b) {
    return a < b ? a : b;
}
