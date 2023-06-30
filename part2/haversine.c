
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>
#include "haversine_stdlib.h"
#include "haversine_stdlib.c"
#include <math.h>
#include "listing_0065_haversine_formula.cpp"

#define PRINT_USAGE HAV_PrintLnFmt("Usage: %s [haversine_input.json] [answers.f64]", argv[0])
int main(int argc, char **argv)
{
    // NOTE: Arg Parsing
    // =========================================================================
    if (argc != 2 && argc != 3) {
        PRINT_USAGE;
        return -1;
    }

    HAV_Str8 arg_json    = {argv[1], strlen(argv[1])};
    HAV_Str8 arg_answers = {0};
    if (argc == 3) {
        arg_answers = {.data = argv[2], .size = strlen(argv[2])};
    }

    size_t input_size = 0;
    size_t pair_count = 0;
    f64 haversine_sum = 0;
    f64 reference_haversine_sum = 0;
    f64 difference = 0;
    HAV_PrintLnFmt("Input size: %zu", input_size);
    HAV_PrintLnFmt("Pair count: %zu", pair_count);
    HAV_PrintLnFmt("Haversine sum: %f", haversine_sum);

    HAV_PrintLn(HAV_STR8("Validation: "));
    HAV_PrintLnFmt("Reference sum: %f", reference_haversine_sum);
    HAV_PrintLnFmt("Difference: %f", difference);
    return 0;
}
