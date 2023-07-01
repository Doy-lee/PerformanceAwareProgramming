
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>
#include "haversine_stdlib.h"
#include "haversine_stdlib.c"
#include <math.h>
#include "listing_0065_haversine_formula.cpp"

typedef struct Str8FindResult {
    bool     found;
    HAV_Str8 match;
    HAV_Str8 match_to_end_of_buffer;
} Str8FindResult;

Str8FindResult FindFirstCharThatLooksLikeANumber(HAV_Str8 buffer)
{
    Str8FindResult result = {0};
    for (size_t index = 0; !result.found && index < buffer.size; index++) {
        if (HAV_CharIsDigit(buffer.data[index]) || buffer.data[index] == '+' || buffer.data[index] == '-') {
            result.found                  = true;
            result.match                  = (HAV_Str8){.data = buffer.data + index, .size = 1};
            result.match_to_end_of_buffer = (HAV_Str8){.data = result.match.data,   .size = buffer.size - index};
            break;
        }
    }
    return result;
}

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
    if (argc == 3)
        arg_answers = (HAV_Str8){.data = argv[2], .size = strlen(argv[2])};

    HAV_Buffer buffer = HAV_FileRead(arg_json.data);
    if (!HAV_BufferIsValid(buffer))
        return 0;

    size_t pair_count = 0;
    HAV_Str8 json_it  = (HAV_Str8){.data = buffer.data, .size = buffer.size};
    for (;; pair_count++) {
        HAV_Str8BinarySplitResult x0_key = HAV_Str8_BinarySplit(json_it, HAV_STR8("x0"));
        if (!x0_key.rhs.size)
            break;

        Str8FindResult            x0_find_value = FindFirstCharThatLooksLikeANumber(x0_key.rhs);
        HAV_Str8BinarySplitResult x0_value      = HAV_Str8_BinarySplit(x0_find_value.match_to_end_of_buffer, HAV_STR8(","));

        HAV_Str8BinarySplitResult y0_key        = HAV_Str8_BinarySplit(x0_value.rhs, HAV_STR8("y0"));
        Str8FindResult            y0_find_value = FindFirstCharThatLooksLikeANumber(y0_key.rhs);
        HAV_Str8BinarySplitResult y0_value      = HAV_Str8_BinarySplit(y0_find_value.match_to_end_of_buffer, HAV_STR8(","));

        HAV_Str8BinarySplitResult x1_key        = HAV_Str8_BinarySplit(y0_value.rhs, HAV_STR8("x1"));
        Str8FindResult            x1_find_value = FindFirstCharThatLooksLikeANumber(x1_key.rhs);
        HAV_Str8BinarySplitResult x1_value      = HAV_Str8_BinarySplit(x1_find_value.match_to_end_of_buffer, HAV_STR8(","));

        HAV_Str8BinarySplitResult y1_key        = HAV_Str8_BinarySplit(x1_value.rhs, HAV_STR8("y1"));
        Str8FindResult            y1_find_value = FindFirstCharThatLooksLikeANumber(y1_key.rhs);
        HAV_Str8BinarySplitResult y1_value      = HAV_Str8_BinarySplit(y1_find_value.match_to_end_of_buffer, HAV_STR8("}"));

        HAV_PrintLnFmt("{x0: %.*s, y0: %.*s, x1 %.*s, y1: %.*s}",
                       HAV_STR8_FMT(x0_value.lhs),
                       HAV_STR8_FMT(y0_value.lhs),
                       HAV_STR8_FMT(x1_value.lhs),
                       HAV_STR8_FMT(y1_value.lhs));

        json_it = y1_value.rhs;
    }

    size_t input_size           = buffer.size;
    f64 haversine_sum           = 0;
    f64 reference_haversine_sum = 0;
    f64 difference              = 0;
    HAV_PrintLnFmt("Input size: %zu", input_size);
    HAV_PrintLnFmt("Pair count: %zu", pair_count);
    HAV_PrintLnFmt("Haversine sum: %f", haversine_sum);

    HAV_PrintLn(HAV_STR8("Validation: "));
    HAV_PrintLnFmt("Reference sum: %f", reference_haversine_sum);
    HAV_PrintLnFmt("Difference: %f", difference);
    return 0;
}
