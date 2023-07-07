
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>
#include "haversine_stdlib.h"
#include "haversine_stdlib.c"
#include <math.h>

#include "listing_0065_haversine_formula.cpp"
#include "listing_0074_platform_metrics.cpp"

typedef struct ProfilerRecord {
    HAV_Str8 label;
    u64      elapsed_tsc;
    u64      elapsed_tsc_child;
    u64      hits;
} ProfilerRecord;

typedef struct ProfilerAnchor {
    u64      parent_index;
    uint32_t index;
    HAV_Str8 label;
    u64      tsc;
} ProfilerAnchor;

typedef struct Profiler {
    ProfilerRecord records[4096];
    u64            begin_tsc;
    u64            end_tsc;
    u64            parent_index;
} Profiler;

static Profiler g_profiler;

#define Profiler_BeginAnchor(label) Profiler_BeginAnchor_(HAV_STR8(label), __COUNTER__ + 1)
static ProfilerAnchor Profiler_BeginAnchor_(HAV_Str8 label, uint32_t index)
{
    ProfilerAnchor result   = {0};
    result.index            = index;
    result.label            = label;
    result.tsc              = ReadCPUTimer();
    result.parent_index     = g_profiler.parent_index;
    g_profiler.parent_index = index;
    return result;
}

static void Profiler_EndAnchor(ProfilerAnchor anchor)
{
    u64 elapsed_tsc         = ReadCPUTimer() - anchor.tsc;
    ProfilerRecord* record  = g_profiler.records + anchor.index;
    record->elapsed_tsc    += elapsed_tsc;
    record->label           = anchor.label;
    record->hits++;

    ProfilerRecord* parent     = g_profiler.records + anchor.parent_index;
    parent->elapsed_tsc_child += elapsed_tsc;
    g_profiler.parent_index    = anchor.parent_index;
}

static void Profiler_Dump()
{
    u64 total_elapsed_tsc = g_profiler.end_tsc - g_profiler.begin_tsc;
    u64 cpu_frequency = EstimateCPUTimerFreq();
    if (cpu_frequency)
        printf("\nTotal time: %0.4fms (CPU freq %llu)\n", 1000.0 * (f64)total_elapsed_tsc / (f64)cpu_frequency, cpu_frequency);

    for (uint32_t index = 1; index < HAV_ARRAY_UCOUNT(g_profiler.records); index++) {
        ProfilerRecord const *record = g_profiler.records + index;
        if (!record->elapsed_tsc)
            break;

        u64 record_exclusive_tsc = record->elapsed_tsc - record->elapsed_tsc_child;
        f64 percent = total_elapsed_tsc ? (f64)record_exclusive_tsc / (f64)total_elapsed_tsc * 100.0 : 100.0;
        printf("   %.*s[%zu]: %llu (%.2f%%", HAV_STR8_FMT(record->label), record->hits, record_exclusive_tsc, percent);
        if (record->elapsed_tsc_child) {
            f64 percent_w_children = total_elapsed_tsc ? ((f64)record->elapsed_tsc / (f64)total_elapsed_tsc * 100.0) : 100.0;
            printf(", %.2f%% w/children", percent_w_children);
        }
        printf(")\n");
    }
}

typedef struct Str8FindResult {
    bool     found;
    HAV_Str8 match;
    HAV_Str8 match_to_end_of_buffer;
} Str8FindResult;

static Str8FindResult FindFirstCharThatLooksLikeANumber(HAV_Str8 buffer)
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

static f64 StringToF64(HAV_Str8 value)
{
    f64 result = 0.f;
    Str8FindResult find_result = FindFirstCharThatLooksLikeANumber(value);
    if (!find_result.found)
        return result;

    bool negative = false;
    HAV_Str8 real_number_string = find_result.match_to_end_of_buffer;
    if (find_result.match.data[0] == '+' || find_result.match.data[0] == '-') {
        negative = find_result.match.data[0] == '-';
        real_number_string.data++;
        real_number_string.size--;
    }

    HAV_Str8BinarySplitResult number_split = HAV_Str8_BinarySplit(real_number_string, HAV_STR8("."));
    HAV_Str8 integer_part                  = number_split.lhs;
    HAV_Str8 decimal_part                  = number_split.rhs;

    uint64_t integer_part_as_u64 = 0;
    for (size_t index = 0; index < integer_part.size; index++) {
        integer_part_as_u64 *= 10;
        integer_part_as_u64 += integer_part.data[index] - '0';
    }

    uint64_t decimal_part_as_u64 = 0;
    uint64_t decimal_magnitude   = 1;
    for (size_t index = 0; index < decimal_part.size; index++) {
        decimal_part_as_u64 *= 10;
        decimal_part_as_u64 += decimal_part.data[index] - '0';
        decimal_magnitude   *= 10;
    }

    typedef union FU64 {
        f64      f64_value;
        uint64_t u64_value;
    } FU64;

    FU64 fu64      = {0};
    fu64.f64_value = HAV_CAST(f64)integer_part_as_u64 + (HAV_CAST(f64)decimal_part_as_u64 / decimal_magnitude);
    if (negative) {
        fu64.u64_value |= (1ULL << 63);
    }
    return fu64.f64_value;
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

    g_profiler.begin_tsc = ReadCPUTimer();
    HAV_Str8 arg_json    = {argv[1], strlen(argv[1])};
    HAV_Str8 arg_answers = {0};
    if (argc == 3)
        arg_answers = (HAV_Str8){.data = argv[2], .size = strlen(argv[2])};

    ProfilerAnchor prof_file_read_anchor = Profiler_BeginAnchor("File Read");
    HAV_Buffer json_buffer = HAV_FileRead(arg_json.data);
    Profiler_EndAnchor(prof_file_read_anchor);

    if (!HAV_BufferIsValid(json_buffer))
        return 0;

    ProfilerAnchor prof_parse_and_sum_anchor = Profiler_BeginAnchor("Parse&Hav Sum");
    f64 haversine_sum = 0;
    size_t pair_count = 0;
    HAV_Str8 json_it  = (HAV_Str8){.data = json_buffer.data, .size = json_buffer.size};
    for (;; pair_count++) {
        ProfilerAnchor prof_json_parse_anchor = Profiler_BeginAnchor("Parse");
        f64 x0 = 0.f, y0 = 0.f, x1 = 0.f, y1 = 0.f;
        HAV_Str8BinarySplitResult x0_key = HAV_Str8_BinarySplit(json_it, HAV_STR8("x0"));
        if (x0_key.rhs.size) {
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

            x0 = StringToF64(x0_value.lhs);
            y0 = StringToF64(y0_value.lhs);
            x1 = StringToF64(x1_value.lhs);
            y1 = StringToF64(y1_value.lhs);

            json_it = y1_value.rhs;
        }
        #if 0
        HAV_PrintLnFmt("{x0: %.*s (%f), y0: %.*s (%f), x1 %.*s (%f), y1: %.*s (%f)}",
                       HAV_STR8_FMT(x0_value.lhs), x0,
                       HAV_STR8_FMT(y0_value.lhs), y0,
                       HAV_STR8_FMT(x1_value.lhs), x1,
                       HAV_STR8_FMT(y1_value.lhs), y1);
        #endif

        Profiler_EndAnchor(prof_json_parse_anchor);
        if (!x0_key.rhs.size)
            break;

        ProfilerAnchor prof_haversine_sum_anchor = Profiler_BeginAnchor("Hav Sum");
        f64 haversine_dist = ReferenceHaversine(x0, y0, x1, y1, /*EarthRadius*/ 6372.8);
        haversine_sum     += haversine_dist;
        Profiler_EndAnchor(prof_haversine_sum_anchor);
    }
    Profiler_EndAnchor(prof_parse_and_sum_anchor);

    haversine_sum /= pair_count;
    size_t input_size = json_buffer.size;
    HAV_PrintLnFmt("Input size: %zu", input_size);
    HAV_PrintLnFmt("Pair count: %zu", pair_count);
    HAV_PrintLnFmt("Haversine sum: %f", haversine_sum);

    if (arg_answers.size) {
        HAV_Buffer answers_buffer = HAV_FileRead(arg_answers.data);
        if (HAV_BufferIsValid(answers_buffer)) {
            HAV_ASSERT(answers_buffer.size == (pair_count * sizeof(f64)) + /*Reference Sum*/ sizeof(f64));

            f64 reference_haversine_sum = 0;
            memcpy(&reference_haversine_sum, answers_buffer.data + (pair_count * sizeof(f64)), sizeof(reference_haversine_sum));

            f64 difference = reference_haversine_sum - haversine_sum;
            HAV_PrintLn(HAV_STR8("\nValidation: "));
            HAV_PrintLnFmt("Reference sum: %f", reference_haversine_sum);
            HAV_PrintLnFmt("Difference: %f", difference);
        }
    }

    g_profiler.end_tsc = ReadCPUTimer();
    Profiler_Dump();
    return 0;
}
