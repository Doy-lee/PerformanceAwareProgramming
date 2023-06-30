
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>
#include "haversine_stdlib.h"
#include "haversine_stdlib.c"
#include <math.h>
#include "listing_0065_haversine_formula.cpp"

#define PRINT_USAGE HAV_PrintLnFmt("Usage: %s [uniform/cluster] [random seed] [number of coordinate pairs to generate]", argv[0])
int main(int argc, char **argv)
{
    // NOTE: Unit Tests
    // =========================================================================
    {
        {
            HAV_Str8ToU64Result result = HAV_Str8_ToU64(HAV_STR8("00"));
            HAV_ASSERT(result.success);
            HAV_ASSERT(result.value == 0);
        }

        {
            HAV_Str8ToU64Result result = HAV_Str8_ToU64(HAV_STR8("+100"));
            HAV_ASSERT(!result.success);
        }

        {
            HAV_Str8ToU64Result result = HAV_Str8_ToU64(HAV_STR8("100,0"));
            HAV_ASSERT(!result.success);
        }

        {
            HAV_Str8ToU64Result result = HAV_Str8_ToU64(HAV_STR8("100a"));
            HAV_ASSERT(!result.success);
        }

        {
            HAV_Str8ToU64Result result = HAV_Str8_ToU64(HAV_STR8("3147"));
            HAV_ASSERT(result.success);
            HAV_ASSERT(result.value == 3147);
        }
    }

    // NOTE: Arg Parsing
    // =========================================================================
    if (argc != 4) {
        PRINT_USAGE;
        return -1;
    }

    HAV_Str8 arg_uniform_cluster                        = {argv[1], strlen(argv[1])};
    HAV_Str8 arg_random_seed                            = {argv[2], strlen(argv[2])};
    HAV_Str8 arg_number_of_coordinate_pairs_to_generate = {argv[3], strlen(argv[3])};

    typedef enum PointGenerator {
        PointGenerator_Invalid,
        PointGenerator_Uniform,
        PointGenerator_Cluster,
    } PointGenerator;

    HAV_Str8ToU64Result random_seed_u64_result                            = HAV_Str8_ToU64(arg_random_seed);
    HAV_Str8ToU64Result number_of_coordinate_pairs_to_generate_u64_result = HAV_Str8_ToU64(arg_number_of_coordinate_pairs_to_generate);
    PointGenerator      point_generator                                   = PointGenerator_Invalid;

    if (HAV_Str8_Equals(arg_uniform_cluster, HAV_STR8("uniform"))) {
        point_generator = PointGenerator_Uniform;
    } else if (HAV_Str8_Equals(arg_uniform_cluster, HAV_STR8("cluster"))) {
        point_generator = PointGenerator_Cluster;
    } else {
        PRINT_USAGE;
        return -1;
    }

    if (!random_seed_u64_result.success) {
        HAV_PrintLnFmt("Random seed was not a valid U64 value [seed=%.*s]", HAV_STR8_FMT(arg_random_seed));
        PRINT_USAGE;
        return -1;
    }

    if (!number_of_coordinate_pairs_to_generate_u64_result.success) {
        HAV_PrintLnFmt("Number of coordinate pairs to generate was not a valid U64 value [seed=%.*s]", HAV_STR8_FMT(arg_number_of_coordinate_pairs_to_generate));
        PRINT_USAGE;
        return -1;
    }

    uint32_t const MAX_COORD_PAIRS = 100'000'000;
    if (number_of_coordinate_pairs_to_generate_u64_result.value > MAX_COORD_PAIRS) {
        HAV_PrintLnFmt("Maximum number of coordinate pairs exceeded, exiting to avoid accidental large files [requested=%zu, max=%zu]",
                       number_of_coordinate_pairs_to_generate_u64_result.value,
                       MAX_COORD_PAIRS);
        PRINT_USAGE;
        return -1;
    }

    // NOTE: Generator
    // =========================================================================
    uint64_t point_count = number_of_coordinate_pairs_to_generate_u64_result.value;
    uint64_t random_seed = random_seed_u64_result.value;
    uint64_t rng_state   = random_seed;

    HANDLE haversine_json_file_handle = CreateFile(
      /*LPCSTR                lpFileName*/            "haversine.json",
      /*DWORD                 dwDesiredAccess*/       GENERIC_WRITE,
      /*DWORD                 dwShareMode*/           0,
      /*LPSECURITY_ATTRIBUTES lpSecurityAttributes*/  NULL,
      /*DWORD                 dwCreationDisposition*/ CREATE_ALWAYS,
      /*DWORD                 dwFlagsAndAttributes*/  0,
      /*HANDLE                hTemplateFile*/         NULL
    );

    HANDLE haversine_f64_file_handle = CreateFile(
      /*LPCSTR                lpFileName*/            "haversine.f64",
      /*DWORD                 dwDesiredAccess*/       GENERIC_WRITE,
      /*DWORD                 dwShareMode*/           0,
      /*LPSECURITY_ATTRIBUTES lpSecurityAttributes*/  NULL,
      /*DWORD                 dwCreationDisposition*/ CREATE_ALWAYS,
      /*DWORD                 dwFlagsAndAttributes*/  0,
      /*HANDLE                hTemplateFile*/         NULL
    );

    f64 const LAT_LON_MIN = -180.0;
    f64 const LAT_LON_MAX =  180.0;

    uint64_t const MAX_CLUSTERS       = 64;
    uint64_t const points_per_cluster = point_count / MAX_CLUSTERS;

    char tmp_buffer[128];
    f64 expected_sum = 0;
    HAV_PrintHandle(haversine_json_file_handle, HAV_STR8("{\"pairs\":[\n"));

    f64 const sum_coefficient = 1.0 / point_count;
    f64 point_centre          = 0;
    f64 point_min_offset      = LAT_LON_MIN;
    f64 point_max_offset      = LAT_LON_MAX;
    for (int index = 0; index < point_count; index++) {
        if (point_generator == PointGenerator_Cluster && (index % points_per_cluster) == 0) {
            point_centre     = HAV_PCG32_PieF64(&rng_state, LAT_LON_MIN, LAT_LON_MAX);
            point_min_offset = -HAV_PCG32_PieF64(&rng_state, 0, 45);
            point_max_offset = -point_min_offset;
        }

        f64 x0 = point_centre + HAV_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 y0 = point_centre + HAV_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 x1 = point_centre + HAV_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 y1 = point_centre + HAV_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);

        f64 haversine_dist  = ReferenceHaversine(x0, y0, x1, y1, /*EarthRadius*/ 6372.8);
        HAV_PrintHandle(haversine_f64_file_handle, (HAV_Str8){.data = (char *)&haversine_dist, .size = sizeof(haversine_dist)});
        expected_sum       += (sum_coefficient * haversine_dist);

        size_t json_line_size = snprintf(tmp_buffer, sizeof(tmp_buffer), "    {\"x0\": %f, \"y0\": %f, \"x1\": %f, \"y1\": %f}%s\n", x0, y0, x1, y1, (index == (point_count - 1) ? "" : ","));
        HAV_ASSERT(json_line_size < sizeof(tmp_buffer));
        HAV_PrintHandle(haversine_json_file_handle, (HAV_Str8){.data = tmp_buffer, .size = json_line_size});
    }
    HAV_PrintHandle(haversine_json_file_handle, HAV_STR8("]}\n"));
    HAV_PrintHandle(haversine_f64_file_handle, (HAV_Str8){.data = (char *)&expected_sum, .size = sizeof(expected_sum)});

    CloseHandle(haversine_json_file_handle);
    CloseHandle(haversine_f64_file_handle);

    HAV_PrintLnFmt("Method: %s", (point_generator == PointGenerator_Uniform ? "uniform" : "cluster"));
    HAV_PrintLnFmt("Seed: %zu", random_seed);
    HAV_PrintLnFmt("Pair Count: %zu", point_count);
    HAV_PrintLnFmt("Expected Sum: %f", expected_sum);
    return 0;
}
