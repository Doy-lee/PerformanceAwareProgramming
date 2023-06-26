
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <Windows.h>
#include "pap2_stdlib.h"
#include "pap2_stdlib.c"
#include <math.h>

typedef float f32;
typedef double f64;

static f64 Square(f64 A)
{
    f64 Result = (A*A);
    return Result;
}

static f64 RadiansFromDegrees(f64 Degrees)
{
    f64 Result = 0.01745329251994329577 * Degrees;
    return Result;
}

// NOTE(casey): EarthRadius is generally expected to be 6372.8
static f64 ReferenceHaversine(f64 X0, f64 Y0, f64 X1, f64 Y1, f64 EarthRadius)
{
    /* NOTE(casey): This is not meant to be a "good" way to calculate the Haversine distance.
       Instead, it attempts to follow, as closely as possible, the formula used in the real-world
       question on which these homework exercises are loosely based.
    */

    f64 lat1 = Y0;
    f64 lat2 = Y1;
    f64 lon1 = X0;
    f64 lon2 = X1;

    f64 dLat = RadiansFromDegrees(lat2 - lat1);
    f64 dLon = RadiansFromDegrees(lon2 - lon1);
    lat1 = RadiansFromDegrees(lat1);
    lat2 = RadiansFromDegrees(lat2);

    f64 a = Square(sin(dLat/2.0)) + cos(lat1)*cos(lat2)*Square(sin(dLon/2));
    f64 c = 2.0*asin(sqrt(a));

    f64 Result = EarthRadius * c;

    return Result;
}

bool PAP_CharIsWhiteSpace(char ch)
{
    bool result = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t';
    return result;
}

typedef struct PAP_Str8ToU64Result {
    bool     success;
    uint64_t value;
} PAP_Str8ToU64Result;

PAP_Str8ToU64Result PAP_Str8_ToU64(PAP_Str8 string)
{
    PAP_Str8ToU64Result result = {0};

    size_t ch_index = 0;
    while (ch_index < string.size && PAP_CharIsWhiteSpace(string.data[ch_index]))
        ch_index++;

    for (; ch_index < string.size; ch_index++) {
        char ch = string.data[ch_index];
        if (ch >= '0' && ch <= '9') {
            result.value = (result.value * 10) + (ch - '0');
        } else {
            return result;
        }
    }

    result.success = true;
    return result;
}

// NOTE: PCG RNG from Demetri Spanos: https://github.com/demetri/scribbles
// pcg32_pie, based on the minimal C version from O'Neill at pcg-random.org;
// I've made a few (subjective) UX improvements for beginner use
//
// I'm not allowing the user to pick the stream/increment constant at all,
// since there is almost never a reason for doing this in common applications.
// This means that the prng state is reduced to a single uint64_t which also
// means we can avoid having a state struct at all. The (fixed) stream constant
// uses the leading hex digits of pi and e multipled by 2^30 (c90fdaa2 and
// adf85459).
//
// I have also added an XOR with the same digits on the output path prior
// to xorshift mixing.  This prevents the "surprising" result that the 
// first "random 32-bit number" from a (very common) 0 seed is 0.
//
// use:
//   uint64_t state = 12345; // whatever you like can go here
//   uint32_t some_random_32_bits = pcg32_pie(&state);
//   uint32_t more_random_32_bits = pcg32_pie(&state);

#pragma warning(push)
#pragma warning(disable: 4146) // warning C4146: unary minus operator applied to unsigned type, result still unsigned
uint32_t PAP_PCG32_Pie (uint64_t *state)
{
    uint64_t old = *state ^ 0xc90fdaa2adf85459ULL;
    *state = *state * 6364136223846793005ULL + 0xc90fdaa2adf85459ULL;
    uint32_t xorshifted = (uint32_t)(((old >> 18u) ^ old) >> 27u);
    uint32_t rot = old >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}
#pragma warning(pop)

f64 PAP_PCG32_PieF64(uint64_t *state, f64 min, f64 max)
{
    uint32_t u32_value = PAP_PCG32_Pie(state);
    f64 t01            = PAP_CAST(f64)u32_value / PAP_CAST(f64)PAP_CAST(uint32_t)-1;
    f64 result         = min + (max - min) * t01;
    return result;
}

#define PRINT_USAGE PAP_PrintLnFmt("Usage: %s [uniform/cluster] [random seed] [number of coordinate pairs to generate]", argv[0])
int main(int argc, char **argv)
{
    // NOTE: Unit Tests
    // =========================================================================
    {
        {
            PAP_Str8ToU64Result result = PAP_Str8_ToU64(PAP_STR8("00"));
            PAP_ASSERT(result.success);
            PAP_ASSERT(result.value == 0);
        }

        {
            PAP_Str8ToU64Result result = PAP_Str8_ToU64(PAP_STR8("+100"));
            PAP_ASSERT(!result.success);
        }

        {
            PAP_Str8ToU64Result result = PAP_Str8_ToU64(PAP_STR8("100,0"));
            PAP_ASSERT(!result.success);
        }

        {
            PAP_Str8ToU64Result result = PAP_Str8_ToU64(PAP_STR8("100a"));
            PAP_ASSERT(!result.success);
        }

        {
            PAP_Str8ToU64Result result = PAP_Str8_ToU64(PAP_STR8("3147"));
            PAP_ASSERT(result.success);
            PAP_ASSERT(result.value == 3147);
        }
    }

    // NOTE: Arg Parsing
    // =========================================================================
    if (argc != 4) {
        PRINT_USAGE;
        return -1;
    }

    PAP_Str8 arg_uniform_cluster                        = {argv[1], strlen(argv[1])};
    PAP_Str8 arg_random_seed                            = {argv[2], strlen(argv[2])};
    PAP_Str8 arg_number_of_coordinate_pairs_to_generate = {argv[3], strlen(argv[3])};

    typedef enum PointGenerator {
        PointGenerator_Invalid,
        PointGenerator_Uniform,
        PointGenerator_Cluster,
    } PointGenerator;

    PAP_Str8ToU64Result random_seed_u64_result                            = PAP_Str8_ToU64(arg_random_seed);
    PAP_Str8ToU64Result number_of_coordinate_pairs_to_generate_u64_result = PAP_Str8_ToU64(arg_number_of_coordinate_pairs_to_generate);
    PointGenerator      point_generator                                   = PointGenerator_Invalid;

    if (PAP_Str8_Equals(arg_uniform_cluster, PAP_STR8("uniform"))) {
        point_generator = PointGenerator_Uniform;
    } else if (PAP_Str8_Equals(arg_uniform_cluster, PAP_STR8("cluster"))) {
        point_generator = PointGenerator_Cluster;
    } else {
        PRINT_USAGE;
        return -1;
    }

    if (!random_seed_u64_result.success) {
        PAP_PrintLnFmt("Random seed was not a valid U64 value [seed=%.*s]", PAP_STR8_FMT(arg_random_seed));
        PRINT_USAGE;
        return -1;
    }

    if (!number_of_coordinate_pairs_to_generate_u64_result.success) {
        PAP_PrintLnFmt("Number of coordinate pairs to generate was not a valid U64 value [seed=%.*s]", PAP_STR8_FMT(arg_number_of_coordinate_pairs_to_generate));
        PRINT_USAGE;
        return -1;
    }

    uint32_t const MAX_COORD_PAIRS = 100'000'000;
    if (number_of_coordinate_pairs_to_generate_u64_result.value > MAX_COORD_PAIRS) {
        PAP_PrintLnFmt("Maximum number of coordinate pairs exceeded, exiting to avoid accidental large files [requested=%zu, max=%zu]",
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
    PAP_PrintHandle(haversine_json_file_handle, PAP_STR8("{\"pairs\":[\n"));

    f64 const sum_coefficient = 1.0 / point_count;
    f64 point_centre          = 0;
    f64 point_min_offset      = LAT_LON_MIN;
    f64 point_max_offset      = LAT_LON_MAX;
    for (int index = 0; index < point_count; index++) {
        if (point_generator == PointGenerator_Cluster && (index % points_per_cluster) == 0) {
            point_centre     = PAP_PCG32_PieF64(&rng_state, LAT_LON_MIN, LAT_LON_MAX);
            point_min_offset = -PAP_PCG32_PieF64(&rng_state, 0, 45);
            point_max_offset = -point_min_offset;
        }

        f64 x0 = point_centre + PAP_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 y0 = point_centre + PAP_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 x1 = point_centre + PAP_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);
        f64 y1 = point_centre + PAP_PCG32_PieF64(&rng_state, point_min_offset, point_max_offset);

        f64 haversine_dist  = ReferenceHaversine(x0, y0, x1, y1, /*EarthRadius*/ 6372.8);
        PAP_PrintHandle(haversine_f64_file_handle, (PAP_Str8){.data = (char *)&haversine_dist, .size = sizeof(haversine_dist)});
        expected_sum       += (sum_coefficient * haversine_dist);

        size_t json_line_size = snprintf(tmp_buffer, sizeof(tmp_buffer), "    {\"x0\": %f, \"y0\": %f, \"x1\": %f, \"y1\": %f}%s\n", x0, y0, x1, y1, (index == (point_count - 1) ? "" : ","));
        PAP_ASSERT(json_line_size < sizeof(tmp_buffer));
        PAP_PrintHandle(haversine_json_file_handle, (PAP_Str8){.data = tmp_buffer, .size = json_line_size});
    }
    PAP_PrintHandle(haversine_json_file_handle, PAP_STR8("]}\n"));
    PAP_PrintHandle(haversine_f64_file_handle, (PAP_Str8){.data = (char *)&expected_sum, .size = sizeof(expected_sum)});

    CloseHandle(haversine_json_file_handle);
    CloseHandle(haversine_f64_file_handle);

    PAP_PrintLnFmt("Method: %s", (point_generator == PointGenerator_Uniform ? "uniform" : "cluster"));
    PAP_PrintLnFmt("Seed: %zu", random_seed);
    PAP_PrintLnFmt("Pair Count: %zu", point_count);
    PAP_PrintLnFmt("Expected Sum: %f", expected_sum);
    return 0;
}
