#include <stdint.h>

// NOTE: Macros ////////////////////////////////////////////////////////////////////////////////////
#define STRINGIFY2(token) #token
#define STRINGIFY(token) STRINGIFY2(token)

#if defined(NDEBUG)
    #define ASSERT(expr)
#else
    #define ASSERT(expr)                                                                         \
        if (!(expr)) {                                                                               \
            PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
            __debugbreak();                                                                          \
        }
#endif

#define ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define CAST(Type) (Type)
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef float    f32;
typedef double   f64;
typedef bool     b32;
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// NOTE: Globals ///////////////////////////////////////////////////////////////////////////////////
typedef struct Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
} Globals;

Globals pap_globals;

// NOTE: Strings ///////////////////////////////////////////////////////////////////////////////////
typedef struct Str8 {
    char *data;
    size_t size;
} Str8;

typedef struct Str8ToU64Result {
    bool     success;
    uint64_t value;
} Str8ToU64Result;

typedef struct Str8BinarySplitResult {
    Str8 lhs;
    Str8 rhs;
} Str8BinarySplitResult;

#define STR8(string) (Str8){.data = (char *)(string), .size = ARRAY_UCOUNT(string) - 1 }
#define STR8_FMT(string) (int)((string).size), (string).data

bool                  Str8_Equals(Str8 lhs, Str8 rhs);
Str8ToU64Result       Str8_ToU64(Str8 string);
Str8BinarySplitResult Str8_BinarySplit(Str8 buffer, Str8 find);

bool                  CharIsWhiteSpace(char ch);
bool                  CharIsDigit(char ch);

// NOTE: Profiler //////////////////////////////////////////////////////////////////////////////////
typedef struct ProfilerAnchor {
    Str8 label;
    u64      elapsed_tsc_exclusive; // Does not include children
    u64      elapsed_tsc_inclusive; // Includes children
    u64      byte_count;
    u64      hits;
} ProfilerAnchor;

typedef struct Profiler {
    ProfilerAnchor anchors[4096];
    u64            begin_tsc;
    u64            end_tsc;
    u64            parent_index;
} Profiler;

typedef struct ProfilerZone {
    u64      parent_index;
    uint32_t index;
    Str8     label;
    u64      elapsed_tsc_inclusive;
    u64      tsc;
    u64      byte_count;
} ProfilerZone;

static Profiler g_profiler;

#define Profiler_BeginZone(label) Profiler_BeginZone_(STR8(label), __COUNTER__ + 1, 0)
#define Profiler_BeginZoneBandwidth(label, byte_count) Profiler_BeginZone_(STR8(label), __COUNTER__ + 1, byte_count)

static void         Profiler_Dump      ();
static ProfilerZone Profiler_BeginZone_(Str8 label, uint32_t index, u64 byte_count);
static void         Profiler_EndZone   (ProfilerZone zone);

// NOTE: PCG32 /////////////////////////////////////////////////////////////////////////////////////
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
uint32_t PCG32_Pie   (uint64_t *state);
f64      PCG32_PieF64(uint64_t *state, f64 min, f64 max);

// NOTE: Buffer ////////////////////////////////////////////////////////////////////////////////////
typedef struct Buffer {
    char *data;
    size_t size;
} Buffer;

typedef struct BufferIterator {
    Buffer buffer;
    size_t     index;
} BufferIterator;

bool           BufferIsValid             (Buffer buffer);
BufferIterator BufferIteratorInit        (Buffer buffer);
bool           BufferIteratorHasMoreBytes(BufferIterator it);
uint8_t        BufferIteratorNextByte    (BufferIterator *it);

// NOTE: File //////////////////////////////////////////////////////////////////////////////////////
Buffer FileRead (char const *file_path);
void   FileFree (Buffer buffer);
bool   FileWrite(char const *file_path, void const *buffer, size_t buffer_size);

// NOTE: Print /////////////////////////////////////////////////////////////////////////////////////
void PrintHandle(void *handle, Str8 string);
void PrintLn    (Str8 string);
void PrintLnFmt (char const *fmt, ...);
