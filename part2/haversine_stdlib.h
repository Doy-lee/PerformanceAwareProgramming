// NOTE: Macros
// ============================================================================
#define HAV_STRINGIFY2(token) #token
#define HAV_STRINGIFY(token) HAV_STRINGIFY2(token)

#if defined(NDEBUG)
    #define HAV_ASSERT(expr)
#else
    #define HAV_ASSERT(expr)                                                                         \
        if (!(expr)) {                                                                               \
            HAV_PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" HAV_STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
            __debugbreak();                                                                          \
        }
#endif

#define HAV_ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define HAV_CAST(Type) (Type)

typedef float f32;
typedef double f64;

// NOTE: Globals
// ============================================================================
typedef struct HAV_Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
} HAV_Globals;

HAV_Globals pap_globals;

// NOTE: Strings
// ============================================================================
typedef struct HAV_Str8 {
    char *data;
    size_t size;
} HAV_Str8;

typedef struct HAV_Str8ToU64Result {
    bool     success;
    uint64_t value;
} HAV_Str8ToU64Result;

#define HAV_STR8(string) (HAV_Str8){.data = (string), .size = HAV_ARRAY_UCOUNT(string) - 1 }
#define HAV_STR8_FMT(string) (int)((string).size), (string).data

bool                HAV_Str8_Equals(HAV_Str8 lhs, HAV_Str8 rhs);
HAV_Str8ToU64Result HAV_Str8_ToU64(HAV_Str8 string);

bool                HAV_CharIsWhiteSpace(char ch);

// NOTE: PCG32
// ============================================================================
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
uint32_t HAV_PCG32_Pie   (uint64_t *state);
f64      HAV_PCG32_PieF64(uint64_t *state, f64 min, f64 max);

// NOTE: Buffer
// ============================================================================
typedef struct HAV_Buffer {
    char *data;
    size_t size;
} HAV_Buffer;

typedef struct HAV_BufferIterator {
    HAV_Buffer buffer;
    size_t     index;
} HAV_BufferIterator;

bool               HAV_BufferIsValid             (HAV_Buffer buffer);
HAV_BufferIterator HAV_BufferIteratorInit        (HAV_Buffer buffer);
bool               HAV_BufferIteratorHasMoreBytes(HAV_BufferIterator it);
uint8_t            HAV_BufferIteratorNextByte    (HAV_BufferIterator *it);

// NOTE: File
// ============================================================================
HAV_Buffer HAV_FileRead (char const *file_path);
void       HAV_FileFree (HAV_Buffer buffer);
bool       HAV_FileWrite(char const *file_path, void const *buffer, size_t buffer_size);

// NOTE: Print
// ============================================================================
void HAV_PrintHandle(void *handle, HAV_Str8 string);
void HAV_PrintLn    (HAV_Str8 string);
void HAV_PrintLnFmt (char const *fmt, ...);
