#define WIN32_MEAN_AND_LEAN
#define NOMINMAX
#include <Windows.h>

#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

// NOTE: Macros
// ============================================================================
#define S86_STRINGIFY2(token) #token
#define S86_STRINGIFY(token) S86_STRINGIFY2(token)
#define S86_ASSERT(expr)                                                                         \
    if (!(expr)) {                                                                               \
        S86_PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" S86_STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
        __debugbreak();                                                                          \
    }                                                                                            \

#define S86_ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define S86_CAST(Type) (Type)

// NOTE: Globals
// ============================================================================
typedef struct S86_Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
} S86_Globals;

S86_Globals s86_globals;

// NOTE: Strings
// ============================================================================
typedef struct S86_Str8 {
    char *data;
    size_t size;
} S86_Str8;

#define S86_STR8(string) (S86_Str8){.data = (string), .size = S86_ARRAY_UCOUNT(string) - 1 }
#define S86_STR8_FMT(string) (int)((string).size), (string).data

bool S86_Str8_Equals(S86_Str8 lhs, S86_Str8 rhs);

// NOTE: Buffer
// ============================================================================
typedef struct S86_Buffer {
    char *data;
    size_t size;
} S86_Buffer;

typedef struct S86_BufferIterator {
    S86_Buffer buffer;
    size_t     index;
} S86_BufferIterator;

bool S86_BufferIsValid(S86_Buffer buffer);
S86_BufferIterator S86_BufferIteratorInit(S86_Buffer buffer);
bool S86_BufferIteratorHasMoreBytes(S86_BufferIterator it);
uint8_t S86_BufferIteratorNextByte(S86_BufferIterator *it);

// NOTE: File
// ============================================================================
S86_Buffer S86_FileRead(char const *file_path);
void S86_FileFree(S86_Buffer buffer);

// NOTE: Print
// ============================================================================
void S86_PrintLn(S86_Str8 string);
void S86_PrintLnFmt(char const *fmt, ...);
