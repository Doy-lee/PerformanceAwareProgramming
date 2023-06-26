// NOTE: Macros
// ============================================================================
#define PAP_STRINGIFY2(token) #token
#define PAP_STRINGIFY(token) PAP_STRINGIFY2(token)

#if defined(NDEBUG)
    #define PAP_ASSERT(expr)
#else
    #define PAP_ASSERT(expr)                                                                         \
        if (!(expr)) {                                                                               \
            PAP_PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" PAP_STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
            __debugbreak();                                                                          \
        }
#endif

#define PAP_ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define PAP_CAST(Type) (Type)

// NOTE: Globals
// ============================================================================
typedef struct PAP_Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
} PAP_Globals;

PAP_Globals pap_globals;

// NOTE: Strings
// ============================================================================
typedef struct PAP_Str8 {
    char *data;
    size_t size;
} PAP_Str8;

#define PAP_STR8(string) (PAP_Str8){.data = (string), .size = PAP_ARRAY_UCOUNT(string) - 1 }
#define PAP_STR8_FMT(string) (int)((string).size), (string).data

bool PAP_Str8_Equals(PAP_Str8 lhs, PAP_Str8 rhs);

// NOTE: Buffer
// ============================================================================
typedef struct PAP_Buffer {
    char *data;
    size_t size;
} PAP_Buffer;

typedef struct PAP_BufferIterator {
    PAP_Buffer buffer;
    size_t     index;
} PAP_BufferIterator;

bool               PAP_BufferIsValid             (PAP_Buffer buffer);
PAP_BufferIterator PAP_BufferIteratorInit        (PAP_Buffer buffer);
bool               PAP_BufferIteratorHasMoreBytes(PAP_BufferIterator it);
uint8_t            PAP_BufferIteratorNextByte    (PAP_BufferIterator *it);

// NOTE: File
// ============================================================================
PAP_Buffer PAP_FileRead (char const *file_path);
void       PAP_FileFree (PAP_Buffer buffer);
bool       PAP_FileWrite(char const *file_path, void const *buffer, size_t buffer_size);

// NOTE: Print
// ============================================================================
void PAP_PrintHandle(void *handle, PAP_Str8 string);
void PAP_PrintLn    (PAP_Str8 string);
void PAP_PrintLnFmt (char const *fmt, ...);
