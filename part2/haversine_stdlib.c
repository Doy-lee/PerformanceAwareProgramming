// NOTE: Implementation
// ============================================================================
bool HAV_Str8_Equals(HAV_Str8 lhs, HAV_Str8 rhs)
{
    bool result = lhs.size == rhs.size && memcmp(lhs.data, rhs.data, lhs.size) == 0;
    return result;
}

HAV_Str8ToU64Result HAV_Str8_ToU64(HAV_Str8 string)
{
    HAV_Str8ToU64Result result = {0};

    size_t ch_index = 0;
    while (ch_index < string.size && HAV_CharIsWhiteSpace(string.data[ch_index]))
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

HAV_Str8BinarySplitResult HAV_Str8_BinarySplit(HAV_Str8 buffer, HAV_Str8 find)
{
    HAV_Str8BinarySplitResult result = {0};
    result.lhs                       = buffer;
    for (size_t index = 0; (index + find.size) <= buffer.size; index++) {
        HAV_Str8 check = {buffer.data + index, find.size};
        if (HAV_Str8_Equals(find, check)) {
            result.lhs.size = index;
            result.rhs.data = check.data + find.size;
            result.rhs.size = buffer.size - (index + find.size);
            break;
        }
    }
    return result;
}

bool HAV_CharIsWhiteSpace(char ch)
{
    bool result = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t';
    return result;
}

bool HAV_CharIsDigit(char ch)
{
    bool result = ch >= '0' && ch <= '9';
    return result;
}

#pragma warning(push)
#pragma warning(disable: 4146) // warning C4146: unary minus operator applied to unsigned type, result still unsigned
uint32_t HAV_PCG32_Pie (uint64_t *state)
{
    uint64_t old = *state ^ 0xc90fdaa2adf85459ULL;
    *state = *state * 6364136223846793005ULL + 0xc90fdaa2adf85459ULL;
    uint32_t xorshifted = (uint32_t)(((old >> 18u) ^ old) >> 27u);
    uint32_t rot = old >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}
#pragma warning(pop)

f64 HAV_PCG32_PieF64(uint64_t *state, f64 min, f64 max)
{
    uint32_t u32_value = HAV_PCG32_Pie(state);
    f64 t01            = HAV_CAST(f64)u32_value / HAV_CAST(f64)HAV_CAST(uint32_t)-1;
    f64 result         = min + (max - min) * t01;
    return result;
}

bool HAV_BufferIsValid(HAV_Buffer buffer)
{
    bool result = buffer.data && buffer.size;
    return result;
}

HAV_BufferIterator HAV_BufferIteratorInit(HAV_Buffer buffer)
{
    HAV_BufferIterator result = {0};
    result.buffer             = buffer;
    return result;
}

bool HAV_BufferIteratorHasMoreBytes(HAV_BufferIterator it)
{
    bool result = HAV_BufferIsValid(it.buffer) && it.index < it.buffer.size;
    return result;
}

uint8_t HAV_BufferIteratorPeekByte(HAV_BufferIterator *it)
{
    HAV_ASSERT(it);
    HAV_ASSERT(HAV_BufferIsValid(it->buffer));
    HAV_ASSERT(it->index < it->buffer.size);
    uint8_t result = it->buffer.data[it->index];
    return result;
}

uint8_t HAV_BufferIteratorNextByte(HAV_BufferIterator *it)
{
    uint8_t result = HAV_BufferIteratorPeekByte(it);
    it->index++;
    return result;
}

HAV_Buffer HAV_FileRead(char const *file_path)
{
    HAV_Buffer result = {0};

    // NOTE: Determine file size
    // =========================================================================
    WIN32_FILE_ATTRIBUTE_DATA file_attrib_data = {0};
    if (GetFileAttributesEx(file_path, GetFileExInfoStandard, &file_attrib_data) == 0)
        return result;

    // NOTE: Open file
    // =========================================================================
    HANDLE file_handle = CreateFile(
      /*LPCSTR                lpFileName*/ file_path,
      /*DWORD                 dwDesiredAccess*/ GENERIC_READ,
      /*DWORD                 dwShareMode*/ 0,
      /*LPSECURITY_ATTRIBUTES lpSecurityAttributes*/ NULL,
      /*DWORD                 dwCreationDisposition*/ OPEN_EXISTING,
      /*DWORD                 dwFlagsAndAttributes*/ 0,
      /*HANDLE                hTemplateFile*/ NULL
    );

    if (file_handle == INVALID_HANDLE_VALUE)
        return result;

    // NOTE: Allocate buffer
    // =========================================================================
    uint64_t file_size = (uint64_t)file_attrib_data.nFileSizeHigh << 32 | (uint64_t)file_attrib_data.nFileSizeLow << 0;
    HAV_ASSERT(file_size < (DWORD)-1);
    char *buffer = VirtualAlloc(
      /*LPVOID lpAddress*/ NULL,
      /*SIZE_T dwSize*/ file_size,
      /*DWORD  flAllocationType*/ MEM_COMMIT | MEM_RESERVE,
      /*DWORD  flProtect*/ PAGE_READWRITE
    );

    if (!buffer)
        goto end;

    // NOTE: Read file to buffer
    // =========================================================================
    DWORD bytes_read = 0;
    BOOL read_file_result = ReadFile(
      /*HANDLE       hFile*/ file_handle,
      /*LPVOID       lpBuffer*/ buffer,
      /*DWORD        nNumberOfBytesToRead*/ HAV_CAST(DWORD)file_size,
      /*LPDWORD      lpNumberOfBytesRead*/ &bytes_read,
      /*LPOVERLAPPED lpOverlapped*/ NULL
    );

    // NOTE: Handle read result
    // =========================================================================
    if (read_file_result == 0) {
        VirtualFree(buffer, 0, MEM_RELEASE);
    } else {
        result.data = buffer;
        result.size = file_size;
    }

end:
    CloseHandle(file_handle);
    return result;
};

void HAV_FileFree(HAV_Buffer buffer)
{
    if (HAV_BufferIsValid(buffer))
        VirtualFree(buffer.data, 0, MEM_RELEASE);
}

bool HAV_FileWrite(char const *file_path, void const *buffer, size_t buffer_size)
{
    bool result = false;

    // NOTE: Open file
    // =========================================================================
    HANDLE file_handle = CreateFile(
      /*LPCSTR                lpFileName*/ file_path,
      /*DWORD                 dwDesiredAccess*/ GENERIC_WRITE,
      /*DWORD                 dwShareMode*/ 0,
      /*LPSECURITY_ATTRIBUTES lpSecurityAttributes*/ NULL,
      /*DWORD                 dwCreationDisposition*/ CREATE_ALWAYS,
      /*DWORD                 dwFlagsAndAttributes*/ 0,
      /*HANDLE                hTemplateFile*/ NULL
    );

    if (file_handle == INVALID_HANDLE_VALUE)
        return result;

    // NOTE: Write file to disk
    // =========================================================================
    DWORD bytes_written = 0;
    BOOL write_file_result = WriteFile(
      /*HANDLE       hFile*/ file_handle,
      /*LPVOID       lpBuffer*/ buffer,
      /*DWORD        nNumberOfBytesToWrite*/ HAV_CAST(DWORD)buffer_size,
      /*LPDWORD      lpNumberOfBytesWrite*/ &bytes_written,
      /*LPOVERLAPPED lpOverlapped*/ NULL
    );

    HAV_ASSERT(bytes_written == buffer_size);
    result = write_file_result && bytes_written == buffer_size;
    CloseHandle(file_handle);
    return result;
};

void HAV_PrintHandle(void *handle, HAV_Str8 string)
{
    DWORD bytes_written = 0;
    WriteFile(handle, string.data, HAV_CAST(DWORD)string.size, &bytes_written, NULL);
    (void)bytes_written;
}

void HAV_Print(HAV_Str8 string)
{
    if (pap_globals.stdout_handle == NULL) {
        pap_globals.stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD mode = 0;
        BOOL get_console_mode_result = GetConsoleMode(
          /*HANDLE  hConsoleHandle*/ pap_globals.stdout_handle,
          /*LPDWORD lpMode*/ &mode
        );
        pap_globals.write_to_console = get_console_mode_result != 0;
    }


    HAV_ASSERT(string.size < HAV_CAST(DWORD)-1);
    if (pap_globals.write_to_console) {
        DWORD chars_written = 0;
        WriteConsoleA(pap_globals.stdout_handle, string.data, (DWORD)string.size, &chars_written, NULL);
    } else {
        HAV_PrintHandle(pap_globals.stdout_handle, string);
    }
}

void HAV_PrintFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    HAV_ASSERT(string_size >= 0 && string_size < HAV_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        HAV_Str8 string = {.data = buffer, .size = string_size};
        HAV_Print(string);
    }

    va_end(args);
}

void HAV_PrintLn(HAV_Str8 string)
{
    HAV_Print(string);
    HAV_Print(HAV_STR8("\n"));
}

void HAV_PrintLnFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    HAV_ASSERT(string_size >= 0 && string_size < HAV_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        HAV_Str8 string = {.data = buffer, .size = string_size};
        HAV_PrintLn(string);
    }

    va_end(args);
}
