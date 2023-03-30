// NOTE: Implementation
// ============================================================================
bool S86_Str8_Equals(S86_Str8 lhs, S86_Str8 rhs)
{
    bool result = lhs.size == rhs.size && memcmp(lhs.data, rhs.data, lhs.size) == 0;
    return result;
}

bool S86_BufferIsValid(S86_Buffer buffer)
{
    bool result = buffer.data && buffer.size;
    return result;
}

S86_BufferIterator S86_BufferIteratorInit(S86_Buffer buffer)
{
    S86_BufferIterator result = {0};
    result.buffer             = buffer;
    return result;
}

bool S86_BufferIteratorHasMoreBytes(S86_BufferIterator it)
{
    bool result = S86_BufferIsValid(it.buffer) && it.index < it.buffer.size;
    return result;
}

uint8_t S86_BufferIteratorPeekByte(S86_BufferIterator *it)
{
    S86_ASSERT(it);
    S86_ASSERT(S86_BufferIsValid(it->buffer));
    S86_ASSERT(it->index < it->buffer.size);
    uint8_t result = it->buffer.data[it->index];
    return result;
}

uint8_t S86_BufferIteratorNextByte(S86_BufferIterator *it)
{
    uint8_t result = S86_BufferIteratorPeekByte(it);
    it->index++;
    return result;
}

S86_Buffer S86_FileRead(char const *file_path)
{
    S86_Buffer result = {0};

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
    S86_ASSERT(file_size < (DWORD)-1);
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
      /*DWORD        nNumberOfBytesToRead*/ S86_CAST(DWORD)file_size,
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

void S86_FileFree(S86_Buffer buffer)
{
    if (S86_BufferIsValid(buffer))
        VirtualFree(buffer.data, 0, MEM_RELEASE);
}

void S86_Print(S86_Str8 string)
{
    if (s86_globals.stdout_handle == NULL) {
        s86_globals.stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD mode = 0;
        BOOL get_console_mode_result = GetConsoleMode(
          /*HANDLE  hConsoleHandle*/ s86_globals.stdout_handle,
          /*LPDWORD lpMode*/ &mode
        );
        s86_globals.write_to_console = get_console_mode_result != 0;
    }


    S86_ASSERT(string.size < S86_CAST(DWORD)-1);
    if (s86_globals.write_to_console) {
        DWORD chars_written = 0;
        WriteConsoleA(s86_globals.stdout_handle, string.data, (DWORD)string.size, &chars_written, NULL);
    } else {
        DWORD bytes_written = 0;
        WriteFile(s86_globals.stdout_handle, string.data, (DWORD)string.size, &bytes_written, NULL);
    }
}

void S86_PrintFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    S86_ASSERT(string_size >= 0 && string_size < S86_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        S86_Str8 string = {.data = buffer, .size = string_size};
        S86_Print(string);
    }

    va_end(args);
}

void S86_PrintLn(S86_Str8 string)
{
    S86_Print(string);
    S86_Print(S86_STR8("\n"));
}

void S86_PrintLnFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    S86_ASSERT(string_size >= 0 && string_size < S86_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        S86_Str8 string = {.data = buffer, .size = string_size};
        S86_PrintLn(string);
    }

    va_end(args);
}
