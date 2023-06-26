// NOTE: Implementation
// ============================================================================
bool PAP_Str8_Equals(PAP_Str8 lhs, PAP_Str8 rhs)
{
    bool result = lhs.size == rhs.size && memcmp(lhs.data, rhs.data, lhs.size) == 0;
    return result;
}

bool PAP_BufferIsValid(PAP_Buffer buffer)
{
    bool result = buffer.data && buffer.size;
    return result;
}

PAP_BufferIterator PAP_BufferIteratorInit(PAP_Buffer buffer)
{
    PAP_BufferIterator result = {0};
    result.buffer             = buffer;
    return result;
}

bool PAP_BufferIteratorHasMoreBytes(PAP_BufferIterator it)
{
    bool result = PAP_BufferIsValid(it.buffer) && it.index < it.buffer.size;
    return result;
}

uint8_t PAP_BufferIteratorPeekByte(PAP_BufferIterator *it)
{
    PAP_ASSERT(it);
    PAP_ASSERT(PAP_BufferIsValid(it->buffer));
    PAP_ASSERT(it->index < it->buffer.size);
    uint8_t result = it->buffer.data[it->index];
    return result;
}

uint8_t PAP_BufferIteratorNextByte(PAP_BufferIterator *it)
{
    uint8_t result = PAP_BufferIteratorPeekByte(it);
    it->index++;
    return result;
}

PAP_Buffer PAP_FileRead(char const *file_path)
{
    PAP_Buffer result = {0};

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
    PAP_ASSERT(file_size < (DWORD)-1);
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
      /*DWORD        nNumberOfBytesToRead*/ PAP_CAST(DWORD)file_size,
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

void PAP_FileFree(PAP_Buffer buffer)
{
    if (PAP_BufferIsValid(buffer))
        VirtualFree(buffer.data, 0, MEM_RELEASE);
}

bool PAP_FileWrite(char const *file_path, void const *buffer, size_t buffer_size)
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
      /*DWORD        nNumberOfBytesToWrite*/ PAP_CAST(DWORD)buffer_size,
      /*LPDWORD      lpNumberOfBytesWrite*/ &bytes_written,
      /*LPOVERLAPPED lpOverlapped*/ NULL
    );

    PAP_ASSERT(bytes_written == buffer_size);
    result = write_file_result && bytes_written == buffer_size;
    CloseHandle(file_handle);
    return result;
};

void PAP_PrintHandle(void *handle, PAP_Str8 string)
{
    DWORD bytes_written = 0;
    WriteFile(handle, string.data, PAP_CAST(DWORD)string.size, &bytes_written, NULL);
    (void)bytes_written;
}

void PAP_Print(PAP_Str8 string)
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


    PAP_ASSERT(string.size < PAP_CAST(DWORD)-1);
    if (pap_globals.write_to_console) {
        DWORD chars_written = 0;
        WriteConsoleA(pap_globals.stdout_handle, string.data, (DWORD)string.size, &chars_written, NULL);
    } else {
        PAP_PrintHandle(pap_globals.stdout_handle, string);
    }
}

void PAP_PrintFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    PAP_ASSERT(string_size >= 0 && string_size < PAP_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        PAP_Str8 string = {.data = buffer, .size = string_size};
        PAP_Print(string);
    }

    va_end(args);
}

void PAP_PrintLn(PAP_Str8 string)
{
    PAP_Print(string);
    PAP_Print(PAP_STR8("\n"));
}

void PAP_PrintLnFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    PAP_ASSERT(string_size >= 0 && string_size < PAP_ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        PAP_Str8 string = {.data = buffer, .size = string_size};
        PAP_PrintLn(string);
    }

    va_end(args);
}
