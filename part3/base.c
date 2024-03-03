// NOTE: Implementation ////////////////////////////////////////////////////////////////////////////
bool Str8_Equals(Str8 lhs, Str8 rhs)
{
    bool result = lhs.size == rhs.size && memcmp(lhs.data, rhs.data, lhs.size) == 0;
    return result;
}

Str8ToU64Result Str8_ToU64(Str8 string)
{
    Str8ToU64Result result = {0};

    size_t ch_index = 0;
    while (ch_index < string.size && CharIsWhiteSpace(string.data[ch_index]))
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

Str8BinarySplitResult Str8_BinarySplit(Str8 buffer, Str8 find)
{
    Str8BinarySplitResult result = {0};
    result.lhs                       = buffer;
    for (size_t index = 0; (index + find.size) <= buffer.size; index++) {
        Str8 check = {buffer.data + index, find.size};
        if (Str8_Equals(find, check)) {
            result.lhs.size = index;
            result.rhs.data = check.data + find.size;
            result.rhs.size = buffer.size - (index + find.size);
            break;
        }
    }
    return result;
}

bool CharIsWhiteSpace(char ch)
{
    bool result = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t';
    return result;
}

bool CharIsDigit(char ch)
{
    bool result = ch >= '0' && ch <= '9';
    return result;
}

void Profiler_Dump()
{
    u64 total_elapsed_tsc = g_profiler.end_tsc - g_profiler.begin_tsc;
    u64 cpu_frequency = EstimateCPUTimerFreq();
    if (cpu_frequency)
        printf("\nTotal time: %0.4fms (CPU freq %llu)\n", 1000.0 * (f64)total_elapsed_tsc / (f64)cpu_frequency, cpu_frequency);

    for (uint32_t index = 1; index < ARRAY_UCOUNT(g_profiler.anchors); index++) {
        ProfilerAnchor const *anchor = g_profiler.anchors + index;
        if (!anchor->elapsed_tsc_inclusive)
            break;

        f64 percent = total_elapsed_tsc ? (f64)anchor->elapsed_tsc_exclusive / (f64)total_elapsed_tsc * 100.0 : 100.0;
        printf("   %.*s[%zu]: %llu (%.2f%%", STR8_FMT(anchor->label), anchor->hits, anchor->elapsed_tsc_exclusive, percent);
        if (anchor->elapsed_tsc_inclusive != anchor->elapsed_tsc_exclusive) {
            f64 percent_w_children = total_elapsed_tsc ? ((f64)anchor->elapsed_tsc_inclusive / (f64)total_elapsed_tsc * 100.0) : 100.0;
            printf(", %.2f%% w/children", percent_w_children);
        }
        printf(")");

        if (anchor->byte_count) {
            f64 megabytes_processed = anchor->byte_count / (1024.f * 1024.f);
            f64 elapsed_s           = anchor->elapsed_tsc_inclusive / CAST(f64)cpu_frequency;
            f64 bytes_per_s         = anchor->byte_count / elapsed_s;
            f64 gigabytes_bandwidth = bytes_per_s / (1024.f * 1024.f * 1024.f);
            printf("  %.3fmb at %.2fgb/s", megabytes_processed, gigabytes_bandwidth);
        }
        printf("\n");
    }
}

ProfilerZone Profiler_BeginZone_(Str8 label, uint32_t index, u64 byte_count)
{
    ProfilerZone result      = {0};
    #if defined(PROFILER)
    result.index                 = index;
    result.label                 = label;
    result.tsc                   = ReadCPUTimer();
    result.elapsed_tsc_inclusive = g_profiler.anchors[index].elapsed_tsc_inclusive;
    result.byte_count            = byte_count;
    result.parent_index          = g_profiler.parent_index;
    g_profiler.parent_index      = index;
    #else
    (void)label; (void)index; (void)byte_count;
    #endif
    return result;
}

void Profiler_EndZone(ProfilerZone zone)
{
    #if defined(PROFILER)
    u64 elapsed_tsc             = ReadCPUTimer() - zone.tsc;
    ProfilerAnchor* anchor  = g_profiler.anchors + zone.index;
    ProfilerAnchor* parent  = g_profiler.anchors + zone.parent_index;

    anchor->elapsed_tsc_exclusive += elapsed_tsc;
    anchor->elapsed_tsc_inclusive  = zone.elapsed_tsc_inclusive + elapsed_tsc;
    anchor->label                  = zone.label;
    anchor->byte_count            += zone.byte_count;
    anchor->hits++;
    parent->elapsed_tsc_exclusive -= elapsed_tsc;
    g_profiler.parent_index        = zone.parent_index;
    #else
    (void)zone;
    #endif
}

#pragma warning(push)
#pragma warning(disable: 4146) // warning C4146: unary minus operator applied to unsigned type, result still unsigned
uint32_t PCG32_Pie (uint64_t *state)
{
    uint64_t old = *state ^ 0xc90fdaa2adf85459ULL;
    *state = *state * 6364136223846793005ULL + 0xc90fdaa2adf85459ULL;
    uint32_t xorshifted = (uint32_t)(((old >> 18u) ^ old) >> 27u);
    uint32_t rot = old >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}
#pragma warning(pop)

f64 PCG32_PieF64(uint64_t *state, f64 min, f64 max)
{
    uint32_t u32_value = PCG32_Pie(state);
    f64 t01            = CAST(f64)u32_value / CAST(f64)CAST(uint32_t)-1;
    f64 result         = min + (max - min) * t01;
    return result;
}

bool BufferIsValid(Buffer buffer)
{
    bool result = buffer.data && buffer.size;
    return result;
}

BufferIterator BufferIteratorInit(Buffer buffer)
{
    BufferIterator result = {0};
    result.buffer             = buffer;
    return result;
}

bool BufferIteratorHasMoreBytes(BufferIterator it)
{
    bool result = BufferIsValid(it.buffer) && it.index < it.buffer.size;
    return result;
}

uint8_t BufferIteratorPeekByte(BufferIterator *it)
{
    ASSERT(it);
    ASSERT(BufferIsValid(it->buffer));
    ASSERT(it->index < it->buffer.size);
    uint8_t result = it->buffer.data[it->index];
    return result;
}

uint8_t BufferIteratorNextByte(BufferIterator *it)
{
    uint8_t result = BufferIteratorPeekByte(it);
    it->index++;
    return result;
}

Buffer FileRead(char const *file_path)
{
    Buffer result = {0};

    // NOTE: Determine file size ///////////////////////////////////////////////////////////////////
    WIN32_FILE_ATTRIBUTE_DATA file_attrib_data = {0};
    if (GetFileAttributesEx(file_path, GetFileExInfoStandard, &file_attrib_data) == 0)
        return result;

    // NOTE: Open file /////////////////////////////////////////////////////////////////////////////
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

    // NOTE: Allocate buffer ///////////////////////////////////////////////////////////////////////
    uint64_t file_size = (uint64_t)file_attrib_data.nFileSizeHigh << 32 | (uint64_t)file_attrib_data.nFileSizeLow << 0;
    ASSERT(file_size < (DWORD)-1);
    char *buffer = VirtualAlloc(
      /*LPVOID lpAddress*/ NULL,
      /*SIZE_T dwSize*/ file_size,
      /*DWORD  flAllocationType*/ MEM_COMMIT | MEM_RESERVE,
      /*DWORD  flProtect*/ PAGE_READWRITE
    );

    if (!buffer)
        goto end;

    // NOTE: Read file to buffer ///////////////////////////////////////////////////////////////////
    DWORD bytes_read = 0;
    ProfilerZone prof_file_read_zone = Profiler_BeginZoneBandwidth("File Read", file_size);
    BOOL read_file_result = ReadFile(
      /*HANDLE       hFile*/ file_handle,
      /*LPVOID       lpBuffer*/ buffer,
      /*DWORD        nNumberOfBytesToRead*/ CAST(DWORD)file_size,
      /*LPDWORD      lpNumberOfBytesRead*/ &bytes_read,
      /*LPOVERLAPPED lpOverlapped*/ NULL
    );
    Profiler_EndZone(prof_file_read_zone);

    // NOTE: Handle read result ////////////////////////////////////////////////////////////////////
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

void FileFree(Buffer buffer)
{
    if (BufferIsValid(buffer))
        VirtualFree(buffer.data, 0, MEM_RELEASE);
}

bool FileWrite(char const *file_path, void const *buffer, size_t buffer_size)
{
    bool result = false;

    // NOTE: Open file /////////////////////////////////////////////////////////////////////////////
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

    // NOTE: Write file to disk ////////////////////////////////////////////////////////////////////
    DWORD bytes_written = 0;
    BOOL write_file_result = WriteFile(
      /*HANDLE       hFile*/ file_handle,
      /*LPVOID       lpBuffer*/ buffer,
      /*DWORD        nNumberOfBytesToWrite*/ CAST(DWORD)buffer_size,
      /*LPDWORD      lpNumberOfBytesWrite*/ &bytes_written,
      /*LPOVERLAPPED lpOverlapped*/ NULL
    );

    ASSERT(bytes_written == buffer_size);
    result = write_file_result && bytes_written == buffer_size;
    CloseHandle(file_handle);
    return result;
};

void PrintHandle(void *handle, Str8 string)
{
    DWORD bytes_written = 0;
    WriteFile(handle, string.data, CAST(DWORD)string.size, &bytes_written, NULL);
    (void)bytes_written;
}

void Print(Str8 string)
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


    ASSERT(string.size < CAST(DWORD)-1);
    if (pap_globals.write_to_console) {
        DWORD chars_written = 0;
        WriteConsoleA(pap_globals.stdout_handle, string.data, (DWORD)string.size, &chars_written, NULL);
    } else {
        PrintHandle(pap_globals.stdout_handle, string);
    }
}

void PrintFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    ASSERT(string_size >= 0 && string_size < ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        Str8 string = {.data = buffer, .size = string_size};
        Print(string);
    }

    va_end(args);
}

void PrintLn(Str8 string)
{
    Print(string);
    Print(STR8("\n"));
}

void PrintLnFmt(char const *fmt, ...)
{
    va_list args, args_copy;
    va_start(args, fmt);

    va_copy(args_copy, args);
    int string_size = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);

    char buffer[8192];
    ASSERT(string_size >= 0 && string_size < ARRAY_UCOUNT(buffer));
    if (string_size) {
        vsnprintf(buffer, sizeof(buffer), fmt, args);
        Str8 string = {.data = buffer, .size = string_size};
        PrintLn(string);
    }

    va_end(args);
}
