#define _CRT_SECURE_NO_WARNINGS
#include <stdbool.h>
#include <stdio.h>
#include <Windows.h>
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>

#include "base.h"
#include "listing_0108_platform_metrics.cpp"
#include "base.c"
#include "repetition_tester.h"

// NOTE: Allocate //////////////////////////////////////////////////////////////////////////////////
Str8 AllocTypeStr8(AllocType type)
{
    Str8 result = {};
    switch (type) {
        case AllocType_None:         result = STR8("Pre-Allocated"); break;
        case AllocType_VirtualAlloc: result = STR8("VirtualAlloc"); break;
        case AllocType_Malloc:       result = STR8("Malloc"); break;
        case AllocType_Count: break;
    }
    return result;
}

void TestAlloc(ReadArgs *args, Buffer *buffer)
{
    switch (args->alloc_type) {
        case AllocType_None: break;
        case AllocType_VirtualAlloc: {
            buffer->data = VirtualAlloc(/*LPVOID lpAddress*/        NULL,
                                        /*SIZE_T dwSize*/           buffer->size,
                                        /*DWORD  flAllocationType*/ MEM_COMMIT | MEM_RESERVE,
                                        /*DWORD  flProtect*/        PAGE_READWRITE);
        } break;

        case AllocType_Malloc: {
            buffer->data = malloc(buffer->size);
        } break;

        case AllocType_Count: break;
    }
}

void TestDealloc(ReadArgs *args, Buffer *buffer)
{
    switch (args->alloc_type) {
        case AllocType_None:         break;
        case AllocType_Malloc:       free(buffer->data); break;
        case AllocType_VirtualAlloc: VirtualFree(buffer->data, 0, MEM_RELEASE); break;
        case AllocType_Count:        break;
    }
}

// NOTE: RepTester /////////////////////////////////////////////////////////////////////////////////
void RepTester_Error(RepTester *tester, Str8 msg)
{
    tester->mode = RepTesterMode_Error;
    fprintf(stderr, "ERROR: %.*s\n", STR8_FMT(msg));
}

void RepTester_BeginTime(RepTester *tester)
{
    tester->open_block_count++;
    tester->accumulated_on_this_test.e[RepTesterValueType_CPUTimer] -= ReadCPUTimer();
    tester->accumulated_on_this_test.e[RepTesterValueType_MemPageFaults] -= ReadOSPageFaultCount();
}

void RepTester_EndTime(RepTester *tester)
{
    tester->close_block_count++;
    tester->accumulated_on_this_test.e[RepTesterValueType_CPUTimer] += ReadCPUTimer();
    tester->accumulated_on_this_test.e[RepTesterValueType_MemPageFaults] += ReadOSPageFaultCount();
}

void RepTester_CountBytes(RepTester *tester, size_t bytes_read)
{
    tester->accumulated_on_this_test.e[RepTesterValueType_ByteCount] += bytes_read;
}

static void PrintTime(Str8 label, f64 cpu_time, u64 cpu_timer_freq, u64 byte_count)
{
    printf("%.*s: %.0f", STR8_FMT(label), cpu_time);
    if (cpu_timer_freq) {
        f64 seconds = cpu_time / CAST(f64)cpu_timer_freq;
        printf(" (%fms)", 1000.0f * seconds);

        if (byte_count) {
            f64 gigabyte      = (1024.0f * 1024.0f * 1024.0f);
            f64 best_bandwidth = byte_count / (gigabyte * seconds);
            printf(" %fgb/s", best_bandwidth);
        }
    }
}

static void RepTester_PrintValue(Str8 label, RepTesterValue value, u64 cpu_timer_freq)
{
    u64 test_count = value.e[RepTesterValueType_TestCount];
    f64 divisor    = test_count ? (f64)test_count : 1;

    f64 avg_values[ARRAY_UCOUNT(value.e)];
    for (u64 index = 0; index < ARRAY_UCOUNT(value.e); index++)
        avg_values[index] = value.e[index] / divisor;

    printf("%.*s: %.0f", STR8_FMT(label), avg_values[RepTesterValueType_CPUTimer]);
    if (cpu_timer_freq) {
        f64 seconds = avg_values[RepTesterValueType_CPUTimer] / cpu_timer_freq;
        printf(" (%fms)", 1000.0f*seconds);

        if (avg_values[RepTesterValueType_ByteCount] > 0) {
            f64 gigabyte  = (1024.0f * 1024.0f * 1024.0f);
            f64 bandwidth = avg_values[RepTesterValueType_ByteCount] / (gigabyte * seconds);
            printf(" %fgb/s", bandwidth);
        }
    }

    if(avg_values[RepTesterValueType_MemPageFaults] > 0) {
        printf(" PF: %0.4f (%0.4fk/fault)",
               avg_values[RepTesterValueType_MemPageFaults],
               avg_values[RepTesterValueType_ByteCount] / (avg_values[RepTesterValueType_MemPageFaults] * 1024.0));
    }
}

static void RepTester_PrintResult(RepTesterResults results, u64 cpu_timer_freq)
{
    RepTester_PrintValue(STR8("Min"), results.min, cpu_timer_freq);
    printf("\n");
    RepTester_PrintValue(STR8("Max"), results.max, cpu_timer_freq);
    printf("\n");
    RepTester_PrintValue(STR8("Avg"), results.total, cpu_timer_freq);
    printf("\n");
}

bool RepTester_IsTesting(RepTester *tester)
{
    if (tester->mode == RepTesterMode_Testing) {
        u64 current_time = ReadCPUTimer();
        if (tester->open_block_count) {
            if (tester->open_block_count != tester->close_block_count)
                RepTester_Error(tester, STR8("Unbalanced begin/end time"));

            RepTesterValue accum = tester->accumulated_on_this_test;
            if (accum.e[RepTesterValueType_ByteCount] != tester->desired_bytes_read)
                RepTester_Error(tester, STR8("Processed byte count mismatch"));

            if (tester->mode == RepTesterMode_Testing) {
                RepTesterResults *results = &tester->results;
                accum.e[RepTesterValueType_TestCount] = 1;

                for (u64 index = 0; index < ARRAY_UCOUNT(accum.e); index++) {
                    results->total.e[index] += accum.e[index];
                }

                if (results->max.e[RepTesterValueType_CPUTimer] < accum.e[RepTesterValueType_CPUTimer]) {
                    results->max = accum;
                }

                if (results->min.e[RepTesterValueType_CPUTimer] > accum.e[RepTesterValueType_CPUTimer]) {
                    results->min = accum;

                    // NOTE: Reset the trial time when new min time is achieved
                    tester->start_time = current_time;
                    RepTester_PrintValue(STR8("Min"), results->min, tester->cpu_timer_freq);
                    printf("                                                  \r");
                }

                tester->open_block_count         = 0;
                tester->close_block_count        = 0;
                tester->accumulated_on_this_test = (RepTesterValue){};
            }
        }

        if ((current_time - tester->start_time) > tester->run_duration) {
            tester->mode = RepTesterMode_Complete;

            printf("                                                          \r");
            RepTester_PrintResult(tester->results, tester->cpu_timer_freq);
        }
    }

    bool result = tester->mode == RepTesterMode_Testing;
    return result;
}

// NOTE: Read testing functions ////////////////////////////////////////////////////////////////////
static void ReadWithFRead(RepTester *tester, ReadArgs *args)
{
    while (RepTester_IsTesting(tester)) {
        FILE *file = fopen(args->file_name.data, "rb");
        if (file) {
            Buffer buffer = args->dest;
            TestAlloc(args, &buffer);

            RepTester_BeginTime(tester);
            size_t result = fread(buffer.data, buffer.size, 1, file);
            RepTester_EndTime(tester);

            if (result == 1) {
                RepTester_CountBytes(tester, buffer.size);
            } else {
                RepTester_Error(tester, STR8("fopen failed"));
            }
            TestDealloc(args, &buffer);
            fclose(file);
        } else {
            RepTester_Error(tester, STR8("fopen failed"));
        }
    }
}

static void ReadWithRead(RepTester *tester, ReadArgs *args)
{
    while (RepTester_IsTesting(tester)) {
        int file = _open(args->file_name.data, _O_BINARY | _O_RDONLY);
        if (file != -1) {
            Buffer buffer = args->dest;
            TestAlloc(args, &buffer);

            char *dest            = buffer.data;
            u64   space_remaining = buffer.size;
            while (space_remaining) {
                u32 read_size = INT_MAX;
                if ((u64)read_size > space_remaining)
                    read_size = (u32)space_remaining;

                RepTester_BeginTime(tester);
                int result = _read(file, dest, read_size);
                RepTester_EndTime(tester);

                if (result == (int)read_size) {
                    RepTester_CountBytes(tester, read_size);
                } else {
                    RepTester_Error(tester, STR8("_read failed"));
                    break;
                }

                space_remaining -= read_size;
                dest += read_size;
            }

            TestDealloc(args, &buffer);
            _close(file);
        } else {
            RepTester_Error(tester, STR8("_open failed"));
        }
    }
}

static void ReadWithReadFile(RepTester *tester, ReadArgs *args)
{
    while (RepTester_IsTesting(tester)) {
        HANDLE file = CreateFileA(args->file_name.data,
                                  GENERIC_READ,
                                  FILE_SHARE_READ | FILE_SHARE_WRITE,
                                  0,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL,
                                  0);

        if (file != INVALID_HANDLE_VALUE) {
            Buffer buffer = args->dest;
            TestAlloc(args, &buffer);

            char *dest            = buffer.data;
            u64   space_remaining = buffer.size;
            while (space_remaining) {
                u32 read_size = UINT32_MAX;
                if ((u64)read_size > space_remaining)
                    read_size = (u32)space_remaining;

                DWORD bytes_read = 0;
                RepTester_BeginTime(tester);
                BOOL result = ReadFile(file, dest, read_size, &bytes_read, 0);
                RepTester_EndTime(tester);

                if (result && (bytes_read == read_size))
                    RepTester_CountBytes(tester, read_size);
                else
                    RepTester_Error(tester, STR8("ReadFile failed"));

                space_remaining -= read_size;
                dest += read_size;
            }

            TestDealloc(args, &buffer);
            CloseHandle(file);
        } else {
            RepTester_Error(tester, STR8("CreateFileA failed"));
        }
    }
}

static void WriteToAllBytes(RepTester *tester, ReadArgs *args)
{
    while (RepTester_IsTesting(tester)) {
        Buffer buffer = args->dest;
        TestAlloc(args, &buffer);
        RepTester_BeginTime(tester);
        for (u64 index = 0; index < buffer.size; index++)
            buffer.data[index] = (u8)index;
        RepTester_EndTime(tester);
        RepTester_CountBytes(tester, buffer.size);
        TestDealloc(args, &buffer);
    }
}

typedef void TestFuncPtr(RepTester *tester, ReadArgs *args);
typedef struct TestFunction {
    Str8         name;
    TestFuncPtr *func;
} TestFunction;

int main(int argc, char const **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [existing filename]\n", argv[0]);
        return -1;
    }

    Str8 file_name = {.data = CAST(char *) argv[1], .size = strlen(argv[1])};
    struct __stat64 stat;
    _stat64(file_name.data, &stat);

    ReadArgs args  = {};
    args.dest.data = VirtualAlloc(NULL, stat.st_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    args.dest.size = stat.st_size;
    args.file_name = file_name;

    if (stat.st_size <= 0) {
        fprintf(stderr, "ERROR: Test data size must be non-zero\n");
        return -1;
    }

    if (!args.dest.data) {
        fprintf(stderr, "ERROR: Failed to allocate %zu bytes\n", stat.st_size);
        return -1;
    }

    InitializeOSMetrics();

    TestFunction test_functions[] = {
        {STR8("WriteToAllBytes"), WriteToAllBytes},
        {STR8("fread"),           ReadWithFRead},
        {STR8("_read"),           ReadWithRead},
        {STR8("ReadFile"),        ReadWithReadFile},
    };

    RepTester testers[ARRAY_UCOUNT(test_functions)][AllocType_Count] = {};
    u64       cpu_timer_freq                                         = EstimateCPUTimerFreq();
    for (u64 index = 0; index != UINT64_MAX; index++) {
        for (u64 func_index = 0; func_index < ARRAY_UCOUNT(testers); func_index++) {
            for (u64 alloc_index = 0; alloc_index < AllocType_Count; alloc_index++) {
                RepTester   *tester    = &testers[func_index][alloc_index];
                TestFunction test_func = test_functions[func_index];

                printf("\n--- %.*s + %.*s ---\n",
                       STR8_FMT(AllocTypeStr8((AllocType)alloc_index)),
                       STR8_FMT(test_func.name));

                if (tester->mode == RepTesterMode_Nil) {
                    tester->mode                                       = RepTesterMode_Testing;
                    tester->desired_bytes_read                         = args.dest.size;
                    tester->cpu_timer_freq                             = cpu_timer_freq;
                    tester->results.min.e[RepTesterValueType_CPUTimer] = (u64)-1;
                } else if (tester->mode == RepTesterMode_Complete) {
                    tester->mode = RepTesterMode_Testing;

                    if (tester->desired_bytes_read != args.dest.size)
                        RepTester_Error(tester, STR8("desired_bytes_read changed"));
                    if (tester->cpu_timer_freq != cpu_timer_freq)
                        RepTester_Error(tester, STR8("cpu frequency changed"));
                }

                tester->run_duration = /*seconds_to_try*/ 10 * cpu_timer_freq;
                tester->start_time   = ReadCPUTimer();
                args.alloc_type      = alloc_index;

                test_func.func(tester, &args);
            }
        }
    }

    return 0;
}
