#define _CRT_SECURE_NO_WARNINGS
#include <stdbool.h>
#include <stdio.h>
#include <Windows.h>
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>

#include "base.h"
#include "listing_0074_platform_metrics.cpp"
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
    tester->time_accumulated_on_this_test -= ReadCPUTimer();
}

void RepTester_EndTime(RepTester *tester)
{
    tester->close_block_count++;
    tester->time_accumulated_on_this_test += ReadCPUTimer();
}

void RepTester_CountBytes(RepTester *tester, size_t bytes_read)
{
    tester->total_bytes_read += bytes_read;
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

static void RepTester_PrintResult(RepTesterResults results, u64 cpu_timer_freq, u64 byte_count)
{
    PrintTime(STR8("Min"), (f64)results.min_time, cpu_timer_freq, byte_count);
    printf("\n");

    PrintTime(STR8("Max"), (f64)results.max_time, cpu_timer_freq, byte_count);
    printf("\n");

    if (results.test_count) {
        PrintTime(STR8("Avg"), (f64)results.total_time / (f64)results.test_count, cpu_timer_freq, byte_count);
        printf("\n");
    }
}

bool RepTester_IsTesting(RepTester *tester)
{
    if (tester->mode == RepTesterMode_Testing) {
        u64 current_time = ReadCPUTimer();
        if (tester->open_block_count) {
            if (tester->open_block_count != tester->close_block_count)
                RepTester_Error(tester, STR8("Unbalanced begin/end time"));

            if (tester->total_bytes_read != tester->desired_bytes_read)
                RepTester_Error(tester, STR8("Processed byte count mismatch"));

            if (tester->mode == RepTesterMode_Testing) {
                RepTesterResults *results       = &tester->results;
                u64               elapsed_time  = tester->time_accumulated_on_this_test;
                results->test_count            += 1;
                results->total_time            += elapsed_time;
                results->max_time               = MAX(results->max_time, elapsed_time);

                if (results->min_time > elapsed_time) {
                    results->min_time = elapsed_time;

                    // NOTE: Reset the trial time when new min time is achieved
                    tester->start_time = current_time;
                    PrintTime(STR8("Min"), (f64)results->min_time, tester->cpu_timer_freq, tester->desired_bytes_read);
                    printf("               \r");
                }

                tester->open_block_count              = 0;
                tester->close_block_count             = 0;
                tester->time_accumulated_on_this_test = 0;
                tester->total_bytes_read              = 0;
            }
        }

        if ((current_time - tester->start_time) > tester->run_duration) {
            tester->mode = RepTesterMode_Complete;

            printf("                                                          \r");
            RepTester_PrintResult(tester->results, tester->cpu_timer_freq, tester->desired_bytes_read);
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
                u32 read_size = UINT32_MAX;
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

    Str8 file_name = {.data = CAST(char *) argv[0], .size = strlen(argv[0])};
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

    TestFunction test_functions[] = {
        {STR8("fread"),    ReadWithFRead},
        {STR8("_read"),    ReadWithRead},
        {STR8("ReadFile"), ReadWithReadFile},
    };

    RepTester testers[ARRAY_UCOUNT(test_functions)][AllocType_Count] = {};
    u64       cpu_timer_freq                                         = EstimateCPUTimerFreq();
    for (u64 index = 0; index != UINT64_MAX; index++) {
        for (u64 func_index = 0; func_index < ARRAY_UCOUNT(testers); func_index++) {
            for (u64 alloc_index = AllocType_VirtualAlloc; alloc_index < AllocType_Count; alloc_index++) {
                RepTester   *tester    = &testers[func_index][alloc_index];
                TestFunction test_func = test_functions[func_index];

                printf("\n--- %.*s + %.*s ---\n",
                       STR8_FMT(AllocTypeStr8((AllocType)alloc_index)),
                       STR8_FMT(test_func.name));

                if (tester->mode == RepTesterMode_Nil) {
                    tester->mode               = RepTesterMode_Testing;
                    tester->desired_bytes_read = args.dest.size;
                    tester->cpu_timer_freq     = cpu_timer_freq;
                    tester->results.min_time   = (u64)-1;
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
