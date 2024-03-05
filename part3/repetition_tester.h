typedef enum RepTesterMode {
    RepTesterMode_Nil,
    RepTesterMode_Testing,
    RepTesterMode_Error,
    RepTesterMode_Complete,
} RepTesterMode;

typedef enum RepTesterValueType {
    RepTesterValueType_TestCount,
    RepTesterValueType_CPUTimer,
    RepTesterValueType_MemPageFaults,
    RepTesterValueType_ByteCount,
    RepTesterValueType_Count,
} RepTesterValueType;

typedef struct RepTesterValue {
    u64 e[RepTesterValueType_Count];
} RepTesterValue;

typedef struct RepTesterResults {
    RepTesterValue total;
    RepTesterValue max;
    RepTesterValue min;
} RepTesterResults;

typedef struct RepTester {
    RepTesterMode    mode;
    u32              open_block_count;
    u32              close_block_count;
    u64              cpu_timer_freq;

    RepTesterResults results;
    RepTesterValue   accumulated_on_this_test;

    size_t           desired_bytes_read;

    u64              start_time;
    u64              run_duration;
} RepTester;

typedef enum AllocType {
    AllocType_None,
    AllocType_VirtualAlloc,
    AllocType_Malloc,
    AllocType_Count,
} AllocType;

typedef struct ReadArgs {
    Buffer    dest;
    Str8      file_name;
    AllocType alloc_type;
} ReadArgs;
