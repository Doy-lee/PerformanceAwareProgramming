typedef enum RepTesterMode {
    RepTesterMode_Nil,
    RepTesterMode_Testing,
    RepTesterMode_Error,
    RepTesterMode_Complete,
} RepTesterMode;

typedef struct RepTesterResults {
    u64 test_count;
    u64 total_time;
    u64 max_time;
    u64 min_time;
} RepTesterResults;

typedef struct RepTester {
    RepTesterMode    mode;
    u32              open_block_count;
    u32              close_block_count;
    u64              cpu_timer_freq;
    RepTesterResults results;

    size_t           total_bytes_read;
    size_t           desired_bytes_read;

    u64              time_accumulated_on_this_test;
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
