#define WIN32_MEAN_AND_LEAN
#define NOMINMAX
#include <Windows.h>

#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

typedef struct S86_Buffer S86_Buffer;
struct S86_Buffer {
    char *data;
    size_t size;
};

typedef struct S86_Str8 S86_Str8;
struct S86_Str8 {
    char *data;
    size_t size;
};

typedef struct S86_Globals S86_Globals;
struct S86_Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
};

S86_Globals s86_globals;

#define S86_STRINGIFY2(token) #token
#define S86_STRINGIFY(token) S86_STRINGIFY2(token)
#define S86_ASSERT(expr)                                                                         \
    if (!(expr)) {                                                                               \
        S86_PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" S86_STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
        __debugbreak();                                                                          \
    }                                                                                            \

#define S86_ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define S86_STR8(string) (S86_Str8){.data = (string), .size = S86_ARRAY_UCOUNT(string) - 1 }
#define S86_STR8_FMT(string) (int)((string).size), (string).data
#define S86_CAST(Type) (Type)

bool S86_BufferIsValid(S86_Buffer buffer);
S86_Buffer S86_FileRead(char const *file_path);
void S86_FileFree(S86_Buffer buffer);
void S86_PrintLn(S86_Str8 string);
void S86_PrintLnFmt(char const *fmt, ...);

bool S86_BufferIsValid(S86_Buffer buffer)
{
    bool result = buffer.data && buffer.size;
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

void S86_PrintLn(S86_Str8 string)
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
        WriteConsoleA(s86_globals.stdout_handle, "\n", 1, &chars_written, NULL);
    } else {
        DWORD bytes_written = 0;
        WriteFile(s86_globals.stdout_handle, string.data, (DWORD)string.size, &bytes_written, NULL);
        WriteFile(s86_globals.stdout_handle, "\n", 1, &bytes_written, NULL);
    }
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

typedef enum S86_ModEncoding S86_ModEncoding;
enum S86_ModEncoding {
    S86_ModEncoding_MemModeNoDisplace = 0b00,
    S86_ModEncoding_MemMode8          = 0b01,
    S86_ModEncoding_MemMode16         = 0b10,
    S86_ModEncoding_RegisterMode      = 0b11,
};

typedef enum S86_InstructionType S86_InstructionType;
enum S86_InstructionType {
    S86_InstructionType_MOVRegOrMemToOrFromReg,
    S86_InstructionType_MOVImmediateToRegOrMem,
    S86_InstructionType_MOVImmediateToReg,
    S86_InstructionType_MOVMemToAccum,
    S86_InstructionType_MOVAccumToMem,
    S86_InstructionType_MOVRegOrMemToSegReg,
    S86_InstructionType_MOVSegRegToRegOrMem,
    S86_InstructionType_Count,
};

typedef struct S86_Instruction S86_Instruction;
struct S86_Instruction {
    uint16_t op_mask;
    uint16_t op_bits;
} S86_INSTRUCTIONS[S86_InstructionType_Count] = {
    [S86_InstructionType_MOVRegOrMemToOrFromReg] = {.op_mask = 0b1111'1100'0000'0000,
                                                    .op_bits = 0b1000'1000'0000'0000},

    [S86_InstructionType_MOVImmediateToRegOrMem] = {.op_mask = 0b1111'1110'0011'1000,
                                                    .op_bits = 0b1100'0110'0000'0000},

    [S86_InstructionType_MOVImmediateToReg]      = {.op_mask = 0b1111'0000'0000'0000,
                                                    .op_bits = 0b1011'0000'0000'0000},

    [S86_InstructionType_MOVMemToAccum]          = {.op_mask = 0b1111'1110'0000'0000,
                                                    .op_bits = 0b1010'0000'0000'0000},

    [S86_InstructionType_MOVAccumToMem]          = {.op_mask = 0b1111'1110'0000'0000,
                                                    .op_bits = 0b1010'0010'0000'0000},

    [S86_InstructionType_MOVRegOrMemToSegReg]    = {.op_mask = 0b1111'1111'0010'0000,
                                                    .op_bits = 0b1000'1110'0000'0000},

    [S86_InstructionType_MOVSegRegToRegOrMem]    = {.op_mask = 0b1111'1111'0010'0000,
                                                    .op_bits = 0b1000'1100'0000'0000},
};

typedef enum S86_OpDataSize S86_OpDataSize;
enum S86_OpDataSize { S86_OpDataSize_Byte, S86_OpDataSize_Word };

int main(int argc, char **argv)
{
    if (argc != 2) {
        S86_PrintLn(S86_STR8("usage: sim8086.exe <binary asm file>"));
        return -1;
    }

    S86_Str8 const REGISTER_FIELD_ENCODING[2][8] = {
        [0b0] =
            {
                   S86_STR8("al"),
                   S86_STR8("cl"),
                   S86_STR8("dl"),
                   S86_STR8("bl"),
                   S86_STR8("ah"),
                   S86_STR8("ch"),
                   S86_STR8("dh"),
                   S86_STR8("bh"),
            },
        [0b1] =
            {
                   S86_STR8("ax"),
                   S86_STR8("cx"),
                   S86_STR8("dx"),
                   S86_STR8("bx"),
                   S86_STR8("sp"),
                   S86_STR8("bp"),
                   S86_STR8("si"),
                   S86_STR8("di"),
             },
    };

    char const *file_path = argv[1];
    S86_Buffer buffer     = S86_FileRead(file_path);
    if (!S86_BufferIsValid(buffer)) {
        S86_PrintLnFmt("File read failed [path=\"%s\"]", argv[1], buffer.size);
        return -1;
    }

    S86_PrintLn(S86_STR8("bits 16"));
    S86_ASSERT(buffer.size % 2 == 0); // We expect 2 byte instructions

    for (size_t buffer_index = 0; buffer_index < (buffer.size / 2); buffer_index++) {
        uint8_t byte0      = (uint8_t)buffer.data[buffer_index + 0];
        uint8_t byte1      = (uint8_t)buffer.data[buffer_index + 1];
        uint16_t byte01 = (uint16_t)byte0 << 8 | (uint16_t)byte1 << 0;

        S86_InstructionType instruction_type = S86_InstructionType_Count;
        for (size_t instruction_index = 0;
             instruction_type == S86_InstructionType_Count && instruction_index < S86_ARRAY_UCOUNT(S86_INSTRUCTIONS);
             instruction_index++)
        {
            S86_Instruction instruction = S86_INSTRUCTIONS[instruction_index];
            if ((byte01 & instruction.op_mask) == instruction.op_bits)
                instruction_type = instruction_index;
        }

        S86_ASSERT(instruction_type != S86_InstructionType_Count && "Unknown instruction");
        switch (instruction_type) {
            case S86_InstructionType_MOVRegOrMemToOrFromReg: {
                uint8_t d   = (byte0 & 0b0000'0010) >> 1;
                uint8_t w   = (byte0 & 0b0000'0001) >> 0;
                uint8_t mod = (byte1 & 0b1100'0000) >> 6;
                uint8_t reg = (byte1 & 0b0011'1000) >> 3;
                uint8_t rm  = (byte1 & 0b0000'0111) >> 0;
                S86_ASSERT(d   < 2);
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(reg < 8);
                S86_ASSERT(rm  < 8);

                uint8_t instr_dest = d ? reg : rm;
                uint8_t instr_src  = d ? rm  : reg;

                // S86_OpDataSize data_size = w ? S86_OpDataSize_Word : S86_OpDataSize_Byte;
                S86_ASSERT(mod == 0b11); // register-to-register

                S86_Str8 src_register  = REGISTER_FIELD_ENCODING[w][instr_src];
                S86_Str8 dest_register = REGISTER_FIELD_ENCODING[w][instr_dest];

                S86_PrintLnFmt("mov %.*s, %.*s", S86_STR8_FMT(dest_register), S86_STR8_FMT(src_register));

                #if 0
                if (mod == 0b01) {
                    // 8 bit displacement
                } else if (mode == 0b10) {
                    // 16 bit displacement
                }
                #endif
            } break;

            case S86_InstructionType_MOVImmediateToRegOrMem: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVImmediateToReg: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVMemToAccum: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVAccumToMem: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVRegOrMemToSegReg: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVSegRegToRegOrMem: {
                S86_ASSERT(!"Unhandled instruction");
            } break;

            default: {
                S86_ASSERT(!"Unknown instruction");
            } break;
        }
    }
}
