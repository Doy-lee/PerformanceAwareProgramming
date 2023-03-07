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
    uint8_t op_mask0;
    uint8_t op_bits0;
    uint8_t op_mask1;
    uint8_t op_bits1;
} S86_INSTRUCTIONS[S86_InstructionType_Count] = {
    [S86_InstructionType_MOVRegOrMemToOrFromReg] = {.op_mask0 = 0b1111'1100,
                                                    .op_bits0 = 0b1000'1000,
                                                    .op_mask1 = 0b0000'0000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVImmediateToRegOrMem] = {.op_mask0 = 0b1111'1110,
                                                    .op_bits0 = 0b1100'0110,
                                                    .op_mask1 = 0b0011'1000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVImmediateToReg]      = {.op_mask0 = 0b1111'0000,
                                                    .op_bits0 = 0b1011'0000,
                                                    .op_mask1 = 0b0000'0000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVMemToAccum]          = {.op_mask0 = 0b1111'1110,
                                                    .op_bits0 = 0b1010'0000,
                                                    .op_mask1 = 0b0000'0000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVAccumToMem]          = {.op_mask0 = 0b1111'1110,
                                                    .op_bits0 = 0b1010'0010,
                                                    .op_mask1 = 0b0000'0000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVRegOrMemToSegReg]    = {.op_mask0 = 0b1111'1111,
                                                    .op_bits0 = 0b1000'1110,
                                                    .op_mask1 = 0b0010'0000,
                                                    .op_bits1 = 0b0000'0000},

    [S86_InstructionType_MOVSegRegToRegOrMem]    = {.op_mask0 = 0b1111'1111,
                                                    .op_bits0 = 0b1000'1100,
                                                    .op_mask1 = 0b0010'0000,
                                                    .op_bits1 = 0b0000'0000},
};

typedef enum S86_OpDataSize S86_OpDataSize;
enum S86_OpDataSize { S86_OpDataSize_Byte, S86_OpDataSize_Word };

typedef struct S86_InstructionStream {
    uint8_t bytes[6];
    uint8_t size;
} S86_InstructionStream;

typedef struct S86_BufferIterator {
    S86_Buffer buffer;
    size_t     index;
} S86_BufferIterator;

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

uint8_t S86_BufferIteratorPeekByte(S86_BufferIterator it)
{
    S86_ASSERT(S86_BufferIsValid(it.buffer));
    S86_ASSERT(it.index < it.buffer.size);
    uint8_t result = it.buffer.data[it.index];
    return result;
}

uint8_t S86_BufferIteratorNextByte(S86_BufferIterator *it)
{
    S86_ASSERT(it);
    S86_ASSERT(S86_BufferIsValid(it->buffer));
    S86_ASSERT(it->index < it->buffer.size);
    uint8_t result = it->buffer.data[it->index++];
    return result;
}

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
    S86_BufferIterator buffer_it = S86_BufferIteratorInit(buffer);
    while (S86_BufferIteratorHasMoreBytes(buffer_it)) {

        S86_InstructionStream stream = {0};
        stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);

        // NOTE: Match the assembly bytes to the desired instruction
        // =====================================================================
        S86_InstructionType    instruction_type = S86_InstructionType_Count;
        S86_Instruction const *instruction      = NULL;
        for (size_t instruction_index = 0;
             instruction_type == S86_InstructionType_Count && instruction_index < S86_ARRAY_UCOUNT(S86_INSTRUCTIONS);
             instruction_index++)
        {
            S86_Instruction *item = S86_INSTRUCTIONS + instruction_index;

            // NOTE: Check first instruction byte
            // =================================================================
            if ((stream.bytes[0] & item->op_mask0) != item->op_bits0)
                continue;

            // NOTE Check multi-byte instruction
            // =================================================================
            // If the matched instruction has a bit mask for the 2nd byte, this
            // is a multi-byte instruction. Check if the 2nd byte checks out.
            bool instruction_matched = true;
            if (item->op_mask1) {
                // TODO: This assumes the iterator is valid
                stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                instruction_matched         = (stream.bytes[stream.size - 1] & item->op_mask1) == item->op_bits1;
            }

            if (instruction_matched) {
                instruction_type = instruction_index;
                instruction      = item;
            }
        }

        // NOTE: Disassemble bytes to assembly mnemonics
        // =================================================================
        S86_ASSERT(instruction_type != S86_InstructionType_Count && "Unknown instruction");
        switch (instruction_type) {
            case S86_InstructionType_MOVRegOrMemToOrFromReg: {
                // NOTE: Instruction does not have opcode bits in the 2nd byte
                S86_ASSERT(stream.size == 1);
                stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);

                uint8_t d   = (stream.bytes[0] & 0b0000'0010) >> 1;
                uint8_t w   = (stream.bytes[0] & 0b0000'0001) >> 0;
                uint8_t mod = (stream.bytes[1] & 0b1100'0000) >> 6;
                uint8_t reg = (stream.bytes[1] & 0b0011'1000) >> 3;
                uint8_t rm  = (stream.bytes[1] & 0b0000'0111) >> 0;
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
                S86_ASSERT(stream.size == 2);
                uint8_t w   = (stream.bytes[0] & 0b0000'0001) >> 0;
                uint8_t mod = (stream.bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (stream.bytes[1] & 0b0000'0111) >> 0;

                uint16_t displacement = 0;
                if (mod == 0b01) {
                    // 8 bit displacement
                    stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                    displacement = stream.bytes[stream.size - 1];
                } else if (mod == 0b10) {
                    // 16 bit displacement
                    stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                    stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                    displacement = (uint16_t)stream.bytes[2] << 0 | (uint16_t)stream.bytes[3] << 8;
                } else {
                    // NOTE: We can't have register-to-register (mod == 0b11)
                    // as this instruction is an immediate to register/memory
                    S86_ASSERT(mod == 0b00);
                }

                stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t data = stream.bytes[stream.size - 1];

                if (w) { // 16 bit data
                    stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                    data |= (uint16_t)(stream.bytes[stream.size - 1]) << 8;
                }

                S86_Str8 effective_address_registers;
                switch (rm) {
                    case 0b000: effective_address_registers = S86_STR8("bx + si"); break;
                    case 0b001: effective_address_registers = S86_STR8("bx + di"); break;
                    case 0b010: effective_address_registers = S86_STR8("bp + si"); break;
                    case 0b011: effective_address_registers = S86_STR8("bp + di"); break;
                    case 0b100: effective_address_registers = S86_STR8("si"); break;
                    case 0b101: effective_address_registers = S86_STR8("di"); break;
                    case 0b110: effective_address_registers = S86_STR8("??"); S86_ASSERT(!"Unhandled instruction"); break;
                    case 0b111: effective_address_registers = S86_STR8("bx"); break;
                    default: S86_ASSERT(!"Invalid rm value, must be 3 bits"); break;
                }

                S86_ASSERT(!"Unhandled instruction");
            } break;

            case S86_InstructionType_MOVImmediateToReg: {
                // NOTE: Parse opcode control bits
                // =============================================================
                S86_ASSERT(stream.size == 1);
                uint8_t w   = (stream.bytes[0] & 0b0000'1000) >> 3;
                uint8_t reg = (stream.bytes[0] & 0b0000'0111) >> 0;

                // NOTE: Parse data payload
                // =============================================================
                stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                int16_t data = stream.bytes[stream.size - 1];

                if (w) { // 16 bit data
                    stream.bytes[stream.size++] = S86_BufferIteratorNextByte(&buffer_it);
                    data |= (int16_t)(stream.bytes[stream.size - 1]) << 8;
                }

                // NOTE: Disassemble
                // =============================================================
                S86_Str8 dest_register = REGISTER_FIELD_ENCODING[w][reg];
                S86_PrintLnFmt("mov %.*s, %d", S86_STR8_FMT(dest_register), data);
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
