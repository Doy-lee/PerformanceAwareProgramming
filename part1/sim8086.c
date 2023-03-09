#define WIN32_MEAN_AND_LEAN
#define NOMINMAX
#include <Windows.h>

#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

// NOTE: Macros
// ============================================================================
#define S86_STRINGIFY2(token) #token
#define S86_STRINGIFY(token) S86_STRINGIFY2(token)
#define S86_ASSERT(expr)                                                                         \
    if (!(expr)) {                                                                               \
        S86_PrintLnFmt("Assertion triggered [file=\"" __FILE__ ":" S86_STRINGIFY(__LINE__) "\", expr=\"" #expr "\"]");                                                                     \
        __debugbreak();                                                                          \
    }                                                                                            \

#define S86_ARRAY_UCOUNT(array) sizeof((array)) / sizeof((array)[0])
#define S86_CAST(Type) (Type)

// NOTE: Globals
// ============================================================================
typedef struct S86_Globals {
    HANDLE stdout_handle;
    bool   write_to_console;
} S86_Globals;

S86_Globals s86_globals;

// NOTE: Strings
// ============================================================================
typedef struct S86_Str8 {
    char *data;
    size_t size;
} S86_Str8;

#define S86_STR8(string) (S86_Str8){.data = (string), .size = S86_ARRAY_UCOUNT(string) - 1 }
#define S86_STR8_FMT(string) (int)((string).size), (string).data

// NOTE: Buffer
// ============================================================================
typedef struct S86_Buffer {
    char *data;
    size_t size;
} S86_Buffer;

typedef struct S86_BufferIterator {
    S86_Buffer buffer;
    size_t     index;
} S86_BufferIterator;

bool S86_BufferIsValid(S86_Buffer buffer);
S86_BufferIterator S86_BufferIteratorInit(S86_Buffer buffer);
bool S86_BufferIteratorHasMoreBytes(S86_BufferIterator it);
uint8_t S86_BufferIteratorPeekByte(S86_BufferIterator it);
uint8_t S86_BufferIteratorNextByte(S86_BufferIterator *it);

// NOTE: File
// ============================================================================
S86_Buffer S86_FileRead(char const *file_path);
void S86_FileFree(S86_Buffer buffer);

// NOTE: Print
// ============================================================================
void S86_PrintLn(S86_Str8 string);
void S86_PrintLnFmt(char const *fmt, ...);

// NOTE: Sim8086
// ============================================================================
typedef enum S86_InstructionType {
    S86_InstructionType_MOVRegOrMemToOrFromReg,
    S86_InstructionType_MOVImmediateToRegOrMem,
    S86_InstructionType_MOVImmediateToReg,
    S86_InstructionType_MOVMemToAccum,
    S86_InstructionType_MOVAccumToMem,
    S86_InstructionType_MOVRegOrMemToSegReg,
    S86_InstructionType_MOVSegRegToRegOrMem,
    S86_InstructionType_Count,
} S86_InstructionType;

/// Bit patterns and masks for decoding 8086 assembly. 8086 opcodes can be up
/// to 2 bytes long and mixed with instruction specific control bits. These
/// masks isolate the opcode bits from the bits can be checked after masking
/// the binary instruction stream.
///
/// Instructions that do not have opcode bits in the 2nd byte will have the mask
/// set to 0.
typedef struct S86_Instruction {
    uint8_t op_mask0;
    uint8_t op_bits0;
    uint8_t op_mask1;
    uint8_t op_bits1;
} S86_Instruction;

S86_Instruction const S86_INSTRUCTIONS[S86_InstructionType_Count] = {
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

// NOTE: Implementation
// ============================================================================
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

int main(int argc, char **argv)
{
    // NOTE: Argument handling
    // =========================================================================
    if (argc != 2) {
        S86_PrintLn(S86_STR8("usage: sim8086.exe <binary asm file>"));
        return -1;
    }

    char const *file_path = argv[1];
    S86_Buffer buffer     = S86_FileRead(file_path);
    if (!S86_BufferIsValid(buffer)) {
        S86_PrintLnFmt("File read failed [path=\"%s\"]", argv[1], buffer.size);
        return -1;
    }

    // NOTE: Sim8086
    // =========================================================================
    // Mapping from a 'reg' encoding to the register name.
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

    // NOTE: Decode assembly
    // =========================================================================
    S86_PrintLn(S86_STR8("bits 16"));
    S86_BufferIterator buffer_it = S86_BufferIteratorInit(buffer);
    while (S86_BufferIteratorHasMoreBytes(buffer_it)) {

        char op_code_bytes[2]         = {0};
        size_t op_code_size           = 0;
        op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(&buffer_it);

        // NOTE: Match the assembly bytes to the desired instruction
        // =====================================================================
        S86_InstructionType    instruction_type = S86_InstructionType_Count;
        S86_Instruction const *instruction      = NULL;
        for (size_t instruction_index = 0;
             instruction_type == S86_InstructionType_Count && instruction_index < S86_ARRAY_UCOUNT(S86_INSTRUCTIONS);
             instruction_index++)
        {
            S86_Instruction const *item = S86_INSTRUCTIONS + instruction_index;

            // NOTE: Check first instruction byte
            // =================================================================
            if ((op_code_bytes[0] & item->op_mask0) != item->op_bits0)
                continue;

            // NOTE Check multi-byte instruction
            // =================================================================
            // If the matched instruction has a bit mask for the 2nd byte, this
            // is a multi-byte instruction. Check if the 2nd byte checks out.
            bool instruction_matched = true;
            if (item->op_mask1) {
                // TODO: This assumes the iterator is valid
                op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(&buffer_it);
                instruction_matched           = (op_code_bytes[op_code_size - 1] & item->op_mask1) == item->op_bits1;
            }

            if (instruction_matched) {
                instruction_type = instruction_index;
                instruction      = item;
            }
        }

        // NOTE: Disassemble bytes to assembly mnemonics
        // =================================================================
        S86_ASSERT(op_code_size > 0 && op_code_size <= S86_ARRAY_UCOUNT(op_code_bytes));
        S86_ASSERT(instruction_type != S86_InstructionType_Count && "Unknown instruction");

        switch (instruction_type) {

            case S86_InstructionType_MOVRegOrMemToOrFromReg: {
                // NOTE: Instruction does not have opcode bits in the 2nd byte
                S86_ASSERT(op_code_size == 1);
                op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(&buffer_it);

                uint8_t d   = (op_code_bytes[0] & 0b0000'0010) >> 1;
                uint8_t w   = (op_code_bytes[0] & 0b0000'0001) >> 0;
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t reg = (op_code_bytes[1] & 0b0011'1000) >> 3;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(d   < 2);
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(reg < 8);
                S86_ASSERT(rm  < 8);

                if (mod == 0b11) {
                    // NOTE: Register-to-register move
                    // =========================================================
                    S86_Str8 src_op    = REGISTER_FIELD_ENCODING[w][d ? rm  : reg];
                    S86_Str8 dest_op   = REGISTER_FIELD_ENCODING[w][d ? reg : rm];
                    S86_PrintLnFmt("mov %.*s, %.*s", S86_STR8_FMT(dest_op), S86_STR8_FMT(src_op));
                } else {
                    // NOTE: Memory mode w/ effective address calculation
                    // =========================================================
                    bool direct_address = mod == 0b00 && rm == 0b110;
                    uint16_t displacement = 0;
                    if (mod == 0b10 || direct_address) { // Mem mode 16 bit displacement
                        uint8_t disp_lo = S86_BufferIteratorNextByte(&buffer_it);
                        uint8_t disp_hi = S86_BufferIteratorNextByte(&buffer_it);
                        displacement    = (uint16_t)disp_lo << 0 | (uint16_t)disp_hi << 8;
                    } else if (mod == 0b01) { // Mem mode 8 bit displacement
                        displacement = S86_BufferIteratorNextByte(&buffer_it);
                    } else {
                        S86_ASSERT(mod == 0b00 /*Mem mode (no displacement)*/);
                    }

                    // NOTE: Generate the effective address calculation string
                    // =========================================================
                    char effective_addr_buffer[64] = {0};
                    int  effective_addr_size       = 0;

                    effective_addr_buffer[effective_addr_size++] = '[';
                    if (direct_address) {
                        effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                        sizeof(effective_addr_buffer) - effective_addr_size,
                                                        "%u",
                                                        displacement);
                    } else {
                        if (rm == 0b110) {
                            effective_addr_buffer[effective_addr_size++] = 'b';
                            effective_addr_buffer[effective_addr_size++] = 'p';
                            if (displacement) {
                                effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                                sizeof(effective_addr_buffer) - effective_addr_size,
                                                                " + %u",
                                                                displacement);
                            }
                        } else {
                            S86_Str8 base_calc = {0};
                            switch (rm) {
                                case 0b000: base_calc = S86_STR8("bx + si"); break;
                                case 0b001: base_calc = S86_STR8("bx + di"); break;
                                case 0b010: base_calc = S86_STR8("bp + si"); break;
                                case 0b011: base_calc = S86_STR8("bp + di"); break;
                                case 0b100: base_calc = S86_STR8("si");      break;
                                case 0b101: base_calc = S86_STR8("di");      break;
                                case 0b111: base_calc = S86_STR8("bx");      break;
                                default: S86_ASSERT(!"Invalid rm value, must be 3 bits"); break;
                            }

                            memcpy(effective_addr_buffer + effective_addr_size, base_calc.data, base_calc.size);
                            effective_addr_size += S86_CAST(int)base_calc.size;

                            if (mod == 0b01 || mod == 0b10) {
                                effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                                sizeof(effective_addr_buffer) - effective_addr_size,
                                                                " + %u",
                                                                displacement);
                            }
                        }
                    }
                    effective_addr_buffer[effective_addr_size++] = ']';

                    // NOTE: Disassemble
                    // =========================================================
                    S86_Str8 effective_addr = { .data = effective_addr_buffer, .size = effective_addr_size };
                    S86_Str8 dest_op        = d ? REGISTER_FIELD_ENCODING[w][reg] : effective_addr;
                    S86_Str8 src_op         = d ? effective_addr                  : REGISTER_FIELD_ENCODING[w][reg];
                    S86_PrintLnFmt("mov %.*s, %.*s", S86_STR8_FMT(dest_op), S86_STR8_FMT(src_op));
                }

            } break;

            case S86_InstructionType_MOVImmediateToRegOrMem: {
                S86_ASSERT(op_code_size == 2);
                uint8_t w   = (op_code_bytes[0] & 0b0000'0001) >> 0;
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(rm  < 8);
                S86_ASSERT(mod != 0b11); // NOTE: Op is IMM->Reg, register-to-register not permitted

                // NOTE: Memory mode w/ effective address calculation
                // =========================================================
                bool direct_address = mod == 0b00 && rm == 0b110;
                uint16_t displacement = 0;
                if (mod == 0b10 || direct_address) { // Mem mode 16 bit displacement
                    uint8_t disp_lo = S86_BufferIteratorNextByte(&buffer_it);
                    uint8_t disp_hi = S86_BufferIteratorNextByte(&buffer_it);
                    displacement    = (uint16_t)disp_lo << 0 | (uint16_t)disp_hi << 8;
                } else if (mod == 0b01) { // Mem mode 8 bit displacement
                    displacement = S86_BufferIteratorNextByte(&buffer_it);
                } else {
                    S86_ASSERT(mod == 0b00 /*Mem mode (no displacement)*/);
                }

                // NOTE: Parse data payload
                // =============================================================
                uint16_t data = S86_BufferIteratorNextByte(&buffer_it);
                if (w) { // 16 bit data
                    uint8_t data_hi = S86_BufferIteratorNextByte(&buffer_it);
                    data |= (uint16_t)(data_hi) << 8;
                }

                // NOTE: Generate the effective address calculation string
                // =========================================================
                char effective_addr_buffer[64] = {0};
                int  effective_addr_size       = 0;

                effective_addr_buffer[effective_addr_size++] = '[';
                if (direct_address) {
                    effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                    sizeof(effective_addr_buffer) - effective_addr_size,
                                                    "%u",
                                                    displacement);
                } else {
                    if (rm == 0b110) {
                        effective_addr_buffer[effective_addr_size++] = 'b';
                        effective_addr_buffer[effective_addr_size++] = 'p';
                        if (displacement) {
                            effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                            sizeof(effective_addr_buffer) - effective_addr_size,
                                                            " + %u",
                                                            displacement);
                        }
                    } else {
                        S86_Str8 base_calc = {0};
                        switch (rm) {
                            case 0b000: base_calc = S86_STR8("bx + si"); break;
                            case 0b001: base_calc = S86_STR8("bx + di"); break;
                            case 0b010: base_calc = S86_STR8("bp + si"); break;
                            case 0b011: base_calc = S86_STR8("bp + di"); break;
                            case 0b100: base_calc = S86_STR8("si");      break;
                            case 0b101: base_calc = S86_STR8("di");      break;
                            case 0b111: base_calc = S86_STR8("bx");      break;
                            default: S86_ASSERT(!"Invalid rm value, must be 3 bits"); break;
                        }

                        memcpy(effective_addr_buffer + effective_addr_size, base_calc.data, base_calc.size);
                        effective_addr_size += S86_CAST(int)base_calc.size;

                        if (mod == 0b01 || mod == 0b10) {
                            effective_addr_size += snprintf(effective_addr_buffer + effective_addr_size,
                                                            sizeof(effective_addr_buffer) - effective_addr_size,
                                                            " + %u",
                                                            displacement);
                        }
                    }
                }
                effective_addr_buffer[effective_addr_size++] = ']';

                // NOTE: Disassemble
                // =========================================================
                S86_PrintLnFmt("mov %.*s, %s %u", effective_addr_size, effective_addr_buffer, w ? "word" : "byte", data);
            } break;

            case S86_InstructionType_MOVImmediateToReg: {
                // NOTE: Parse opcode control bits
                // =============================================================
                S86_ASSERT(op_code_size == 1);
                uint8_t w   = (op_code_bytes[0] & 0b0000'1000) >> 3;
                uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;

                // NOTE: Parse data payload
                // =============================================================
                uint16_t data = S86_BufferIteratorNextByte(&buffer_it);
                if (w) { // 16 bit data
                    uint8_t data_hi = S86_BufferIteratorNextByte(&buffer_it);
                    data |= (uint16_t)(data_hi) << 8;
                }

                // NOTE: Disassemble
                // =============================================================
                S86_Str8 dest_register = REGISTER_FIELD_ENCODING[w][reg];
                S86_PrintLnFmt("mov %.*s, %d", S86_STR8_FMT(dest_register), (int16_t)data);
            } break;

            case S86_InstructionType_MOVAccumToMem: /*FALLTHRU*/
            case S86_InstructionType_MOVMemToAccum: {
                S86_ASSERT(op_code_size == 1);
                uint16_t addr_lo = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t addr_hi = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t addr    = (addr_hi << 8) | (addr_lo << 0);

                S86_Str8 fmt = {0};
                if (instruction_type == S86_InstructionType_MOVAccumToMem) {
                    fmt = S86_STR8("mov [%u], ax");
                } else {
                    S86_ASSERT(instruction_type == S86_InstructionType_MOVMemToAccum);
                    fmt = S86_STR8("mov ax, [%u]");
                }
                S86_PrintLnFmt(fmt.data, addr);
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
