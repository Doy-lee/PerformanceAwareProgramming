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

    S86_InstructionType_PUSHRegOrMem,
    S86_InstructionType_PUSHReg,
    S86_InstructionType_PUSHSegReg,

    S86_InstructionType_POPRegOrMem,
    S86_InstructionType_POPReg,
    S86_InstructionType_POPSegReg,

    S86_InstructionType_ADDRegOrMemToOrFromReg,
    S86_InstructionType_ADDImmediateToRegOrMem,
    S86_InstructionType_ADDImmediateToAccum,

    S86_InstructionType_SUBRegOrMemToOrFromReg,
    S86_InstructionType_SUBImmediateFromRegOrMem,
    S86_InstructionType_SUBImmediateFromAccum,

    S86_InstructionType_CMPRegOrMemAndReg,
    S86_InstructionType_CMPImmediateWithRegOrMem,
    S86_InstructionType_CMPImmediateWithAccum,

    S86_InstructionType_JE_JZ,
    S86_InstructionType_JL_JNGE,
    S86_InstructionType_JLE_JNG,
    S86_InstructionType_JB_JNAE,
    S86_InstructionType_JBE_JNA,
    S86_InstructionType_JP_JPE,
    S86_InstructionType_JO,
    S86_InstructionType_JS,
    S86_InstructionType_JNE_JNZ,
    S86_InstructionType_JNL_JGE,
    S86_InstructionType_JNLE_JG,
    S86_InstructionType_JNB_JAE,
    S86_InstructionType_JNBE_JA,
    S86_InstructionType_JNP_JO,
    S86_InstructionType_JNO,
    S86_InstructionType_JNS,
    S86_InstructionType_LOOP,
    S86_InstructionType_LOOPZ_LOOPE,
    S86_InstructionType_LOOPNZ_LOOPNE,
    S86_InstructionType_JCXZ,

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
    S86_Str8 mnemonic;
} S86_Instruction;

typedef struct S86_EffectiveAddressStr8 {
    char   data[32];
    size_t size;
} S86_EffectiveAddressStr8;

S86_EffectiveAddressStr8 S86_EffectiveAddressCalc(S86_BufferIterator *buffer_it, uint8_t rm, uint8_t mod, uint8_t w);

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

S86_Str8 REGISTER_FIELD_ENCODING[2][8];
S86_EffectiveAddressStr8 S86_EffectiveAddressCalc(S86_BufferIterator *buffer_it, uint8_t rm, uint8_t mod, uint8_t w)
{
    // NOTE: Calculate displacement
    // =========================================================================
    bool direct_address  = mod == 0b00 && rm == 0b110;
    int16_t displacement = 0;
    if (mod == 0b10 || direct_address) { // Mem mode 16 bit displacement
        uint8_t disp_lo = S86_BufferIteratorNextByte(buffer_it);
        uint8_t disp_hi = S86_BufferIteratorNextByte(buffer_it);
        displacement    = (int16_t)((uint16_t)disp_lo << 0 | (uint16_t)disp_hi << 8);
    } else if (mod == 0b01) { // Mem mode 8 bit displacement
        displacement = (int8_t)S86_BufferIteratorNextByte(buffer_it);
    } else {
        S86_ASSERT(mod == 0b00 || mod == 0b11 /*Mem mode (no displacement)*/);
    }

    S86_EffectiveAddressStr8 result = {0};
    if (mod == 0b11) {
        S86_Str8 register_field = REGISTER_FIELD_ENCODING[w][rm];
        memcpy(result.data, register_field.data, register_field.size);
        result.size = register_field.size;
    } else {
        // NOTE: Effective address calculation w/ displacement
        // =========================================================================
        result.data[result.size++] = '[';
        if (direct_address) {
            result.size += snprintf(result.data + result.size,
                                    sizeof(result.data) - result.size,
                                    "%s%d",
                                    displacement >= 0 ? "" : "-", displacement >= 0 ? displacement : -displacement);
        } else {
            S86_Str8 base_calc = {0};
            switch (rm) {
                case 0b000: base_calc = S86_STR8("bx + si"); break;
                case 0b001: base_calc = S86_STR8("bx + di"); break;
                case 0b010: base_calc = S86_STR8("bp + si"); break;
                case 0b011: base_calc = S86_STR8("bp + di"); break;
                case 0b100: base_calc = S86_STR8("si");      break;
                case 0b101: base_calc = S86_STR8("di");      break;
                case 0b110: base_calc = S86_STR8("bp");      break;
                case 0b111: base_calc = S86_STR8("bx");      break;
                default: S86_ASSERT(!"Invalid rm value, must be 3 bits"); break;
            }

            memcpy(result.data + result.size, base_calc.data, base_calc.size);
            result.size += S86_CAST(int)base_calc.size;

            if ((mod == 0b01 || mod == 0b10) && displacement) {
                result.size += snprintf(result.data + result.size,
                                        sizeof(result.data) - result.size,
                                        " %c %d",
                                        displacement >= 0 ? '+' : '-', displacement >= 0 ? displacement : -displacement);
            }
        }
        result.data[result.size++] = ']';
    }

    S86_ASSERT(result.size < S86_ARRAY_UCOUNT(result.data));
    return result;
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
    REGISTER_FIELD_ENCODING[0b0][0] = S86_STR8("al");
    REGISTER_FIELD_ENCODING[0b0][1] = S86_STR8("cl");
    REGISTER_FIELD_ENCODING[0b0][2] = S86_STR8("dl");
    REGISTER_FIELD_ENCODING[0b0][3] = S86_STR8("bl");
    REGISTER_FIELD_ENCODING[0b0][4] = S86_STR8("ah");
    REGISTER_FIELD_ENCODING[0b0][5] = S86_STR8("ch");
    REGISTER_FIELD_ENCODING[0b0][6] = S86_STR8("dh");
    REGISTER_FIELD_ENCODING[0b0][7] = S86_STR8("bh");

    REGISTER_FIELD_ENCODING[0b1][0] = S86_STR8("ax");
    REGISTER_FIELD_ENCODING[0b1][1] = S86_STR8("cx");
    REGISTER_FIELD_ENCODING[0b1][2] = S86_STR8("dx");
    REGISTER_FIELD_ENCODING[0b1][3] = S86_STR8("bx");
    REGISTER_FIELD_ENCODING[0b1][4] = S86_STR8("sp");
    REGISTER_FIELD_ENCODING[0b1][5] = S86_STR8("bp");
    REGISTER_FIELD_ENCODING[0b1][6] = S86_STR8("si");
    REGISTER_FIELD_ENCODING[0b1][7] = S86_STR8("di");

    S86_Instruction const S86_INSTRUCTIONS[S86_InstructionType_Count] = {
        [S86_InstructionType_MOVRegOrMemToOrFromReg]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1000'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVImmediateToRegOrMem]   = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1100'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVImmediateToReg]        = {.op_mask0 = 0b1111'0000, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1011'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVMemToAccum]            = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1010'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVAccumToMem]            = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1010'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVRegOrMemToSegReg]      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                          .op_bits0 = 0b1000'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVSegRegToRegOrMem]      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                          .op_bits0 = 0b1000'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},

        [S86_InstructionType_PUSHRegOrMem]             = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1111'1111, .op_bits1 = 0b0011'0000, .mnemonic = S86_STR8("push")},
        [S86_InstructionType_PUSHReg]                  = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0101'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("push")},
        [S86_InstructionType_PUSHSegReg]               = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0000'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("push")},

        [S86_InstructionType_POPRegOrMem]              = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1000'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},
        [S86_InstructionType_POPReg]                   = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0101'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},
        [S86_InstructionType_POPSegReg]                = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0000'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},


        [S86_InstructionType_ADDRegOrMemToOrFromReg]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},
        [S86_InstructionType_ADDImmediateToRegOrMem]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},
        [S86_InstructionType_ADDImmediateToAccum]      = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0000'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},
        [S86_InstructionType_SUBRegOrMemToOrFromReg]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0010'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sub")},
        [S86_InstructionType_SUBImmediateFromRegOrMem] = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1000'0000, .op_bits1 = 0b0010'1000, .mnemonic = S86_STR8("sub")},
        [S86_InstructionType_SUBImmediateFromAccum]    = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0010'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sub")},
        [S86_InstructionType_CMPRegOrMemAndReg]        = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0011'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cmp")},
        [S86_InstructionType_CMPImmediateWithRegOrMem] = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                          .op_bits0 = 0b1000'0000, .op_bits1 = 0b0011'1000, .mnemonic = S86_STR8("cmp")},
        [S86_InstructionType_CMPImmediateWithAccum]    = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0011'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cmp")},
        [S86_InstructionType_JE_JZ]                    = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("je")},
        [S86_InstructionType_JL_JNGE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jl")},
        [S86_InstructionType_JLE_JNG]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jle")},
        [S86_InstructionType_JB_JNAE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jb")},
        [S86_InstructionType_JBE_JNA]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jbe")},
        [S86_InstructionType_JP_JPE]                   = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jp")},
        [S86_InstructionType_JO]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jo")},
        [S86_InstructionType_JS]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("js")},
        [S86_InstructionType_JNE_JNZ]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnz")},
        [S86_InstructionType_JNL_JGE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnl")},
        [S86_InstructionType_JNLE_JG]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jg")},
        [S86_InstructionType_JNB_JAE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnb")},
        [S86_InstructionType_JNBE_JA]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ja")},
        [S86_InstructionType_JNP_JO]                   = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnp")},
        [S86_InstructionType_JNO]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jno")},
        [S86_InstructionType_JNS]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b0111'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jns")},
        [S86_InstructionType_LOOP]                     = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1110'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loop")},
        [S86_InstructionType_LOOPZ_LOOPE]              = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1110'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loopz")},
        [S86_InstructionType_LOOPNZ_LOOPNE]            = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1110'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loopnz")},
        [S86_InstructionType_JCXZ]                     = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                          .op_bits0 = 0b1110'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jcxz")},
    };

    S86_Str8 SEGMENT_REGISTER_NAME[] = {
        [0b00] = S86_STR8("es"),
        [0b01] = S86_STR8("cs"),
        [0b10] = S86_STR8("ss"),
        [0b11] = S86_STR8("ds"),
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
                uint8_t op_byte = S86_BufferIteratorPeekByte(&buffer_it);
                instruction_matched = (op_byte & item->op_mask1) == item->op_bits1;
                if (instruction_matched) {
                    op_code_bytes[op_code_size++] = op_byte;
                    S86_BufferIteratorNextByte(&buffer_it);
                }
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

            case S86_InstructionType_POPRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_PUSHRegOrMem: {
                S86_ASSERT(op_code_size == 2);
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(mod < 4); S86_ASSERT(rm  < 8);
                S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, 0 /*w*/);
                S86_PrintLnFmt("%.*s word %.*s", S86_STR8_FMT(instruction->mnemonic), S86_STR8_FMT(effective_address));
            } break;

            case S86_InstructionType_PUSHReg:    /*FALLTHRU*/
            case S86_InstructionType_POPReg:     /*FALLTHRU*/
            case S86_InstructionType_PUSHSegReg: /*FALLTHRU*/
            case S86_InstructionType_POPSegReg: {
                S86_ASSERT(op_code_size == 1);
                S86_Str8 reg_name = {0};
                if (instruction_type == S86_InstructionType_PUSHReg ||
                    instruction_type == S86_InstructionType_POPReg) {
                    uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;
                    reg_name    = REGISTER_FIELD_ENCODING[/*w*/1][reg];
                } else {
                    S86_ASSERT(instruction_type == S86_InstructionType_PUSHSegReg ||
                               instruction_type == S86_InstructionType_POPSegReg);
                    uint8_t sr = (op_code_bytes[0] & 0b0001'1000) >> 3;
                    reg_name   = SEGMENT_REGISTER_NAME[sr];
                }
                S86_PrintLnFmt("%.*s %.*s", S86_STR8_FMT(instruction->mnemonic), S86_STR8_FMT(reg_name));
            } break;

            case S86_InstructionType_CMPRegOrMemAndReg: /*FALLTHRU*/
            case S86_InstructionType_SUBRegOrMemToOrFromReg: /*FALLTHRU*/
            case S86_InstructionType_ADDRegOrMemToOrFromReg: /*FALLTHRU*/
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
                    S86_PrintLnFmt("%.*s %.*s, %.*s", S86_STR8_FMT(instruction->mnemonic), S86_STR8_FMT(dest_op), S86_STR8_FMT(src_op));
                } else {
                    // NOTE: Memory mode w/ effective address calculation
                    // =========================================================
                    S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, w);
                    S86_Str8 addr    = { .data = effective_address.data, .size = effective_address.size };
                    S86_Str8 dest_op = d ? REGISTER_FIELD_ENCODING[w][reg] : addr;
                    S86_Str8 src_op  = d ? addr                            : REGISTER_FIELD_ENCODING[w][reg];
                    S86_PrintLnFmt("%.*s %.*s, %.*s", S86_STR8_FMT(instruction->mnemonic), S86_STR8_FMT(dest_op), S86_STR8_FMT(src_op));
                }
            } break;

            case S86_InstructionType_CMPImmediateWithRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_SUBImmediateFromRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_ADDImmediateToRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_MOVImmediateToRegOrMem: {
                S86_ASSERT(op_code_size == 2);
                uint8_t w   = (op_code_bytes[0] & 0b0000'0001) >> 0;
                uint8_t s   = (op_code_bytes[0] & 0b0000'0010) >> 1;
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(rm  < 8);

                S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, w);

                // NOTE: Parse data payload
                // =============================================================
                uint16_t data              = S86_BufferIteratorNextByte(&buffer_it);
                bool sign_extend_8bit_data = false;
                if (w) { // 16 bit data
                    if ((instruction_type == S86_InstructionType_ADDImmediateToRegOrMem ||
                         instruction_type == S86_InstructionType_SUBImmediateFromRegOrMem ||
                         instruction_type == S86_InstructionType_CMPImmediateWithRegOrMem) && s) {
                        sign_extend_8bit_data = true;
                    } else {
                        uint8_t data_hi = S86_BufferIteratorNextByte(&buffer_it);
                        data |= (uint16_t)(data_hi) << 8;
                    }
                }

                if (instruction_type == S86_InstructionType_MOVImmediateToRegOrMem) {
                    S86_ASSERT(mod != 0b11); // NOTE: Op is IMM->Reg, register-to-register not permitted
                }

                // NOTE: Disassemble
                // =========================================================
                if (instruction_type == S86_InstructionType_MOVImmediateToRegOrMem) {
                    S86_PrintLnFmt("%.*s %.*s, %s %u", S86_STR8_FMT(instruction->mnemonic), effective_address.size, effective_address.data, w ? "word" : "byte", data);
                } else {
                    if (effective_address.data[0] == '[') {
                        if (sign_extend_8bit_data) {
                            S86_PrintLnFmt("%.*s %s %.*s, %d", S86_STR8_FMT(instruction->mnemonic), w ? "word" : "byte", effective_address.size, effective_address.data, (int16_t)data);
                        } else {
                            S86_PrintLnFmt("%.*s %s %.*s, %u", S86_STR8_FMT(instruction->mnemonic), w ? "word" : "byte", effective_address.size, effective_address.data, data);
                        }
                    } else {
                        if (sign_extend_8bit_data) {
                            S86_PrintLnFmt("%.*s %.*s, %d", S86_STR8_FMT(instruction->mnemonic), effective_address.size, effective_address.data, (int16_t)data);
                        } else {
                            S86_PrintLnFmt("%.*s %.*s, %u", S86_STR8_FMT(instruction->mnemonic), effective_address.size, effective_address.data, data);
                        }
                    }
                }
            } break;

            case S86_InstructionType_CMPImmediateWithAccum: /*FALLTHRU*/
            case S86_InstructionType_SUBImmediateFromAccum: /*FALLTHRU*/
            case S86_InstructionType_ADDImmediateToAccum: /*FALLTHRU*/
            case S86_InstructionType_MOVImmediateToReg: {
                // NOTE: Parse opcode control bits
                // =============================================================
                S86_ASSERT(op_code_size == 1);
                uint8_t w   = 0;
                if (instruction_type == S86_InstructionType_ADDImmediateToAccum ||
                    instruction_type == S86_InstructionType_SUBImmediateFromAccum ||
                    instruction_type == S86_InstructionType_CMPImmediateWithAccum) {
                    w = (op_code_bytes[0] & 0b0000'0001) >> 0;
                } else {
                    w = (op_code_bytes[0] & 0b0000'1000) >> 3;
                }
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
                S86_Str8 dest_register = {0};
                if (instruction_type == S86_InstructionType_MOVImmediateToReg) {
                    dest_register = REGISTER_FIELD_ENCODING[w][reg];
                } else {
                    S86_ASSERT(instruction_type == S86_InstructionType_ADDImmediateToAccum ||
                               instruction_type == S86_InstructionType_SUBImmediateFromAccum ||
                               instruction_type == S86_InstructionType_CMPImmediateWithAccum);
                    if (w) {
                        dest_register = S86_STR8("ax");
                    } else {
                        data = (uint16_t)(int8_t)data; // Sign extension
                        dest_register = S86_STR8("al");
                    }
                }

                S86_PrintLnFmt("%.*s %.*s, %d", S86_STR8_FMT(instruction->mnemonic), S86_STR8_FMT(dest_register), (int16_t)data);
            } break;

            case S86_InstructionType_MOVAccumToMem: /*FALLTHRU*/
            case S86_InstructionType_MOVMemToAccum: {
                S86_ASSERT(op_code_size == 1);
                uint16_t addr_lo = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t addr_hi = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t addr    = (addr_hi << 8) | (addr_lo << 0);

                S86_Str8 fmt = {0};
                if (instruction_type == S86_InstructionType_MOVAccumToMem) {
                    fmt = S86_STR8("%.*s [%u], ax");
                } else {
                    S86_ASSERT(instruction_type == S86_InstructionType_MOVMemToAccum);
                    fmt = S86_STR8("%.*s ax, [%u]");
                }
                S86_PrintLnFmt(fmt.data, S86_STR8_FMT(instruction->mnemonic), addr);
            } break;

            default: {
                if (instruction_type >= S86_InstructionType_JE_JZ && instruction_type <= S86_InstructionType_JCXZ) {
                    S86_ASSERT(op_code_size == 1);
                    int8_t jump_offset = S86_CAST(int8_t)S86_BufferIteratorNextByte(&buffer_it);
                    char sign = 0;
                    if (jump_offset > 0) {
                        sign = '+';
                    } else {
                        jump_offset *= -1;
                        sign = '-';
                    }
                    S86_PrintLnFmt("%.*s $+2%c%d", S86_STR8_FMT(instruction->mnemonic), sign, jump_offset);
                } else {
                    S86_ASSERT(!"Unhandled instruction");
                }
            } break;
        }
    }
}
