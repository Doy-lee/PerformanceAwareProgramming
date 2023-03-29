#include "sim8086_stdlib.h"
#include "sim8086.h"

#include "sim8086_stdlib.c"

S86_RegisterType S86_RegisterTypeFromWReg(bool w, uint8_t reg)
{
    S86_ASSERT(reg < 8);
    S86_RegisterType const type_table[2][8] = {
        [0b0] = {
            [0] = S86_RegisterType_AL,
            [1] = S86_RegisterType_CL,
            [2] = S86_RegisterType_DL,
            [3] = S86_RegisterType_BL,
            [4] = S86_RegisterType_AH,
            [5] = S86_RegisterType_CH,
            [6] = S86_RegisterType_DH,
            [7] = S86_RegisterType_BH,
        },
        [0b1] = {
            [0] = S86_RegisterType_AX,
            [1] = S86_RegisterType_CX,
            [2] = S86_RegisterType_DX,
            [3] = S86_RegisterType_BX,
            [4] = S86_RegisterType_SP,
            [5] = S86_RegisterType_BP,
            [6] = S86_RegisterType_SI,
            [7] = S86_RegisterType_DI,
        },
    };

    S86_RegisterType result = type_table[w][reg];
    return result;
}

S86_SegmentRegisterType S86_SegmentRegisterTypeFromSR(uint8_t sr)
{
    S86_ASSERT(sr < 4);
    S86_SegmentRegisterType result = S86_SegmentRegisterType_ES + sr;
    return result;
}

S86_Str8 S86_SegmentRegisterStr8(S86_SegmentRegisterType type)
{
    S86_Str8 result = {0};
    switch (type) {
        case S86_SegmentRegisterType_Invalid: S86_ASSERT(0); break;
        case S86_SegmentRegisterType_ES:      result = S86_STR8("es"); break;
        case S86_SegmentRegisterType_CS:      result = S86_STR8("cs"); break;
        case S86_SegmentRegisterType_SS:      result = S86_STR8("ss"); break;
        case S86_SegmentRegisterType_DS:      result = S86_STR8("ds"); break;
        case S86_SegmentRegisterType_Count:   S86_ASSERT(0); break;
    }
    return result;
}

S86_Str8 S86_RegisterStr8(S86_RegisterType type)
{
    S86_Str8 result = {0};
    switch (type) {
        case S86_RegisterType_AL:            result = S86_STR8("al"); break;
        case S86_RegisterType_CL:            result = S86_STR8("cl"); break;
        case S86_RegisterType_DL:            result = S86_STR8("dl"); break;
        case S86_RegisterType_BL:            result = S86_STR8("bl"); break;
        case S86_RegisterType_AH:            result = S86_STR8("ah"); break;
        case S86_RegisterType_CH:            result = S86_STR8("ch"); break;
        case S86_RegisterType_DH:            result = S86_STR8("dh"); break;
        case S86_RegisterType_BH:            result = S86_STR8("bh"); break;
        case S86_RegisterType_AX:            result = S86_STR8("ax"); break;
        case S86_RegisterType_CX:            result = S86_STR8("cx"); break;
        case S86_RegisterType_DX:            result = S86_STR8("dx"); break;
        case S86_RegisterType_BX:            result = S86_STR8("bx"); break;
        case S86_RegisterType_SP:            result = S86_STR8("sp"); break;
        case S86_RegisterType_BP:            result = S86_STR8("bp"); break;
        case S86_RegisterType_SI:            result = S86_STR8("si"); break;
        case S86_RegisterType_DI:            result = S86_STR8("di"); break;
        case S86_RegisterType_BX_SI:         result = S86_STR8("bx + si"); break;
        case S86_RegisterType_BX_DI:         result = S86_STR8("bx + di"); break;
        case S86_RegisterType_BP_SI:         result = S86_STR8("bp + si"); break;
        case S86_RegisterType_BP_DI:         result = S86_STR8("bp + di"); break;
        case S86_RegisterType_DirectAddress: result = S86_STR8(""); break;
        case S86_RegisterType_Immediate:     result = S86_STR8(""); break;
    }
    return result;
}

void S86_PrintAsmOp(S86_AsmOp asm_op)
{
    S86_Print(S86_STR8(" "));
    {
        if (asm_op.wide_prefix == S86_WidePrefix_Dest)
            S86_PrintFmt("%s ", asm_op.wide ? "word" : "byte");

        if (asm_op.has_displacement && asm_op.effective_addr == S86_EffectiveAddress_Dest)
            S86_Print(S86_STR8("["));

        if (asm_op.effective_addr == S86_EffectiveAddress_Dest && asm_op.seg_reg != S86_SegmentRegisterType_Invalid) {
            S86_Str8 seg_reg_str8 = S86_SegmentRegisterStr8(asm_op.seg_reg);
            S86_PrintFmt("%.*s:", S86_STR8_FMT(seg_reg_str8));
        }

        if (asm_op.dest == S86_RegisterType_DirectAddress) {
            S86_PrintFmt("%s%d",
                         asm_op.displacement >= 0 ? "" : "-",
                         asm_op.displacement >= 0 ? asm_op.displacement : -asm_op.displacement);
        } else if (asm_op.dest == S86_RegisterType_Immediate) {
            S86_PrintFmt("%u", asm_op.immediate);
        } else {
            S86_Str8 reg_str8 = S86_RegisterStr8(asm_op.dest);
            S86_PrintFmt("%.*s", S86_STR8_FMT(reg_str8));
            if (asm_op.effective_addr == S86_EffectiveAddress_Dest && asm_op.displacement) {
                S86_PrintFmt(" %c %d",
                             asm_op.displacement >= 0 ? '+' : '-',
                             asm_op.displacement >= 0 ? asm_op.displacement : -asm_op.displacement);
            }
        }
        if (asm_op.has_displacement && asm_op.effective_addr == S86_EffectiveAddress_Dest)
            S86_Print(S86_STR8("]"));
    }
    S86_PrintFmt(", ");
    {
        if (asm_op.wide_prefix == S86_WidePrefix_Src)
            S86_PrintFmt("%s ", asm_op.wide ? "word" : "byte");

        if (asm_op.has_displacement && asm_op.effective_addr == S86_EffectiveAddress_Src)
            S86_Print(S86_STR8("["));

        if (asm_op.effective_addr == S86_EffectiveAddress_Src && asm_op.seg_reg != S86_SegmentRegisterType_Invalid) {
            S86_Str8 seg_reg_str8 = S86_SegmentRegisterStr8(asm_op.seg_reg);
            S86_PrintFmt("%.*s:", S86_STR8_FMT(seg_reg_str8));
        }

        if (asm_op.src == S86_RegisterType_DirectAddress) {
            S86_PrintFmt("%s%d",
                         asm_op.displacement >= 0 ? "" : "-",
                         asm_op.displacement >= 0 ? asm_op.displacement : -asm_op.displacement);
        } else if (asm_op.src == S86_RegisterType_Immediate) {
            S86_PrintFmt("%u", asm_op.immediate);
        } else {
            S86_Str8 reg_str8 = S86_RegisterStr8(asm_op.src);
            S86_PrintFmt("%.*s", S86_STR8_FMT(reg_str8));
            if (asm_op.effective_addr == S86_EffectiveAddress_Src && asm_op.displacement) {
                S86_PrintFmt(" %c %d",
                             asm_op.displacement >= 0 ? '+' : '-',
                             asm_op.displacement >= 0 ? asm_op.displacement : -asm_op.displacement);
            }
        }
        if (asm_op.has_displacement && asm_op.effective_addr == S86_EffectiveAddress_Src)
            S86_Print(S86_STR8("]"));
    }
    S86_Print(S86_STR8("\n"));
}


// NOTE: Implementation
// ============================================================================
S86_Str8 REGISTER_FIELD_ENCODING[2][8];
S86_EffectiveAddressStr8 S86_EffectiveAddressCalc(S86_BufferIterator *buffer_it, uint8_t rm, uint8_t mod, uint8_t w, S86_SegmentRegisterType seg_reg)
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
    result.displacement             = displacement;

    if (mod == 0b11) {
        result.reg_type = S86_RegisterTypeFromWReg(w, rm);

        S86_Str8 register_field = REGISTER_FIELD_ENCODING[w][rm];
        memcpy(result.data + result.size, register_field.data, register_field.size);
        result.size += register_field.size;
    } else {
        // NOTE: Effective address calculation w/ displacement
        // =========================================================================
        if (seg_reg != S86_SegmentRegisterType_Invalid) {
            result.seg_reg_type = seg_reg;
            S86_Str8 seg_reg_str8 = S86_SegmentRegisterStr8(seg_reg);
            memcpy(result.data + result.size, seg_reg_str8.data, seg_reg_str8.size);
            result.size += seg_reg_str8.size;
            result.data[result.size++] = ':';
        }

        result.has_displacement    = true;
        result.data[result.size++] = '[';
        if (direct_address) {
            result.reg_type = S86_RegisterType_DirectAddress;
            result.size += snprintf(result.data + result.size,
                                    sizeof(result.data) - result.size,
                                    "%s%d",
                                    displacement >= 0 ? "" : "-", displacement >= 0 ? displacement : -displacement);
        } else {
            S86_Str8 base_calc = {0};
            switch (rm) {
                case 0b000: base_calc = S86_STR8("bx + si"); result.reg_type = S86_RegisterType_BX_SI; break;
                case 0b001: base_calc = S86_STR8("bx + di"); result.reg_type = S86_RegisterType_BX_DI; break;
                case 0b010: base_calc = S86_STR8("bp + si"); result.reg_type = S86_RegisterType_BP_SI; break;
                case 0b011: base_calc = S86_STR8("bp + di"); result.reg_type = S86_RegisterType_BP_DI; break;
                case 0b100: base_calc = S86_STR8("si");      result.reg_type = S86_RegisterType_SI; break;
                case 0b101: base_calc = S86_STR8("di");      result.reg_type = S86_RegisterType_DI; break;
                case 0b110: base_calc = S86_STR8("bp");      result.reg_type = S86_RegisterType_BP; break;
                case 0b111: base_calc = S86_STR8("bx");      result.reg_type = S86_RegisterType_BX; break;
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

    S86_Instruction const S86_INSTRUCTIONS[] = {
        [S86_InstructionType_MOVRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1000'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1100'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVImmediateToReg]            = {.op_mask0 = 0b1111'0000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1011'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVMemToAccum]                = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1010'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVAccumToMem]                = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1010'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVRegOrMemToSegReg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                              .op_bits0 = 0b1000'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},
        [S86_InstructionType_MOVSegRegToRegOrMem]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                              .op_bits0 = 0b1000'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("mov")},

        [S86_InstructionType_PUSHRegOrMem]                 = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1111, .op_bits1 = 0b0011'0000, .mnemonic = S86_STR8("push")},
        [S86_InstructionType_PUSHReg]                      = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0101'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("push")},
        [S86_InstructionType_PUSHSegReg]                   = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("push")},

        [S86_InstructionType_POPRegOrMem]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},
        [S86_InstructionType_POPReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0101'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},
        [S86_InstructionType_POPSegReg]                    = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pop")},

        [S86_InstructionType_XCHGRegOrMemWithReg]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1000'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("xchg")},
        [S86_InstructionType_XCHGRegWithAccum]             = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("xchg")},

        [S86_InstructionType_INFixedPort]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("in")},
        [S86_InstructionType_INVariablePort]               = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("in")},

        [S86_InstructionType_OUTFixedPort]                 = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("out")},
        [S86_InstructionType_OUTVariablePort]              = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("out")},

        [S86_InstructionType_XLAT]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1101'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("xlat")},

        [S86_InstructionType_LEA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1000'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("lea")},
        [S86_InstructionType_LDS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("lds")},
        [S86_InstructionType_LES]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("les")},
        [S86_InstructionType_LAHF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("lahf")},
        [S86_InstructionType_SAHF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sahf")},
        [S86_InstructionType_PUSHF]                        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("pushf")},
        [S86_InstructionType_POPF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("popf")},

        [S86_InstructionType_ADDRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},
        [S86_InstructionType_ADDImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},
        [S86_InstructionType_ADDImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("add")},

        [S86_InstructionType_ADCRegOrMemWithRegToEither]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0001'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("adc")},
        [S86_InstructionType_ADCImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0001'0000, .mnemonic = S86_STR8("adc")},
        [S86_InstructionType_ADCImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0001'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("adc")},

        [S86_InstructionType_INCRegOrMem]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("inc")},
        [S86_InstructionType_INCReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0100'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("inc")},

        [S86_InstructionType_AAA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("aaa")},
        [S86_InstructionType_DAA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("daa")},

        [S86_InstructionType_SUBRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sub")},
        [S86_InstructionType_SUBImmediateFromRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0010'1000, .mnemonic = S86_STR8("sub")},
        [S86_InstructionType_SUBImmediateFromAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sub")},

        [S86_InstructionType_SBBRegOrMemAndRegToEither]    = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0001'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sbb")},
        [S86_InstructionType_SBBImmediateFromRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0001'1000, .mnemonic = S86_STR8("sbb")},
        [S86_InstructionType_SBBImmediateFromAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0001'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sbb")},

        [S86_InstructionType_DECRegOrMem]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1110, .op_bits1 = 0b0000'1000, .mnemonic = S86_STR8("dec")},
        [S86_InstructionType_DECReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0100'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("dec")},
        [S86_InstructionType_NEG]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0001'1000, .mnemonic = S86_STR8("neg")},

        [S86_InstructionType_CMPRegOrMemAndReg]            = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cmp")},
        [S86_InstructionType_CMPImmediateWithRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0011'1000, .mnemonic = S86_STR8("cmp")},
        [S86_InstructionType_CMPImmediateWithAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cmp")},

        [S86_InstructionType_AAS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("aas")},
        [S86_InstructionType_DAS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("das")},

        [S86_InstructionType_MUL]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0010'0000, .mnemonic = S86_STR8("mul")},
        [S86_InstructionType_IMUL]                         = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0010'1000, .mnemonic = S86_STR8("imul")},
        [S86_InstructionType_AAM]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b1111'1111,
                                                              .op_bits0 = 0b1101'0100, .op_bits1 = 0b0000'1010, .mnemonic = S86_STR8("aam")},
        [S86_InstructionType_DIV]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0011'0000, .mnemonic = S86_STR8("div")},
        [S86_InstructionType_IDIV]                         = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0011'1000, .mnemonic = S86_STR8("idiv")},
        [S86_InstructionType_AAD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b1111'1111,
                                                              .op_bits0 = 0b1101'0101, .op_bits1 = 0b0000'1010, .mnemonic = S86_STR8("aad")},
        [S86_InstructionType_CBW]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cbw")},
        [S86_InstructionType_CWD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cwd")},

        [S86_InstructionType_NOT]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0001'0000, .mnemonic = S86_STR8("not")},
        [S86_InstructionType_SHL_SAL]                      = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0010'0000, .mnemonic = S86_STR8("shl")},
        [S86_InstructionType_SHR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0010'1000, .mnemonic = S86_STR8("shr")},
        [S86_InstructionType_SAR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0011'1000, .mnemonic = S86_STR8("sar")},
        [S86_InstructionType_ROL]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("rol")},
        [S86_InstructionType_ROR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0000'1000, .mnemonic = S86_STR8("ror")},
        [S86_InstructionType_RCL]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0001'0000, .mnemonic = S86_STR8("rcl")},
        [S86_InstructionType_RCR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1101'0000, .op_bits1 = 0b0001'1000, .mnemonic = S86_STR8("rcr")},

        [S86_InstructionType_ANDRegWithMemToEither]        = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("and")},
        [S86_InstructionType_ANDImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0010'0000, .mnemonic = S86_STR8("and")},
        [S86_InstructionType_ANDImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("and")},

        [S86_InstructionType_TESTRegOrMemAndReg]           = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1000'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("test")},
        [S86_InstructionType_TESTImmediateAndRegOrMem]     = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("test")},
        [S86_InstructionType_TESTImmediateAndAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1010'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("test")},

        [S86_InstructionType_ORRegOrMemAndRegToEither]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("or")},
        [S86_InstructionType_ORImmediateToRegOrMem]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0000'1000, .mnemonic = S86_STR8("or")},
        [S86_InstructionType_ORImmediateToAccum]           = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0000'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("or")},

        [S86_InstructionType_XORRegOrMemAndRegToEither]    = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("xor")},
        [S86_InstructionType_XORImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1000'0000, .op_bits1 = 0b0011'0000, .mnemonic = S86_STR8("xor")},
        [S86_InstructionType_XORImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0011'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("xor")},

        [S86_InstructionType_REP]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("rep")},

        [S86_InstructionType_CALLDirectWithinSeg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("call")},
        [S86_InstructionType_CALLIndirectWithinSeg]        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1111, .op_bits1 = 0b0001'0000, .mnemonic = S86_STR8("call")},
        [S86_InstructionType_CALLDirectInterSeg]           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("call")},
        [S86_InstructionType_CALLIndirectInterSeg]         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1111, .op_bits1 = 0b0001'1000, .mnemonic = S86_STR8("call")},

        [S86_InstructionType_JMPDirectWithinSeg]           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jmp")},
        [S86_InstructionType_JMPDirectWithinSegShort]      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jmp")},
        [S86_InstructionType_JMPIndirectWithinSeg]         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1111, .op_bits1 = 0b0010'0000, .mnemonic = S86_STR8("jmp")},
        [S86_InstructionType_JMPDirectInterSeg]            = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jmp")},
        [S86_InstructionType_JMPIndirectInterSeg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                              .op_bits0 = 0b1111'1111, .op_bits1 = 0b0010'1000, .mnemonic = S86_STR8("jmp")},

        [S86_InstructionType_RETWithinSeg]                 = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ret")},
        [S86_InstructionType_RETWithinSegAddImmediateToSP] = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ret")},
        [S86_InstructionType_RETInterSeg]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ret")},
        [S86_InstructionType_RETInterSegAddImmediateToSP]  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ret")},

        [S86_InstructionType_JE_JZ]                        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("je")},
        [S86_InstructionType_JL_JNGE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jl")},
        [S86_InstructionType_JLE_JNG]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jle")},
        [S86_InstructionType_JB_JNAE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jb")},
        [S86_InstructionType_JBE_JNA]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jbe")},
        [S86_InstructionType_JP_JPE]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jp")},
        [S86_InstructionType_JO]                           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jo")},
        [S86_InstructionType_JS]                           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("js")},
        [S86_InstructionType_JNE_JNZ]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnz")},
        [S86_InstructionType_JNL_JGE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnl")},
        [S86_InstructionType_JNLE_JG]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jg")},
        [S86_InstructionType_JNB_JAE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnb")},
        [S86_InstructionType_JNBE_JA]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("ja")},
        [S86_InstructionType_JNP_JO]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jnp")},
        [S86_InstructionType_JNO]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jno")},
        [S86_InstructionType_JNS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0111'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jns")},
        [S86_InstructionType_LOOP]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loop")},
        [S86_InstructionType_LOOPZ_LOOPE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loopz")},
        [S86_InstructionType_LOOPNZ_LOOPNE]                = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("loopnz")},
        [S86_InstructionType_JCXZ]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1110'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("jcxz")},

        [S86_InstructionType_INT]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("int")},
        [S86_InstructionType_INT3]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("int3")},
        [S86_InstructionType_INTO]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("into")},
        [S86_InstructionType_IRET]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1100'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("iret")},

        [S86_InstructionType_CLC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("clc")},
        [S86_InstructionType_CMC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cmc")},
        [S86_InstructionType_STC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("stc")},
        [S86_InstructionType_CLD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cld")},
        [S86_InstructionType_STD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("std")},
        [S86_InstructionType_CLI]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("cli")},
        [S86_InstructionType_STI]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("sti")},
        [S86_InstructionType_HLT]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("hlt")},
        [S86_InstructionType_WAIT]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1001'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("wait")},

        [S86_InstructionType_LOCK]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b1111'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("lock")},

        [S86_InstructionType_SEGMENT]                      = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                              .op_bits0 = 0b0010'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_STR8("")},
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

    S86_BufferIterator buffer_it    = S86_BufferIteratorInit(buffer);
    S86_SegmentRegisterType seg_reg = {0};
    bool lock_prefix                = false;

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

        S86_Print(instruction->mnemonic);
        S86_AsmOp asm_op = {0};
        asm_op.lock      = lock_prefix;
        asm_op.seg_reg   = seg_reg;

        switch (instruction_type) {
            // NOTE: Instruction Pattern => [0b0000'0000W | 0bAA00'0CCC | DISP-LO | DISP-HI]
            // Where, W: Optional, AA: mod, CCC: R/M
            case S86_InstructionType_JMPIndirectWithinSeg:  /*FALLTHRU*/
            case S86_InstructionType_CALLIndirectWithinSeg: /*FALLTHRU*/
            case S86_InstructionType_NOT:                   /*FALLTHRU*/
            case S86_InstructionType_SHL_SAL:               /*FALLTHRU*/
            case S86_InstructionType_SHR:                   /*FALLTHRU*/
            case S86_InstructionType_SAR:                   /*FALLTHRU*/
            case S86_InstructionType_ROL:                   /*FALLTHRU*/
            case S86_InstructionType_ROR:                   /*FALLTHRU*/
            case S86_InstructionType_RCL:                   /*FALLTHRU*/
            case S86_InstructionType_RCR:                   /*FALLTHRU*/
            case S86_InstructionType_MUL:                   /*FALLTHRU*/
            case S86_InstructionType_IMUL:                  /*FALLTHRU*/
            case S86_InstructionType_DIV:                   /*FALLTHRU*/
            case S86_InstructionType_IDIV:                  /*FALLTHRU*/
            case S86_InstructionType_INCRegOrMem:           /*FALLTHRU*/
            case S86_InstructionType_DECRegOrMem:           /*FALLTHRU*/
            case S86_InstructionType_NEG:                   /*FALLTHRU*/
            case S86_InstructionType_POPRegOrMem:           /*FALLTHRU*/
            case S86_InstructionType_PUSHRegOrMem: {
                S86_ASSERT(op_code_size == 2);
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(mod < 4); S86_ASSERT(rm  < 8);

                uint8_t w = 1;
                if (instruction_type == S86_InstructionType_INCRegOrMem ||
                    instruction_type == S86_InstructionType_DECRegOrMem ||
                    instruction_type == S86_InstructionType_NEG         ||
                    instruction_type == S86_InstructionType_MUL         ||
                    instruction_type == S86_InstructionType_MUL         ||
                    instruction_type == S86_InstructionType_IMUL        ||
                    instruction_type == S86_InstructionType_DIV         ||
                    instruction_type == S86_InstructionType_IDIV        ||
                    (instruction_type >= S86_InstructionType_NOT &&
                     instruction_type <= S86_InstructionType_RCR)) {
                    w = op_code_bytes[0] & 0b0000'0001;
                }

                S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, w, seg_reg);
                if (effective_address.has_displacement)
                    S86_PrintFmt(" %s", w ? "word" : "byte");
                S86_PrintFmt(" %.*s", S86_STR8_FMT(effective_address));

                // NOTE: Bit shifts use 'v' to indicate if shift distance should 
                // come from cl register otherwise bitshift by 1
                if (instruction_type >= S86_InstructionType_SHL_SAL && instruction_type <= S86_InstructionType_RCR) {
                    uint8_t v = (op_code_bytes[0] & 0b0000'0010) >> 1;
                    S86_PrintFmt(", %s", v ? "cl" : "1");
                }

                S86_Print(S86_STR8("\n"));
            } break;

            // NOTE: Instruction Pattern => [0b0000'0000]
            // Generally handles instructions with control bits in any position in the first byte
            case S86_InstructionType_DECReg:           /*FALLTHRU*/
            case S86_InstructionType_INCReg:           /*FALLTHRU*/
            case S86_InstructionType_XCHGRegWithAccum: /*FALLTHRU*/
            case S86_InstructionType_PUSHReg:          /*FALLTHRU*/
            case S86_InstructionType_POPReg:           /*FALLTHRU*/
            case S86_InstructionType_PUSHSegReg:       /*FALLTHRU*/
            case S86_InstructionType_POPSegReg: {
                S86_ASSERT(op_code_size == 1);
                S86_Str8 reg_name = {0};
                if (instruction_type == S86_InstructionType_PUSHReg ||
                    instruction_type == S86_InstructionType_POPReg ||
                    instruction_type == S86_InstructionType_INCReg ||
                    instruction_type == S86_InstructionType_DECReg ||
                    instruction_type == S86_InstructionType_XCHGRegWithAccum) {
                    uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;
                    reg_name    = REGISTER_FIELD_ENCODING[/*w*/1][reg];
                } else {
                    S86_ASSERT(instruction_type == S86_InstructionType_PUSHSegReg ||
                               instruction_type == S86_InstructionType_POPSegReg);
                    uint8_t sr = (op_code_bytes[0] & 0b0001'1000) >> 3;
                    reg_name   = SEGMENT_REGISTER_NAME[sr];
                }

                if (instruction_type == S86_InstructionType_XCHGRegWithAccum)
                    S86_Print(S86_STR8(" ax,"));

                S86_PrintLnFmt(" %.*s", S86_STR8_FMT(reg_name));
            } break;

            // NOTE: Instruction Pattern => [0b0000'000DW | 0bAABB'BCCC | DISP-LO | DISP-HI | DATA-LO | DATA-HI]
            // Where, D: optional, W: optional, AA: mod, BBB: reg, CCC: r/m
            case S86_InstructionType_ADDRegOrMemToOrFromReg:     /*FALLTHRU*/
            case S86_InstructionType_ADCRegOrMemWithRegToEither: /*FALLTHRU*/
            case S86_InstructionType_SUBRegOrMemToOrFromReg:     /*FALLTHRU*/
            case S86_InstructionType_SBBRegOrMemAndRegToEither:  /*FALLTHRU*/
            case S86_InstructionType_ANDRegWithMemToEither:      /*FALLTHRU*/
            case S86_InstructionType_TESTRegOrMemAndReg:         /*FALLTHRU*/
            case S86_InstructionType_ORRegOrMemAndRegToEither:   /*FALLTHRU*/
            case S86_InstructionType_XORRegOrMemAndRegToEither:  /*FALLTHRU*/
            case S86_InstructionType_LEA:                        /*FALLTHRU*/
            case S86_InstructionType_LDS:                        /*FALLTHRU*/
            case S86_InstructionType_LES:                        /*FALLTHRU*/
            case S86_InstructionType_XCHGRegOrMemWithReg:        /*FALLTHRU*/
            case S86_InstructionType_CMPRegOrMemAndReg:          /*FALLTHRU*/
            case S86_InstructionType_MOVRegOrMemToOrFromReg: {
                // NOTE: Instruction does not have opcode bits in the 2nd byte
                S86_ASSERT(op_code_size == 1);
                op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(&buffer_it);

                uint8_t w = (op_code_bytes[0] & 0b0000'0001) >> 0;
                uint8_t d = (op_code_bytes[0] & 0b0000'0010) >> 1;
                if (instruction_type == S86_InstructionType_XCHGRegOrMemWithReg ||
                    instruction_type == S86_InstructionType_LEA ||
                    instruction_type == S86_InstructionType_LDS ||
                    instruction_type == S86_InstructionType_LES) {
                    d = 1; // Destintation is always the register
                    if (instruction_type == S86_InstructionType_XCHGRegOrMemWithReg) {
                        if (lock_prefix) {
                            // NOTE: When we XCHG, NASM complains that the
                            // instruction is not lockable, unless, the memory
                            // operand comes first. Here we flip the direction
                            // to ensure the memory operand is the destination.
                            //
                            // listing_0042_completionist_decode_disassembled.asm|319| warning: instruction is not lockable [-w+prefix-lock]
                            d = 0;
                        }
                    } else {
                        w = 1; // Always 16 bit (load into register)
                    }
                }

                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t reg = (op_code_bytes[1] & 0b0011'1000) >> 3;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(d   < 2);
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(reg < 8);
                S86_ASSERT(rm  < 8);

                asm_op.wide = w;
                asm_op.src  = S86_RegisterTypeFromWReg(asm_op.wide, reg);
                if (mod == 0b11) { // NOTE: Register-to-register move
                    asm_op.dest = S86_RegisterTypeFromWReg(asm_op.wide, rm);
                } else { // NOTE: Memory mode w/ effective address calculation
                    S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, w, seg_reg);
                    asm_op.displacement     = effective_address.displacement;
                    asm_op.src              = S86_RegisterTypeFromWReg(w, reg);
                    asm_op.dest             = effective_address.reg_type;
                    asm_op.has_displacement = effective_address.has_displacement;
                    if (d) {
                        asm_op.effective_addr = S86_EffectiveAddress_Src;
                    } else {
                        asm_op.effective_addr = S86_EffectiveAddress_Dest;
                    }
                }

                if (d) {
                    S86_RegisterType tmp = asm_op.src;
                    asm_op.src           = asm_op.dest;
                    asm_op.dest          = tmp;
                }

                S86_PrintAsmOp(asm_op);
            } break;

            // NOTE: Instruction Pattern => [0b0000'00SW | 0bAAA00BBB | DISP-LO | DISP-HI | DATA-LO | DATA-HI]
            // Where S: optional, W: optional, AAA: mod, BBB: rm
            case S86_InstructionType_ADDImmediateToRegOrMem:   /*FALLTHRU*/
            case S86_InstructionType_ADCImmediateToRegOrMem:   /*FALLTHRU*/
            case S86_InstructionType_SUBImmediateFromRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_SBBImmediateFromRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_CMPImmediateWithRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_ANDImmediateToRegOrMem:   /*FALLTHRU*/
            case S86_InstructionType_TESTImmediateAndRegOrMem: /*FALLTHRU*/
            case S86_InstructionType_ORImmediateToRegOrMem:    /*FALLTHRU*/
            case S86_InstructionType_XORImmediateToRegOrMem:   /*FALLTHRU*/
            case S86_InstructionType_MOVImmediateToRegOrMem: {
                S86_ASSERT(op_code_size == 2);
                uint8_t w   = (op_code_bytes[0] & 0b0000'0001) >> 0;
                uint8_t s   = (op_code_bytes[0] & 0b0000'0010) >> 1;
                uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
                uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
                S86_ASSERT(w   < 2);
                S86_ASSERT(mod < 4);
                S86_ASSERT(rm  < 8);

                S86_EffectiveAddressStr8 effective_address = S86_EffectiveAddressCalc(&buffer_it, rm, mod, w, seg_reg);
                asm_op.wide = w;

                // NOTE: Parse data payload
                // =============================================================
                uint16_t data = S86_BufferIteratorNextByte(&buffer_it);
                if (w) { // 16 bit data
                    if ((instruction_type == S86_InstructionType_ADDImmediateToRegOrMem ||
                         instruction_type == S86_InstructionType_ADCImmediateToRegOrMem ||
                         instruction_type == S86_InstructionType_SUBImmediateFromRegOrMem ||
                         instruction_type == S86_InstructionType_SBBImmediateFromRegOrMem ||
                         instruction_type == S86_InstructionType_CMPImmediateWithRegOrMem ||
                         instruction_type == S86_InstructionType_ANDImmediateToRegOrMem ||
                         instruction_type == S86_InstructionType_TESTImmediateAndRegOrMem ||
                         instruction_type == S86_InstructionType_ORImmediateToRegOrMem ||
                         instruction_type == S86_InstructionType_XORImmediateToRegOrMem) && s) {
                        // NOTE: Sign extend 8 bit, since we store into a
                        // int32_t in asm_op this is done for free for us.
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
                asm_op.immediate        = data;
                asm_op.has_displacement = effective_address.has_displacement;
                asm_op.displacement     = effective_address.displacement;
                asm_op.dest             = effective_address.reg_type;
                asm_op.effective_addr   = S86_EffectiveAddress_Dest;
                asm_op.src              = S86_RegisterType_Immediate;

                if (instruction_type == S86_InstructionType_MOVImmediateToRegOrMem)
                    asm_op.wide_prefix = S86_WidePrefix_Src;
                else if (effective_address.has_displacement)
                    asm_op.wide_prefix = S86_WidePrefix_Dest;
                S86_PrintAsmOp(asm_op);
            } break;

            // NOTE: Instruction Pattern => [0b0000'W00W | DATA-LO | DATA-HI]
            case S86_InstructionType_ADDImmediateToAccum:   /*FALLTHRU*/
            case S86_InstructionType_ADCImmediateToAccum:   /*FALLTHRU*/
            case S86_InstructionType_SUBImmediateFromAccum: /*FALLTHRU*/
            case S86_InstructionType_SBBImmediateFromAccum: /*FALLTHRU*/
            case S86_InstructionType_CMPImmediateWithAccum: /*FALLTHRU*/
            case S86_InstructionType_ANDImmediateToAccum:   /*FALLTHRU*/
            case S86_InstructionType_TESTImmediateAndAccum: /*FALLTHRU*/
            case S86_InstructionType_ORImmediateToAccum:    /*FALLTHRU*/
            case S86_InstructionType_XORImmediateToAccum:   /*FALLTHRU*/
            case S86_InstructionType_MOVImmediateToReg: {
                // NOTE: Parse opcode control bits
                // =============================================================
                S86_ASSERT(op_code_size == 1);
                uint8_t w   = 0;
                if (instruction_type == S86_InstructionType_ADDImmediateToAccum ||
                    instruction_type == S86_InstructionType_ADCImmediateToAccum ||
                    instruction_type == S86_InstructionType_SUBImmediateFromAccum ||
                    instruction_type == S86_InstructionType_SBBImmediateFromAccum ||
                    instruction_type == S86_InstructionType_CMPImmediateWithAccum ||
                    instruction_type == S86_InstructionType_ANDImmediateToAccum ||
                    instruction_type == S86_InstructionType_TESTImmediateAndAccum ||
                    instruction_type == S86_InstructionType_ORImmediateToAccum ||
                    instruction_type == S86_InstructionType_XORImmediateToAccum) {
                    w = (op_code_bytes[0] & 0b0000'0001) >> 0;
                } else {
                    w = (op_code_bytes[0] & 0b0000'1000) >> 3;
                }

                // NOTE: Parse data payload
                // =============================================================
                uint16_t data = S86_BufferIteratorNextByte(&buffer_it);
                if (w) { // 16 bit data
                    uint8_t data_hi = S86_BufferIteratorNextByte(&buffer_it);
                    data |= (uint16_t)(data_hi) << 8;
                }

                // NOTE: Disassemble
                // =============================================================
                asm_op.effective_addr = S86_EffectiveAddress_Dest;
                asm_op.src            = S86_RegisterType_Immediate;
                asm_op.wide           = w;
                asm_op.src            = S86_RegisterType_Immediate;
                asm_op.immediate      = data;
                if (instruction_type == S86_InstructionType_MOVImmediateToReg) {
                    uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;
                    asm_op.dest = S86_RegisterTypeFromWReg(w, reg);
                } else {
                    asm_op.dest = asm_op.wide ? S86_RegisterType_AX : S86_RegisterType_AL;
                }
                S86_PrintAsmOp(asm_op);
            } break;

            // NOTE: Instruction Pattern => [0b0000'000W | DATA-LO]
            case S86_InstructionType_INFixedPort:    /*FALLTHRU*/
            case S86_InstructionType_INVariablePort: /*FALLTHRU*/
            case S86_InstructionType_OUTFixedPort:   /*FALLTHRU*/
            case S86_InstructionType_OUTVariablePort: {
                S86_ASSERT(op_code_size == 1);
                asm_op.wide = (op_code_bytes[0] & 0b0000'0001) >> 0;
                asm_op.dest = asm_op.wide ? S86_RegisterType_AX : S86_RegisterType_AL;
                if (instruction_type == S86_InstructionType_INFixedPort ||
                    instruction_type == S86_InstructionType_OUTFixedPort) {
                    asm_op.src       = S86_RegisterType_Immediate;
                    asm_op.immediate = S86_BufferIteratorNextByte(&buffer_it);
                } else {
                    asm_op.src = S86_RegisterType_DX;
                }

                if (instruction_type == S86_InstructionType_OUTFixedPort ||
                    instruction_type == S86_InstructionType_OUTVariablePort) {
                    S86_RegisterType tmp = asm_op.src;
                    asm_op.src           = asm_op.dest;
                    asm_op.dest          = tmp;
                }

                S86_PrintAsmOp(asm_op);
            } break;

            case S86_InstructionType_REP: {
                S86_ASSERT(op_code_size == 1);
                uint8_t string_op = S86_BufferIteratorNextByte(&buffer_it);
                uint8_t w_mask    = 0b0000'0001;
                uint8_t w         = string_op & w_mask;

                S86_Str8 string_type = {0};
                switch (string_op & ~w_mask) {
                    case 0b1010'0100: string_type = S86_STR8("movs"); break;
                    case 0b1010'0110: string_type = S86_STR8("cmps"); break;
                    case 0b1010'1110: string_type = S86_STR8("scas"); break;
                    case 0b1010'1100: string_type = S86_STR8("lods"); break;
                    case 0b1010'1010: string_type = S86_STR8("stos"); break;
                    default: S86_ASSERT(!"Unhandled REP string type"); break;
                }

                S86_PrintLnFmt(" %.*s%c", S86_STR8_FMT(string_type), w ? 'w' : 'b');
            } break;

            // NOTE: Instruction Pattern => [0b0000'0000 | DATA-LO | DATA-HI]
            case S86_InstructionType_MOVAccumToMem:                /*FALLTHRU*/
            case S86_InstructionType_MOVMemToAccum:                /*FALLTHRU*/
            case S86_InstructionType_CALLDirectInterSeg:           /*FALLTHRU*/
            case S86_InstructionType_CALLDirectWithinSeg:          /*FALLTHRU*/
            case S86_InstructionType_JMPDirectInterSeg:            /*FALLTHRU*/
            case S86_InstructionType_RETWithinSegAddImmediateToSP: /*FALLTHRU*/
            case S86_InstructionType_INT: {
                S86_ASSERT(op_code_size == 1);
                uint8_t data_lo = S86_BufferIteratorNextByte(&buffer_it);
                uint16_t data   = data_lo;
                if (instruction_type != S86_InstructionType_INT) {
                    uint8_t data_hi = S86_BufferIteratorNextByte(&buffer_it);
                    data = S86_CAST(uint16_t)data_hi << 8 | (S86_CAST(uint16_t)data_lo);
                }

                if (instruction_type == S86_InstructionType_CALLDirectWithinSeg) {
                    S86_PrintLnFmt(" [bp - %d]", S86_CAST(int16_t)data);
                } else if (instruction_type == S86_InstructionType_RETWithinSegAddImmediateToSP) {
                    S86_PrintLnFmt(" %d", S86_CAST(int16_t)data);
                } else if (instruction_type == S86_InstructionType_CALLDirectInterSeg ||
                           instruction_type == S86_InstructionType_JMPDirectInterSeg) {
                    uint8_t cs_lo = S86_BufferIteratorNextByte(&buffer_it);
                    uint8_t cs_hi = S86_BufferIteratorNextByte(&buffer_it);
                    uint16_t cs   = S86_CAST(uint16_t)cs_hi << 8 | (S86_CAST(uint16_t)cs_lo);
                    S86_PrintLnFmt(" %u:%u", cs, data);
                } else if (instruction_type == S86_InstructionType_MOVAccumToMem) {
                    S86_PrintLnFmt(" [%u], ax", data);
                } else if (instruction_type == S86_InstructionType_MOVMemToAccum) {
                    S86_PrintLnFmt(" ax, [%u]", data);
                } else {
                    S86_PrintLnFmt(" %u", data);
                }
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
                    S86_PrintLnFmt(" $+2%c%d", sign, jump_offset);
                } else if (instruction_type == S86_InstructionType_XLAT         ||
                           instruction_type == S86_InstructionType_LAHF         ||
                           instruction_type == S86_InstructionType_SAHF         ||
                           instruction_type == S86_InstructionType_PUSHF        ||
                           instruction_type == S86_InstructionType_POPF         ||
                           instruction_type == S86_InstructionType_DAA          ||
                           instruction_type == S86_InstructionType_AAA          ||
                           instruction_type == S86_InstructionType_DAS          ||
                           instruction_type == S86_InstructionType_AAS          ||
                           instruction_type == S86_InstructionType_AAM          ||
                           instruction_type == S86_InstructionType_AAD          ||
                           instruction_type == S86_InstructionType_CBW          ||
                           instruction_type == S86_InstructionType_CWD          ||
                           instruction_type == S86_InstructionType_RETWithinSeg ||
                           instruction_type == S86_InstructionType_INT3         ||
                           instruction_type == S86_InstructionType_INTO         ||
                           instruction_type == S86_InstructionType_IRET         ||
                           instruction_type == S86_InstructionType_CLC          ||
                           instruction_type == S86_InstructionType_CMC          ||
                           instruction_type == S86_InstructionType_STC          ||
                           instruction_type == S86_InstructionType_CLD          ||
                           instruction_type == S86_InstructionType_STD          ||
                           instruction_type == S86_InstructionType_CLI          ||
                           instruction_type == S86_InstructionType_STI          ||
                           instruction_type == S86_InstructionType_HLT          ||
                           instruction_type == S86_InstructionType_WAIT) {
                    // NOTE: Mnemonic instruction only, already printed
                    S86_Print(S86_STR8("\n"));
                } else if (instruction_type == S86_InstructionType_LOCK) {
                    // NOTE: Mnemonic prefix, no new line as the next instruction
                    // will be prefixed with this instruction
                    S86_Print(S86_STR8(" "));
                    lock_prefix = true;
                } else if (instruction_type == S86_InstructionType_SEGMENT) {
                    // NOTE: Mnemonic does not generate any assembly
                    S86_ASSERT(op_code_size == 1);
                    uint8_t sr = (op_code_bytes[0] & 0b0001'1000) >> 3;
                    seg_reg    = S86_SegmentRegisterTypeFromSR(sr);
                } else {
                    S86_Print(S86_STR8("\n"));
                    S86_ASSERT(!"Unhandled instruction");
                }
            } break;
        }

        if (instruction_type != S86_InstructionType_LOCK)
            lock_prefix = false;

        if (instruction_type != S86_InstructionType_SEGMENT)
            seg_reg = S86_SegmentRegisterType_Invalid;
    }
}
