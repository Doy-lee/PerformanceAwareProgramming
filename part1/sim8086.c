#define WIN32_MEAN_AND_LEAN
#define NOMINMAX
#include <Windows.h>
#include <immintrin.h>

#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

#include "sim8086_stdlib.h"
#include "sim8086.h"

#include "sim8086_stdlib.c"

bool S86_RegisterFileFlagsEq(S86_RegisterFileFlags lhs, S86_RegisterFileFlags rhs)
{
    bool result = lhs.carry           == rhs.carry    &&
                  lhs.zero            == rhs.zero     &&
                  lhs.sign            == rhs.sign     &&
                  lhs.overflow        == rhs.overflow &&
                  lhs.parity          == rhs.parity   &&
                  lhs.auxiliary_carry == rhs.auxiliary_carry;
    return result;
}

S86_Str8 S86_MnemonicStr8(S86_Mnemonic type)
{
    S86_Str8 result = {0};
    switch (type) {
        case S86_Mnemonic_MOV: result = S86_STR8("mov"); break;
        case S86_Mnemonic_PUSH: result = S86_STR8("push"); break;
        case S86_Mnemonic_POP: result = S86_STR8("pop"); break;
        case S86_Mnemonic_XCHG: result = S86_STR8("xchg"); break;
        case S86_Mnemonic_IN: result = S86_STR8("in"); break;
        case S86_Mnemonic_OUT: result = S86_STR8("out"); break;
        case S86_Mnemonic_XLAT: result = S86_STR8("xlat"); break;
        case S86_Mnemonic_LEA: result = S86_STR8("lea"); break;
        case S86_Mnemonic_LDS: result = S86_STR8("lds"); break;
        case S86_Mnemonic_LES: result = S86_STR8("les"); break;
        case S86_Mnemonic_LAHF: result = S86_STR8("lahf"); break;
        case S86_Mnemonic_SAHF: result = S86_STR8("sahf"); break;
        case S86_Mnemonic_PUSHF: result = S86_STR8("pushf"); break;
        case S86_Mnemonic_POPF: result = S86_STR8("popf"); break;
        case S86_Mnemonic_ADD: result = S86_STR8("add"); break;
        case S86_Mnemonic_ADC: result = S86_STR8("adc"); break;
        case S86_Mnemonic_INC: result = S86_STR8("inc"); break;
        case S86_Mnemonic_AAA: result = S86_STR8("aaa"); break;
        case S86_Mnemonic_DAA: result = S86_STR8("daa"); break;
        case S86_Mnemonic_SUB: result = S86_STR8("sub"); break;
        case S86_Mnemonic_SBB: result = S86_STR8("sbb"); break;
        case S86_Mnemonic_DEC: result = S86_STR8("dec"); break;
        case S86_Mnemonic_NEG: result = S86_STR8("neg"); break;
        case S86_Mnemonic_CMP: result = S86_STR8("cmp"); break;
        case S86_Mnemonic_AAS: result = S86_STR8("aas"); break;
        case S86_Mnemonic_DAS: result = S86_STR8("das"); break;
        case S86_Mnemonic_MUL: result = S86_STR8("mul"); break;
        case S86_Mnemonic_IMUL: result = S86_STR8("imul"); break;
        case S86_Mnemonic_AAM: result = S86_STR8("aam"); break;
        case S86_Mnemonic_DIV: result = S86_STR8("div"); break;
        case S86_Mnemonic_IDIV: result = S86_STR8("idiv"); break;
        case S86_Mnemonic_AAD: result = S86_STR8("aad"); break;
        case S86_Mnemonic_CBW: result = S86_STR8("cbw"); break;
        case S86_Mnemonic_CWD: result = S86_STR8("cwd"); break;
        case S86_Mnemonic_NOT: result = S86_STR8("not"); break;
        case S86_Mnemonic_SHL_SAL: result = S86_STR8("sal"); break;
        case S86_Mnemonic_SHR: result = S86_STR8("shr"); break;
        case S86_Mnemonic_SAR: result = S86_STR8("sar"); break;
        case S86_Mnemonic_ROL: result = S86_STR8("rol"); break;
        case S86_Mnemonic_ROR: result = S86_STR8("ror"); break;
        case S86_Mnemonic_RCL: result = S86_STR8("rcl"); break;
        case S86_Mnemonic_RCR: result = S86_STR8("rcr"); break;
        case S86_Mnemonic_AND: result = S86_STR8("and"); break;
        case S86_Mnemonic_TEST: result = S86_STR8("test"); break;
        case S86_Mnemonic_OR: result = S86_STR8("or"); break;
        case S86_Mnemonic_XOR: result = S86_STR8("xor"); break;
        case S86_Mnemonic_REP: result = S86_STR8("rep"); break;
        case S86_Mnemonic_CALL: result = S86_STR8("call"); break;
        case S86_Mnemonic_JMP: result = S86_STR8("jmp"); break;
        case S86_Mnemonic_RET: result = S86_STR8("ret"); break;
        case S86_Mnemonic_JE_JZ: result = S86_STR8("je"); break;
        case S86_Mnemonic_JL_JNGE: result = S86_STR8("jl"); break;
        case S86_Mnemonic_JLE_JNG: result = S86_STR8("jle"); break;
        case S86_Mnemonic_JB_JNAE: result = S86_STR8("jb"); break;
        case S86_Mnemonic_JBE_JNA: result = S86_STR8("jbe"); break;
        case S86_Mnemonic_JP_JPE: result = S86_STR8("jp"); break;
        case S86_Mnemonic_JO: result = S86_STR8("jo"); break;
        case S86_Mnemonic_JS: result = S86_STR8("js"); break;
        case S86_Mnemonic_JNE_JNZ: result = S86_STR8("jne"); break;
        case S86_Mnemonic_JNL_JGE: result = S86_STR8("jnl"); break;
        case S86_Mnemonic_JNLE_JG: result = S86_STR8("jg"); break;
        case S86_Mnemonic_JNB_JAE: result = S86_STR8("jnb"); break;
        case S86_Mnemonic_JNBE_JA: result = S86_STR8("ja"); break;
        case S86_Mnemonic_JNP_JO: result = S86_STR8("jnp"); break;
        case S86_Mnemonic_JNO: result = S86_STR8("jno"); break;
        case S86_Mnemonic_JNS: result = S86_STR8("jns"); break;
        case S86_Mnemonic_LOOP: result = S86_STR8("loop"); break;
        case S86_Mnemonic_LOOPZ_LOOPE: result = S86_STR8("loopz"); break;
        case S86_Mnemonic_LOOPNZ_LOOPNE: result = S86_STR8("loopnz"); break;
        case S86_Mnemonic_JCXZ: result = S86_STR8("jcxz"); break;
        case S86_Mnemonic_INT: result = S86_STR8("int"); break;
        case S86_Mnemonic_INT3: result = S86_STR8("int3"); break;
        case S86_Mnemonic_INTO: result = S86_STR8("into"); break;
        case S86_Mnemonic_IRET: result = S86_STR8("iret"); break;
        case S86_Mnemonic_CLC: result = S86_STR8("clc"); break;
        case S86_Mnemonic_CMC: result = S86_STR8("cmc"); break;
        case S86_Mnemonic_STC: result = S86_STR8("stc"); break;
        case S86_Mnemonic_CLD: result = S86_STR8("cld"); break;
        case S86_Mnemonic_STD: result = S86_STR8("std"); break;
        case S86_Mnemonic_CLI: result = S86_STR8("cli"); break;
        case S86_Mnemonic_STI: result = S86_STR8("sti"); break;
        case S86_Mnemonic_HLT: result = S86_STR8("hlt"); break;
        case S86_Mnemonic_WAIT: result = S86_STR8("wait"); break;
        case S86_Mnemonic_LOCK: result = S86_STR8("lock"); break;
        case S86_Mnemonic_SEGMENT: result = S86_STR8("segment"); break;
    }
    return result;
}

S86_MnemonicOp S86_MnemonicOpFromWReg(bool w, uint8_t reg)
{
    S86_ASSERT(reg < 8);
    S86_MnemonicOp const type_table[2][8] = {
        [0b0] = {
            [0] = S86_MnemonicOp_AL,
            [1] = S86_MnemonicOp_CL,
            [2] = S86_MnemonicOp_DL,
            [3] = S86_MnemonicOp_BL,
            [4] = S86_MnemonicOp_AH,
            [5] = S86_MnemonicOp_CH,
            [6] = S86_MnemonicOp_DH,
            [7] = S86_MnemonicOp_BH,
        },
        [0b1] = {
            [0] = S86_MnemonicOp_AX,
            [1] = S86_MnemonicOp_CX,
            [2] = S86_MnemonicOp_DX,
            [3] = S86_MnemonicOp_BX,
            [4] = S86_MnemonicOp_SP,
            [5] = S86_MnemonicOp_BP,
            [6] = S86_MnemonicOp_SI,
            [7] = S86_MnemonicOp_DI,
        },
    };

    S86_MnemonicOp result = type_table[w][reg];
    return result;
}

S86_MnemonicOp S86_MnemonicOpFromSR(uint8_t sr)
{
    S86_ASSERT(sr < 4);
    S86_MnemonicOp result = S86_MnemonicOp_ES + sr;
    return result;
}

S86_Str8 S86_MnemonicOpStr8(S86_MnemonicOp type)
{
    S86_Str8 result = {0};
    switch (type) {
        case S86_MnemonicOp_Invalid:            result = S86_STR8("");        break;
        case S86_MnemonicOp_AL:                 result = S86_STR8("al");      break;
        case S86_MnemonicOp_CL:                 result = S86_STR8("cl");      break;
        case S86_MnemonicOp_DL:                 result = S86_STR8("dl");      break;
        case S86_MnemonicOp_BL:                 result = S86_STR8("bl");      break;
        case S86_MnemonicOp_AH:                 result = S86_STR8("ah");      break;
        case S86_MnemonicOp_CH:                 result = S86_STR8("ch");      break;
        case S86_MnemonicOp_DH:                 result = S86_STR8("dh");      break;
        case S86_MnemonicOp_BH:                 result = S86_STR8("bh");      break;
        case S86_MnemonicOp_AX:                 result = S86_STR8("ax");      break;
        case S86_MnemonicOp_CX:                 result = S86_STR8("cx");      break;
        case S86_MnemonicOp_DX:                 result = S86_STR8("dx");      break;
        case S86_MnemonicOp_BX:                 result = S86_STR8("bx");      break;
        case S86_MnemonicOp_SP:                 result = S86_STR8("sp");      break;
        case S86_MnemonicOp_BP:                 result = S86_STR8("bp");      break;
        case S86_MnemonicOp_SI:                 result = S86_STR8("si");      break;
        case S86_MnemonicOp_DI:                 result = S86_STR8("di");      break;
        case S86_MnemonicOp_BX_SI:              result = S86_STR8("bx+si"); break;
        case S86_MnemonicOp_BX_DI:              result = S86_STR8("bx+di"); break;
        case S86_MnemonicOp_BP_SI:              result = S86_STR8("bp+si"); break;
        case S86_MnemonicOp_BP_DI:              result = S86_STR8("bp+di"); break;
        case S86_MnemonicOp_DirectAddress:      result = S86_STR8("");        break;
        case S86_MnemonicOp_Immediate:          result = S86_STR8("");        break;
        case S86_MnemonicOp_ES:                 result = S86_STR8("es");      break;
        case S86_MnemonicOp_CS:                 result = S86_STR8("cs");      break;
        case S86_MnemonicOp_SS:                 result = S86_STR8("ss");      break;
        case S86_MnemonicOp_DS:                 result = S86_STR8("ds");      break;
        case S86_MnemonicOp_MOVS:               result = S86_STR8("movs");    break;
        case S86_MnemonicOp_CMPS:               result = S86_STR8("cmps");    break;
        case S86_MnemonicOp_SCAS:               result = S86_STR8("scas");    break;
        case S86_MnemonicOp_LODS:               result = S86_STR8("lods");    break;
        case S86_MnemonicOp_STOS:               result = S86_STR8("stos");    break;
        case S86_MnemonicOp_DirectInterSegment: result = S86_STR8("");        break;
        case S86_MnemonicOp_Jump:               result = S86_STR8("");        break;
    }
    return result;
}

bool S86_MnemonicOpIsAccumulator(S86_MnemonicOp type)
{
    bool result = type == S86_MnemonicOp_AX ||
                  type == S86_MnemonicOp_AL ||
                  type == S86_MnemonicOp_AH;
    return result;
}

bool S86_MnemonicOpIsRegister(S86_MnemonicOp type)
{
    bool result = (type >= S86_MnemonicOp_AL && type <= S86_MnemonicOp_DI) ||
                  (type >= S86_MnemonicOp_ES && type <= S86_MnemonicOp_DS);
    return result;
}

bool S86_MnemonicOpIsEffectiveAddress(S86_MnemonicOp type)
{
    bool result = (type >= S86_MnemonicOp_BX_SI && type <= S86_MnemonicOp_BP_DI) ||
                  (type == S86_MnemonicOp_DirectAddress);
    return result;
}

S86_Str8 S86_RegisterFileRegArrayStr8(S86_RegisterFileRegArray type)
{
    S86_Str8 result = {0};
    switch (type) {
        case S86_RegisterFileRegArray_AX: result = S86_MnemonicOpStr8(S86_MnemonicOp_AX); break;
        case S86_RegisterFileRegArray_BX: result = S86_MnemonicOpStr8(S86_MnemonicOp_BX); break;
        case S86_RegisterFileRegArray_CX: result = S86_MnemonicOpStr8(S86_MnemonicOp_CX); break;
        case S86_RegisterFileRegArray_DX: result = S86_MnemonicOpStr8(S86_MnemonicOp_DX); break;
        case S86_RegisterFileRegArray_SP: result = S86_MnemonicOpStr8(S86_MnemonicOp_SP); break;
        case S86_RegisterFileRegArray_BP: result = S86_MnemonicOpStr8(S86_MnemonicOp_BP); break;
        case S86_RegisterFileRegArray_SI: result = S86_MnemonicOpStr8(S86_MnemonicOp_SI); break;
        case S86_RegisterFileRegArray_DI: result = S86_MnemonicOpStr8(S86_MnemonicOp_DI); break;
        case S86_RegisterFileRegArray_ES: result = S86_MnemonicOpStr8(S86_MnemonicOp_ES); break;
        case S86_RegisterFileRegArray_CS: result = S86_MnemonicOpStr8(S86_MnemonicOp_DS); break;
        case S86_RegisterFileRegArray_SS: result = S86_MnemonicOpStr8(S86_MnemonicOp_SS); break;
        case S86_RegisterFileRegArray_DS: result = S86_MnemonicOpStr8(S86_MnemonicOp_DS); break;
        case S86_RegisterFileRegArray_Count: break;
    }
    return result;
}

void S86_PrintOpcodeMnemonicOp(S86_Opcode opcode, bool src)
{
    // TODO: It sucks to have these enums that specify source or dest because
    // we can't have a nice generic codepath to handle the dest, src mnemonic
    // ops without these pre-emptive checks here inorder to make it generic
    // here.
    //
    // It's probably better to just have flags for the src and dest mnemonic op
    // and then you can have one code path that just checks the flags on each op
    S86_MnemonicOp mnemonic_op = src ? opcode.src : opcode.dest;

    bool effective_addr = ( src && opcode.effective_addr == S86_EffectiveAddress_Src) ||
                          (!src && opcode.effective_addr == S86_EffectiveAddress_Dest);

    if (mnemonic_op == S86_MnemonicOp_Invalid)
        return;

    if (src)
        S86_PrintFmt(", ");
    else
        S86_PrintFmt(" ");

    if (!src && opcode.word_byte_prefix != S86_WordBytePrefix_None)
        S86_PrintFmt("%s ", opcode.word_byte_prefix == S86_WordBytePrefix_Word ? "word" : "byte");

    if (effective_addr && opcode.seg_reg_prefix != S86_MnemonicOp_Invalid) {
        S86_Str8 prefix = S86_MnemonicOpStr8(opcode.seg_reg_prefix);
        S86_PrintFmt("%.*s:", S86_STR8_FMT(prefix));
    }

    if (opcode.effective_addr_loads_mem && effective_addr)
        S86_Print(S86_STR8("["));

    if (mnemonic_op == S86_MnemonicOp_DirectAddress) {
        S86_PrintFmt("%s%d",
                     opcode.displacement >= 0 ? "+" : "-",
                     opcode.displacement >= 0 ? opcode.displacement : -opcode.displacement);
    } else if (mnemonic_op == S86_MnemonicOp_Jump) {
        // NOTE: Account for the opcode itself which is 2 bytes, e.g. we can print $+2-8 or just $-6
        int32_t displacement = opcode.displacement + 2;
        S86_PrintFmt("$%c%d",
                     displacement > 0 ? '+' : '-',
                     displacement > 0 ? displacement : -displacement);
    } else if (mnemonic_op == S86_MnemonicOp_Immediate) {
        if (opcode.immediate_is_8bit) {
            S86_PrintFmt("%d", (int8_t)opcode.immediate);
        } else {
            S86_PrintFmt("%u", (uint16_t)opcode.immediate);
        }
    } else if (mnemonic_op == S86_MnemonicOp_DirectInterSegment) {
        uint16_t left  = (uint32_t)opcode.displacement >> 16;
        uint16_t right = (uint32_t)opcode.displacement  & 0xFFFF;
        S86_PrintFmt("%u:%u", left, right);
    } else {
        S86_Str8 reg_str8 = S86_MnemonicOpStr8(mnemonic_op);
        S86_PrintFmt("%.*s", S86_STR8_FMT(reg_str8));

        if (mnemonic_op >= S86_MnemonicOp_MOVS && mnemonic_op <= S86_MnemonicOp_STOS) {
            S86_PrintFmt("%c", opcode.wide ? 'w' : 'b');
        }

        if (effective_addr && opcode.displacement) {
            S86_PrintFmt("%s%d",
                         opcode.displacement >= 0 ? "+" : "-",
                         opcode.displacement >= 0 ? opcode.displacement : -opcode.displacement);
        }
    }

    if (effective_addr && opcode.effective_addr_loads_mem)
        S86_Print(S86_STR8("]"));
}

void S86_PrintOpcode(S86_Opcode opcode)
{
    if (opcode.mnemonic == S86_Mnemonic_SEGMENT)
        return;

    S86_Str8 mnemonic = S86_MnemonicStr8(opcode.mnemonic);
    S86_PrintFmt("%.*s", S86_STR8_FMT(mnemonic));
    S86_PrintOpcodeMnemonicOp(opcode, false /*src*/);
    S86_PrintOpcodeMnemonicOp(opcode, true /*dest*/);

    if (opcode.mnemonic == S86_Mnemonic_LOCK)
        S86_Print(S86_STR8(" "));
}

void S86_DecodeEffectiveAddr(S86_Opcode *opcode, S86_BufferIterator *buffer_it, uint8_t rm, uint8_t mod, uint8_t w)
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

    opcode->wide           = w;
    opcode->displacement   = displacement;
    opcode->effective_addr = S86_EffectiveAddress_Dest;
    if (mod == 0b11) {
        opcode->dest = S86_MnemonicOpFromWReg(w, rm);
    } else {
        // NOTE: Effective address calculation w/ displacement
        // =====================================================================
        opcode->effective_addr_loads_mem = true;
        if (direct_address) {
            opcode->dest = S86_MnemonicOp_DirectAddress;
        } else {
            switch (rm) {
                case 0b000: opcode->dest = S86_MnemonicOp_BX_SI; break;
                case 0b001: opcode->dest = S86_MnemonicOp_BX_DI; break;
                case 0b010: opcode->dest = S86_MnemonicOp_BP_SI; break;
                case 0b011: opcode->dest = S86_MnemonicOp_BP_DI; break;
                case 0b100: opcode->dest = S86_MnemonicOp_SI; break;
                case 0b101: opcode->dest = S86_MnemonicOp_DI; break;
                case 0b110: opcode->dest = S86_MnemonicOp_BP; break;
                case 0b111: opcode->dest = S86_MnemonicOp_BX; break;
                default: S86_ASSERT(!"Invalid rm value, must be 3 bits"); break;
            }
        }
    }
}

S86_Opcode S86_DecodeOpcode(S86_BufferIterator *buffer_it,
                            S86_OpDecode const *decode_table,
                            uint16_t            decode_table_size,
                            bool               *lock_prefix,
                            S86_MnemonicOp     *seg_reg,
                            bool                cycle_count_8088)
{
    (void)cycle_count_8088;

    size_t buffer_start_index     = buffer_it->index;
    char op_code_bytes[2]         = {0};
    size_t op_code_size           = 0;
    op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(buffer_it);

    // NOTE: Match the assembly bytes to the desired instruction ///////////////////////////////////
    S86_OpDecodeType    op_decode_type = S86_OpDecodeType_Count;
    S86_OpDecode const *op_decode      = NULL;
    for (size_t op_index = 0;
         op_decode_type == S86_OpDecodeType_Count && op_index < decode_table_size;
         op_index++)
    {
        S86_OpDecode const *item = decode_table + op_index;

        // NOTE: Check first instruction byte //////////////////////////////////////////////////////
        if ((op_code_bytes[0] & item->op_mask0) != item->op_bits0)
            continue;

        // NOTE Check multi-byte instruction ///////////////////////////////////////////////////////
        // If the matched instruction has a bit mask for the 2nd byte, this
        // is a multi-byte instruction. Check if the 2nd byte checks out.
        bool op_match = true;
        if (item->op_mask1) {
            // TODO: This assumes the iterator is valid
            uint8_t op_byte = S86_BufferIteratorPeekByte(buffer_it);
            op_match = (op_byte & item->op_mask1) == item->op_bits1;
            if (op_match) {
                op_code_bytes[op_code_size++] = op_byte;
                S86_BufferIteratorNextByte(buffer_it);
            }
        }

        if (op_match) {
            op_decode_type = op_index;
            op_decode      = item;
        }
    }

    // NOTE: Disassemble bytes to assembly mnemonics ///////////////////////////////////////////////
    S86_ASSERT(op_code_size > 0 && op_code_size <= S86_ARRAY_UCOUNT(op_code_bytes));
    S86_ASSERT(op_decode_type != S86_OpDecodeType_Count && "Unknown instruction");

    S86_Opcode result      = {0};
    result.type            = op_decode_type;
    result.mnemonic        = op_decode->mnemonic;
    result.lock_prefix     = *lock_prefix;
    result.seg_reg_prefix  = *seg_reg;

    S86_ASSERT(*seg_reg == S86_MnemonicOp_Invalid || (*seg_reg >= S86_MnemonicOp_ES && *seg_reg <= S86_MnemonicOp_DS));
    switch (op_decode_type) {
        // NOTE: Instruction Pattern => [0b0000'0000W | 0bAA00'0CCC | DISP-LO | DISP-HI]
        // Where, W: Optional, AA: mod, CCC: R/M
        case S86_OpDecodeType_JMPIndirectWithinSeg:  /*FALLTHRU*/
        case S86_OpDecodeType_CALLIndirectWithinSeg: /*FALLTHRU*/
        case S86_OpDecodeType_NOT:                   /*FALLTHRU*/
        case S86_OpDecodeType_SHL_SAL:               /*FALLTHRU*/
        case S86_OpDecodeType_SHR:                   /*FALLTHRU*/
        case S86_OpDecodeType_SAR:                   /*FALLTHRU*/
        case S86_OpDecodeType_ROL:                   /*FALLTHRU*/
        case S86_OpDecodeType_ROR:                   /*FALLTHRU*/
        case S86_OpDecodeType_RCL:                   /*FALLTHRU*/
        case S86_OpDecodeType_RCR:                   /*FALLTHRU*/
        case S86_OpDecodeType_MUL:                   /*FALLTHRU*/
        case S86_OpDecodeType_IMUL:                  /*FALLTHRU*/
        case S86_OpDecodeType_DIV:                   /*FALLTHRU*/
        case S86_OpDecodeType_IDIV:                  /*FALLTHRU*/
        case S86_OpDecodeType_INCRegOrMem:           /*FALLTHRU*/
        case S86_OpDecodeType_DECRegOrMem:           /*FALLTHRU*/
        case S86_OpDecodeType_NEG:                   /*FALLTHRU*/
        case S86_OpDecodeType_POPRegOrMem:           /*FALLTHRU*/
        case S86_OpDecodeType_PUSHRegOrMem: {
            S86_ASSERT(op_code_size == 2);
            uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
            uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
            S86_ASSERT(mod < 4); S86_ASSERT(rm  < 8);

            uint8_t w = 1;
            if (op_decode_type == S86_OpDecodeType_INCRegOrMem ||
                op_decode_type == S86_OpDecodeType_DECRegOrMem ||
                op_decode_type == S86_OpDecodeType_NEG         ||
                op_decode_type == S86_OpDecodeType_MUL         ||
                op_decode_type == S86_OpDecodeType_MUL         ||
                op_decode_type == S86_OpDecodeType_IMUL        ||
                op_decode_type == S86_OpDecodeType_DIV         ||
                op_decode_type == S86_OpDecodeType_IDIV        ||
                (op_decode_type >= S86_OpDecodeType_NOT &&
                 op_decode_type <= S86_OpDecodeType_RCR)) {
                w = op_code_bytes[0] & 0b0000'0001;
            }

            S86_DecodeEffectiveAddr(&result, buffer_it, rm, mod, w);
            // NOTE: Bit shifts use 'v' to indicate if shift distance should
            // come from cl register otherwise bitshift by 1
            if (op_decode_type >= S86_OpDecodeType_SHL_SAL && op_decode_type <= S86_OpDecodeType_RCR) {
                uint8_t v = (op_code_bytes[0] & 0b0000'0010) >> 1;
                if (v) {
                    result.src = S86_MnemonicOp_CL;
                } else {
                    result.src       = S86_MnemonicOp_Immediate;
                    result.immediate = 1;
                }
            }
        } break;

        // NOTE: Instruction Pattern => [0b0000'0000]
        // Generally handles instructions with control bits in any position in the first byte
        case S86_OpDecodeType_DECReg:           /*FALLTHRU*/
        case S86_OpDecodeType_INCReg:           /*FALLTHRU*/
        case S86_OpDecodeType_XCHGRegWithAccum: /*FALLTHRU*/
        case S86_OpDecodeType_PUSHReg:          /*FALLTHRU*/
        case S86_OpDecodeType_POPReg:           /*FALLTHRU*/
        case S86_OpDecodeType_PUSHSegReg:       /*FALLTHRU*/
        case S86_OpDecodeType_POPSegReg: {
            S86_ASSERT(op_code_size == 1);
            if (op_decode_type == S86_OpDecodeType_PUSHReg ||
                op_decode_type == S86_OpDecodeType_POPReg ||
                op_decode_type == S86_OpDecodeType_INCReg ||
                op_decode_type == S86_OpDecodeType_DECReg ||
                op_decode_type == S86_OpDecodeType_XCHGRegWithAccum) {
                uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;
                result.dest  = S86_MnemonicOpFromWReg(1, reg);
            } else {
                S86_ASSERT(op_decode_type == S86_OpDecodeType_PUSHSegReg ||
                           op_decode_type == S86_OpDecodeType_POPSegReg);
                uint8_t sr = (op_code_bytes[0] & 0b0001'1000) >> 3;
                result.dest = S86_MnemonicOpFromSR(sr);
            }

            if (op_decode_type == S86_OpDecodeType_XCHGRegWithAccum) {
                result.src  = result.dest;
                result.dest = S86_MnemonicOp_AX;
            }
        } break;

        // NOTE: Instruction Pattern => [0b0000'000DW | 0bAABB'BCCC | DISP-LO | DISP-HI | DATA-LO | DATA-HI]
        // Where, D: optional, W: optional, AA: mod, BBB: reg, CCC: r/m
        case S86_OpDecodeType_ADDRegOrMemToOrFromReg:     /*FALLTHRU*/
        case S86_OpDecodeType_ADCRegOrMemWithRegToEither: /*FALLTHRU*/
        case S86_OpDecodeType_SUBRegOrMemToOrFromReg:     /*FALLTHRU*/
        case S86_OpDecodeType_SBBRegOrMemAndRegToEither:  /*FALLTHRU*/
        case S86_OpDecodeType_ANDRegWithMemToEither:      /*FALLTHRU*/
        case S86_OpDecodeType_TESTRegOrMemAndReg:         /*FALLTHRU*/
        case S86_OpDecodeType_ORRegOrMemAndRegToEither:   /*FALLTHRU*/
        case S86_OpDecodeType_XORRegOrMemAndRegToEither:  /*FALLTHRU*/
        case S86_OpDecodeType_LEA:                        /*FALLTHRU*/
        case S86_OpDecodeType_LDS:                        /*FALLTHRU*/
        case S86_OpDecodeType_LES:                        /*FALLTHRU*/
        case S86_OpDecodeType_XCHGRegOrMemWithReg:        /*FALLTHRU*/
        case S86_OpDecodeType_CMPRegOrMemAndReg:          /*FALLTHRU*/
        case S86_OpDecodeType_MOVRegOrMemToOrFromReg:     /*FALLTHRU*/
        case S86_OpDecodeType_MOVSegRegToRegOrMem:        /*FALLTHRU*/
        case S86_OpDecodeType_MOVRegOrMemToSegReg: {
            // NOTE: Instruction does not have opcode bits in the 2nd byte
            if (op_decode_type == S86_OpDecodeType_MOVSegRegToRegOrMem ||
                op_decode_type == S86_OpDecodeType_MOVRegOrMemToSegReg) {
                S86_ASSERT(op_code_size == 2);
            } else {
                S86_ASSERT(op_code_size == 1);
                op_code_bytes[op_code_size++] = S86_BufferIteratorNextByte(buffer_it);
            }

            uint8_t d   = 0;
            uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
            uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
            S86_ASSERT(mod < 4);
            S86_ASSERT(rm  < 8);

            if (op_decode_type == S86_OpDecodeType_MOVRegOrMemToSegReg ||
                op_decode_type == S86_OpDecodeType_MOVSegRegToRegOrMem) {
                uint8_t sr  = (op_code_bytes[1] & 0b0001'1000) >> 3;
                result.src  = S86_MnemonicOpFromSR(sr);
                result.wide = 1;
                if (op_decode_type == S86_OpDecodeType_MOVRegOrMemToSegReg)
                    d = 1;
            } else {
                result.wide = (op_code_bytes[0] & 0b0000'0001) >> 0;
                d = (op_code_bytes[0] & 0b0000'0010) >> 1;
                if (op_decode_type == S86_OpDecodeType_XCHGRegOrMemWithReg ||
                    op_decode_type == S86_OpDecodeType_LEA ||
                    op_decode_type == S86_OpDecodeType_LDS ||
                    op_decode_type == S86_OpDecodeType_LES) {
                    d = 1; // Destintation is always the register
                    if (op_decode_type == S86_OpDecodeType_XCHGRegOrMemWithReg) {
                        if (*lock_prefix) {
                            // NOTE: When we XCHG, NASM complains that the
                            // instruction is not lockable, unless, the memory
                            // operand comes first. Here we flip the direction
                            // to ensure the memory operand is the destination.
                            //
                            // listing_0042_completionist_decode_disassembled.asm|319| warning: instruction is not lockable [-w+prefix-lock]
                            d = 0;
                        }
                    } else {
                        result.wide = 1; // Always 16 bit (load into register)
                    }
                }

                uint8_t reg = (op_code_bytes[1] & 0b0011'1000) >> 3;
                result.src  = S86_MnemonicOpFromWReg(result.wide, reg);
            }
            S86_ASSERT(result.wide < 2);
            S86_ASSERT(d < 2);

            if (mod == 0b11) { // NOTE: Register-to-register move
                result.dest = S86_MnemonicOpFromWReg(result.wide, rm);
            } else { // NOTE: Memory mode w/ effective address calculation
                S86_DecodeEffectiveAddr(&result, buffer_it, rm, mod, result.wide);
                if (d)
                    result.effective_addr = S86_EffectiveAddress_Src;
                else
                    result.effective_addr = S86_EffectiveAddress_Dest;
            }

            if (d) {
                S86_MnemonicOp tmp = result.src;
                result.src         = result.dest;
                result.dest        = tmp;
            }
        } break;

        // NOTE: Instruction Pattern => [0b0000'00SW | 0bAAA00BBB | DISP-LO | DISP-HI | DATA-LO | DATA-HI]
        // Where S: optional, W: optional, AAA: mod, BBB: rm
        case S86_OpDecodeType_ADDImmediateToRegOrMem:   /*FALLTHRU*/
        case S86_OpDecodeType_ADCImmediateToRegOrMem:   /*FALLTHRU*/
        case S86_OpDecodeType_SUBImmediateFromRegOrMem: /*FALLTHRU*/
        case S86_OpDecodeType_SBBImmediateFromRegOrMem: /*FALLTHRU*/
        case S86_OpDecodeType_CMPImmediateWithRegOrMem: /*FALLTHRU*/
        case S86_OpDecodeType_ANDImmediateToRegOrMem:   /*FALLTHRU*/
        case S86_OpDecodeType_TESTImmediateAndRegOrMem: /*FALLTHRU*/
        case S86_OpDecodeType_ORImmediateToRegOrMem:    /*FALLTHRU*/
        case S86_OpDecodeType_XORImmediateToRegOrMem:   /*FALLTHRU*/
        case S86_OpDecodeType_MOVImmediateToRegOrMem: {
            S86_ASSERT(op_code_size == 2);
            uint8_t w   = (op_code_bytes[0] & 0b0000'0001) >> 0;
            uint8_t s   = (op_code_bytes[0] & 0b0000'0010) >> 1;
            uint8_t mod = (op_code_bytes[1] & 0b1100'0000) >> 6;
            uint8_t rm  = (op_code_bytes[1] & 0b0000'0111) >> 0;
            S86_ASSERT(w   < 2);
            S86_ASSERT(mod < 4);
            S86_ASSERT(rm  < 8);
            S86_DecodeEffectiveAddr(&result, buffer_it, rm, mod, w);

            // NOTE: Parse data payload
            // =============================================================
            uint16_t data = S86_BufferIteratorNextByte(buffer_it);
            if (w) { // 16 bit data
                if ((op_decode_type == S86_OpDecodeType_ADDImmediateToRegOrMem ||
                     op_decode_type == S86_OpDecodeType_ADCImmediateToRegOrMem ||
                     op_decode_type == S86_OpDecodeType_SUBImmediateFromRegOrMem ||
                     op_decode_type == S86_OpDecodeType_SBBImmediateFromRegOrMem ||
                     op_decode_type == S86_OpDecodeType_CMPImmediateWithRegOrMem ||
                     op_decode_type == S86_OpDecodeType_ANDImmediateToRegOrMem ||
                     op_decode_type == S86_OpDecodeType_TESTImmediateAndRegOrMem ||
                     op_decode_type == S86_OpDecodeType_ORImmediateToRegOrMem ||
                     op_decode_type == S86_OpDecodeType_XORImmediateToRegOrMem) && s) {
                    // NOTE: Sign extend 8 bit to 16 bit
                    uint16_t sign_mask = 0b1000'0000;
                    uint8_t data_u8    = S86_CAST(uint8_t)data;
                    if (sign_mask & data_u8) {
                        data = (uint16_t)((uint8_t)~data_u8 + 1); // Convert back to 8 bit unsigned
                        data = ~data + 1;                         // Convert to 16bit signed
                        result.immediate_is_8bit = true;
                    }
                } else {
                    uint8_t data_hi = S86_BufferIteratorNextByte(buffer_it);
                    data |= (uint16_t)(data_hi) << 8;
                }
            }

            if (op_decode_type == S86_OpDecodeType_MOVImmediateToRegOrMem) {
                S86_ASSERT(mod != 0b11); // NOTE: Op is IMM->Reg, register-to-register not permitted
            }

            // NOTE: Sign extend 16bit to 32bit
            uint16_t sign_mask16 = 0b1000'0000'0000'0000;
            if (data & sign_mask16) {
                uint32_t data_no_sign_bit = (uint32_t)((uint16_t)~data + 1); // Convert back to 16 bit unsigned
                result.immediate = ~data_no_sign_bit + 1;                    // Convert to signed 32bit
            } else {
                result.immediate = data;
            }

            result.src = S86_MnemonicOp_Immediate;
        } break;

        // NOTE: Instruction Pattern => [0b0000'W00W | DATA-LO | DATA-HI]
        case S86_OpDecodeType_ADDImmediateToAccum:   /*FALLTHRU*/
        case S86_OpDecodeType_ADCImmediateToAccum:   /*FALLTHRU*/
        case S86_OpDecodeType_SUBImmediateFromAccum: /*FALLTHRU*/
        case S86_OpDecodeType_SBBImmediateFromAccum: /*FALLTHRU*/
        case S86_OpDecodeType_CMPImmediateWithAccum: /*FALLTHRU*/
        case S86_OpDecodeType_ANDImmediateToAccum:   /*FALLTHRU*/
        case S86_OpDecodeType_TESTImmediateAndAccum: /*FALLTHRU*/
        case S86_OpDecodeType_ORImmediateToAccum:    /*FALLTHRU*/
        case S86_OpDecodeType_XORImmediateToAccum:   /*FALLTHRU*/
        case S86_OpDecodeType_MOVImmediateToReg: {
            // NOTE: Parse opcode control bits /////////////////////////////////////////////////////
            S86_ASSERT(op_code_size == 1);
            uint8_t w   = 0;
            if (op_decode_type == S86_OpDecodeType_ADDImmediateToAccum ||
                op_decode_type == S86_OpDecodeType_ADCImmediateToAccum ||
                op_decode_type == S86_OpDecodeType_SUBImmediateFromAccum ||
                op_decode_type == S86_OpDecodeType_SBBImmediateFromAccum ||
                op_decode_type == S86_OpDecodeType_CMPImmediateWithAccum ||
                op_decode_type == S86_OpDecodeType_ANDImmediateToAccum ||
                op_decode_type == S86_OpDecodeType_TESTImmediateAndAccum ||
                op_decode_type == S86_OpDecodeType_ORImmediateToAccum ||
                op_decode_type == S86_OpDecodeType_XORImmediateToAccum) {
                w = (op_code_bytes[0] & 0b0000'0001) >> 0;
            } else {
                w = (op_code_bytes[0] & 0b0000'1000) >> 3;
            }

            // NOTE: Parse data payload ////////////////////////////////////////////////////////////
            uint16_t data = S86_BufferIteratorNextByte(buffer_it);
            if (w) { // 16 bit data
                uint8_t data_hi = S86_BufferIteratorNextByte(buffer_it);
                data |= (uint16_t)(data_hi) << 8;
            }

            // NOTE: Disassemble ///////////////////////////////////////////////////////////////////
            result.effective_addr = S86_EffectiveAddress_Dest;
            result.src            = S86_MnemonicOp_Immediate;
            result.wide           = w;
            result.src            = S86_MnemonicOp_Immediate;
            result.immediate      = data;
            if (op_decode_type == S86_OpDecodeType_MOVImmediateToReg) {
                uint8_t reg = (op_code_bytes[0] & 0b0000'0111) >> 0;
                result.dest = S86_MnemonicOpFromWReg(w, reg);
            } else {
                result.dest = result.wide ? S86_MnemonicOp_AX : S86_MnemonicOp_AL;
            }
        } break;

        // NOTE: Instruction Pattern => [0b0000'000W | DATA-LO]
        case S86_OpDecodeType_INFixedPort:    /*FALLTHRU*/
        case S86_OpDecodeType_INVariablePort: /*FALLTHRU*/
        case S86_OpDecodeType_OUTFixedPort:   /*FALLTHRU*/
        case S86_OpDecodeType_OUTVariablePort: {
            S86_ASSERT(op_code_size == 1);
            result.wide = (op_code_bytes[0] & 0b0000'0001) >> 0;
            result.dest = result.wide ? S86_MnemonicOp_AX : S86_MnemonicOp_AL;
            if (op_decode_type == S86_OpDecodeType_INFixedPort ||
                op_decode_type == S86_OpDecodeType_OUTFixedPort) {
                result.src       = S86_MnemonicOp_Immediate;
                result.immediate = S86_BufferIteratorNextByte(buffer_it);
            } else {
                result.src = S86_MnemonicOp_DX;
            }

            if (op_decode_type == S86_OpDecodeType_OUTFixedPort ||
                op_decode_type == S86_OpDecodeType_OUTVariablePort) {
                S86_MnemonicOp tmp = result.src;
                result.src           = result.dest;
                result.dest          = tmp;
            }
        } break;

        case S86_OpDecodeType_REP: {
            S86_ASSERT(op_code_size == 1);
            uint8_t string_op = S86_BufferIteratorNextByte(buffer_it);
            uint8_t w_mask    = 0b0000'0001;
            result.rep_prefix = true;
            result.wide       = string_op & w_mask;
            switch (string_op & ~w_mask) {
                case 0b1010'0100: result.dest = S86_MnemonicOp_MOVS; break;
                case 0b1010'0110: result.dest = S86_MnemonicOp_CMPS; break;
                case 0b1010'1110: result.dest = S86_MnemonicOp_SCAS; break;
                case 0b1010'1100: result.dest = S86_MnemonicOp_LODS; break;
                case 0b1010'1010: result.dest = S86_MnemonicOp_STOS; break;
                default: S86_ASSERT(!"Unhandled REP string type"); break;
            }
        } break;

        // NOTE: Instruction Pattern => [0b0000'0000 | DATA-LO | DATA-HI]
        case S86_OpDecodeType_MOVAccumToMem:                /*FALLTHRU*/
        case S86_OpDecodeType_MOVMemToAccum:                /*FALLTHRU*/
        case S86_OpDecodeType_CALLDirectInterSeg:           /*FALLTHRU*/
        case S86_OpDecodeType_CALLDirectWithinSeg:          /*FALLTHRU*/
        case S86_OpDecodeType_JMPDirectInterSeg:            /*FALLTHRU*/
        case S86_OpDecodeType_RETWithinSegAddImmediateToSP: /*FALLTHRU*/
        case S86_OpDecodeType_INT: {
            S86_ASSERT(op_code_size == 1);
            uint8_t data_lo = S86_BufferIteratorNextByte(buffer_it);
            uint16_t data   = data_lo;
            if (op_decode_type != S86_OpDecodeType_INT) {
                uint8_t data_hi = S86_BufferIteratorNextByte(buffer_it);
                data = S86_CAST(uint16_t)data_hi << 8 | (S86_CAST(uint16_t)data_lo);
            }

            if (op_decode_type == S86_OpDecodeType_CALLDirectWithinSeg) {
                result.effective_addr = S86_EffectiveAddress_Dest;
                result.dest           = S86_MnemonicOp_BP;
                result.displacement   = -S86_CAST(int32_t)data;
            } else if (op_decode_type == S86_OpDecodeType_RETWithinSegAddImmediateToSP) {
                result.dest = S86_MnemonicOp_DirectAddress;
                result.displacement = data;
            } else if (op_decode_type == S86_OpDecodeType_CALLDirectInterSeg ||
                       op_decode_type == S86_OpDecodeType_JMPDirectInterSeg) {
                uint8_t cs_lo       = S86_BufferIteratorNextByte(buffer_it);
                uint8_t cs_hi       = S86_BufferIteratorNextByte(buffer_it);
                uint16_t cs         = S86_CAST(uint16_t)cs_hi << 8 | (S86_CAST(uint16_t)cs_lo);
                result.displacement = (uint32_t)cs << 16 | (uint32_t)data << 0;
                result.dest         = S86_MnemonicOp_DirectInterSegment;
            } else if (op_decode_type == S86_OpDecodeType_MOVAccumToMem) {
                result.effective_addr_loads_mem = true;
                result.effective_addr           = S86_EffectiveAddress_Dest;
                result.dest                     = S86_MnemonicOp_DirectAddress;
                result.displacement             = data;
                result.src                      = S86_MnemonicOp_AX;
            } else if (op_decode_type == S86_OpDecodeType_MOVMemToAccum) {
                result.effective_addr_loads_mem  = true;
                result.effective_addr            = S86_EffectiveAddress_Src;
                result.src                       = S86_MnemonicOp_DirectAddress;
                result.displacement              = data;
                result.dest                      = S86_MnemonicOp_AX;
            } else {
                result.dest      = S86_MnemonicOp_Immediate;
                result.immediate = data;
            }
        } break;

        default: {
            if (op_decode_type >= S86_OpDecodeType_JE_JZ && op_decode_type <= S86_OpDecodeType_JCXZ) {
                S86_ASSERT(op_code_size == 1);
                result.displacement = S86_CAST(int8_t)S86_BufferIteratorNextByte(buffer_it);
                result.dest         = S86_MnemonicOp_Jump;
            } else if (op_decode_type == S86_OpDecodeType_XLAT         ||
                       op_decode_type == S86_OpDecodeType_LAHF         ||
                       op_decode_type == S86_OpDecodeType_SAHF         ||
                       op_decode_type == S86_OpDecodeType_PUSHF        ||
                       op_decode_type == S86_OpDecodeType_POPF         ||
                       op_decode_type == S86_OpDecodeType_DAA          ||
                       op_decode_type == S86_OpDecodeType_AAA          ||
                       op_decode_type == S86_OpDecodeType_DAS          ||
                       op_decode_type == S86_OpDecodeType_AAS          ||
                       op_decode_type == S86_OpDecodeType_AAM          ||
                       op_decode_type == S86_OpDecodeType_AAD          ||
                       op_decode_type == S86_OpDecodeType_CBW          ||
                       op_decode_type == S86_OpDecodeType_CWD          ||
                       op_decode_type == S86_OpDecodeType_RETWithinSeg ||
                       op_decode_type == S86_OpDecodeType_INT3         ||
                       op_decode_type == S86_OpDecodeType_INTO         ||
                       op_decode_type == S86_OpDecodeType_IRET         ||
                       op_decode_type == S86_OpDecodeType_CLC          ||
                       op_decode_type == S86_OpDecodeType_CMC          ||
                       op_decode_type == S86_OpDecodeType_STC          ||
                       op_decode_type == S86_OpDecodeType_CLD          ||
                       op_decode_type == S86_OpDecodeType_STD          ||
                       op_decode_type == S86_OpDecodeType_CLI          ||
                       op_decode_type == S86_OpDecodeType_STI          ||
                       op_decode_type == S86_OpDecodeType_HLT          ||
                       op_decode_type == S86_OpDecodeType_WAIT) {
               // NOTE: Mnemonic only instruction
            } else if (op_decode_type == S86_OpDecodeType_LOCK) {
                *lock_prefix = true;
            } else if (op_decode_type == S86_OpDecodeType_SEGMENT) {
                // NOTE: Mnemonic does not generate any assembly
                S86_ASSERT(op_code_size == 1);
                uint8_t sr = (op_code_bytes[0] & 0b0001'1000) >> 3;
                *seg_reg   = S86_MnemonicOpFromSR(sr);
            } else {
                S86_ASSERT(!"Unhandled instruction");
            }
        } break;
    }

    if (op_decode_type != S86_OpDecodeType_LOCK)
        *lock_prefix = false;

    if (op_decode_type != S86_OpDecodeType_SEGMENT)
        *seg_reg = S86_MnemonicOp_Invalid;

    if (result.effective_addr == S86_EffectiveAddress_Dest && result.effective_addr_loads_mem) {
        result.word_byte_prefix = (result.wide || result.src >= S86_MnemonicOp_AX && result.src <= S86_MnemonicOp_BX)
            ? S86_WordBytePrefix_Word
            : S86_WordBytePrefix_Byte;
    }


    size_t buffer_end_index = buffer_it->index;
    result.byte_size        = S86_CAST(uint8_t)(buffer_end_index - buffer_start_index);
    S86_ASSERT(result.immediate < S86_CAST(uint16_t)-1);
    return result;
}

typedef struct S86_MnemonicOpToRegisterFileMap {
    S86_MnemonicOp    mnemonic_op;       ///< Register/op that the mnemonic is using
    S86_Register16   *reg;               ///< Pointer to the register memory this mnemonic op is using
    S86_RegisterByte  byte;              ///< The 'byte' that the mnemonic operates on (hi, lo or nil e.g. word)
} S86_MnemonicOpToRegisterFileMap;

char const CLI_ARG_EXEC[]                = "--exec";
char const CLI_ARG_LOG_INSTRUCTION_PTR[] = "--log-instruction-ptr";
char const CLI_ARG_LOG_CYCLE_COUNTS[]    = "--log-cycle-counts";
char const CLI_ARG_DUMP[]                = "--dump";
#define PRINT_USAGE                                                                    \
    S86_PrintLnFmt("USAGE: sim8086.exe [%.*s] [%.*s] [%.*s] [%.*s <8086|8088>] <binary asm file>", \
                   S86_ARRAY_UCOUNT(CLI_ARG_EXEC) - 1,                                 \
                   CLI_ARG_EXEC,                                                       \
                   S86_ARRAY_UCOUNT(CLI_ARG_LOG_INSTRUCTION_PTR) - 1,                  \
                   CLI_ARG_LOG_INSTRUCTION_PTR,                                        \
                   S86_ARRAY_UCOUNT(CLI_ARG_LOG_CYCLE_COUNTS) - 1,                     \
                   CLI_ARG_LOG_CYCLE_COUNTS,                                           \
                   S86_ARRAY_UCOUNT(CLI_ARG_DUMP) - 1,                                 \
                   CLI_ARG_DUMP)

int main(int argc, char **argv)
{
    // NOTE: Argument handling /////////////////////////////////////////////////////////////////////
    if (argc < 2) {
        PRINT_USAGE;
        return -1;
    }

    S86_Str8 CLI_ARG_EXEC_STR8                = (S86_Str8){(char *)CLI_ARG_EXEC,                S86_ARRAY_UCOUNT(CLI_ARG_EXEC)                - 1};
    S86_Str8 CLI_ARG_LOG_INSTRUCTION_PTR_STR8 = (S86_Str8){(char *)CLI_ARG_LOG_INSTRUCTION_PTR, S86_ARRAY_UCOUNT(CLI_ARG_LOG_INSTRUCTION_PTR) - 1};
    S86_Str8 CLI_ARG_LOG_CYCLE_COUNTS_STR8    = (S86_Str8){(char *)CLI_ARG_LOG_CYCLE_COUNTS,    S86_ARRAY_UCOUNT(CLI_ARG_LOG_CYCLE_COUNTS)    - 1};
    S86_Str8 CLI_ARG_DUMP_STR8                = (S86_Str8){(char *)CLI_ARG_DUMP,                S86_ARRAY_UCOUNT(CLI_ARG_DUMP)                - 1};

    typedef enum CycleCount {
        CycleCount_None,
        CycleCount_8086,
        CycleCount_8088,
    } CycleCount;

    CycleCount log_cycle_counts = CycleCount_None;
    bool exec_mode              = false;
    bool log_instruction_ptr    = false;
    bool dump                   = false;
    S86_Str8 file_path          = {0};
    for (int arg_index = 1; arg_index < argc; arg_index++) {

        char const *arg_cstring = argv[arg_index];
        S86_Str8 arg_str8       = (S86_Str8){(char *)arg_cstring, strlen(arg_cstring)};
        S86_Str8 next_arg_str8  = S86_STR8("");
        if ((arg_index + 1) < argc)
            next_arg_str8 = (S86_Str8){(char *)argv[arg_index + 1], strlen(argv[arg_index + 1])};

        if (S86_Str8_Equals(arg_str8, CLI_ARG_EXEC_STR8)) {
            exec_mode = true;
        } else if (S86_Str8_Equals(arg_str8, CLI_ARG_LOG_INSTRUCTION_PTR_STR8)) {
            log_instruction_ptr = true;
        } else if (S86_Str8_Equals(arg_str8, CLI_ARG_LOG_CYCLE_COUNTS_STR8)) {
            if (S86_Str8_Equals(next_arg_str8, S86_STR8("8086"))) {
                log_cycle_counts = CycleCount_8086;
            } else if (S86_Str8_Equals(next_arg_str8, S86_STR8("8088"))) {
                log_cycle_counts = CycleCount_8088;
            } else {
                S86_PrintLnFmt("ERROR: Only '8086' or '8088' is accepted after the cycle count argument [arg=\"%.*s\"]", S86_STR8_FMT(next_arg_str8));
                PRINT_USAGE;
                return -1;
            }
            arg_index++;
        } else if (S86_Str8_Equals(arg_str8, CLI_ARG_DUMP_STR8)) {
            dump = true;
        } else {
            if (file_path.size) {
                S86_PrintLnFmt("ERROR: Only 1 ASM binary file is supported per invocation [file=\"%.*s\"]", S86_STR8_FMT(file_path));
                PRINT_USAGE;
                return -1;
            }
            file_path = arg_str8;
        }
    }

    char const *file_name = file_path.data;
    for (size_t index = file_path.size - 1; index < file_path.size; index--) {
        if (file_path.data[index] == '\\' || file_path.data[index] == '/') {
            file_name = file_path.data + index + 1;
            break;
        }
    }

    S86_Buffer buffer = S86_FileRead(file_path.data);
    if (!S86_BufferIsValid(buffer)) {
        S86_PrintLnFmt("ERROR: Failed to read file [path=\"%.*s\"]", S86_STR8_FMT(file_path));
        return -1;
    }

    // NOTE: Sim8086 ///////////////////////////////////////////////////////////////////////////////
    S86_OpDecode const DECODE_TABLE[] = {
        [S86_OpDecodeType_MOVRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1000'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1100'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVImmediateToReg]            = {.op_mask0 = 0b1111'0000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1011'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVMemToAccum]                = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1010'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVAccumToMem]                = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1010'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVRegOrMemToSegReg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                           .op_bits0 = 0b1000'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},
        [S86_OpDecodeType_MOVSegRegToRegOrMem]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0010'0000,
                                                           .op_bits0 = 0b1000'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_MOV},

        [S86_OpDecodeType_PUSHRegOrMem]                 = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1111, .op_bits1 = 0b0011'0000, .mnemonic = S86_Mnemonic_PUSH},
        [S86_OpDecodeType_PUSHReg]                      = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0101'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_PUSH},
        [S86_OpDecodeType_PUSHSegReg]                   = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_PUSH},

        [S86_OpDecodeType_POPRegOrMem]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_POP},
        [S86_OpDecodeType_POPReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0101'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_POP},
        [S86_OpDecodeType_POPSegReg]                    = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_POP},

        [S86_OpDecodeType_XCHGRegOrMemWithReg]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1000'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_XCHG},
        [S86_OpDecodeType_XCHGRegWithAccum]             = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_XCHG},

        [S86_OpDecodeType_INFixedPort]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_IN},
        [S86_OpDecodeType_INVariablePort]               = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_IN},

        [S86_OpDecodeType_OUTFixedPort]                 = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_OUT},
        [S86_OpDecodeType_OUTVariablePort]              = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_OUT},

        [S86_OpDecodeType_XLAT]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1101'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_XLAT},

        [S86_OpDecodeType_LEA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1000'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LEA},
        [S86_OpDecodeType_LDS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LDS},
        [S86_OpDecodeType_LES]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LES},
        [S86_OpDecodeType_LAHF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LAHF},
        [S86_OpDecodeType_SAHF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SAHF},
        [S86_OpDecodeType_PUSHF]                        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_PUSHF},
        [S86_OpDecodeType_POPF]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_POPF},

        [S86_OpDecodeType_ADDRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ADD},
        [S86_OpDecodeType_ADDImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ADD},
        [S86_OpDecodeType_ADDImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ADD},

        [S86_OpDecodeType_ADCRegOrMemWithRegToEither]   = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0001'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ADC},
        [S86_OpDecodeType_ADCImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0001'0000, .mnemonic = S86_Mnemonic_ADC},
        [S86_OpDecodeType_ADCImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0001'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ADC},

        [S86_OpDecodeType_INCRegOrMem]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_INC},
        [S86_OpDecodeType_INCReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0100'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_INC},

        [S86_OpDecodeType_AAA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_AAA},
        [S86_OpDecodeType_DAA]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_DAA},

        [S86_OpDecodeType_SUBRegOrMemToOrFromReg]       = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SUB},
        [S86_OpDecodeType_SUBImmediateFromRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0010'1000, .mnemonic = S86_Mnemonic_SUB},
        [S86_OpDecodeType_SUBImmediateFromAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SUB},

        [S86_OpDecodeType_SBBRegOrMemAndRegToEither]    = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0001'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SBB},
        [S86_OpDecodeType_SBBImmediateFromRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0001'1000, .mnemonic = S86_Mnemonic_SBB},
        [S86_OpDecodeType_SBBImmediateFromAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0001'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SBB},

        [S86_OpDecodeType_DECRegOrMem]                  = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1110, .op_bits1 = 0b0000'1000, .mnemonic = S86_Mnemonic_DEC},
        [S86_OpDecodeType_DECReg]                       = {.op_mask0 = 0b1111'1000, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0100'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_DEC},
        [S86_OpDecodeType_NEG]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0001'1000, .mnemonic = S86_Mnemonic_NEG},

        [S86_OpDecodeType_CMPRegOrMemAndReg]            = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CMP},
        [S86_OpDecodeType_CMPImmediateWithRegOrMem]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0011'1000, .mnemonic = S86_Mnemonic_CMP},
        [S86_OpDecodeType_CMPImmediateWithAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CMP},

        [S86_OpDecodeType_AAS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_AAS},
        [S86_OpDecodeType_DAS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_DAS},

        [S86_OpDecodeType_MUL]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0010'0000, .mnemonic = S86_Mnemonic_MUL},
        [S86_OpDecodeType_IMUL]                         = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0010'1000, .mnemonic = S86_Mnemonic_IMUL},
        [S86_OpDecodeType_AAM]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b1111'1111,
                                                           .op_bits0 = 0b1101'0100, .op_bits1 = 0b0000'1010, .mnemonic = S86_Mnemonic_AAM},
        [S86_OpDecodeType_DIV]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0011'0000, .mnemonic = S86_Mnemonic_DIV},
        [S86_OpDecodeType_IDIV]                         = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0011'1000, .mnemonic = S86_Mnemonic_IDIV},
        [S86_OpDecodeType_AAD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b1111'1111,
                                                           .op_bits0 = 0b1101'0101, .op_bits1 = 0b0000'1010, .mnemonic = S86_Mnemonic_AAD},
        [S86_OpDecodeType_CBW]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CBW},
        [S86_OpDecodeType_CWD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CWD},

        [S86_OpDecodeType_NOT]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0001'0000, .mnemonic = S86_Mnemonic_NOT},
        [S86_OpDecodeType_SHL_SAL]                      = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0010'0000, .mnemonic = S86_Mnemonic_SHL_SAL},
        [S86_OpDecodeType_SHR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0010'1000, .mnemonic = S86_Mnemonic_SHR},
        [S86_OpDecodeType_SAR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0011'1000, .mnemonic = S86_Mnemonic_SAR},
        [S86_OpDecodeType_ROL]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_ROL},
        [S86_OpDecodeType_ROR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0000'1000, .mnemonic = S86_Mnemonic_ROR},
        [S86_OpDecodeType_RCL]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0001'0000, .mnemonic = S86_Mnemonic_RCL},
        [S86_OpDecodeType_RCR]                          = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1101'0000, .op_bits1 = 0b0001'1000, .mnemonic = S86_Mnemonic_RCR},

        [S86_OpDecodeType_ANDRegWithMemToEither]        = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_AND},
        [S86_OpDecodeType_ANDImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0010'0000, .mnemonic = S86_Mnemonic_AND},
        [S86_OpDecodeType_ANDImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_AND},

        [S86_OpDecodeType_TESTRegOrMemAndReg]           = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1000'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_TEST},
        [S86_OpDecodeType_TESTImmediateAndRegOrMem]     = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_TEST},
        [S86_OpDecodeType_TESTImmediateAndAccum]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1010'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_TEST},

        [S86_OpDecodeType_ORRegOrMemAndRegToEither]     = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_OR},
        [S86_OpDecodeType_ORImmediateToRegOrMem]        = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0000'1000, .mnemonic = S86_Mnemonic_OR},
        [S86_OpDecodeType_ORImmediateToAccum]           = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0000'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_OR},

        [S86_OpDecodeType_XORRegOrMemAndRegToEither]    = {.op_mask0 = 0b1111'1100, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_XOR},
        [S86_OpDecodeType_XORImmediateToRegOrMem]       = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1000'0000, .op_bits1 = 0b0011'0000, .mnemonic = S86_Mnemonic_XOR},
        [S86_OpDecodeType_XORImmediateToAccum]          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0011'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_XOR},

        [S86_OpDecodeType_REP]                          = {.op_mask0 = 0b1111'1110, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_REP},

        [S86_OpDecodeType_CALLDirectWithinSeg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CALL},
        [S86_OpDecodeType_CALLIndirectWithinSeg]        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1111, .op_bits1 = 0b0001'0000, .mnemonic = S86_Mnemonic_CALL},
        [S86_OpDecodeType_CALLDirectInterSeg]           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CALL},
        [S86_OpDecodeType_CALLIndirectInterSeg]         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1111, .op_bits1 = 0b0001'1000, .mnemonic = S86_Mnemonic_CALL},

        [S86_OpDecodeType_JMPDirectWithinSeg]           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JMP},
        [S86_OpDecodeType_JMPDirectWithinSegShort]      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JMP},
        [S86_OpDecodeType_JMPIndirectWithinSeg]         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1111, .op_bits1 = 0b0010'0000, .mnemonic = S86_Mnemonic_JMP},
        [S86_OpDecodeType_JMPDirectInterSeg]            = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JMP},
        [S86_OpDecodeType_JMPIndirectInterSeg]          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0011'1000,
                                                           .op_bits0 = 0b1111'1111, .op_bits1 = 0b0010'1000, .mnemonic = S86_Mnemonic_JMP},

        [S86_OpDecodeType_RETWithinSeg]                 = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_RET},
        [S86_OpDecodeType_RETWithinSegAddImmediateToSP] = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_RET},
        [S86_OpDecodeType_RETInterSeg]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_RET},
        [S86_OpDecodeType_RETInterSegAddImmediateToSP]  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_RET},

        [S86_OpDecodeType_JE_JZ]                        = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JE_JZ},
        [S86_OpDecodeType_JL_JNGE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JL_JNGE},
        [S86_OpDecodeType_JLE_JNG]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JLE_JNG},
        [S86_OpDecodeType_JB_JNAE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JB_JNAE},
        [S86_OpDecodeType_JBE_JNA]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JBE_JNA},
        [S86_OpDecodeType_JP_JPE]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JP_JPE},
        [S86_OpDecodeType_JO]                           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JO},
        [S86_OpDecodeType_JS]                           = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JS},
        [S86_OpDecodeType_JNE_JNZ]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNE_JNZ},
        [S86_OpDecodeType_JNL_JGE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNL_JGE},
        [S86_OpDecodeType_JNLE_JG]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNLE_JG},
        [S86_OpDecodeType_JNB_JAE]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNB_JAE},
        [S86_OpDecodeType_JNBE_JA]                      = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNBE_JA},
        [S86_OpDecodeType_JNP_JO]                       = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNP_JO},
        [S86_OpDecodeType_JNO]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNO},
        [S86_OpDecodeType_JNS]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0111'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JNS},
        [S86_OpDecodeType_LOOP]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LOOP},
        [S86_OpDecodeType_LOOPZ_LOOPE]                  = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LOOPZ_LOOPE},
        [S86_OpDecodeType_LOOPNZ_LOOPNE]                = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LOOPNZ_LOOPNE},
        [S86_OpDecodeType_JCXZ]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1110'0011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_JCXZ},

        [S86_OpDecodeType_INT]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_INT},
        [S86_OpDecodeType_INT3]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_INT3},
        [S86_OpDecodeType_INTO]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_INTO},
        [S86_OpDecodeType_IRET]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1100'1111, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_IRET},

        [S86_OpDecodeType_CLC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CLC},
        [S86_OpDecodeType_CMC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'0101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CMC},
        [S86_OpDecodeType_STC]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1001, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_STC},
        [S86_OpDecodeType_CLD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CLD},
        [S86_OpDecodeType_STD]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1101, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_STD},
        [S86_OpDecodeType_CLI]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1010, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_CLI},
        [S86_OpDecodeType_STI]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_STI},
        [S86_OpDecodeType_HLT]                          = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'0100, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_HLT},
        [S86_OpDecodeType_WAIT]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1001'1011, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_WAIT},

        [S86_OpDecodeType_LOCK]                         = {.op_mask0 = 0b1111'1111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b1111'0000, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_LOCK},

        [S86_OpDecodeType_SEGMENT]                      = {.op_mask0 = 0b1110'0111, .op_mask1 = 0b0000'0000,
                                                           .op_bits0 = 0b0010'0110, .op_bits1 = 0b0000'0000, .mnemonic = S86_Mnemonic_SEGMENT},
    };

    // NOTE: Decode assembly ///////////////////////////////////////////////////////////////////////
    if (exec_mode) {
        if (log_cycle_counts != CycleCount_None) { // NOTE: Print disclaimer + header
            S86_PrintLn(S86_STR8("**************"));
            S86_PrintLnFmt("**** %s ****", log_cycle_counts == CycleCount_8086 ? "8086" : "8088");
            S86_PrintLn(S86_STR8("**************"));
            S86_PrintLn(S86_STR8(""));
            S86_PrintLn(S86_STR8("WARNING: Clocks reported by this utility are strictly from the 8086 manual."));
            S86_PrintLn(S86_STR8("They will be inaccurate, both because the manual clocks are estimates, and because"));
            S86_PrintLn(S86_STR8("some of the entries in the manual look highly suspicious and are probably typos."));
            S86_PrintLn(S86_STR8(""));
        }
        S86_PrintLnFmt("--- test\\%s execution ---", file_name);
    } else {
        S86_PrintLn(S86_STR8("bits 16"));
    }

    uint32_t const S86_MEMORY_SIZE = 1024 * 1024;
    S86_RegisterFile register_file = {0};
    uint8_t *memory                = VirtualAlloc(0, S86_MEMORY_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    memcpy(memory, buffer.data, buffer.size);

    S86_MnemonicOpToRegisterFileMap mnemonic_op_to_register_file_map[] = {
        {.mnemonic_op = S86_MnemonicOp_AX,    .reg = &register_file.reg.file.ax, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_AL,    .reg = &register_file.reg.file.ax, .byte = S86_RegisterByte_Lo},
        {.mnemonic_op = S86_MnemonicOp_AH,    .reg = &register_file.reg.file.ax, .byte = S86_RegisterByte_Hi},

        {.mnemonic_op = S86_MnemonicOp_CX,    .reg = &register_file.reg.file.cx, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_CL,    .reg = &register_file.reg.file.cx, .byte = S86_RegisterByte_Lo},
        {.mnemonic_op = S86_MnemonicOp_CH,    .reg = &register_file.reg.file.cx, .byte = S86_RegisterByte_Lo},

        {.mnemonic_op = S86_MnemonicOp_DX,    .reg = &register_file.reg.file.dx, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_DL,    .reg = &register_file.reg.file.dx, .byte = S86_RegisterByte_Lo},
        {.mnemonic_op = S86_MnemonicOp_DH,    .reg = &register_file.reg.file.dx, .byte = S86_RegisterByte_Hi},

        {.mnemonic_op = S86_MnemonicOp_BX,    .reg = &register_file.reg.file.bx, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_BL,    .reg = &register_file.reg.file.bx, .byte = S86_RegisterByte_Lo},
        {.mnemonic_op = S86_MnemonicOp_BH,    .reg = &register_file.reg.file.bx, .byte = S86_RegisterByte_Hi},

        {.mnemonic_op = S86_MnemonicOp_SP,    .reg = &register_file.reg.file.sp, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_BP,    .reg = &register_file.reg.file.bp, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_SI,    .reg = &register_file.reg.file.si, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_DI,    .reg = &register_file.reg.file.di, .byte = S86_RegisterByte_Nil},

        {.mnemonic_op = S86_MnemonicOp_ES,    .reg = &register_file.reg.file.es, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_CS,    .reg = &register_file.reg.file.cs, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_SS,    .reg = &register_file.reg.file.ss, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_DS,    .reg = &register_file.reg.file.ds, .byte = S86_RegisterByte_Nil},

        {.mnemonic_op = S86_MnemonicOp_BX_SI, .reg = &register_file.reg.file.bx, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_BX_DI, .reg = &register_file.reg.file.bx, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_BP_SI, .reg = &register_file.reg.file.bp, .byte = S86_RegisterByte_Nil},
        {.mnemonic_op = S86_MnemonicOp_BP_DI, .reg = &register_file.reg.file.bp, .byte = S86_RegisterByte_Nil},
    };

    // NOTE: Execute the assembly
    // =========================================================================
    S86_Buffer instruction_buffer     = {0};
    instruction_buffer.data           = (char *)&memory[register_file.instruction_ptr];
    instruction_buffer.size           = buffer.size;
    S86_BufferIterator instruction_it = S86_BufferIteratorInit(instruction_buffer);
    uint32_t clocks_counter           = 0;

    bool lock_prefix       = false;
    S86_MnemonicOp seg_reg = S86_CAST(S86_MnemonicOp)0;
    for (uint16_t prev_ip = 0; register_file.instruction_ptr < buffer.size; prev_ip = register_file.instruction_ptr) {
        instruction_it.index = register_file.instruction_ptr;
        S86_Opcode opcode    = S86_DecodeOpcode(&instruction_it, DECODE_TABLE, S86_ARRAY_UCOUNT(DECODE_TABLE), &lock_prefix, &seg_reg, log_cycle_counts == CycleCount_8088);
        S86_PrintOpcode(opcode);

        register_file.instruction_ptr += opcode.byte_size;
        if (opcode.mnemonic == S86_Mnemonic_LOCK || opcode.mnemonic == S86_Mnemonic_SEGMENT)
            continue;

        if (!exec_mode) {
            S86_Print(S86_STR8("\n"));
            continue;
        }

        // NOTE: Simulate instruction //////////////////////////////////////////////////////////////
        bool cycle_count_8088             = log_cycle_counts == CycleCount_8088;
        uint32_t base_clocks              = 0;
        uint32_t effective_address_clocks = 0;
        uint32_t transfer_penalty_clocks  = 0;

        S86_RegisterFile prev_register_file = register_file;
        switch (opcode.mnemonic) {
            case S86_Mnemonic_PUSH:          /*FALLTHRU*/
            case S86_Mnemonic_POP:           /*FALLTHRU*/
            case S86_Mnemonic_XCHG:          /*FALLTHRU*/
            case S86_Mnemonic_IN:            /*FALLTHRU*/
            case S86_Mnemonic_OUT:           /*FALLTHRU*/
            case S86_Mnemonic_XLAT:          /*FALLTHRU*/
            case S86_Mnemonic_LEA:           /*FALLTHRU*/
            case S86_Mnemonic_LDS:           /*FALLTHRU*/
            case S86_Mnemonic_LES:           /*FALLTHRU*/
            case S86_Mnemonic_LAHF:          /*FALLTHRU*/
            case S86_Mnemonic_SAHF:          /*FALLTHRU*/
            case S86_Mnemonic_PUSHF:         /*FALLTHRU*/
            case S86_Mnemonic_POPF:          /*FALLTHRU*/
            case S86_Mnemonic_ADC:           /*FALLTHRU*/
            case S86_Mnemonic_INC:           /*FALLTHRU*/
            case S86_Mnemonic_AAA:           /*FALLTHRU*/
            case S86_Mnemonic_DAA:           /*FALLTHRU*/
            case S86_Mnemonic_SBB:           /*FALLTHRU*/
            case S86_Mnemonic_DEC:           /*FALLTHRU*/
            case S86_Mnemonic_NEG:           /*FALLTHRU*/
            case S86_Mnemonic_AAS:           /*FALLTHRU*/
            case S86_Mnemonic_DAS:           /*FALLTHRU*/
            case S86_Mnemonic_MUL:           /*FALLTHRU*/
            case S86_Mnemonic_IMUL:          /*FALLTHRU*/
            case S86_Mnemonic_AAM:           /*FALLTHRU*/
            case S86_Mnemonic_DIV:           /*FALLTHRU*/
            case S86_Mnemonic_IDIV:          /*FALLTHRU*/
            case S86_Mnemonic_AAD:           /*FALLTHRU*/
            case S86_Mnemonic_CBW:           /*FALLTHRU*/
            case S86_Mnemonic_CWD:           /*FALLTHRU*/
            case S86_Mnemonic_NOT:           /*FALLTHRU*/
            case S86_Mnemonic_SHL_SAL:       /*FALLTHRU*/
            case S86_Mnemonic_SHR:           /*FALLTHRU*/
            case S86_Mnemonic_SAR:           /*FALLTHRU*/
            case S86_Mnemonic_ROL:           /*FALLTHRU*/
            case S86_Mnemonic_ROR:           /*FALLTHRU*/
            case S86_Mnemonic_RCL:           /*FALLTHRU*/
            case S86_Mnemonic_RCR:           /*FALLTHRU*/
            case S86_Mnemonic_AND:           /*FALLTHRU*/
            case S86_Mnemonic_TEST:          /*FALLTHRU*/
            case S86_Mnemonic_OR:            /*FALLTHRU*/
            case S86_Mnemonic_XOR:           /*FALLTHRU*/
            case S86_Mnemonic_REP:           /*FALLTHRU*/
            case S86_Mnemonic_CALL:          /*FALLTHRU*/
            case S86_Mnemonic_JMP:           /*FALLTHRU*/
            case S86_Mnemonic_RET:           /*FALLTHRU*/
            case S86_Mnemonic_JL_JNGE:       /*FALLTHRU*/
            case S86_Mnemonic_JLE_JNG:       /*FALLTHRU*/
            case S86_Mnemonic_JBE_JNA:       /*FALLTHRU*/
            case S86_Mnemonic_JO:            /*FALLTHRU*/
            case S86_Mnemonic_JS:            /*FALLTHRU*/
            case S86_Mnemonic_JNL_JGE:       /*FALLTHRU*/
            case S86_Mnemonic_JNLE_JG:       /*FALLTHRU*/
            case S86_Mnemonic_JNB_JAE:       /*FALLTHRU*/
            case S86_Mnemonic_JNBE_JA:       /*FALLTHRU*/
            case S86_Mnemonic_JNP_JO:        /*FALLTHRU*/
            case S86_Mnemonic_JNO:           /*FALLTHRU*/
            case S86_Mnemonic_JNS:           /*FALLTHRU*/
            case S86_Mnemonic_LOOPZ_LOOPE:   /*FALLTHRU*/
            case S86_Mnemonic_JCXZ:          /*FALLTHRU*/
            case S86_Mnemonic_INT:           /*FALLTHRU*/
            case S86_Mnemonic_INT3:          /*FALLTHRU*/
            case S86_Mnemonic_INTO:          /*FALLTHRU*/
            case S86_Mnemonic_IRET:          /*FALLTHRU*/
            case S86_Mnemonic_CLC:           /*FALLTHRU*/
            case S86_Mnemonic_CMC:           /*FALLTHRU*/
            case S86_Mnemonic_STC:           /*FALLTHRU*/
            case S86_Mnemonic_CLD:           /*FALLTHRU*/
            case S86_Mnemonic_STD:           /*FALLTHRU*/
            case S86_Mnemonic_CLI:           /*FALLTHRU*/
            case S86_Mnemonic_STI:           /*FALLTHRU*/
            case S86_Mnemonic_HLT:           /*FALLTHRU*/
            case S86_Mnemonic_WAIT:          /*FALLTHRU*/
            case S86_Mnemonic_LOCK:          /*FALLTHRU*/
            case S86_Mnemonic_SEGMENT:       break;

            case S86_Mnemonic_MOV: {
                uint16_t src     = 0;
                bool     byte_op = opcode.dest >= S86_MnemonicOp_AL && opcode.dest <= S86_MnemonicOp_BH;
                if (opcode.src == S86_MnemonicOp_Immediate) {
                    if (byte_op) {
                        S86_ASSERT(opcode.immediate < S86_CAST(uint8_t)-1);
                        src = S86_CAST(uint8_t)opcode.immediate;
                    } else {
                        src = S86_CAST(uint16_t)opcode.immediate;
                    }
                } else if (opcode.src == S86_MnemonicOp_DirectAddress) {
                    S86_ASSERT(opcode.displacement >= 0);
                    src = memory[opcode.displacement];
                } else {
                    S86_MnemonicOpToRegisterFileMap const *src_map = NULL;
                    for (size_t index = 0; !src_map && index < S86_ARRAY_UCOUNT(mnemonic_op_to_register_file_map); index++) {
                        S86_MnemonicOpToRegisterFileMap const *item = mnemonic_op_to_register_file_map + index;
                        if (item->mnemonic_op == opcode.src)
                            src_map = item;
                    }

                    if ((src_map->mnemonic_op >= S86_MnemonicOp_BX_SI &&
                        src_map->mnemonic_op <= S86_MnemonicOp_BP_DI) || opcode.effective_addr == S86_EffectiveAddress_Src) {
                        uint16_t address = 0;
                        if (src_map->mnemonic_op == S86_MnemonicOp_BX_SI) {
                            address = src_map->reg->word + register_file.reg.file.si.word;
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BX_DI) {
                            address = src_map->reg->word + register_file.reg.file.di.word;
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BP_SI) {
                            address = src_map->reg->word + register_file.reg.file.si.word;
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BP_DI) {
                            address = src_map->reg->word + register_file.reg.file.di.word;
                        } else if (opcode.effective_addr == S86_EffectiveAddress_Src) {
                            address = src_map->reg->word;
                        } else {
                            S86_ASSERT(!"Invalid code path");
                        }
                        src = *(uint16_t *)&memory[address];
                    } else {
                        src = byte_op ? src_map->reg->bytes[src_map->byte] : src_map->reg->word;
                    }
                }

                uint8_t *dest_lo = NULL;
                uint8_t *dest_hi = NULL;
                if (opcode.dest == S86_MnemonicOp_DirectAddress) {
                    // NOTE: The 8086 doesn't support load to store directly 
                    // memory to memory afaict
                    S86_ASSERT(opcode.dest != opcode.src);
                    S86_ASSERT(opcode.displacement >= 0);

                    dest_lo = memory + opcode.displacement;
                    dest_hi = byte_op ? NULL : memory + (opcode.displacement + 1);
                } else {
                    S86_MnemonicOpToRegisterFileMap const *dest_map = NULL;
                    for (size_t index = 0; !dest_map && index < S86_ARRAY_UCOUNT(mnemonic_op_to_register_file_map); index++) {
                        S86_MnemonicOpToRegisterFileMap const *item = mnemonic_op_to_register_file_map + index;
                        if (item->mnemonic_op == opcode.dest)
                            dest_map = item;
                    }
                    S86_ASSERT(dest_map);

                    // NOTE: Effective address means we're store/load from memory
                    // The opcode value is the address.
                    if (opcode.effective_addr == S86_EffectiveAddress_Dest && opcode.effective_addr_loads_mem) {
                        uint16_t address = dest_map->reg->word + S86_CAST(uint16_t)opcode.displacement;
                        if (dest_map->mnemonic_op == S86_MnemonicOp_BX_SI) {
                            address = dest_map->reg->word + register_file.reg.file.si.word;
                        } else if (dest_map->mnemonic_op == S86_MnemonicOp_BX_DI) {
                            address = dest_map->reg->word + register_file.reg.file.di.word;
                        } else if (dest_map->mnemonic_op == S86_MnemonicOp_BP_SI) {
                            address = dest_map->reg->word + register_file.reg.file.si.word;
                        } else if (dest_map->mnemonic_op == S86_MnemonicOp_BP_DI) {
                            address = dest_map->reg->word + register_file.reg.file.di.word;
                        }
                        dest_lo = memory + address;
                        dest_hi = byte_op ? NULL : memory + (address + 1);

                    } else {
                        if (byte_op) {
                            dest_lo = &dest_map->reg->bytes[dest_map->byte];
                        } else {
                            dest_lo = &dest_map->reg->bytes[0];
                            dest_hi = &dest_map->reg->bytes[1];
                        }
                    }
                }

                if (dest_lo)
                    *dest_lo = S86_CAST(uint8_t)(src >> 0);
                if (dest_hi)
                    *dest_hi = S86_CAST(uint8_t)(src >> 8);

                if (S86_MnemonicOpIsRegister(opcode.dest) && opcode.src == S86_MnemonicOp_Immediate && !opcode.effective_addr_loads_mem) {
                    base_clocks = 4;
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && !opcode.effective_addr_loads_mem) {
                    base_clocks = 2;
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && opcode.src == S86_MnemonicOp_DirectAddress && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Src) {
                    base_clocks              = 8;
                    effective_address_clocks = 6;
                    if (cycle_count_8088 && opcode.wide) {
                        transfer_penalty_clocks = 4;
                    }
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Src) {
                    base_clocks              = 8;
                    effective_address_clocks = opcode.displacement ? 9 : 5;
                    if (cycle_count_8088 && opcode.wide) {
                        transfer_penalty_clocks = 4;
                    }
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Dest) {
                    base_clocks              = 9;
                    effective_address_clocks = opcode.displacement ? 9 : 5;
                    if (cycle_count_8088 && opcode.wide) {
                        transfer_penalty_clocks = 4;
                    }
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsEffectiveAddress(opcode.src) && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Src) {
                    base_clocks = 8;
                    if (cycle_count_8088 && opcode.wide)
                        transfer_penalty_clocks = 4;
                    if (opcode.src == S86_MnemonicOp_BP_DI || opcode.src == S86_MnemonicOp_BX_SI) {
                        effective_address_clocks = opcode.displacement ? 11 : 7;
                    } else if (opcode.src == S86_MnemonicOp_BP_SI || opcode.src == S86_MnemonicOp_BX_DI) {
                        effective_address_clocks = opcode.displacement ? 12 : 8;
                    }
                } else if (S86_MnemonicOpIsEffectiveAddress(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Dest) {
                    base_clocks = 9;
                    if (cycle_count_8088 && opcode.wide)
                        transfer_penalty_clocks = 4;
                    if (opcode.dest == S86_MnemonicOp_BP_DI || opcode.dest == S86_MnemonicOp_BX_SI) {
                        effective_address_clocks = opcode.displacement ? 11 : 7;
                    } else if (opcode.dest == S86_MnemonicOp_BP_SI || opcode.dest == S86_MnemonicOp_BX_DI) {
                        effective_address_clocks = opcode.displacement ? 12 : 8;
                    }
                }
            } break;

            case S86_Mnemonic_ADD: /*FALLTHRU*/
            case S86_Mnemonic_SUB: /*FALLTHRU*/
            case S86_Mnemonic_CMP: {
                S86_MnemonicOpToRegisterFileMap *dest_map = NULL;
                for (size_t index = 0; !dest_map && index < S86_ARRAY_UCOUNT(mnemonic_op_to_register_file_map); index++) {
                    S86_MnemonicOpToRegisterFileMap *item = mnemonic_op_to_register_file_map + index;
                    if (item->mnemonic_op == opcode.dest)
                        dest_map = item;
                }
                S86_ASSERT(dest_map);

                bool subtract = opcode.mnemonic != S86_Mnemonic_ADD;
                bool byte_op  = opcode.dest >= S86_MnemonicOp_AL && opcode.dest <= S86_MnemonicOp_BH;

                uint16_t src         = 0;
                uint16_t src_address = 0;
                if (opcode.src == S86_MnemonicOp_Immediate) {
                    if (byte_op) {
                        S86_ASSERT(opcode.immediate < S86_CAST(uint8_t)-1);
                        src = S86_CAST(uint8_t)opcode.immediate;
                    } else {
                        S86_ASSERT(opcode.immediate < S86_CAST(uint16_t)-1);
                        src = S86_CAST(uint16_t)opcode.immediate;
                    }
                } else if (opcode.src == S86_MnemonicOp_DirectAddress) {
                    S86_ASSERT(opcode.displacement >= 0);
                    src = memory[opcode.displacement];
                } else {
                    S86_MnemonicOpToRegisterFileMap const *src_map = NULL;
                    for (size_t index = 0; !src_map && index < S86_ARRAY_UCOUNT(mnemonic_op_to_register_file_map); index++) {
                        S86_MnemonicOpToRegisterFileMap const *item = mnemonic_op_to_register_file_map + index;
                        if (item->mnemonic_op == opcode.src)
                            src_map = item;
                    }

                    if ((src_map->mnemonic_op >= S86_MnemonicOp_BX_SI &&
                        src_map->mnemonic_op <= S86_MnemonicOp_BP_DI) || (opcode.effective_addr == S86_EffectiveAddress_Src && opcode.effective_addr_loads_mem)) {
                        if (src_map->mnemonic_op == S86_MnemonicOp_BX_SI) {
                            src_address = S86_CAST(uint16_t)(src_map->reg->word + register_file.reg.file.si.word + opcode.displacement);
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BX_DI) {
                            src_address = S86_CAST(uint16_t)(src_map->reg->word + register_file.reg.file.di.word + opcode.displacement);
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BP_SI) {
                            src_address = S86_CAST(uint16_t)(src_map->reg->word + register_file.reg.file.si.word + opcode.displacement);
                        } else if (src_map->mnemonic_op == S86_MnemonicOp_BP_DI) {
                            src_address = S86_CAST(uint16_t)(src_map->reg->word + register_file.reg.file.di.word + opcode.displacement);
                        } else if (opcode.effective_addr == S86_EffectiveAddress_Src) {
                            src_address = S86_CAST(uint16_t)(src_map->reg->word + opcode.displacement);
                        } else {
                            S86_ASSERT(!"Invalid code path");
                        }
                        src = *(uint16_t *)&memory[src_address];
                    } else {
                        src = byte_op ? src_map->reg->bytes[src_map->byte] : src_map->reg->word;
                    }
                }

                // NOTE: Overflow if the sign masks were initially the same,
                // but, after the operation the sign masked changed.
                uint8_t  const sign_mask8  = 0b1000'0000;
                uint16_t const sign_mask16 = 0b1000'0000'0000'0000;

                // NOTE: Effective address means we're store/load from memory
                // The opcode value is the address.
                S86_Register16 dummy_register = {0};
                uint8_t *dest_lo = NULL;
                uint8_t *dest_hi = NULL;
                if (opcode.effective_addr == S86_EffectiveAddress_Dest && opcode.effective_addr_loads_mem) {
                    uint16_t address = dest_map->reg->word;
                    if (dest_map->mnemonic_op == S86_MnemonicOp_BX_SI) {
                        address = S86_CAST(uint16_t)(dest_map->reg->word + register_file.reg.file.si.word + opcode.displacement);
                    } else if (dest_map->mnemonic_op == S86_MnemonicOp_BX_DI) {
                        address = S86_CAST(uint16_t)(dest_map->reg->word + register_file.reg.file.di.word + opcode.displacement);
                    } else if (dest_map->mnemonic_op == S86_MnemonicOp_BP_SI) {
                        address = S86_CAST(uint16_t)(dest_map->reg->word + register_file.reg.file.si.word + opcode.displacement);
                    } else if (dest_map->mnemonic_op == S86_MnemonicOp_BP_DI) {
                        address = S86_CAST(uint16_t)(dest_map->reg->word + register_file.reg.file.di.word + opcode.displacement);
                    } else if (opcode.effective_addr == S86_EffectiveAddress_Dest) {
                        address = S86_CAST(uint16_t)(dest_map->reg->word + opcode.displacement);
                    }
                    dest_lo = memory + address;
                    dest_hi = byte_op ? NULL : memory + (address + 1);

                } else {
                    if (byte_op) {
                        dest_lo = &dest_map->reg->bytes[dest_map->byte];
                    } else {
                        dest_lo = &dest_map->reg->bytes[0];
                        dest_hi = &dest_map->reg->bytes[1];
                    }
                }

                if (opcode.mnemonic == S86_Mnemonic_CMP) {
                    dummy_register.bytes[0] = *dest_lo;
                    dummy_register.bytes[1] = *dest_hi;
                    dest_lo = &dummy_register.bytes[0];
                    dest_hi = &dummy_register.bytes[1];
                }

                if (byte_op) {
                    uint8_t src_u8 = S86_CAST(uint8_t)src;
                    if (subtract)
                        src_u8 = ~src_u8 + 1;

                    uint8_t dest_u8     = *dest_lo;
                    uint8_t new_dest_u8 = dest_u8 + src_u8;

                    // NOTE: Overflow check
                    bool initially_matching_sign_masks = (dest_u8 & sign_mask8) == (src_u8 & sign_mask8);
                    bool sign_masks_changed            = (dest_u8 & sign_mask8) != (new_dest_u8 & sign_mask8);
                    register_file.flags.overflow       = initially_matching_sign_masks && sign_masks_changed;

                    // NOTE: Carry check
                    register_file.flags.carry          = subtract ? new_dest_u8 > dest_u8 : new_dest_u8 < dest_u8;

                    // NOTE: Auxiliary carry check
                    uint8_t     dest_u8_nibble_lo =     dest_u8 & 0b0000'1111 >> 0;
                    uint8_t     dest_u8_nibble_hi =     dest_u8 & 0b1111'0000 >> 4;
                    uint8_t new_dest_u8_nibble_lo = new_dest_u8 & 0b0000'1111 >> 0;
                    uint8_t new_dest_u8_nibble_hi = new_dest_u8 & 0b1111'0000 >> 4;
                    register_file.flags.auxiliary_carry = subtract ? new_dest_u8_nibble_hi > dest_u8_nibble_hi
                                                                     : new_dest_u8_nibble_lo < dest_u8_nibble_lo;

                    // NOTE: Sign check
                    register_file.flags.sign = new_dest_u8 & 0b1000'0000;

                    // NOTE: Update the register
                    *dest_lo = new_dest_u8;
                } else {
                    if (subtract)
                        src = ~src + 1;

                    uint16_t dest_word = *(uint16_t *)dest_lo;
                    S86_Register16 new_dest = {0};
                    new_dest.word           = dest_word + src;

                    // NOTE: Overflow check
                    bool initially_matching_sign_masks = (dest_word & sign_mask16) == (src           & sign_mask16);
                    bool sign_masks_changed            = (dest_word & sign_mask16) != (new_dest.word & sign_mask16);
                    register_file.flags.overflow       = initially_matching_sign_masks && sign_masks_changed;

                    // NOTE: Auxiliary carry check
                    uint8_t     dest_lo_nibble_lo = *dest_lo & 0b0000'1111 >> 0;
                    uint8_t     dest_lo_nibble_hi = *dest_lo & 0b1111'0000 >> 4;
                    uint8_t new_dest_lo_nibble_lo = new_dest.bytes[S86_RegisterByte_Lo] & 0b0000'1111 >> 0;
                    uint8_t new_dest_lo_nibble_hi = new_dest.bytes[S86_RegisterByte_Lo] & 0b1111'0000 >> 4;
                    register_file.flags.auxiliary_carry = subtract ? new_dest_lo_nibble_hi > dest_lo_nibble_hi
                                                                   : new_dest_lo_nibble_lo < dest_lo_nibble_lo;

                    // NOTE: Carry check
                    register_file.flags.carry = subtract ? new_dest.word > dest_word : new_dest.word < dest_word;

                    // NOTE: Sign check
                    register_file.flags.sign = new_dest.word & 0b1000'0000'0000'0000;

                    // NOTE: Update the register
                    *dest_lo = new_dest.bytes[0];
                    *dest_hi = new_dest.bytes[1];
                }

                int lo_bit_count           = _mm_popcnt_u32(S86_CAST(uint32_t)*dest_lo);
                register_file.flags.parity = lo_bit_count % 2 == 0;
                register_file.flags.zero   = byte_op ? *dest_lo == 0 : *(uint16_t*)dest_lo == 0;

                if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && opcode.effective_addr == S86_EffectiveAddress_None) {
                    base_clocks = 3;
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsRegister(opcode.src) && opcode.effective_addr == S86_EffectiveAddress_Dest) {
                    base_clocks              = 16;
                    effective_address_clocks = opcode.displacement ? 9 : 5;
                    if (cycle_count_8088) {
                        if (opcode.wide) {
                            transfer_penalty_clocks = 8;
                        }
                    } else {
                        if ((uintptr_t)dest_lo & 1) {
                            transfer_penalty_clocks = 8;
                        }
                    }
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && opcode.src == S86_MnemonicOp_Immediate) {
                    base_clocks = 4;
                } else if (S86_MnemonicOpIsEffectiveAddress(opcode.dest) && opcode.src == S86_MnemonicOp_Immediate && opcode.effective_addr_loads_mem) {
                    base_clocks = 17;
                    if (cycle_count_8088 && opcode.wide)
                        transfer_penalty_clocks = 8;
                    if (opcode.dest == S86_MnemonicOp_BP_DI || opcode.dest == S86_MnemonicOp_BX_SI) {
                        effective_address_clocks = opcode.displacement ? 11 : 7;
                    } else if (opcode.dest == S86_MnemonicOp_BP_SI || opcode.dest == S86_MnemonicOp_BX_DI) {
                        effective_address_clocks = opcode.displacement ? 12 : 8;
                    }
                } else if (S86_MnemonicOpIsRegister(opcode.dest) && S86_MnemonicOpIsEffectiveAddress(opcode.src) && opcode.effective_addr_loads_mem && opcode.effective_addr == S86_EffectiveAddress_Src) {
                    base_clocks = 9;
                    if (cycle_count_8088) {
                        if (opcode.wide) {
                            transfer_penalty_clocks = 4;
                        }
                    } else {
                        if (src_address & 1) {
                            transfer_penalty_clocks = 4;
                        }
                    }

                    if (opcode.src == S86_MnemonicOp_BP_DI || opcode.src == S86_MnemonicOp_BX_SI) {
                        effective_address_clocks = opcode.displacement ? 11 : 7;
                    } else if (opcode.src == S86_MnemonicOp_BP_SI || opcode.src == S86_MnemonicOp_BX_DI) {
                        effective_address_clocks = opcode.displacement ? 12 : 8;
                    }
                }
            } break;

            case S86_Mnemonic_JNE_JNZ: {
                if (!register_file.flags.zero)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;

            case S86_Mnemonic_JE_JZ: {
                if (register_file.flags.zero)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;

            case S86_Mnemonic_JP_JPE: {
                if (register_file.flags.parity)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;

            case S86_Mnemonic_JB_JNAE: {
                if (register_file.flags.carry)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;

            case S86_Mnemonic_LOOP: {
                register_file.reg.file.cx.word -= 1;
                if (register_file.reg.file.cx.word != 0)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;

            case S86_Mnemonic_LOOPNZ_LOOPNE: {
                register_file.reg.file.cx.word -= 1;
                if (register_file.reg.file.cx.word != 0 && !register_file.flags.zero)
                    register_file.instruction_ptr += S86_CAST(int16_t)opcode.displacement;
            } break;
        }

        clocks_counter += base_clocks + effective_address_clocks + transfer_penalty_clocks;

        // NOTE: Printing //////////////////////////////////////////////////////////////////////////
        S86_PrintFmt(" ; ");

        // NOTE: Clocks
        if (log_cycle_counts) {
            S86_PrintFmt("Clocks: +%u = %u", base_clocks + effective_address_clocks + transfer_penalty_clocks, clocks_counter);
            if (effective_address_clocks || transfer_penalty_clocks) {
                S86_PrintFmt(" (%u", base_clocks);
                if (effective_address_clocks)
                    S86_PrintFmt(" + %uea", effective_address_clocks);
                if (transfer_penalty_clocks)
                    S86_PrintFmt(" + %up", transfer_penalty_clocks);
                S86_PrintFmt(")");
            }
            S86_PrintFmt(" | ");
        }

        // NOTE: Registers
        for (size_t index = 0; index < S86_RegisterFileRegArray_Count; index++) {
            if (register_file.reg.array[index].word != prev_register_file.reg.array[index].word) {
                S86_Str8 label = S86_RegisterFileRegArrayStr8(index);
                S86_PrintFmt("%.*s:0x%x->0x%x ",
                             S86_STR8_FMT(label),
                             prev_register_file.reg.array[index].word,
                             register_file.reg.array[index].word);

                // NOTE: In 8086, instructions can only change one register at a
                // time. Once we find the first delta in the register file, we
                // exit.
                break;
            }
        }

        // NOTE: Instruction Pointer
        if (log_instruction_ptr)
            S86_PrintFmt("ip:0x%x->0x%x ", prev_ip, register_file.instruction_ptr);

        // NOTE: Flags
        if (!S86_RegisterFileFlagsEq(register_file.flags, prev_register_file.flags)) {
            S86_PrintFmt("flags:");
            if (prev_register_file.flags.carry)
                S86_PrintFmt("C");
            if (prev_register_file.flags.parity)
                S86_PrintFmt("P");
            if (prev_register_file.flags.auxiliary_carry)
                S86_PrintFmt("A");
            if (prev_register_file.flags.zero)
                S86_PrintFmt("Z");
            if (prev_register_file.flags.sign)
                S86_PrintFmt("S");
            if (prev_register_file.flags.overflow)
                S86_PrintFmt("O");

            S86_PrintFmt("->");
            if (register_file.flags.carry)
                S86_PrintFmt("C");
            if (register_file.flags.parity)
                S86_PrintFmt("P");
            if (register_file.flags.auxiliary_carry)
                S86_PrintFmt("A");
            if (register_file.flags.zero)
                S86_PrintFmt("Z");
            if (register_file.flags.sign)
                S86_PrintFmt("S");
            if (register_file.flags.overflow)
                S86_PrintFmt("O");
            S86_PrintFmt(" ");
        }

        S86_Print(S86_STR8("\n"));
    }

    if (exec_mode) {
        S86_PrintLn(S86_STR8("\nFinal registers:"));
        if (register_file.reg.file.ax.word)
            S86_PrintLnFmt("      ax: 0x%04x (%u)", register_file.reg.file.ax.word, register_file.reg.file.ax.word);
        if (register_file.reg.file.bx.word)
            S86_PrintLnFmt("      bx: 0x%04x (%u)", register_file.reg.file.bx.word, register_file.reg.file.bx.word);
        if (register_file.reg.file.cx.word)
            S86_PrintLnFmt("      cx: 0x%04x (%u)", register_file.reg.file.cx.word, register_file.reg.file.cx.word);
        if (register_file.reg.file.dx.word)
            S86_PrintLnFmt("      dx: 0x%04x (%u)", register_file.reg.file.dx.word, register_file.reg.file.dx.word);
        if (register_file.reg.file.sp.word)
            S86_PrintLnFmt("      sp: 0x%04x (%u)", register_file.reg.file.sp.word, register_file.reg.file.sp.word);
        if (register_file.reg.file.bp.word)
            S86_PrintLnFmt("      bp: 0x%04x (%u)", register_file.reg.file.bp.word, register_file.reg.file.bp.word);
        if (register_file.reg.file.si.word)
            S86_PrintLnFmt("      si: 0x%04x (%u)", register_file.reg.file.si.word, register_file.reg.file.si.word);
        if (register_file.reg.file.di.word)
            S86_PrintLnFmt("      di: 0x%04x (%u)", register_file.reg.file.di.word, register_file.reg.file.di.word);
        if (register_file.reg.file.es.word)
            S86_PrintLnFmt("      es: 0x%04x (%u)", register_file.reg.file.es.word, register_file.reg.file.es.word);
        if (register_file.reg.file.ss.word)
            S86_PrintLnFmt("      ss: 0x%04x (%u)", register_file.reg.file.ss.word, register_file.reg.file.ss.word);
        if (register_file.reg.file.ds.word)
            S86_PrintLnFmt("      ds: 0x%04x (%u)", register_file.reg.file.ds.word, register_file.reg.file.ds.word);

        if (log_instruction_ptr)
            S86_PrintLnFmt("      ip: 0x%04x (%u)", register_file.instruction_ptr, register_file.instruction_ptr);

        S86_RegisterFileFlags nil_flags = {0};
        if (!S86_RegisterFileFlagsEq(register_file.flags, nil_flags)) {
            S86_PrintFmt("   flags: ");
            if (register_file.flags.carry)
                S86_PrintFmt("C");
            if (register_file.flags.parity)
                S86_PrintFmt("P");
            if (register_file.flags.auxiliary_carry)
                S86_PrintFmt("A");
            if (register_file.flags.zero)
                S86_PrintFmt("Z");
            if (register_file.flags.sign)
                S86_PrintFmt("S");
            if (register_file.flags.overflow)
                S86_PrintFmt("O");
            S86_Print(S86_STR8("\n"));
        }
        S86_Print(S86_STR8("\n"));
    }

    if (dump) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "%s_mem_dump.data", file_name);
        S86_FileWrite(buf, memory, S86_MEMORY_SIZE);
    }
}
