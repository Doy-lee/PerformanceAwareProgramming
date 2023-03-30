// NOTE: Sim8086
// ============================================================================
typedef enum S86_OpDecodeType {
    S86_OpDecodeType_MOVRegOrMemToOrFromReg,
    S86_OpDecodeType_MOVImmediateToRegOrMem,
    S86_OpDecodeType_MOVImmediateToReg,
    S86_OpDecodeType_MOVMemToAccum,
    S86_OpDecodeType_MOVAccumToMem,
    S86_OpDecodeType_MOVRegOrMemToSegReg,
    S86_OpDecodeType_MOVSegRegToRegOrMem,

    S86_OpDecodeType_PUSHRegOrMem,
    S86_OpDecodeType_PUSHReg,
    S86_OpDecodeType_PUSHSegReg,

    S86_OpDecodeType_POPRegOrMem,
    S86_OpDecodeType_POPReg,
    S86_OpDecodeType_POPSegReg,

    S86_OpDecodeType_XCHGRegOrMemWithReg,
    S86_OpDecodeType_XCHGRegWithAccum,

    S86_OpDecodeType_INFixedPort,
    S86_OpDecodeType_INVariablePort,

    S86_OpDecodeType_OUTFixedPort,
    S86_OpDecodeType_OUTVariablePort,

    S86_OpDecodeType_XLAT,

    S86_OpDecodeType_LEA,
    S86_OpDecodeType_LDS,
    S86_OpDecodeType_LES,
    S86_OpDecodeType_LAHF,
    S86_OpDecodeType_SAHF,
    S86_OpDecodeType_PUSHF,
    S86_OpDecodeType_POPF,

    S86_OpDecodeType_ADDRegOrMemToOrFromReg,
    S86_OpDecodeType_ADDImmediateToRegOrMem,
    S86_OpDecodeType_ADDImmediateToAccum,

    S86_OpDecodeType_ADCRegOrMemWithRegToEither,
    S86_OpDecodeType_ADCImmediateToRegOrMem,
    S86_OpDecodeType_ADCImmediateToAccum,

    S86_OpDecodeType_INCRegOrMem,
    S86_OpDecodeType_INCReg,

    S86_OpDecodeType_AAA,
    S86_OpDecodeType_DAA,

    S86_OpDecodeType_SUBRegOrMemToOrFromReg,
    S86_OpDecodeType_SUBImmediateFromRegOrMem,
    S86_OpDecodeType_SUBImmediateFromAccum,

    S86_OpDecodeType_SBBRegOrMemAndRegToEither,
    S86_OpDecodeType_SBBImmediateFromRegOrMem,
    S86_OpDecodeType_SBBImmediateFromAccum,

    S86_OpDecodeType_DECRegOrMem,
    S86_OpDecodeType_DECReg,
    S86_OpDecodeType_NEG,

    S86_OpDecodeType_CMPRegOrMemAndReg,
    S86_OpDecodeType_CMPImmediateWithRegOrMem,
    S86_OpDecodeType_CMPImmediateWithAccum,

    S86_OpDecodeType_AAS,
    S86_OpDecodeType_DAS,

    S86_OpDecodeType_MUL,
    S86_OpDecodeType_IMUL,
    S86_OpDecodeType_AAM,
    S86_OpDecodeType_DIV,
    S86_OpDecodeType_IDIV,
    S86_OpDecodeType_AAD,
    S86_OpDecodeType_CBW,
    S86_OpDecodeType_CWD,

    S86_OpDecodeType_NOT,
    S86_OpDecodeType_SHL_SAL,
    S86_OpDecodeType_SHR,
    S86_OpDecodeType_SAR,
    S86_OpDecodeType_ROL,
    S86_OpDecodeType_ROR,
    S86_OpDecodeType_RCL,
    S86_OpDecodeType_RCR,

    S86_OpDecodeType_ANDRegWithMemToEither,
    S86_OpDecodeType_ANDImmediateToRegOrMem,
    S86_OpDecodeType_ANDImmediateToAccum,

    S86_OpDecodeType_TESTRegOrMemAndReg,
    S86_OpDecodeType_TESTImmediateAndRegOrMem,
    S86_OpDecodeType_TESTImmediateAndAccum,

    S86_OpDecodeType_ORRegOrMemAndRegToEither,
    S86_OpDecodeType_ORImmediateToRegOrMem,
    S86_OpDecodeType_ORImmediateToAccum,

    S86_OpDecodeType_XORRegOrMemAndRegToEither,
    S86_OpDecodeType_XORImmediateToRegOrMem,
    S86_OpDecodeType_XORImmediateToAccum,

    S86_OpDecodeType_REP,

    S86_OpDecodeType_CALLDirectWithinSeg,
    S86_OpDecodeType_CALLIndirectWithinSeg,
    S86_OpDecodeType_CALLDirectInterSeg,
    S86_OpDecodeType_CALLIndirectInterSeg,

    S86_OpDecodeType_JMPDirectWithinSeg,
    S86_OpDecodeType_JMPDirectWithinSegShort,
    S86_OpDecodeType_JMPIndirectWithinSeg,
    S86_OpDecodeType_JMPDirectInterSeg,
    S86_OpDecodeType_JMPIndirectInterSeg,

    S86_OpDecodeType_RETWithinSeg,
    S86_OpDecodeType_RETWithinSegAddImmediateToSP,
    S86_OpDecodeType_RETInterSeg,
    S86_OpDecodeType_RETInterSegAddImmediateToSP,

    S86_OpDecodeType_JE_JZ,
    S86_OpDecodeType_JL_JNGE,
    S86_OpDecodeType_JLE_JNG,
    S86_OpDecodeType_JB_JNAE,
    S86_OpDecodeType_JBE_JNA,
    S86_OpDecodeType_JP_JPE,
    S86_OpDecodeType_JO,
    S86_OpDecodeType_JS,
    S86_OpDecodeType_JNE_JNZ,
    S86_OpDecodeType_JNL_JGE,
    S86_OpDecodeType_JNLE_JG,
    S86_OpDecodeType_JNB_JAE,
    S86_OpDecodeType_JNBE_JA,
    S86_OpDecodeType_JNP_JO,
    S86_OpDecodeType_JNO,
    S86_OpDecodeType_JNS,
    S86_OpDecodeType_LOOP,
    S86_OpDecodeType_LOOPZ_LOOPE,
    S86_OpDecodeType_LOOPNZ_LOOPNE,
    S86_OpDecodeType_JCXZ,

    S86_OpDecodeType_INT,
    S86_OpDecodeType_INT3,
    S86_OpDecodeType_INTO,
    S86_OpDecodeType_IRET,

    S86_OpDecodeType_CLC,
    S86_OpDecodeType_CMC,
    S86_OpDecodeType_STC,
    S86_OpDecodeType_CLD,
    S86_OpDecodeType_STD,
    S86_OpDecodeType_CLI,
    S86_OpDecodeType_STI,
    S86_OpDecodeType_HLT,
    S86_OpDecodeType_WAIT,

    S86_OpDecodeType_LOCK,
    S86_OpDecodeType_SEGMENT,

    S86_OpDecodeType_Count,
} S86_OpDecodeType;

typedef enum S86_Mnemonic {
    S86_Mnemonic_MOV,
    S86_Mnemonic_PUSH,
    S86_Mnemonic_POP,
    S86_Mnemonic_XCHG,
    S86_Mnemonic_IN,
    S86_Mnemonic_OUT,
    S86_Mnemonic_XLAT,
    S86_Mnemonic_LEA,
    S86_Mnemonic_LDS,
    S86_Mnemonic_LES,
    S86_Mnemonic_LAHF,
    S86_Mnemonic_SAHF,
    S86_Mnemonic_PUSHF,
    S86_Mnemonic_POPF,
    S86_Mnemonic_ADD,
    S86_Mnemonic_ADC,
    S86_Mnemonic_INC,
    S86_Mnemonic_AAA,
    S86_Mnemonic_DAA,
    S86_Mnemonic_SUB,
    S86_Mnemonic_SBB,
    S86_Mnemonic_DEC,
    S86_Mnemonic_NEG,
    S86_Mnemonic_CMP,
    S86_Mnemonic_AAS,
    S86_Mnemonic_DAS,
    S86_Mnemonic_MUL,
    S86_Mnemonic_IMUL,
    S86_Mnemonic_AAM,
    S86_Mnemonic_DIV,
    S86_Mnemonic_IDIV,
    S86_Mnemonic_AAD,
    S86_Mnemonic_CBW,
    S86_Mnemonic_CWD,
    S86_Mnemonic_NOT,
    S86_Mnemonic_SHL_SAL,
    S86_Mnemonic_SHR,
    S86_Mnemonic_SAR,
    S86_Mnemonic_ROL,
    S86_Mnemonic_ROR,
    S86_Mnemonic_RCL,
    S86_Mnemonic_RCR,
    S86_Mnemonic_AND,
    S86_Mnemonic_TEST,
    S86_Mnemonic_OR,
    S86_Mnemonic_XOR,
    S86_Mnemonic_REP,
    S86_Mnemonic_CALL,
    S86_Mnemonic_JMP,
    S86_Mnemonic_RET,
    S86_Mnemonic_JE_JZ,
    S86_Mnemonic_JL_JNGE,
    S86_Mnemonic_JLE_JNG,
    S86_Mnemonic_JB_JNAE,
    S86_Mnemonic_JBE_JNA,
    S86_Mnemonic_JP_JPE,
    S86_Mnemonic_JO,
    S86_Mnemonic_JS,
    S86_Mnemonic_JNE_JNZ,
    S86_Mnemonic_JNL_JGE,
    S86_Mnemonic_JNLE_JG,
    S86_Mnemonic_JNB_JAE,
    S86_Mnemonic_JNBE_JA,
    S86_Mnemonic_JNP_JO,
    S86_Mnemonic_JNO,
    S86_Mnemonic_JNS,
    S86_Mnemonic_LOOP,
    S86_Mnemonic_LOOPZ_LOOPE,
    S86_Mnemonic_LOOPNZ_LOOPNE,
    S86_Mnemonic_JCXZ,
    S86_Mnemonic_INT,
    S86_Mnemonic_INT3,
    S86_Mnemonic_INTO,
    S86_Mnemonic_IRET,
    S86_Mnemonic_CLC,
    S86_Mnemonic_CMC,
    S86_Mnemonic_STC,
    S86_Mnemonic_CLD,
    S86_Mnemonic_STD,
    S86_Mnemonic_CLI,
    S86_Mnemonic_STI,
    S86_Mnemonic_HLT,
    S86_Mnemonic_WAIT,
    S86_Mnemonic_LOCK,
    S86_Mnemonic_SEGMENT,
} S86_Mnemonic;

/// Bit patterns and masks for decoding 8086 assembly. 8086 opcodes can be up
/// to 2 bytes long and mixed with instruction specific control bits. These
/// masks isolate the opcode bits from the bits can be checked after masking
/// the binary instruction stream.
///
/// Instructions that do not have opcode bits in the 2nd byte will have the mask
/// set to 0.
typedef struct S86_OpDecode {
    S86_Mnemonic mnemonic;
    uint8_t      op_mask0;
    uint8_t      op_bits0;
    uint8_t      op_mask1;
    uint8_t      op_bits1;
} S86_OpDecode;

typedef enum S86_MnemonicOp {
    S86_MnemonicOp_Invalid,
    S86_MnemonicOp_AL,
    S86_MnemonicOp_CL,
    S86_MnemonicOp_DL,
    S86_MnemonicOp_BL,
    S86_MnemonicOp_AH,
    S86_MnemonicOp_CH,
    S86_MnemonicOp_DH,
    S86_MnemonicOp_BH,

    S86_MnemonicOp_AX,
    S86_MnemonicOp_CX,
    S86_MnemonicOp_DX,
    S86_MnemonicOp_BX,
    S86_MnemonicOp_SP,
    S86_MnemonicOp_BP,
    S86_MnemonicOp_SI,
    S86_MnemonicOp_DI,

    S86_MnemonicOp_BX_SI,
    S86_MnemonicOp_BX_DI,
    S86_MnemonicOp_BP_SI,
    S86_MnemonicOp_BP_DI,

    S86_MnemonicOp_DirectAddress,
    S86_MnemonicOp_Immediate,

    S86_MnemonicOp_ES,
    S86_MnemonicOp_CS,
    S86_MnemonicOp_SS,
    S86_MnemonicOp_DS,

    S86_MnemonicOp_MOVS,
    S86_MnemonicOp_CMPS,
    S86_MnemonicOp_SCAS,
    S86_MnemonicOp_LODS,
    S86_MnemonicOp_STOS,

    S86_MnemonicOp_DirectInterSegment,
    S86_MnemonicOp_Jump,
} S86_MnemonicOp;

typedef enum S86_EffectiveAddress {
    S86_EffectiveAddress_None,
    S86_EffectiveAddress_Src,
    S86_EffectiveAddress_Dest,
} S86_EffectiveAddress;

typedef enum S86_WidePrefix {
    S86_WidePrefix_None,
    S86_WidePrefix_Src,
    S86_WidePrefix_Dest,
} S86_WidePrefix;

typedef struct S86_Opcode {
    S86_Mnemonic         mnemonic;                 ///< Mnemonic type
    S86_EffectiveAddress effective_addr;           ///< Src/dest op is an effective address calculation
    bool                 effective_addr_loads_mem; ///< Effective address uses '[]' notation to load address memory
    bool                 lock_prefix;              ///< Prefix the opcode with "lock" instruction
    bool                 rep_prefix;               ///< Prefix the opcode with "rep" instruction
    bool                 wide;                     ///< Opcode has the 'w' flag set
    S86_WidePrefix       wide_prefix;              ///< Mnemonic src/dest op requires a 'word' or 'byte' prefix (e.g. ambiguous immediate size)
    S86_MnemonicOp       src;                      ///< Source op for the mnemonic
    S86_MnemonicOp       dest;                     ///< Destination op for the mnemonic
    int32_t              displacement;             ///< Opcode has displacement/data/offset
    int32_t              immediate;                ///< Immediate value when src/dest op is an immediate
    S86_MnemonicOp       seg_reg_prefix;           ///< Segment register that should prefix the upcoming instruction
} S86_Opcode;

S86_Str8       S86_MnemonicStr8         (S86_Mnemonic type);
S86_MnemonicOp S86_MnemonicOpFromWReg   (bool w, uint8_t reg);
S86_MnemonicOp S86_MnemonicOpFromSR     (uint8_t sr);
S86_Str8       S86_MnemonicOpStr8       (S86_MnemonicOp type);
void           S86_PrintOpcodeMnemonicOp(S86_Opcode opcode, bool src);
void           S86_PrintOpcode          (S86_Opcode opcode);
void           S86_DecodeEffectiveAddr  (S86_Opcode *opcode, S86_BufferIterator *it, uint8_t rm, uint8_t mod, uint8_t w);
