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

    S86_InstructionType_XCHGRegOrMemWithReg,
    S86_InstructionType_XCHGRegWithAccum,

    S86_InstructionType_INFixedPort,
    S86_InstructionType_INVariablePort,

    S86_InstructionType_OUTFixedPort,
    S86_InstructionType_OUTVariablePort,

    S86_InstructionType_XLAT,

    S86_InstructionType_LEA,
    S86_InstructionType_LDS,
    S86_InstructionType_LES,
    S86_InstructionType_LAHF,
    S86_InstructionType_SAHF,
    S86_InstructionType_PUSHF,
    S86_InstructionType_POPF,

    S86_InstructionType_ADDRegOrMemToOrFromReg,
    S86_InstructionType_ADDImmediateToRegOrMem,
    S86_InstructionType_ADDImmediateToAccum,

    S86_InstructionType_ADCRegOrMemWithRegToEither,
    S86_InstructionType_ADCImmediateToRegOrMem,
    S86_InstructionType_ADCImmediateToAccum,

    S86_InstructionType_INCRegOrMem,
    S86_InstructionType_INCReg,

    S86_InstructionType_AAA,
    S86_InstructionType_DAA,

    S86_InstructionType_SUBRegOrMemToOrFromReg,
    S86_InstructionType_SUBImmediateFromRegOrMem,
    S86_InstructionType_SUBImmediateFromAccum,

    S86_InstructionType_SBBRegOrMemAndRegToEither,
    S86_InstructionType_SBBImmediateFromRegOrMem,
    S86_InstructionType_SBBImmediateFromAccum,

    S86_InstructionType_DECRegOrMem,
    S86_InstructionType_DECReg,
    S86_InstructionType_NEG,

    S86_InstructionType_CMPRegOrMemAndReg,
    S86_InstructionType_CMPImmediateWithRegOrMem,
    S86_InstructionType_CMPImmediateWithAccum,

    S86_InstructionType_AAS,
    S86_InstructionType_DAS,

    S86_InstructionType_MUL,
    S86_InstructionType_IMUL,
    S86_InstructionType_AAM,
    S86_InstructionType_DIV,
    S86_InstructionType_IDIV,
    S86_InstructionType_AAD,
    S86_InstructionType_CBW,
    S86_InstructionType_CWD,

    S86_InstructionType_NOT,
    S86_InstructionType_SHL_SAL,
    S86_InstructionType_SHR,
    S86_InstructionType_SAR,
    S86_InstructionType_ROL,
    S86_InstructionType_ROR,
    S86_InstructionType_RCL,
    S86_InstructionType_RCR,

    S86_InstructionType_ANDRegWithMemToEither,
    S86_InstructionType_ANDImmediateToRegOrMem,
    S86_InstructionType_ANDImmediateToAccum,

    S86_InstructionType_TESTRegOrMemAndReg,
    S86_InstructionType_TESTImmediateAndRegOrMem,
    S86_InstructionType_TESTImmediateAndAccum,

    S86_InstructionType_ORRegOrMemAndRegToEither,
    S86_InstructionType_ORImmediateToRegOrMem,
    S86_InstructionType_ORImmediateToAccum,

    S86_InstructionType_XORRegOrMemAndRegToEither,
    S86_InstructionType_XORImmediateToRegOrMem,
    S86_InstructionType_XORImmediateToAccum,

    S86_InstructionType_REP,

    S86_InstructionType_CALLDirectWithinSeg,
    S86_InstructionType_CALLIndirectWithinSeg,
    S86_InstructionType_CALLDirectInterSeg,
    S86_InstructionType_CALLIndirectInterSeg,

    S86_InstructionType_JMPDirectWithinSeg,
    S86_InstructionType_JMPDirectWithinSegShort,
    S86_InstructionType_JMPIndirectWithinSeg,
    S86_InstructionType_JMPDirectInterSeg,
    S86_InstructionType_JMPIndirectInterSeg,

    S86_InstructionType_RETWithinSeg,
    S86_InstructionType_RETWithinSegAddImmediateToSP,
    S86_InstructionType_RETInterSeg,
    S86_InstructionType_RETInterSegAddImmediateToSP,

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

    S86_InstructionType_INT,
    S86_InstructionType_INT3,
    S86_InstructionType_INTO,
    S86_InstructionType_IRET,

    S86_InstructionType_CLC,
    S86_InstructionType_CMC,
    S86_InstructionType_STC,
    S86_InstructionType_CLD,
    S86_InstructionType_STD,
    S86_InstructionType_CLI,
    S86_InstructionType_STI,
    S86_InstructionType_HLT,
    S86_InstructionType_WAIT,

    S86_InstructionType_LOCK,
    S86_InstructionType_SEGMENT,

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
    bool   has_displacement;
} S86_EffectiveAddressStr8;

S86_EffectiveAddressStr8 S86_EffectiveAddressCalc(S86_BufferIterator *buffer_it, uint8_t rm, uint8_t mod, uint8_t w, S86_Str8 seg_reg);
