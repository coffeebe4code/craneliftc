#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "craneliftc_extra.h"

typedef enum CCallConv {
  Fast,
  Cold,
  Tail,
  SystemV,
  WindowsFastcall,
  AppleAarch64,
  Probestack,
  WasmtimeSystemV,
  WasmtimeFastcall,
  WasmtimeAppleAarch64,
} CCallConv;

typedef enum CType {
  I8,
  I16,
  I32,
  I64,
  I128,
  F32,
  F64,
  R32,
  R64,
  I8X8,
  I16X4,
  I32X2,
  F32X2,
  I8X16,
  I16X8,
  I32X4,
  I64X2,
  F32X4,
  F64X2,
  F32X8,
  F64X4,
  F32X16,
  F64X8,
} CType;

typedef uint32_t CBlock;

typedef uint32_t CVariable;

typedef uint32_t CValue;

typedef uint32_t CInst;

typedef enum CTrapCode_Tag {
  StackOverflow,
  HeapOutOfBounds,
  HeapMisaligned,
  TableOutOfBounds,
  IndirectCallToNull,
  BadSignature,
  IntegerOverflow,
  IntegerDivisionByZero,
  BadConversionToInteger,
  UnreachableCodeReached,
  Interrupt,
  User,
} CTrapCode_Tag;

typedef struct CTrapCode {
  CTrapCode_Tag tag;
  union {
    struct {
      uint16_t user;
    };
  };
} CTrapCode;

typedef int64_t CImm64;

typedef uint32_t CJumpTable;

void cstr_free(char *s);

FunctionBuilderContext *CL_FunctionBuilderContext_new(void);

void CL_FunctionBuilderContext_dispose(FunctionBuilderContext *val);

void CL_FunctionBuilder_dispose(FunctionBuilder *val);

FunctionBuilder *CL_FunctionBuilder_new(Function *func, FunctionBuilderContext *ctx);

CBlock CL_FunctionBuilder_create_block(FunctionBuilder *builder);

void CL_FunctionBuilder_declare_var(FunctionBuilder *builder, CVariable variable, enum CType typ);

void CL_FunctionBuilder_def_var(FunctionBuilder *builder, CVariable variable, CValue val);

CValue CL_FunctionBuilder_use_var(FunctionBuilder *builder, CVariable variable);

void CL_FunctionBuilder_append_block_params_for_function_params(FunctionBuilder *builder,
                                                                CBlock block);

void CL_FunctionBuilder_finalize(FunctionBuilder *builder);

void CL_FunctionBuilder_switch_to_block(FunctionBuilder *builder, CBlock block);

void CL_FunctionBuilder_seal_block(FunctionBuilder *builder, CBlock block);

CValue CL_FunctionBuilder_block_params(FunctionBuilder *builder, CBlock block, uintptr_t idx);

void CL_UserFuncName_dispose(UserFuncName *val);

UserFuncName *CL_UserFuncName_user(uint32_t one, uint32_t two);

CVariable CL_Variable_from_u32(uint32_t val);

void CL_Function_dispose(Function *val);

Function *CL_Function_with_name_signature(UserFuncName *user, Signature *sig);

void CL_Function_verify(Function *func, Flags *flags);

char *CL_Function_display(Function *func);

AbiParam *CL_AbiParam_new(enum CType one);

Signature *CL_Signature_new(enum CCallConv one);

void CL_Signature_returns_push(Signature *sig, AbiParam *abi);

void CL_Signature_params_push(Signature *sig, AbiParam *abi);

void CL_Flags_dispose(Flags *val);

Flags *CL_Flags_new(Builder *builder);

Builder *CL_Builder_builder(void);

CInst CL_FunctionBuilder_debugtrap(FunctionBuilder *builder);

CInst CL_FunctionBuilder_fence(FunctionBuilder *builder);

CInst CL_FunctionBuilder_trap(FunctionBuilder *builder, struct CTrapCode code, uint16_t user);

CInst CL_FunctionBuilder_resumable_trap(FunctionBuilder *builder,
                                        struct CTrapCode code,
                                        uint16_t user);

CInst CL_FunctionBuilder_trapz(FunctionBuilder *builder,
                               CValue val,
                               struct CTrapCode code,
                               uint16_t user);

CInst CL_FunctionBuilder_trapnz(FunctionBuilder *builder,
                                CValue val,
                                struct CTrapCode code,
                                uint16_t user);

CInst CL_FunctionBuilder_return_(FunctionBuilder *builder, CValue *rvals_raw, uintptr_t len);

CValue CL_FunctionBuilder_iconst(FunctionBuilder *builder, enum CType one, CImm64 imm);

CValue CL_FunctionBuilder_iadd_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_imul_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_udiv_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_sdiv_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_urem_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_srem_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_irsub_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_band_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_bor_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_bxor_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_rotl_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_rotr_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_ishl_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_ushr_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CValue CL_FunctionBuilder_sshr_imm(FunctionBuilder *builder, CValue one, CImm64 imm);

CInst CL_FunctionBuilder_br_table(FunctionBuilder *builder, CValue val, CJumpTable jt);

CInst CL_FunctionBuilder_jump(FunctionBuilder *builder,
                              CBlock block_call_label,
                              CValue *block_call_args,
                              uintptr_t len);

CValue CL_FunctionBuilder_select(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_select_spectre_guard(FunctionBuilder *builder,
                                               CValue c,
                                               CValue left,
                                               CValue right);

CValue CL_FunctionBuilder_bitselect(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_x86_blendv(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_iadd_cin(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_isub_bin(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_fma(FunctionBuilder *builder, CValue c, CValue left, CValue right);

CValue CL_FunctionBuilder_iadd(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_uadd_sat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_sadd_sat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_isub(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_udiv(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_sdiv(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_urem(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_srem(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_usub_sat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_ssub_sat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_imul(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_umulhi(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_smulhi(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_sqmul_round_sat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_x86_pmulhrsw(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_smin(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_umin(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_smax(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_umax(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_avg_round(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_swizzle(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_x86_pshufb(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_band(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_bor(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_bxor(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_band_not(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_bor_not(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_bxor_not(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_rotl(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_rotr(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_ishl(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_ushr(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_sshr(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fadd(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fsub(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fmul(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fdiv(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fcopysign(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fmin(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fmin_pseudo(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fmax(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_fmax_pseudo(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_snarrow(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_unarrow(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_uunarrow(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_iadd_pairwise(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_x86_pmaddubsw(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_iconcat(FunctionBuilder *builder, CValue left, CValue right);

CValue CL_FunctionBuilder_ineg(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_iabs(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_vany_true(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_vall_true(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_bnot(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_bitrev(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_clz(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_cls(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_ctz(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_bswap(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_popcnt(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_sqrt(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_fneg(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_fabs(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_ceil(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_floor(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_trunc(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_nearest(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_is_null(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_is_invalid(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_swiden_low(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_uwiden_low(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_swiden_high(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_uwiden_high(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_fvdemote(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_fvpromote_low(FunctionBuilder *builder, CValue one);

CInst CL_FunctionBuilder_set_pinned_reg(FunctionBuilder *builder, CValue one);

CInst CL_FunctionBuilder_brif(FunctionBuilder *builder,
                              CValue val,
                              CBlock block_one_label,
                              CValue *block_one_args,
                              uintptr_t one_len,
                              CBlock block_two_label,
                              CValue *block_two_args,
                              uintptr_t two_len);
