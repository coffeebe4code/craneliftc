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

typedef int64_t CImm64;

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

void CL_Signature_returns_push(Signature *sig, AbiParam *conv);

void CL_Signature_params_push(Signature *sig, AbiParam *conv);

void CL_Flags_dispose(Flags *val);

Flags *CL_Flags_new(Builder *builder);

Builder *CL_Builder_builder(void);

CInst CL_FunctionBuilder_return_(FunctionBuilder *builder, CValue *rvals_raw, uintptr_t len);

CValue CL_FunctionBuilder_iconst(FunctionBuilder *builder, enum CType one, CImm64 two);

CValue CL_FunctionBuilder_iadd(FunctionBuilder *builder, CValue one, CValue two);

CValue CL_FunctionBuilder_isub(FunctionBuilder *builder, CValue one, CValue two);

CValue CL_FunctionBuilder_imul(FunctionBuilder *builder, CValue one, CValue two);

CValue CL_FunctionBuilder_umulhi(FunctionBuilder *builder, CValue one, CValue two);

CValue CL_FunctionBuilder_ineg(FunctionBuilder *builder, CValue one);

CValue CL_FunctionBuilder_iabs(FunctionBuilder *builder, CValue one);
