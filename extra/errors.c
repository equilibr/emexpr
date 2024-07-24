/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022-2024 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "errors.h"

static const char * eelib_compile_status_strings_short[] =
{
	"ok",
	"stored",
	"empty",
	"error",
	"no result",
	"bad input",
	"stack error",
	"stack underflow",
	"rt underflow",
	"memory",
	"stack overflow",
	"ins. overflow",
	"const. overflow",
	"var. overflow",
	"func. overflow",
	"binding",
	"unknown var",
	"dup. varfunc",
	"no func.",
	"wrong arity",
	"no prefix",
	"no infix",
	"no postfix",
	"expression",
	"unexpected",
	"rouge end",
	"not constants",
	"not identifier",
	"empty group",
	"over. group",
	"null assign"
};

static const char * eelib_compile_status_strings_long[] =
{
	"Ok",
	"Last value stored in variable",
	"Expression is empty",
	"General error",
	"Expression does not return a result",
	"Unknown symbol in input string",
	"Compilation stack error",
	"Compilation stack underflow",
	"Calculated runtime stack underflow",
	"Not enough memory to compile",
	"Compilation stack overflow",
	"Not enough instruction memory to compile",
	"Not enough constants memory to compile",
	"Not enough variables memory to compile",
	"Not enough functions memory to compile",
	"Binding error",
	"A variable named by the expression is not provided",
	"A variable and a zero-arity function of the same name are defined at the same time",
	"A function used by the expression is not implemented",
	"A function used by the expression is not implemented with the requested arity",
	"A prefix operator used by the expression is not implementation",
	"An infix operator used by the expression is not implementation",
	"An postfix operator used by the expression is not implementation",
	"A malformed expression was parsed with an unspecified error",
	"An unexpected token was encountered",
	"The expression has an unmatched end delimiter",
	"Something that looked like a constant, but was not an actual constant, was encountered",
	"An identifier was expected",
	"An empty group (enclosed in parens) was specified",
	"An group (enclosed in parens) with too many elements was specified",
	"Assignment from an empty expression"
};

static const char * eelibi_evaluate_status_strings_short[] =
{
	"ok",
	"empty",
	"stack ext.",
	"stack und.",
};

static const char * eelibi_evaluate_status_strings_long[] =
{
	"Ok",
	"Expression is empty",
	"There are more values than expected left on the stack",
	"A result was expected on the stack but there was none",
};


static inline const char * eelibi_string_or_error(int index, const char * strings[], int items)
{
	static const char * unknown = "unknown";

	return (index < items) ? strings[index] : unknown;
}

const char * eelib_compile_status_string_short(ee_parser_reply reply)
{
	return eelibi_string_or_error(
				reply,
				eelib_compile_status_strings_short,
				sizeof(eelib_compile_status_strings_short) / sizeof(eelib_compile_status_strings_short[0]));
}

const char * eelib_compile_status_string_long(ee_parser_reply reply)
{
	return eelibi_string_or_error(
				reply,
				eelib_compile_status_strings_long,
				sizeof(eelib_compile_status_strings_long) / sizeof(eelib_compile_status_strings_long[0]));
}

const char * eelib_evaluate_status_string_short(ee_evaluator_reply reply)
{
	return eelibi_string_or_error(
				reply,
				eelibi_evaluate_status_strings_short,
				sizeof(eelibi_evaluate_status_strings_short) / sizeof(eelibi_evaluate_status_strings_short[0]));
}

const char * eelib_evaluate_status_string_long(ee_evaluator_reply reply)
{
	return eelibi_string_or_error(
				reply,
				eelibi_evaluate_status_strings_long,
				sizeof(eelibi_evaluate_status_strings_long) / sizeof(eelibi_evaluate_status_strings_long[0]));
}
