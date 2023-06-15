/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EMEXPRLIB_ERRORS_H
#define EMEXPRLIB_ERRORS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../src/emexpr.h"

const char * eelib_compile_status_string_short(ee_parser_reply reply);
const char * eelib_compile_status_string_long(ee_parser_reply reply);
const char * eelib_evaluate_status_string_short(ee_evaluator_reply reply);
const char * eelib_evaluate_status_string_long(ee_evaluator_reply reply);

#ifdef __cplusplus
}
#endif

#endif // EMEXPRLIB_ERRORS_H
