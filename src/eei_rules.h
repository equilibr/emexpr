/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EEI_RULES_H
#define EEI_RULES_H

#include "emexpr.h"

//This file holds all the definitions needed for creating parser rule tables.
//For a usage example see the default rules table.


//User-selectable definitions
//---------------------------

//Operator precedence list.
enum
{
	//Must have value of 0 since the algorithm extensively depends on this!
	eei_precedence_group = 0,

	eei_precedence_assign,
	eei_precedence_comma,
	eei_precedence_select,
	eei_precedence_select_else=eei_precedence_select,
	eei_precedence_logical_or,
	eei_precedence_logical_and,
	eei_precedence_compare,
	eei_precedence_power1,
	eei_precedence_power2,
	eei_precedence_power3,
	eei_precedence_function,
	eei_precedence_postfix,

	//Must be the last item to allow some compile-time checks!
	eei_precedence_sentinel
};


//Generic macros
//--------------

//Create a bitmask of specified width and offset
#define BITMASK(width) ((( (1ULL<<((width) - 1)) - 1) << 1) | 1 )
#define BITMASKS(width, offset) (BITMASK(width) << (offset))

#define MAKE_PART_BITS(data, offset, size) ( ((data) & BITMASK(size)) << (offset) )
#define GET_PART_BITS(data, offset, size) ( ((data) >> (offset)) & BITMASK(size) )


//Lexer
//-----

typedef enum
{
	//Internally used tokens
	eei_token_internal,

	eei_token_identifier,
	eei_token_constant,
	eei_token_delimiter,
	eei_token_operator,

	//Sentinel value to count the number of enumeraions
	//The _check_type_sizes struct uses this to validate all values fit in the data type used for the token
	//DO NOT use it in the actual tokens since it may not fit in the allocated bits!
	eei_token_sentinel
} eei_token_type;

typedef enum
{
	eei_token_internal_error,
	eei_token_internal_eof,
	eei_token_internal_parser
} eei_token_internals;

//Holds the token type and the symbol
//A token has two parts:
//	A type, as enumerated in eei_token_type
//	A symbol, used to differentiate between the various delimiters and operators
typedef unsigned short int eei_token;

//A list of token symbols with special meaning
enum
{
	//Denote a wildcard (any) possible symbol
	token_symbol_any = 0,

	//This is a _hack_ to represent known double-character operators using a single character.
	//We can do this becase we know it is impossible for an operator to actually be one of the symbols used below.
	//Moreover, no function can also never consist(nor even start) from those symbols. Thus we make sure there will
	//	never be name clashes between theese synthetic token symbols and actual symbols that might be used for
	//	operators/functions.
	token_symbol_op_eq = '0',
	token_symbol_op_neq = '1',
	token_symbol_op_gte = '2',
	token_symbol_op_lte = '3',
	token_symbol_op_or = '4',
	token_symbol_op_and = '5',
	token_symbol_op_xor = '6',
};

//Field sizes and offsets of the various parts for the token
enum
{
	//Base offset for the token data
	eei_token_bits_start_offset = 0,

	//Token symbol
	eei_token_bits_char_size = sizeof(ee_char_type) * __CHAR_BIT__,

	//Token type. Must accomodate eei_token_type (without the sentinel)
	eei_token_bits_type_size = 3,

	//Offset for the symbol part of a token
	eei_token_bits_char_offset = eei_token_bits_start_offset,

	//Offset for the type part of a token
	eei_token_bits_type_offset = eei_token_bits_char_offset + eei_token_bits_char_size,

	//Compound size of a token
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the token
	eei_token_bits_size = eei_token_bits_char_size + eei_token_bits_type_size + eei_token_bits_start_offset,
};

//Create a token from its parts
#define TOKEN(token_type, token_symbol) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_token_bits_type_offset, eei_token_bits_type_size) |\
	MAKE_PART_BITS(token_symbol, eei_token_bits_char_offset, eei_token_bits_char_size) \
	)

//Create a simple token that does not use the symbol
#define SIMPLE_TOKEN(token_type) TOKEN(token_type, token_symbol_any)

//Special tokens
#define ERROR_TOKEN() TOKEN(eei_token_internal,eei_token_internal_error)
#define EOF_TOKEN() TOKEN(eei_token_internal,eei_token_internal_eof)

//Extract token data from a token
#define GET_TOKEN_TYPE(token) (eei_token_type)GET_PART_BITS(token, eei_token_bits_type_offset, eei_token_bits_type_size)
#define GET_TOKEN_SYMBOL(token) (ee_char_type)GET_PART_BITS(token, eei_token_bits_char_offset, eei_token_bits_char_size)


//Parser rule types
//-----------------

typedef enum
{
	eei_token_internal_praser_sof = eei_token_internal_parser,
	eei_token_internal_praser_group,
	eei_token_internal_praser_end,
	eei_token_internal_praser_delimit,
	eei_token_internal_praser_copy
} eei_token_internals_parser;

typedef enum
{
	eei_rule_prefix,
	eei_rule_infix,
	eei_rule_postfix,
	eei_rule_end
} eei_rule_type;

typedef enum
{
	eei_rule_left,
	eei_rule_right
} eei_rule_associativity;

typedef enum
{
	eei_rule_normal_fold,
	eei_rule_delay_fold,
	eei_rule_no_fold,
} eei_rule_fold;

//Handler functions for rule folding
typedef enum
{
	eei_rule_handle_none,
	eei_rule_handle_error,
	eei_rule_handle_constant,
	eei_rule_handle_variable,
	eei_rule_handle_delimiter,
	eei_rule_handle_group,
	eei_rule_handle_prefix,
	eei_rule_handle_infix,
	eei_rule_handle_postfix,
	eei_rule_handle_function,
	eei_rule_handle_assign,

	//Sentinel value to count the number of enumeraions
	//The _check_type_sizes struct uses this to validate all values fit in the data type used for the rule
	//DO NOT use it in the actual rules since it may not fit in the allocated bits!
	eei_rule_handle_sentinel
} eei_rule_handle;


//Holds a parsing rule
//A rule consists of a token and the rule type, as enumerated in eei_rule_type
typedef unsigned short int eei_rule;

//Holds a rule precedence
typedef unsigned char eei_precedence;

//Holds a parsing rule description
//A description consists of:
//	A rule - when this token is expected
//	Its associativity & precedence - how to combine it with other rules
//	Delimited flag - when set this rule must have a matching end-dilimiter rule
//	Next - the rule type to expected next
typedef unsigned int eei_rule_description;

typedef struct
{
	//The currently parsed rule
	eei_rule_description current;

	//The condition for matching the current rule
	eei_rule_description condition;

	//The rule the current one is morphed-to in case of a match
	eei_rule_description morphed;
} eei_conditional_table_item;

//Parser rule tables
typedef struct
{
	const eei_rule_description * prefix;
	const eei_rule_description * infix;
	const eei_rule_description * postfix;
	const eei_conditional_table_item * lookbehind;
	const eei_conditional_table_item * group;
	const eei_conditional_table_item * fold;
} ee_parser_rules;


//Parser rules
//------------

//Field sizes of the various parts for the token, rule & rule description
typedef enum
{
	//Token symbol
	eei_rule_bits_token_size = eei_token_bits_size,

	//Rule type. Must accomodate eei_rule_type
	eei_rule_bits_type_size = 2,

	//Rule description precedence. Must be wide enough to alllow for the highest used precedence
	eei_rule_bits_precedence_size = 4,

	//Rule description accosiativity. Must accomodate eei_rule_associativity
	eei_rule_bits_accosiativity_size = 1,

	//Rule description end-delimited flag. Single bit.
	eei_rule_bits_end_delimiter_size = 1,

	//Rule description fold indicator. Must accomodate eei_rule_fold
	eei_rule_bits_fold_size = 2,

	//Rule description look behind flag. Single bit.
	eei_rule_bits_look_behind_size = 1,

	//Rule description in-group flag. Single bit.
	eei_rule_bits_grouped_size = 1,

	//Rule handler index. Must accomodate eei_rule_handler (without the sentinel).
	eei_rule_bits_handler_size = 4,

	//Compound size of a rule
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the rule
	eei_rule_bits_rule_size = eei_rule_bits_token_size + eei_rule_bits_type_size,

	//Size of the 'next' part of a rule description.
	eei_rule_bits_next_size = eei_rule_bits_type_size
} eei_rule_sizes;

//Field offsets of the various parts for the token, rule & rule description
typedef enum
{
	//Global offset for all parts.
	//Kept here to simplify injecting custom data into the items.
	//The offset is used in all manifestations of the item - as stand alone or as parts of a compound
	eei_rule_bits_start_offset = 0,


	//Token
	//-----

	//Offset of the complete token
	eei_rule_bits_token_offset = eei_rule_bits_start_offset,


	//Rule
	//----

	//Offset for the type part of a rule
	eei_rule_bits_type_offset = eei_rule_bits_token_offset + eei_rule_bits_token_size,

	//Offset of the complete rule
	eei_rule_bits_rule_offset = eei_rule_bits_token_offset,


	//Rule description
	//----------------

	//Offset for the precedence part of a rule description
	eei_rule_bits_precedence_offset = eei_rule_bits_rule_offset + eei_rule_bits_rule_size,

	//Offset for the next  part of a rule description
	eei_rule_bits_next_offset = eei_rule_bits_precedence_offset + eei_rule_bits_precedence_size,

	//Offset for the accosiativity part of a rule description
	eei_rule_bits_accosiativity_offset = eei_rule_bits_next_offset + eei_rule_bits_next_size,

	//Offset for the end-delimited flag of a rule description
	eei_rule_bits_end_delimiter_offset = eei_rule_bits_accosiativity_offset + eei_rule_bits_accosiativity_size,

	//Offset for the delayed fold flag of a rule description
	eei_rule_bits_fold_offset = eei_rule_bits_end_delimiter_offset + eei_rule_bits_end_delimiter_size,

	//Offset for the look behind flag of a rule description
	eei_rule_bits_look_behind_offset = eei_rule_bits_fold_offset + eei_rule_bits_fold_size,

	//Offset for the in-group flag of a rule description
	eei_rule_bits_grouped_offset =  eei_rule_bits_look_behind_offset + eei_rule_bits_look_behind_size,

	//Offset for the handler part of a rule description
	eei_rule_bits_handler_offset = eei_rule_bits_grouped_offset + eei_rule_bits_grouped_size,

	//Total size of a rule description, in bits
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the rule description
	eei_rule_bits_total_size = eei_rule_bits_handler_offset + eei_rule_bits_handler_size
} eei_rule_offsets;


//Special tokens used only by the parser
//--------------------------------------

#define SOF_TOKEN() TOKEN(eei_token_internal,eei_token_internal_praser_sof)
#define GROUP_TOKEN() TOKEN(eei_token_internal,eei_token_internal_praser_group)
#define END_TOKEN() TOKEN(eei_token_internal,eei_token_internal_praser_end)
#define DELIMIT_TOKEN() TOKEN(eei_token_internal,eei_token_internal_praser_delimit)
#define COPY_TOKEN() TOKEN(eei_token_internal,eei_token_internal_praser_copy)


//Create a rule from its parts
#define MAKE_RULE(token, type) \
	(eei_rule)(\
	MAKE_PART_BITS(token, eei_rule_bits_token_offset, eei_rule_bits_token_size) |\
	MAKE_PART_BITS(type, eei_rule_bits_type_offset, eei_rule_bits_type_size) \
	)

//Create a rule description from its parts
#define MAKE_RULE_DESCRIPTION(rule, precedence, next, accosiativity) \
	(eei_rule_description)(\
	MAKE_PART_BITS(rule, eei_rule_bits_rule_offset, eei_rule_bits_rule_size) |\
	MAKE_PART_BITS(precedence, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size) |\
	MAKE_PART_BITS(next, eei_rule_bits_next_offset, eei_rule_bits_next_size) |\
	MAKE_PART_BITS(accosiativity, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size)\
	)


//Basic rule descriptoins

//A terminal prefix rule
#define TERMINAL(token) MAKE_RULE_DESCRIPTION(MAKE_RULE((token), eei_rule_prefix), 0, eei_rule_infix, eei_rule_right)

//A non-terminal prefix rule
#define PREFIX(token) MAKE_RULE_DESCRIPTION(MAKE_RULE((token), eei_rule_prefix), 0, eei_rule_prefix, eei_rule_right)

//A regular left-binding infix rule
#define INFIX(token, precedence) MAKE_RULE_DESCRIPTION(MAKE_RULE((token), eei_rule_infix), (precedence), eei_rule_prefix, eei_rule_left)

//A regular right-binding infix rule
#define RIGHTINFIX(token, precedence) MAKE_RULE_DESCRIPTION(MAKE_RULE((token), eei_rule_infix), (precedence), eei_rule_prefix, eei_rule_right)

//A regular left-biding postfix rule
#define POSTFIX(token) MAKE_RULE_DESCRIPTION(MAKE_RULE((token), eei_rule_postfix), eei_precedence_postfix, eei_rule_infix, eei_rule_left)


//Rule description modifiers

//Changes the precedence of the rule
#define PRECEDENCE_RULE(rule_description, precedence) ((eei_rule_description)( (rule_description &\
	~BITMASKS(eei_rule_bits_precedence_size, eei_rule_bits_precedence_offset)) |\
	MAKE_PART_BITS(precedence, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size)))

//Changes the next rule
#define NEXT_RULE(rule_description, next) ((eei_rule_description)( (rule_description &\
	~BITMASKS(eei_rule_bits_next_size, eei_rule_bits_next_offset)) |\
	MAKE_PART_BITS(next, eei_rule_bits_next_offset, eei_rule_bits_next_size)))

//Changes the rule type
#define TYPE_RULE(rule_description, type) ((eei_rule_description)( (rule_description &\
	~BITMASKS(eei_rule_bits_type_size, eei_rule_bits_type_offset)) |\
	MAKE_PART_BITS(type, eei_rule_bits_type_offset, eei_rule_bits_type_size)))

//A rule that starts a group with (end) delimiters, with it own precedence stack
#define DELIMITED(rule_description) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(1, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)))

//A rule that is not folded when a delimited rule prefix-folds
#define DELAYED(rule_description) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(eei_rule_delay_fold, eei_rule_bits_fold_offset, eei_rule_bits_fold_size)))

//A rule that does not folds the stack prefixes
#define NOFOLD(rule_description) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(eei_rule_no_fold, eei_rule_bits_fold_offset, eei_rule_bits_fold_size)))

//A rule with a look-behind condition
//The matching rule MUST appear in the look-behind table
#define LOOKBEHIND(rule_description) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(1, eei_rule_bits_look_behind_offset, eei_rule_bits_look_behind_size)))

//A rule with a grouped condition
//The matching rule MUST appear in the look-behind table
#define GROUPED(rule_description) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(1, eei_rule_bits_grouped_offset, eei_rule_bits_grouped_size)))

//A rule with a folding handle
#define HANDLE(rule_description, handler) ((eei_rule_description)( (rule_description) |\
	MAKE_PART_BITS(handler, eei_rule_bits_handler_offset, eei_rule_bits_handler_size)))

//A rule with a default error handle
#define ERROR(rule_description) HANDLE(rule_description, eei_rule_handle_error)

//Conditional table rules

//A generic prefix rule for using in the conditiona lables
#define PREFIX_RULE(token) MAKE_RULE(token, eei_rule_prefix)

//A generic infix rule for using in the conditiona lables
#define INFIX_RULE(token) MAKE_RULE(token, eei_rule_infix)

//A rule for the conditional table that uses the original and ends a group
#define END_RULE(handler) HANDLE(END_TOKEN(),handler)

//A rule for the conditional table that uses the original with a handle added
#define DELIMIT_RULE(handler) HANDLE(DELIMIT_TOKEN(),handler)

//A rule for the conditional table that uses the original as-is
#define COPY_RULE() COPY_TOKEN()


//Special rules

//A sentinel invalid rule to mark the end of a rules table
#define SENTINEL_RULE() PREFIX(ERROR_TOKEN())

//A special rule to mark the start of the expression
#define SOF_RULE() DELIMITED(PREFIX(SOF_TOKEN()))

//A special rule that marks parsing groups
#define GROUP_RULE() INFIX(GROUP_TOKEN(),eei_precedence_group)


#endif // EEI_RULES_H
