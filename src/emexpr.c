/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "emexpr.h"
#include "eei_symboltable.h"

#include <stddef.h>

//Notation and formalism:
// All implementation details have a prefix of "eei" - Embedded Expression Implementation
// All externally visible definition have a prefix of "ee" - Embedded Expression

//User-selectable definitions
//---------------------------

//Allow using prefix operators with functional syntax
#define EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS 1

//Allow assigning into variables
#define EEI_ALLOW_ASSIGN 1



//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_CONSTANT_SCANNER) && defined(EE_USER_CONSTANT_PARSER)
#	define EEI_DEFAULT_CONSTANT 0
#	define EEI_CONSTANT_SCANNER EE_USER_CONSTANT_SCANNER
#	define EEI_CONSTANT_PARSER EE_USER_CONSTANT_PARSER
#else
#	define EEI_DEFAULT_CONSTANT 1
#	define EEI_CONSTANT_SCANNER eei_constant_scanner
#	define EEI_CONSTANT_PARSER eei_constant_parser
#endif

#if defined(EE_USER_VARIBALE_LOAD)
#	define EEI_VARIABLE_LOAD EE_USER_VARIBALE_LOAD
#else
#	define EEI_VARIABLE_LOAD(dst,src) *dst = *src
#endif

#if defined(EE_USER_VARIBALE_STORE)
#	define EEI_VARIABLE_STORE EE_USER_VARIBALE_STORE
#else
#	define EEI_VARIABLE_STORE(dst,src) *dst = *src
#endif

//Default implementations of user modifiable items
//------------------------------------------------

//This provides the library default implementation of things the user can modify/override.
//The implementation is at the top of the code to make sure it only uses declarations visible
//	to the external user of the library, e.g. our header file.

#if EEI_DEFAULT_CONSTANT

const ee_char_type * eei_constant_scanner(const ee_char_type * start)
{
	//A naive base-10 scanner

	while ( (*start >= '0') && (*start <= '9') )
		++start;

	//Look for fractional part
	if (*start != '.')
		return start;

	//Make sure a number follows the decimal dot
	if ( (*(start+1) < '0') || (*(start+1) > '9') )
		return start;

	//Skip the decimal
	++start;

	while ( (*start >= '0') && (*start <= '9') )
		++start;

	return start;
}

int eei_constant_parser(const ee_char_type * start, const ee_char_type * end, ee_variable_type * result)
{
	//A naive base-10 parser
	enum {numeric_base = 10};

	ee_variable_type integer = 0;
	ee_variable_type fractional = 0;

	while (start < end)
	{
		if (*start == '.')
			break;

		integer *= numeric_base;
		integer += *start - '0';
		start++;
	}

	if (*start == '.')
		while (start < --end)
		{
			fractional += *end - '0';
			fractional /= numeric_base;
		}

	*result = integer + fractional;

	return 0;
}

#endif


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
	eei_token_error,
	eei_token_eof,

	eei_token_sof,
	eei_token_group,

	//Marker for the END of internal tokens
	eei_token_internals,

	eei_token_identifier = eei_token_internals,
	eei_token_constant,
	eei_token_delimiter,
	eei_token_operator,

	//Sentinel value to count the number of enumeraions
	//The _check_type_sizes struct uses this to validate all values fit in the data type used for the token
	//DO NOT use it in the actual tokens since it may not fit in the allocated bits!
	eei_token_sentinel
} eei_token_type;

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
#define MAKE_TOKEN(token_type, token_symbol) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_token_bits_type_offset, eei_token_bits_type_size) |\
	MAKE_PART_BITS(token_symbol, eei_token_bits_char_offset, eei_token_bits_char_size) \
	)

//Create a simple token that does not use the symbol
#define MAKE_SIMPLE_TOKEN(token_type) MAKE_TOKEN(token_type, token_symbol_any)

//Extract token data from a token
#define GET_TOKEN_TYPE(token) (eei_token_type)GET_PART_BITS(token, eei_token_bits_type_offset, eei_token_bits_type_size)
#define GET_TOKEN_SYMBOL(token) (ee_char_type)GET_PART_BITS(token, eei_token_bits_char_offset, eei_token_bits_char_size)


typedef struct
{
	const ee_char_type * start;
	const ee_char_type * head;
} eei_lexer_state;

static inline int eei_lexer_is_eof(const ee_char_type c)
{
	return c == '\0';
}

static inline int eei_lexer_is_space(const ee_char_type c)
{
	return
			(c == ' ') || (c == '\t') || (c == '\r') || (c == '\n');
}


static inline int eei_lexer_is_alpha(const ee_char_type c)
{
	return
			((c >= 'a') && (c <= 'z'))
			|| ((c >= 'A') && (c <= 'Z'))
			|| ( c == '_');
}

static inline int eei_lexer_is_number(const ee_char_type c)
{
	return (c >= '0') && (c <= '9');
}

static inline int eei_lexer_is_delimiter(const ee_char_type c)
{
	return
			(c == ',')
			|| (c == '(') || (c == ')')
			|| (c == '[') || (c == ']')
			|| (c == '{') || (c == '}');
}

static inline int eei_lexer_is_operator(const ee_char_type c)
{
	return
			(c == ':') ||(c == '\'') || (c == '.')
			|| (c == '+') || (c == '-') || (c == '*') || (c == '/')
			|| (c == '%') ||(c == '^') || (c == '&') || (c == '|')
			|| (c == '~') ||(c == '!')
			|| (c == '=') ||(c == '>') || (c == '<');
}

static inline ee_char_type eei_lexer_create_double_character_operator(const ee_char_type first, const ee_char_type second)
{
	if (second == '=')
	{
		switch (first)
		{
			case '=': return token_symbol_op_eq;
			case '!': return token_symbol_op_neq;
			case '>': return token_symbol_op_gte;
			case '<': return token_symbol_op_lte;
		}
		return token_symbol_any;
	}

	if (first != second)
		return token_symbol_any;

	switch (first)
	{
		case '|': return token_symbol_op_or;
		case '&': return token_symbol_op_and;
		case '^': return token_symbol_op_xor;
	}

	return token_symbol_any;
}

int eei_lexer_consume_digits(eei_lexer_state * state)
{
	enum { decimal_base = 10 };
	int number = 0;

	do
	{
		number *= decimal_base;
		number += *state->head - '0';
		state->head++;
	} while (
			 !eei_lexer_is_eof(*state->head)
			 &&  eei_lexer_is_number(*state->head));

	return number;
}

static inline void eei_lexer_consume_identifier(eei_lexer_state * state)
{
	//An identifier can consist of alpha or numerics
	do { ++state->head; } while
			(
			 !eei_lexer_is_eof(*state->head)
			 &&
			 (
				 eei_lexer_is_alpha(*state->head)
				 || eei_lexer_is_number(*state->head)
				 )
			 );
}

eei_token eei_lexer_next_token(eei_lexer_state * state)
{
	//Parse the input stream for the next token

	//Skip spaces
	while (eei_lexer_is_space(*state->head)) state->head++;

	state->start = state->head;

	if (eei_lexer_is_eof(*state->head))
		return MAKE_SIMPLE_TOKEN(eei_token_eof);

	//The first character determines the token class

	if (eei_lexer_is_number(*state->head))
	{
		state->head = EEI_CONSTANT_SCANNER(state->head);

		if (state->head != state->start)
			return MAKE_SIMPLE_TOKEN(eei_token_constant);
	}

	if (eei_lexer_is_alpha(*state->head))
	{
		eei_lexer_consume_identifier(state);
		return MAKE_SIMPLE_TOKEN(eei_token_identifier);
	}

	if (eei_lexer_is_delimiter(*state->head))
	{
		//Delimiters are always single character
		state->head++;
		return MAKE_TOKEN(eei_token_delimiter, *state->start);
	}

	if (eei_lexer_is_operator(*state->head))
	{
		const ee_char_type op =
				eei_lexer_create_double_character_operator(
					*state->head,
					*(state->head+1));

		state->head++;

		if (op != token_symbol_any)
		{
			//If this is a known double-character operator it consumes another char
			state->head++;

			return MAKE_TOKEN(eei_token_operator, op);
		}

		return MAKE_TOKEN(eei_token_operator, *state->start);
	}

	return MAKE_SIMPLE_TOKEN(eei_token_error);
}

//Parser rule types
//-----------------

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
	eei_rule_handle_constant,
	eei_rule_handle_variable,
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

//Forward declarations for the handler
typedef struct eei_parser_ eei_parser;
typedef struct eei_parser_node_ eei_parser_node;

//Rule handler
typedef ee_parser_reply (*eei_rule_handler)(eei_parser * parser, const eei_parser_node * node);

typedef struct
{
	//The currently parsed rule
	eei_rule_description current;

	//The previous rule
	eei_rule_description previous;

	//The rule the current one is morphed-to in case of a match
	eei_rule_description morphed;
} eei_look_behind_table_item;

typedef struct
{
	//The currently parsed rule
	eei_rule_description rule;

	//The matching end rule
	eei_rule_description end;
} eei_end_rules_table_item;


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

	//Offset for the handler part of a rule description
	eei_rule_bits_handler_offset = eei_rule_bits_look_behind_offset + eei_rule_bits_look_behind_size,

	//Total size of a rule description, in bits
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the rule description
	eei_rule_bits_total_size = eei_rule_bits_handler_offset + eei_rule_bits_handler_size
} eei_rule_offsets;


//Create a rule from its parts
#define MAKE_RULE(token, type) \
	(eei_rule)(\
	MAKE_PART_BITS(token, eei_rule_bits_token_offset, eei_rule_bits_token_size) |\
	MAKE_PART_BITS(type, eei_rule_bits_type_offset, eei_rule_bits_type_size) \
	)

//Create a rule description from its parts
#define MAKE_RULE_DESCRIPTION(rule, precedence, next, accosiativity, end_delimiter, fold, look_behind, handler) \
	(eei_rule_description)(\
	MAKE_PART_BITS(rule, eei_rule_bits_rule_offset, eei_rule_bits_rule_size) |\
	MAKE_PART_BITS(precedence, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size) |\
	MAKE_PART_BITS(next, eei_rule_bits_next_offset, eei_rule_bits_next_size) |\
	MAKE_PART_BITS(accosiativity, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size) |\
	MAKE_PART_BITS(end_delimiter, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size) |\
	MAKE_PART_BITS(fold, eei_rule_bits_fold_offset, eei_rule_bits_fold_size) |\
	MAKE_PART_BITS(look_behind, eei_rule_bits_look_behind_offset, eei_rule_bits_look_behind_size) |\
	MAKE_PART_BITS(handler, eei_rule_bits_handler_offset, eei_rule_bits_handler_size)\
	)

//Make a prefix rule description
#define MAKE_PREFIX_RULE(token, end_delimiter, fold, look_behind, next) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_prefix), 0, next, eei_rule_right, end_delimiter, fold, look_behind, eei_rule_handle_none)

//Make a default prefix rule description
#define MAKE_DEFAULT_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 0, eei_rule_normal_fold, 0, eei_rule_prefix)

//Make a default-delayed prefix rule description
#define MAKE_DELAYED_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 0, eei_rule_delay_fold, 0, eei_rule_prefix)

//Make a leaf prefix rule description
#define MAKE_TERMINAL_PREFIX_RULE(token, fold) MAKE_PREFIX_RULE(token, 0, fold, 0, eei_rule_infix)

//Make a prefix rule with a look-behind condition description
//The matching rule MUST appear in the look-behind table
#define MAKE_LOOK_BEHIND_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 0, eei_rule_normal_fold, 1, eei_rule_prefix)

//Make a delimited prefix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 1, eei_rule_normal_fold, 0, eei_rule_prefix)

//Make a infix rule description
#define MAKE_INFIX_RULE(token, precedence, accosiativity, end_delimiter) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_infix), precedence, eei_rule_prefix, accosiativity, end_delimiter, eei_rule_normal_fold, 0, eei_rule_handle_none)

//Make a default infix rule description
#define MAKE_DEFAULT_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_left, 0)

//Make a default infix rule description
#define MAKE_NOFOLD_INFIX_RULE(token, precedence) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_infix), precedence, eei_rule_prefix, eei_rule_left, 0, eei_rule_no_fold, 0, eei_rule_handle_none)

//Make a delimited infix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_right, 1)

//Make a default postfix rule description
#define MAKE_POSTFIX_RULE(token, precedence) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_postfix), precedence, eei_rule_infix, eei_rule_left, 0, eei_rule_normal_fold, 0, eei_rule_handle_none)

//Make an end rule to match a delimited rule description
#define MAKE_END_RULE(token) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_end), 0, eei_rule_prefix, eei_rule_left, 0, eei_rule_normal_fold, 0, eei_rule_handle_none)

//Make a sentinel invalid rule to mark the end of a rules table
#define MAKE_SENTINEL_RULE() MAKE_DEFAULT_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))
#define MAKE_SENTINEL_END_RULE() MAKE_END_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))

//Make a special rule that marks parsing groups
#define MAKE_GROUP_RULE() MAKE_DEFAULT_INFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_group),0)

//Create a rule than only represents state
#define STATE(rule_description) rule_description

//Create a rule with a handler
#define HANDLE(rule_description, handler) \
	MAKE_PART_BITS(rule_description, eei_rule_bits_start_offset, eei_rule_bits_total_size) |\
	MAKE_PART_BITS(handler, eei_rule_bits_handler_offset, eei_rule_bits_handler_size)



//Macros to simplify working with a ruleset

//Test if the rule is valid
#define IS_RULE_VALID(rule) ((rule) != MAKE_SENTINEL_RULE())

#define GET_RULE_TYPE(rule) (eei_rule_type)GET_PART_BITS(rule, eei_rule_bits_type_offset, eei_rule_bits_type_size)
#define GET_RULE_PRECEDENCE(rule) (eei_precedence)GET_PART_BITS(rule, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size)
#define GET_RULE_NEXT(rule) (eei_rule)GET_PART_BITS(rule, eei_rule_bits_next_offset, eei_rule_bits_next_size)
#define GET_RULE_ACCOSIATIVITY(rule) (eei_rule_associativity)GET_PART_BITS(rule, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size)
#define GET_RULE_ENDDELIMITER(rule) GET_PART_BITS(rule, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)
#define GET_RULE_FOLD(rule) (eei_rule_fold)GET_PART_BITS(rule, eei_rule_bits_fold_offset, eei_rule_bits_fold_size)
#define GET_RULE_LOOK_BEHIND(rule) GET_PART_BITS(rule, eei_rule_bits_look_behind_offset, eei_rule_bits_look_behind_size)
#define GET_RULE_HANDLER(rule) GET_PART_BITS(rule, eei_rule_bits_handler_offset, eei_rule_bits_handler_size)

//Extract a token and data from a rule description
#define GET_RULE_TOKEN(rule_description) ((eei_token)GET_PART_BITS(rule_description, eei_rule_bits_token_offset, eei_rule_bits_token_size))
#define GET_RULE_TOKEN_TYPE(rule_description) GET_TOKEN_TYPE(GET_RULE_TOKEN(rule_description))
#define GET_RULE_TOKEN_SYMBOL(rule_description) GET_TOKEN_SYMBOL(GET_RULE_TOKEN(rule_description))

//Clear the token part of a rule
#define CLEAR_RULE_TOKEN(rule_description) ((rule_description) & ~BITMASKS(eei_rule_bits_token_size, eei_rule_bits_token_offset))


//Parser rule tables
//------------------

//Operator precedence
enum
{
	eei_precedence_assign = 1,
	eei_precedence_comma = 2,
	eei_precedence_logical_or = 3,
	eei_precedence_logical_and = 4,
	eei_precedence_compare = 5,
	eei_precedence_power1 = 6,
	eei_precedence_power2 = 7,
	eei_precedence_power3 = 8,
	eei_precedence_function = 10,
	eei_precedence_postfix = 11,

	eei_precedence_sentinel
};

static const eei_rule_description eei_parser_prefix_rules[] =
{
	HANDLE(MAKE_TERMINAL_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_constant),eei_rule_normal_fold), eei_rule_handle_constant),
	HANDLE(MAKE_TERMINAL_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier),eei_rule_delay_fold), eei_rule_handle_variable),

	//Catch-all for all operators
	HANDLE(MAKE_DELAYED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_operator)), eei_rule_handle_prefix),

#	if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
	//Prefix-operator call
	STATE(MAKE_LOOK_BEHIND_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('))),
#	endif

	//Grouping parens
	HANDLE(MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'(')), eei_rule_handle_group),

	//Expression start
	STATE(MAKE_DELIMITED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_sof))),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_description eei_parser_infix_rules[] =
{
#	if EEI_ALLOW_ASSIGN
	//Variable assignment
	HANDLE(MAKE_NOFOLD_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'='), eei_precedence_assign), eei_rule_handle_assign),
#	endif

	//Sequence delimiter
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,','), eei_precedence_comma)),

	//Function call
	HANDLE(MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function), eei_rule_handle_function),

	//Normal operators
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_or), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_xor), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_and), eei_precedence_logical_and), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'<'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'>'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_eq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_neq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_gte), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,token_symbol_op_lte), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'+'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'-'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'|'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'*'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'&'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'/'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'%'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(MAKE_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'^'), eei_precedence_power3, eei_rule_right, 0), eei_rule_handle_infix),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_description eei_parser_postfix_rules[] =
{
	HANDLE(MAKE_POSTFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier), eei_precedence_postfix), eei_rule_handle_postfix),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_look_behind_table_item eei_parser_lookbehind_rules[] =
{
#	if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
	//Prefix-operator call
	{
		STATE(MAKE_LOOK_BEHIND_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('))),
		HANDLE(MAKE_DELAYED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_operator)), eei_rule_handle_prefix),
		HANDLE(MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function), eei_rule_handle_function)
	},
#	endif

	{MAKE_SENTINEL_RULE(),MAKE_SENTINEL_RULE(),STATE(MAKE_SENTINEL_RULE())}
};

static const eei_end_rules_table_item eei_parser_end_rules[] =
{
	//Grouping parens
	{
		HANDLE(MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'(')), eei_rule_handle_group),
		MAKE_END_RULE(MAKE_TOKEN(eei_token_delimiter,')'))
	},

	//Function call
	{
		HANDLE(MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function), eei_rule_handle_function),
		MAKE_END_RULE(MAKE_TOKEN(eei_token_delimiter,')'))
	},

	//Expression end
	{
		MAKE_DELIMITED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_sof)),
		MAKE_END_RULE(MAKE_SIMPLE_TOKEN(eei_token_eof))
	},

	{MAKE_SENTINEL_RULE(),MAKE_SENTINEL_END_RULE()}
};

//A group rule description for internal use
//static const eei_rule_description eei_parser_group_rule = STATE(MAKE_GROUP_RULE());

//A sentinel rule description to use as a return value when needed
//static const eei_rule_description eei_parser_sentinel_rule = STATE(MAKE_SENTINEL_RULE());

//Compare two tokens
static inline int eei_compare_tokens(eei_token a, eei_token b)
{
	return
			//Types must match
			(GET_TOKEN_TYPE(a) == GET_TOKEN_TYPE(b))
			&&
			(
				//Any one of the token symbols can be a wildcard
				(GET_TOKEN_SYMBOL(a) == token_symbol_any)
				||
				(GET_TOKEN_SYMBOL(b) == token_symbol_any)
				||
				//Or the symbols must match
				(GET_TOKEN_SYMBOL(a) == GET_TOKEN_SYMBOL(b))
			);
}

//Compare two rules
static inline int eei_compare_rules(eei_rule_description a, eei_rule_description b)
{
	return
			(CLEAR_RULE_TOKEN(a) == CLEAR_RULE_TOKEN(b))
			&& eei_compare_tokens(GET_RULE_TOKEN(a), GET_RULE_TOKEN(b));
}

eei_rule_description eei_find_rule(eei_token token, eei_rule_type expected, eei_rule_description previous)
{
	//Find the rule of the expected type matching the given token
	//Will return the sentinel rule on failure

	const eei_rule_description * table = NULL;

	switch (expected)
	{
		case eei_rule_prefix:
			table = eei_parser_prefix_rules;
			break;

		case eei_rule_infix:
			table = eei_parser_infix_rules;
			break;

		case eei_rule_postfix:
			table = eei_parser_postfix_rules;
			break;

		case eei_rule_end:
			//The end rules tables can not be searched using this function
			return MAKE_SENTINEL_RULE();
	}

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it
	for (; *table != MAKE_SENTINEL_RULE(); ++table)
	{
		const eei_rule_description tested = *table;

		if (!eei_compare_tokens(GET_RULE_TOKEN(tested), token))
			//The tokens don't match, no more tests needed
			continue;

		//Test if this rule has a look-behind condition.
		//It is possible to make use without this flag and always search
		//	the look-begind table, but it costs us no additional memory and avoids
		//	redundant searches for most rules.
		//The only caveat is that when identical rules are present, with and without a look-begind flag,
		//	the one with the flag MUST appear BEFORE the other one in the table.
		if (!GET_RULE_LOOK_BEHIND(tested))
			//This rule has no additional constraints - we found what we're looking for!
			break;

		//We need to compare the look-behind table with the previous token
		const eei_look_behind_table_item * lb_table = eei_parser_lookbehind_rules;

		//Use a simple linear search since the tables are small and the
		// overhead of a binary search is not worth it
		while (lb_table->current != MAKE_SENTINEL_RULE())
		{
			if (eei_compare_rules(tested, lb_table->current) && eei_compare_rules(previous, lb_table->previous))
				//We found a match in the look-behind table - return the new rule
				return lb_table->morphed;

			lb_table++;
		}
	}

	return *table;
}

int eei_find_end_rule(eei_token token, eei_rule_description rule)
{
	//Find the end rule with the given token for the requested rule
	//Will return the sentinel end rule on failure

	const eei_end_rules_table_item * table = eei_parser_end_rules;

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it
	while (table->rule != MAKE_SENTINEL_RULE())
	{
		if ((rule == table->rule) && (token == GET_RULE_TOKEN(table->end)))
			break;

		table++;
	}

	return table->rule != MAKE_SENTINEL_RULE();
}


//Parser stack
//------------

typedef struct
{
	ee_element_count start;
	ee_element_count end;
} eei_text_location;

struct eei_parser_node_
{
	//The rule being processed
	eei_rule_description rule;

	//The source token for the rule being processed
	eei_text_location text;

	//The token type expected next
	unsigned char next;

	//The precedence of the current node
	eei_precedence precedence;

	//_Execution_ stack index when this node was pushed.
	//Used to calculate arity of functions and validate groups are correct.
	ee_element_count stack_top;
};

//Holds management data for the parser stack that is used instead
//	of functional recursion to hold the parser data
typedef struct
{
	//TODO: Combine next & precedence into a rule and use the access macros
	//TODO: Breakout the stack_top into a separate vector of node_index/top, to save on alignment and space.
	//TODO: The maximum size of the vector is KNOWN - it is the count of starting delimiters in the expression!
	//Location of the stack
	eei_parser_node * stack;

	//Total allocated size of the stack
	int size;

	//Location of the current stack top
	//This always points to the element ABOVE the top
	int top;

	//The highest top seen
	int high;
} eei_parser_stack;

ee_parser_reply eei_stack_copynode(eei_parser_node * dst, const eei_parser_node * src)
{
	if (!dst || !src)
		return ee_parser_stack_error;

	dst->rule = src->rule;
	dst->text.start = src->text.start;
	dst->text.end = src->text.end;
	dst->next = src->next;
	dst->precedence = src->precedence;
	dst->stack_top = src->stack_top;

	return ee_parser_ok;
}

ee_parser_reply eei_stack_push(eei_parser_stack * stack, const eei_parser_node * node)
{
	//Push a node to the top of the stack

	if (stack->top >= stack->size)
		return ee_parser_stack_overflow;

	eei_stack_copynode(&stack->stack[stack->top], node);
	stack->top++;

	if (stack->high < stack->top)
		stack->high = stack->top;

	return ee_parser_ok;
}

ee_parser_reply eei_stack_pop(eei_parser_stack * stack, eei_parser_node * node)
{
	if (stack->top == 0)
		return ee_parser_stack_underflow;

	if (eei_stack_copynode(node, &stack->stack[stack->top - 1]) != ee_parser_ok)
		return ee_parser_stack_error;

	stack->top--;
	return ee_parser_ok;
}

eei_parser_node * eei_stack_top(eei_parser_stack * stack, int distance)
{
	//Get the element 'distance' from the stack top
	//Returns NULL on error

	if (distance >= stack->top)
		return (eei_parser_node*)0;

	return &stack->stack[stack->top - distance - 1];
}


//Virtual machine
//---------------

//Type used to hold a single VM bytecode
typedef unsigned char eei_vm_bytecode;

//The instruction is decoded using the following definitions.
enum
{
	//Shift value for the immediate bits
	eei_vm_immediate_shift = 0,

	//Bits allocated to the instrictions
	eei_vm_insturction_bits = 3,

	//Shift value for the instruction bits
	eei_vm_insturction_shift = 8 - eei_vm_insturction_bits,

	//Bits allocated to the immediate
	eei_vm_immediate_bits = eei_vm_insturction_shift,


	//Mask for the instruction
	eei_vm_mask_instruction = ((1U << eei_vm_insturction_bits) - 1) << eei_vm_insturction_shift,

	//Mask for the immediate values for each instruction
	eei_vm_mask_immediate_shifted = ((1U << eei_vm_immediate_bits) - 1),
	eei_vm_mask_immediate = eei_vm_mask_immediate_shifted << eei_vm_immediate_shift,



	//Add immediate to runtime accumulator
	eei_vm_insturction_immediate = (0x00) << eei_vm_insturction_shift,

	//Push a constant to the execution stack
	eei_vm_insturction_constant = (0x01) << eei_vm_insturction_shift,

	//Read and push a variable to the execution stack
	eei_vm_insturction_variable = (0x02) << eei_vm_insturction_shift,

	//Set the arity of the nearest function to execute
	eei_vm_insturction_arity = (0x03) << eei_vm_insturction_shift,

	//Execute a function with an arity of 1 or 2
	eei_vm_insturction_function2 = (0x04) << eei_vm_insturction_shift,

	//Execute a function
	eei_vm_insturction_function = (0x05) << eei_vm_insturction_shift,

	//Pop a variable from the stack and store it
	eei_vm_insturction_store = (0x06) << eei_vm_insturction_shift,
};

typedef struct
{
	ee_variable_type * constants;
	ee_variable_type ** variables;
	ee_function * functions;
	eei_vm_bytecode * instructions;
} eei_vmmake_data;

typedef struct
{
	ee_element_count constants;
	ee_element_count variables;
	ee_element_count functions;
	ee_element_count instructions;

	//This tracks the run-time stack usage
	ee_element_count stack;
} eei_vmmake_counters;

typedef struct
{
	//Pointers to the data tables
	eei_vmmake_data data;

	//Allocated/maximum size for each datum
	eei_vmmake_counters max;

	//Currently used size for each datum
	eei_vmmake_counters current;

} eei_vmmake_environment;

ee_parser_reply eei_vmmake_append_instruction(
		eei_vmmake_environment * vm,
		eei_vm_bytecode instruction,
		unsigned int immediate)
{
	//Insert an instruction with an immediate

	//Count number of immediate opcodes needed
	int total = -1;
	unsigned int data = immediate;
	do
	{
		total++;
		data >>= eei_vm_immediate_bits;
	} while (data);

	if (vm->current.instructions + total + 1 > vm->max.instructions)
		return ee_parser_instrictions_overflow;

	int i = total;
	while (i)
	{
		vm->data.instructions[vm->current.instructions++] =
				(((immediate >> (eei_vm_immediate_bits * i)) & eei_vm_mask_immediate_shifted)
				<< eei_vm_immediate_shift)
				| eei_vm_insturction_immediate;

		i--;
	}

	vm->data.instructions[vm->current.instructions++] =
			((immediate & eei_vm_mask_immediate_shifted)
			<< eei_vm_immediate_shift)
			| instruction;

	return ee_parser_ok;
}

ee_parser_reply eei_vmmake_load_constant(
		eei_vmmake_environment * vm,
		const ee_variable_type constant)
{
	//Add a constant load bytecode

	//This searches the current constants table for a match
	//We trade speed during parsing for lower memory usage of the final environment

	//Linear search through the constants table
	int index;
	for (index = 0; index < vm->current.constants; ++index)
		if (vm->data.constants[index] == constant)
			break;

	if (index == vm->current.constants)
	{
		//Nothing found, we need to add a new constant to the table

		if (vm->current.constants >= vm->max.constants)
			return ee_parser_constants_overflow;

		vm->data.constants[index] = constant;
		vm->current.constants++;
	}

	//Update stack usage
	vm->current.stack++;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	return eei_vmmake_append_instruction(vm, eei_vm_insturction_constant, index);
}

ee_parser_reply eei_vmmake_load_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable)
{
	//Add a variable load bytecode

	//Linear search through the variables table
	int index;
	for (index = 0; index < vm->current.variables; ++index)
		if (vm->data.variables[index] == variable)
			break;

	if (index == vm->current.variables)
	{
		//Nothing found, we need to add a new variable to the table

		if (vm->current.variables >= vm->max.variables)
			return ee_parser_variables_overflow;

		vm->data.variables[index] = variable;
		vm->current.variables++;
	}

	//Update stack usage
	vm->current.stack++;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	return eei_vmmake_append_instruction(vm, eei_vm_insturction_variable, index);
}

ee_parser_reply eei_vmmake_execute_functions(
		eei_vmmake_environment * vm,
		ee_function function,
		int arity)
{
	//Add a function execute bytecode

	//Linear search through the functions table
	int index;
	for (index = 0; index < vm->current.functions; ++index)
		if (vm->data.functions[index] == function)
			break;

	if (index == vm->current.functions)
	{
		//Nothing found, we need to add a new function to the table

		if (vm->current.functions >= vm->max.functions)
			return ee_parser_functions_overflow;

		vm->data.functions[index] = function;
		vm->current.functions++;
	}

	//Make sure the stack will hold enough elements
	if (vm->current.stack < arity)
		return ee_parser_stack_runtime_underflow;

	//Update stack usage: "arity" elements will be popped and a single result pushed
	vm->current.stack += 1 - arity;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	if (
		((arity == 1) || (arity == 2))
		&& (index <= (((1 << (eei_vm_insturction_bits-1)) - 1))) )
	{
		//Use the special command
		return eei_vmmake_append_instruction(
					vm,
					eei_vm_insturction_function2,
					index | ( (arity == 1) ? 0 : (1 << (eei_vm_immediate_bits - 1)) ));
	}

	if (arity != 0)
	{
		const ee_parser_reply reply =
				eei_vmmake_append_instruction(vm, eei_vm_insturction_arity, arity);

		if (reply != ee_parser_ok)
			return reply;
	}

	return eei_vmmake_append_instruction(vm, eei_vm_insturction_function, index);
}

ee_parser_reply eei_vmmake_store_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable)
{
	//Add a variable store bytecode

	//Make sure there is something on the stack
	if (vm->current.stack == 0)
		return ee_parser_expression_empty_assign;

	//Linear search through the variables table
	int index;
	for (index = 0; index < vm->current.variables; ++index)
		if (vm->data.variables[index] == variable)
			break;

	if (index == vm->current.variables)
	{
		//Nothing found, we need to add a new variable to the table

		if (vm->current.variables >= vm->max.variables)
			return ee_parser_variables_overflow;

		vm->data.variables[index] = variable;
		vm->current.variables++;
	}

	//Update stack usage
	vm->current.stack--;

	return eei_vmmake_append_instruction(vm, eei_vm_insturction_store, index);
}




//Parser
//------

typedef struct
{
	eei_token token;
	eei_text_location text;
} eei_parser_token;

struct eei_parser_
{
	eei_parser_stack stack;
	eei_vmmake_environment vm;
	eei_symboltable symboltable;

	const ee_char_type * expression;
	ee_element_count expression_size;

	eei_parser_token error_token;
	ee_parser_reply status;

	//Stack index of the current group
	//This points to the actual rule and not the synthetic group rule
	int currentGroup;
};


//Parser utility functions
//------------------------

ee_parser_reply eei_parse_set_error(
		eei_parser * parser,
		ee_parser_reply error,
		const eei_parser_token * token)
{
	parser->status = error;

	parser->error_token.token = token->token;
	parser->error_token.text.start = token->text.start;
	parser->error_token.text.end = token->text.end;

	return error;
}

ee_parser_reply eei_parse_error(
		eei_parser * parser,
		ee_parser_reply reply,
		const eei_parser_token * token)
{
	if (reply == ee_parser_ok)
	{
		if (token && (parser->error_token.token != MAKE_SIMPLE_TOKEN(eei_token_sof)))
		{
			//Save the current progress, but do not override existng data
			parser->error_token.token = token->token;
			parser->error_token.text.start = token->text.start;
			parser->error_token.text.end = token->text.end;
		}

		return ee_parser_ok;
	}

	//Do not change an existing error status
	if (parser->status != ee_parser_ok)
		return reply;

	parser->status = reply;

	if (token)
	{
		//Save the error position
		parser->error_token.token = token->token;
		parser->error_token.text.start = token->text.start;
		parser->error_token.text.end = token->text.end;
	}

	return reply;
}

ee_parser_reply eei_parse_error_notoken(
		eei_parser * parser,
		ee_parser_reply reply)
{
	if (reply == ee_parser_ok)
		return ee_parser_ok;

	//Do not change an existing error status
	if (parser->status == ee_parser_ok)
		parser->status = reply;

	return reply;
}

static inline ee_parser_reply eei_parse_push(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_rule_type next,
		const eei_precedence precedence,
		const eei_parser_token * token
		)
{
	eei_parser_node node;

	node.text.start = token->text.start;
	node.text.end = token->text.end;
	node.rule = rule;
	node.next = next;
	node.precedence = precedence;
	node.stack_top = parser->vm.current.stack;

	return eei_parse_error(parser, eei_stack_push(&parser->stack, &node), token);
}

static inline ee_parser_reply eei_parse_pop(
		eei_parser * parser,
		eei_parser_node * node)
{
	return eei_parse_error_notoken(parser, eei_stack_pop(&parser->stack, node));
}

static inline ee_parser_reply eei_parse_popT(
		eei_parser * parser,
		eei_parser_node * node,
		const eei_parser_token * token)
{
	return eei_parse_error(parser, eei_stack_pop(&parser->stack, node), token);
}

static inline ee_parser_reply eei_parse_pushGroupRule(
		eei_parser * parser,
		const eei_parser_token * token)
{
	eei_parser_node node;

	node.text.start = token->text.start;
	node.text.end = token->text.end;
	node.rule = MAKE_GROUP_RULE();
	node.next = eei_rule_prefix;
	node.precedence = 0;
	node.stack_top = parser->vm.current.stack;

	//Update the current group to the point the current stack top
	//	since that must be the rule that created this group
	parser->currentGroup = parser->stack.top - 1;

	return eei_parse_error(parser, eei_stack_push(&parser->stack, &node), token);
}

//Parser symbol table helper functions
//------------------------------------

static inline ee_variable_type * eei_parse_symbols_get_variable_from_index(
		const eei_parser * parser,
		const eei_symboltable_index * index)
{
	if (index->third >= 0)
		return parser->symboltable.third.locations[index->third].variable;
	else
		return NULL;
}

static inline ee_function eei_parse_symbols_get_function_from_index(
		const eei_parser * parser,
		const eei_symboltable_index * index)
{
	if (index->third >= 0)
		return parser->symboltable.third.locations[index->third].function;
	else
		return NULL;
}

ee_variable_type * eei_parse_symbols_get_variable(
		eei_parser * parser,
		const eei_parser_node * node)
{
	//Returns the variable pointed to by the node, or NULL on error.

	const ee_char_type * token_start = &parser->expression[node->text.start];
	const ee_element_count token_length = node->text.end - node->text.start;

	eei_symboltable_index index;

	eei_symboltable_find_text(
				&parser->symboltable,
				token_start,
				token_length,
				&index);

	eei_symboltable_get(&parser->symboltable, &index,0,0,0,~ee_function_flag_invalid);
	return eei_parse_symbols_get_variable_from_index(parser, &index);
}

ee_function eei_parse_symbols_get_function(
		eei_parser * parser,
		const eei_parser_node * node,
		ee_arity arity,
		ee_function_flags any_flags,
		ee_function_flags all_flags,
		ee_function_flags not_flags,
		int * wrong_arity)
{
	//Returns the function pointed to by the node, or NULL on error.

	const ee_char_type * token_start = &parser->expression[node->text.start];
	const ee_element_count token_length = node->text.end - node->text.start;

	eei_symboltable_index index;

	eei_symboltable_find_text(
				&parser->symboltable,
				token_start,
				token_length,
				&index);

	const ee_symboltable_reply reply =
			eei_symboltable_get(
				&parser->symboltable,
				&index,
				arity,
				any_flags,
				all_flags,
				not_flags);

	if (reply == ee_symboltable_ok)
	{
		if (wrong_arity)
			*wrong_arity = 0;

		return eei_parse_symbols_get_function_from_index(parser, &index);
	}

	if (wrong_arity)
		*wrong_arity = reply == ee_symboltable_filtered;

	return NULL;
}

//Parser core functions
//---------------------

//Parser rule handler forward declarations
//----------------------------------------
ee_parser_reply eei_rule_handler_constant(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_variable(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_group(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_prefix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_infix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_postfix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_function(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_assign(eei_parser * parser, const eei_parser_node * node);

ee_parser_reply eei_parse_done_node(eei_parser * parser, const eei_parser_node * node)
{
	static const eei_rule_handler handlers[eei_rule_handle_sentinel] =
	{
		NULL,
		eei_rule_handler_constant,
		eei_rule_handler_variable,
		eei_rule_handler_group,
		eei_rule_handler_prefix,
		eei_rule_handler_infix,
		eei_rule_handler_postfix,
		eei_rule_handler_function,
		eei_rule_handler_assign
	};

	//Test for a handler function
	const eei_rule_handler handler = handlers[GET_RULE_HANDLER(node->rule)];

	if (handler)
	{
		const ee_parser_reply reply = handler(parser, node);
		if (reply != ee_parser_ok)
		{
			eei_parser_token token;
			token.token = GET_RULE_TOKEN(node->rule);
			token.text.start = node->text.start;
			token.text.end = node->text.end;
			return eei_parse_error(parser, reply, &token);
		}
	}

	return ee_parser_ok;
}

ee_parser_reply eei_parse_done(eei_parser * parser)
{
	//Process the top node

	eei_parser_node node;
	const ee_parser_reply reply = eei_parse_pop(parser, &node);

	if (reply != ee_parser_ok)
		return reply;

	return eei_parse_done_node(parser, &node);
}

static inline void eei_parse_parsePrefix(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_parser_token * token)
{
	//Push the rule itself
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule),
				GET_RULE_PRECEDENCE(eei_stack_top(&parser->stack, 0)->rule),
				token);

	if (GET_RULE_ENDDELIMITER(rule))
		//Push a special group to reset the precedence inside the delimited group
		//	without affecting the precedence processing of the tokens that will
		//	follow the group.
		eei_parse_pushGroupRule(parser, token);
}

static inline void eei_parse_parseInfix(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_parser_token * token)
{
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule),
				(GET_RULE_ACCOSIATIVITY(rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule)
				: GET_RULE_PRECEDENCE(rule) - 1,
				token);

	if (GET_RULE_ENDDELIMITER(rule))
		//Push a special group to reset the precedence inside the delimited group
		//	without affecting the precedence processing of the tokens that will
		//	follow the group.
		eei_parse_pushGroupRule(parser, token);
}

static inline void eei_parse_parsePostfix(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_parser_token * token)
{
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule),
				(GET_RULE_ACCOSIATIVITY(rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule)
				: GET_RULE_PRECEDENCE(rule) - 1,
				token);
}

static inline void eei_parse_foldPrefix(eei_parser * parser, int delay)
{
	//Fold immediately preceeding prefix nodes.
	//This will stop on any non-prefix node, any special node and,
	//	also, on any node that expects an end token
	while (parser->stack.top)
	{
		const eei_rule_description rule =
				parser->stack.stack[parser->stack.top - 1].rule;

		if (GET_RULE_TOKEN_TYPE(rule) < eei_token_internals)
			break;

		if (GET_RULE_ENDDELIMITER(rule))
			break;

		if (GET_RULE_TYPE(rule) != eei_rule_prefix)
			break;

		if (delay && (GET_RULE_FOLD(rule) == eei_rule_delay_fold))
			break;

		eei_parse_done(parser);
	}
}

static inline void eei_parse_foldPrecedence(eei_parser * parser, const eei_rule_description rule)
{
	const eei_precedence precedence = GET_RULE_PRECEDENCE(rule);

	//The current infix/postfix token is of lower precedence than
	//	the currently top node.
	//We need to take as a child the node tree that has a precedence
	//	higher then the current token.

	//Fold the nodes until a fold would cause the top node to
	//	become of a precedence lower than us.
	//This stop codition ensures that we end this loop
	//	with the top node at a precedence higher, and the node above
	//	it with a lower one, than us.
	while (
		   (parser->stack.top > 1)
		   && (eei_stack_top(&parser->stack, 0)->precedence >= precedence))
		eei_parse_done(parser);
}

void eei_parse_foldEndDilimiter(
		eei_parser * parser,
		const eei_parser_token * token)
{
	//Sanity check
	if (parser->stack.top <= parser->currentGroup)
	{
		eei_parse_error(parser, ee_parser_stack_underflow, token);
		return;
	}

	//Additional sanity check to make sure the stack was built correctly
	if (!GET_RULE_ENDDELIMITER(parser->stack.stack[parser->currentGroup].rule))
	{
		eei_parse_set_error(parser, ee_parser_error, token);
		return;
	}

	//Test the end-delimited rule that created this group
	const int matched = eei_find_end_rule(
							token->token,
							parser->stack.stack[parser->currentGroup].rule);

	if (!matched)
	{
		//The tested node is an end delimited rule but
		//	the current token is not that delimiter!
		//Use the END token as the error position.
		eei_parse_set_error(parser, ee_parser_expression_unmatched_end, token);
		return;
	}

	//We are now sure we have a correctly delimited group

	int group = parser->currentGroup;

	//Fold up to, but not including, this rule
	//This will fold the synthetic group rule as well
	++group;
	while (parser->stack.top > group)
	{
		eei_parse_done(parser);
		if (parser->status != ee_parser_ok)
			return;
	}

	//Before folding the rule itself we need to update the current group
	//	to the previous one, since the rule that created the current group is,
	//	in itself, an element of the previous one, and must be folded as part of that.

	//Account for the stack top pointing one element ABOVE the actual top.
	group = parser->stack.top - 1;

	//Special handling for the SOF token to avoid many check in the following code.
	if (!group)
	{
		//This code is reached when a EOF token was encountered that correctly matched the
		// SOF delimited rule that exists at the very bottom of the stack.
		//The current group is now a lie but we can stil fold this rule, thus completely clearing the stack.
		parser->currentGroup = 0;
		eei_parse_done(parser);
		return;
	}

	//Walk the stack back and find the previous group.
	//Pre-decrementing skips the current top, that we know is not a group rule!
	while (--group > 0)
		if (parser->stack.stack[group].rule == MAKE_GROUP_RULE())
			break;

	//Sanity check
	if (group <= 0)
	{
		eei_parse_set_error(parser, ee_parser_error, token);
		return;
	}

	//We need the rule that created the previous group, not the synthetic group itself.
	parser->currentGroup = group - 1;

	//At this point the current group is the one the top rule was created in so we can
	//	fold the rule that created the just-folded group.
	eei_parse_done(parser);
	if (parser->status != ee_parser_ok)
		return;

	//Sanity check
	if (!parser->stack.top)
	{
		eei_parse_error(parser, ee_parser_stack_underflow, token);
		return;
	}

	//The token is an end delimiter - that was just completely processed.
	//This means the folded node can be treated as a postfix -
	//	so an infix must follow
	eei_stack_top(&parser->stack,0)->next = eei_rule_infix;
}


void eei_parse_token(eei_parser * parser, eei_parser_token * token)
{
	eei_rule_type expected = eei_stack_top(&parser->stack, 0)->next;
	const eei_rule_description previous = eei_stack_top(&parser->stack, 0)->rule;

	eei_rule_description rule = eei_find_rule(token->token, expected, previous);
	int foundExpected = IS_RULE_VALID(rule);

	if (!foundExpected && (expected != eei_rule_postfix))
	{
		//A postfix can appear when not expected - so look for it
		rule = eei_find_rule(token->token, eei_rule_postfix, previous);
		foundExpected = IS_RULE_VALID(rule);
	}

	//Update the expected rule, since it might have been modified
	expected = GET_RULE_TYPE(rule);

	if (foundExpected)
	{
		switch (expected)
		{
			case eei_rule_prefix:
				eei_parse_parsePrefix(parser, rule, token);
				break;

			case eei_rule_infix:
				//Fold all prefixes before going any further
				//	since they bind stonger than the infixes.
				if (GET_RULE_FOLD(rule) != eei_rule_no_fold)
					eei_parse_foldPrefix(parser, GET_RULE_ENDDELIMITER(rule));

				eei_parse_foldPrecedence(parser, rule);
				eei_parse_parseInfix(parser, rule, token);
				break;

			case eei_rule_postfix:
				if (GET_RULE_FOLD(rule) != eei_rule_no_fold)
					eei_parse_foldPrefix(parser, 0);
				eei_parse_foldPrecedence(parser, rule);
				eei_parse_parsePostfix(parser, rule, token);
				break;

			default:
				eei_parse_error(parser, ee_parser_error, token);
		}
	}
	else
	{
		//The current token is not any expected token - it must be some END delimiter
		eei_parse_foldEndDilimiter(parser, token);
	}
}

void eei_parse_init(eei_parser * parser)
{
	eei_parser_node node;
	eei_parser_token token;

	parser->status = ee_parser_ok;
	parser->error_token.token = MAKE_SIMPLE_TOKEN(eei_token_sof);
	parser->error_token.text.start = 0;
	parser->error_token.text.end = parser->expression_size;

	node.text.start = 0;
	node.text.end = parser->expression_size;
	node.precedence = 0;
	node.next = eei_rule_prefix;
	node.stack_top = 0;

	//Push a "start" rule - this will simplify things during processing
	node.rule = eei_find_rule(MAKE_SIMPLE_TOKEN(eei_token_sof), eei_rule_prefix, MAKE_SENTINEL_RULE());
	eei_stack_push(&parser->stack, &node);

	//Start a group - this allows to remove many tests for an empty stack
	token.token = MAKE_SIMPLE_TOKEN(eei_token_group);
	token.text.start = 0;
	token.text.end = parser->expression_size;
	eei_parse_pushGroupRule(parser, &token);
}

void eei_parse_expression(eei_parser * parser)
{
	eei_lexer_state lexer_state;
	lexer_state.head = parser->expression;

	eei_parse_init(parser);

	while ((parser->status == ee_parser_ok) && (parser->stack.top))
	{
		eei_parser_token token;
		token.token = eei_lexer_next_token(&lexer_state);

		//Convert from lexer to parser representation
		token.text.start = lexer_state.start - parser->expression;
		token.text.end = lexer_state.head - parser->expression;

		eei_parse_token(parser, &token);
	}

	if (parser->stack.top && (parser->status == ee_parser_ok))
		parser->status = ee_parser_error;
}


//Parser handler functions
//------------------------

ee_parser_reply eei_rule_handler_constant(eei_parser * parser, const eei_parser_node * node)
{
	ee_variable_type constant;

	if (EEI_CONSTANT_PARSER(
			&parser->expression[node->text.start],
			&parser->expression[node->text.end],
			&constant) != 0)
		return ee_parser_expression_not_a_constant;

	return eei_vmmake_load_constant(&parser->vm, constant);
}

ee_parser_reply eei_rule_handler_variable(eei_parser * parser, const eei_parser_node * node)
{
	const ee_char_type * token_start = &parser->expression[node->text.start];
	const ee_element_count token_length = node->text.end - node->text.start;

	eei_symboltable_index index;

	eei_symboltable_find_text(
				&parser->symboltable,
				token_start,
				token_length,
				&index);

	//Look for the variable
	eei_symboltable_get(
				&parser->symboltable,
				&index,
				0,
				0,
				0,
				~ee_function_flag_invalid);
	ee_variable_type * var = eei_parse_symbols_get_variable_from_index(parser, &index);

	//Look for a zero-arity function with the same name
	eei_symboltable_get(
				&parser->symboltable,
				&index,
				0,
				0,
				ee_function_flag_prefix,
				0);
	ee_function op = eei_parse_symbols_get_function_from_index(parser, &index);

	if (var && op)
		//Do not allow both to be defined to avoid confusion
		return ee_parser_varfunction_duplicate;

	if (!var && !op)
		//None found - assume this should have been a variable
		return ee_parser_unknown_variable;

	if (var)
		return eei_vmmake_load_variable(&parser->vm, var);
	else
		return eei_vmmake_execute_functions(&parser->vm, op, 0);
}

ee_parser_reply eei_rule_handler_group(eei_parser * parser, const eei_parser_node * node)
{
	//A group must end with exactly ONE new value on the runtime stack
	const int items = parser->vm.current.stack - node->stack_top;

	if (items == 1)
		return ee_parser_ok;

	if (items == 0)
		return ee_parser_expression_empty_group;

	return ee_parser_expression_overfull_group;
}

static inline ee_parser_reply eei_rule_handler_operator(
		eei_parser * parser,
		const eei_parser_node * node,
		ee_function_flag flag,
		ee_arity arity,
		ee_parser_reply error
		)
{
	const ee_function op =
			eei_parse_symbols_get_function(
				parser,
				node,
				arity,
				((GET_RULE_TOKEN_TYPE(node->rule) == eei_token_operator) ? ee_function_flag_operator : 0),
				flag,
				0,
				NULL);

	if (!op)
		return error;

	return eei_vmmake_execute_functions(&parser->vm, op, arity);
}

ee_parser_reply eei_rule_handler_prefix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_prefix, 1, ee_parser_prefix_not_implemented);
}

ee_parser_reply eei_rule_handler_infix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_infix, 2, ee_parser_infix_not_implemented);
}

ee_parser_reply eei_rule_handler_postfix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_postfix, 1, ee_parser_postfix_not_implemented);
}

ee_parser_reply eei_rule_handler_function(eei_parser * parser, const eei_parser_node * node)
{
	//Set the error token to the folded node, since we don't have the correct identifier yet
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};

	//An identifier must be on the stack at this point
	eei_parser_node identifier;

	//Pop it since we're going to use it as the function name
	ee_parser_reply reply = eei_parse_popT(parser, &identifier, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Now that the identifier is available change the possible error to that
	error.token = GET_RULE_TOKEN(identifier.rule);
	error.text = identifier.text;

	//Make sure this is acutally an indentifier, or an operator if allowed, as theese are the only things curently supported
	if (
		(GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_identifier)
#		if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
		&& (GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_operator)
#		endif
		)
		return eei_parse_set_error(
					parser,
					ee_parser_expression_identifier_expected,
					&error);

	//The arity is just the amount of items on the run-time stack added since the identifier itself was parsed
	const ee_arity arity = (ee_arity)(parser->vm.current.stack - identifier.stack_top);

	int wrong_arity = 0;
	ee_function op =
			eei_parse_symbols_get_function(
				parser,
				&identifier,
				arity,
				ee_function_flag_infix,
				((GET_RULE_TOKEN_TYPE(identifier.rule) == eei_token_operator) ? ee_function_flag_operator : 0),
				0,
				&wrong_arity);

	//In case of an error make sure we report exactly what happened
	if (!op)
		return eei_parse_set_error(
					parser,
					wrong_arity
					? ee_parser_function_wrong_arity
					: ee_parser_function_not_implemented,
					&error);

	return eei_vmmake_execute_functions(&parser->vm, op, arity);
}

ee_parser_reply eei_rule_handler_assign(eei_parser * parser, const eei_parser_node * node)
{
	//Set the error token to the folded node, since we don't have the correct identifier yet
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};

	//An identifier must be on the stack at this point
	eei_parser_node identifier;

	//Pop it since we're going to use it as the LHS identifier
	ee_parser_reply reply = eei_parse_popT(parser, &identifier, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Now that the identifier is available change the possible error to that
	error.token = GET_RULE_TOKEN(identifier.rule);
	error.text = identifier.text;

	//Make sure this is acutally an indentifier as this is the only thing curently supported
	if (GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_identifier)
		return eei_parse_set_error(
					parser,
					ee_parser_expression_identifier_expected,
					&error);

	//Get the actual variable
	ee_variable_type * var = eei_parse_symbols_get_variable(parser, &identifier);

	//In case of an error make sure we report what happened
	if (!var)
		return eei_parse_set_error(
					parser,
					ee_parser_unknown_variable,
					&error);

	return eei_vmmake_store_variable(&parser->vm, var);
}

//Virtual machine execute
//-----------------------

//Holds the VM environment data
typedef struct
{
	//The constants table
	const ee_variable_type * constants;

	//The (pointers-to) variables table
	ee_variable_type * const * variables;

	//The functions table
	const ee_function * functions;

	//Start of the VM bytecode stream
	const eei_vm_bytecode * instructions;

	//Pointer to the runtime stack
	ee_variable_type * stack;

	//Count of instructions to execute
	int instruction_count;
} eei_vm_environment;

//Holds VM runtime data
typedef struct
{
	//Command parameter accumulator
	unsigned int accumulator;

	//Function arity accumulator
	ee_element_count arity;

	//Pointer to the one-past stack top
	//When pushing this address is the new write address
	ee_variable_type * stack_top;

	//The VM bytecode to execute next
	const eei_vm_bytecode * instruction;

	//One-past the last instruction to execute
	const eei_vm_bytecode * instruction_end;
} eei_vm_runtime;

//Exeucute the VM environment
ee_evaluator_reply eei_vm_execute(const eei_vm_environment * vm_environment)
{
	eei_vm_runtime rt =
	{
		0,
		0,
		vm_environment->stack,
		vm_environment->instructions,
		vm_environment->instructions + vm_environment->instruction_count
	};

	//We assume the environment was correctly constructed
	//	and all needed data is allocated and valid, thus
	//	no overflow, underflow or null pointer tests are being made.

	while (rt.instruction != rt.instruction_end)
	{
		//Update the accumulator from the immediate bits
		rt.accumulator <<= eei_vm_immediate_bits;
		rt.accumulator |=
				((*rt.instruction) >> eei_vm_immediate_shift)
				& eei_vm_mask_immediate_shifted;

		//Decode and advance the current instruction
		switch ((*rt.instruction++) & eei_vm_mask_instruction)
		{
			case eei_vm_insturction_immediate:
				//Do nothing
				//This allows using this instruction to accumulate any value
				break;

			case eei_vm_insturction_constant:
				//The constant table holds the values directly so just
				//	copy the one at the requested index.
				*rt.stack_top++ =
						vm_environment->constants[rt.accumulator];

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;

			case eei_vm_insturction_variable:
				//Use a user-modifiable load.
				//At this point the destination holds garbage and can be safely ignored.
				//The variables table holds pointers to the variables
				//	so the requested index needs to be dereferenced to access
				//	the actual user variable.
				EEI_VARIABLE_LOAD(rt.stack_top++, vm_environment->variables[rt.accumulator]);

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;

			case eei_vm_insturction_arity:
				//Set the arity for the next function to execute
				rt.arity = rt.accumulator;

				//Test if we can go directly to the "function" case since
				//	this is the expected sequence when the function table is small.
				//The instruction was already advanced in the switch header
				if (((*rt.instruction) & eei_vm_mask_instruction) != eei_vm_insturction_function)
				{
					//Clear the accumulator in preparation for the next instruction
					rt.accumulator = 0;
					break;
				}

				//Set the accumulator from the immediate bits and advance the instruction
				rt.accumulator =
						((*rt.instruction++) >> eei_vm_immediate_shift)
						& eei_vm_mask_immediate_shifted;

				//fall through

			case eei_vm_insturction_function2:
				if (rt.arity == 0)
				{
					//Only execute this is we did not come directly from the case above

					//Set the arity according to the high bit of the accumulator
					rt.arity =
							(rt.accumulator & (1 << (eei_vm_immediate_bits - 1)))
							? 2
							: 1;

					//Clear that bit to use the rest as the index
					rt.accumulator &= ~(1 << (eei_vm_immediate_bits - 1));
				}

				//fall through

			case eei_vm_insturction_function:
			{
				int error;
				ee_variable_type function_result;

				//Adjust the stack by the arity so the function would receive
				//	the correct actuals
				rt.stack_top -= rt.arity;

				if (rt.stack_top < vm_environment->stack)
					return ee_evaluator_stack_underflow;

				//Call the function
				error =
						vm_environment->functions[rt.accumulator]
						(
							rt.arity,
							rt.stack_top,
							&function_result);

				//Push the result to the stack top
				*rt.stack_top++ = function_result;

				//Clear the arity for the next function to execute
				rt.arity = 0;

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;

				//Any function can return a non-zero error code.
				//We do not analyze its meaning but simply halt and return
				//	it as it is.
				if (error != 0)
					return error;

				break;
			}

			case eei_vm_insturction_store:
				//Use a user-modifiable store.
				//After the store the source is popped of the stack and no longer used.
				//The variables table holds pointers to the variables
				//	so the requested index needs to be dereferenced to access
				//	the actual user variable.
				EEI_VARIABLE_STORE(vm_environment->variables[rt.accumulator], --rt.stack_top);

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;
		}
	};

	//The stack top holds the result of the evaluation
	const int stack = rt.stack_top - vm_environment->stack;

	if (stack == 0)
		return ee_evaluator_empty;

	if (stack > 1)
		return ee_evaluator_stack_extra;

	return ee_evaluator_ok;
}


//External API semi-opaque structures
//-----------------------------------

//Internal direct data held inside a compilation data structure
typedef struct
{
	ee_compilation_header header;

	char data[1];
} eei_compilation_struct;

typedef struct
{
	ee_memory_size constants;
	ee_memory_size variables;
	ee_memory_size functions;
	ee_memory_size instructions;
	ee_memory_size stack;
} eei_environment_offsets;


//Internal direct data held inside an execution environment
typedef struct
{
	ee_environment_header header;

	//Byte offsets from "data" for the various tables
	eei_environment_offsets offsets;

	//Count of instructions
	ee_element_count instruction_count;

	char data[1];
} eei_environment_struct;


//External API utility
//--------------------

//Helper macro to calculate alignment of a type
#define alignof(type) ((ptrdiff_t)&((struct { char c; type d; } *)0)->d)

void eei_guestimate_calculate_sizes(ee_data_size * size)
{
	size->compilation_size = (ee_memory_size)(
			sizeof(eei_compilation_struct)
			+ alignof(eei_parser_node) + sizeof(eei_parser_node) * size->compilation_stack);

	size->environment_size = (ee_memory_size)(
			sizeof(eei_environment_struct)
			+ alignof(ee_variable_type) + sizeof(ee_variable_type) * size->constants
			+ alignof(ee_variable_type) + sizeof(ee_variable_type) * size->variables
			+ alignof(ee_function) + sizeof(ee_function) * size->functions
			+ alignof(eei_vm_bytecode) + sizeof(eei_vm_bytecode) * size->instructions);

	size->stack_size = (ee_memory_size)(
			sizeof(ee_variable_type) * size->runtime_stack);

	size->full_environment_size = (ee_memory_size)(
			size->environment_size
			+ alignof(ee_variable_type) + size->stack_size);
}

//Environment API utility
//-----------------------

char * eei_environment_calculate_offsets(
		eei_environment_offsets * offsets,
		char * base,
		const ee_data_size * size)
{
	//Calculate the memory locations of all tables

	char * ptr = base;

	//Constants
	if (alignof(ee_variable_type))
		while ((ptrdiff_t)ptr % alignof(ee_variable_type))
			ptr++;

	offsets->constants = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_variable_type) * size->constants;

	//Variables
	while ((ptrdiff_t)ptr % alignof(const ee_variable_type*))
		ptr++;

	offsets->variables = (ee_memory_size)(ptr - base);
	ptr += sizeof(const ee_variable_type*) * size->variables;

	//Functions
	while ((ptrdiff_t)ptr % alignof(ee_function))
		ptr++;

	offsets->functions = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_function) * size->functions;

	//Instructions
	while ((ptrdiff_t)ptr % alignof(eei_vm_bytecode))
		ptr++;

	offsets->instructions = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_vm_bytecode) * size->instructions;

	//Runtime stack
	while ((ptrdiff_t)ptr % alignof(ee_variable_type))
		ptr++;

	offsets->stack = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_variable_type) * size->runtime_stack;

	return ptr;
}

char * eei_environment_calculate_pointers(
		eei_vmmake_data * pointers,
		eei_environment_offsets * offsets,
		char * base)
{
	pointers->constants = (ee_variable_type*)(base + offsets->constants);
	pointers->variables = (ee_variable_type**)(base + offsets->variables);
	pointers->functions = (ee_function*)(base + offsets->functions);
	pointers->instructions = (eei_vm_bytecode*)(base + offsets->instructions);

	return 	base + offsets->stack;
}

void eei_environment_compact(
		eei_environment_struct * environment,
		ee_data_size * size,
		const eei_parser * parser)
{
	//Calculate the new offset and pointers
	//The offsets are immediately saved to the environment since we don't need them anymore
	eei_vmmake_data pointers;
	eei_environment_calculate_offsets(&environment->offsets, &environment->data[0], size);
	eei_environment_calculate_pointers(&pointers, &environment->offsets, &environment->data[0]);

	//Copy over the data

	if (pointers.constants != parser->vm.data.constants)
		for (int i = 0; i < size->constants; ++i)
			pointers.constants[i] = parser->vm.data.constants[i];

	if (pointers.variables != parser->vm.data.variables)
		for (int i = 0; i < size->variables; ++i)
			pointers.variables[i] = parser->vm.data.variables[i];

	if (pointers.functions != parser->vm.data.functions)
		for (int i = 0; i < size->functions; ++i)
			pointers.functions[i] = parser->vm.data.functions[i];

	if (pointers.instructions != parser->vm.data.instructions)
		for (int i = 0; i < size->instructions; ++i)
			pointers.instructions[i] = parser->vm.data.instructions[i];
}

//External API
//------------


ee_parser_reply ee_guestimate(const ee_char_type * expression, ee_data_size * size)
{
	int identifiers = 0;
	int numbers = 0;
	int actuals = 0;
	int groups = 0;
	int operators = 0;

	eei_lexer_state state;
	eei_token_type token_type = eei_token_eof;

	state.head = expression;
	do
	{
		token_type = GET_TOKEN_TYPE(eei_lexer_next_token(&state));

		switch (token_type)
		{
			case eei_token_identifier:
				identifiers++;
				break;

			case eei_token_constant:
				numbers++;
				break;

			case eei_token_delimiter:
				switch (*state.start)
				{
					case ',':
						actuals++;
						break;

					case '(':
						groups++;
						break;
				}
				break;

			case eei_token_operator:
				operators++;
				break;

			default:
				break;

		}

	} while ((token_type != eei_token_eof) && (token_type != eei_token_error));

	size->expression = state.head - expression;


	//Guestimate the number of elements

	size->constants = numbers;
	size->variables = identifiers;
	size->functions = identifiers + operators;
	size->instructions = numbers + identifiers + operators;
	size->instructions *= size->instructions;
	size->compilation_stack = 2 + numbers + identifiers + operators * 2 + groups * 2;
	size->runtime_stack = numbers + actuals + groups * 2 + operators + identifiers;

	eei_guestimate_calculate_sizes(size);

	return (token_type == eei_token_error) ? ee_parser_error : ee_parser_ok;
}

ee_parser_reply ee_compile(
		const ee_char_type * expression,
		ee_data_size * size,
		const ee_symboltable_header * symboltable,
		ee_compilation_header * compilation,
		ee_environment environment)
{
	eei_parser parser;

	//Setup the parser memory

	eei_environment_struct * full_env = (eei_environment_struct *)environment;
	eei_compilation_struct * full_compilation = (eei_compilation_struct *)compilation;
	eei_symboltable_struct * full_symboltable = (eei_symboltable_struct *)symboltable;

	char * ptr = &full_compilation->data[0];
	if (alignof(eei_parser_node))
		while ((ptrdiff_t)ptr % alignof(eei_parser_node))
			ptr++;

	parser.stack.stack = (eei_parser_node*)ptr;
	parser.stack.size = size->compilation_stack;
	parser.stack.top = 0;
	parser.stack.high = 0;

	parser.vm.current.constants = 0;
	parser.vm.current.variables = 0;
	parser.vm.current.functions = 0;
	parser.vm.current.instructions = 0;
	parser.vm.current.stack = 0;

	parser.vm.max.constants = size->constants;
	parser.vm.max.variables = size->variables;
	parser.vm.max.functions = size->functions;
	parser.vm.max.instructions = size->instructions;
	//Reset the stack info since the parser will count the actual stack usage
	parser.vm.max.stack = 0;

	//Calculate the VM tables memory locations
	eei_environment_calculate_offsets(&full_env->offsets,(char *)&full_env->data[0],size);
	full_env->instruction_count = parser.vm.max.instructions;

	//Setup the VM tables memory
	eei_environment_calculate_pointers(
			&parser.vm.data,
			&full_env->offsets,
			(char *)&full_env->data[0]);

	parser.expression = expression;
	parser.expression_size = size->expression;

	eei_symboltable_calculate_pointers(&parser.symboltable, full_symboltable);
	eei_parse_expression(&parser);

	//Fill back data

	//Setup the new sizes
	size->constants = parser.vm.current.constants;
	size->variables = parser.vm.current.variables;
	size->functions = parser.vm.current.functions;
	size->instructions = parser.vm.current.instructions;
	full_env->instruction_count = parser.vm.current.instructions;

	//Fill the actually used compilation stack size
	size->compilation_stack = parser.stack.high;

	//Fill the calculated maximum runtime stack size
	size->runtime_stack = parser.vm.max.stack;

	//TODO: Allow to disable the compaction
	eei_environment_compact(full_env, size, &parser);
	eei_guestimate_calculate_sizes(size);

	compilation->reply = parser.status;

	//Detect special conditions on correctly parsed code
	if (parser.status == ee_parser_ok)
	{
		if (parser.vm.current.instructions == 0)
			//No instructions were generated
			compilation->reply = ee_parser_empty;
		else if (parser.vm.current.stack == 0)
		{
			//The runtime variable stack would be empty after evaluation

			if (parser.vm.max.stack > 0)
				//The stack had data that was (most likely) stored
				compilation->reply = ee_parser_store;
			else
				//The stack never had any data, thus surely none was stored,
				//	and this is an actual error.
				compilation->reply = ee_parser_noresult;
		}
	}

	compilation->error_token_start = &parser.expression[parser.error_token.text.start];
	compilation->error_token_end = &parser.expression[parser.error_token.text.end];

	return compilation->reply;
}

ee_evaluator_reply ee_evaluate(ee_environment environment, ee_variable result)
{
	eei_vm_environment vm_environment;

	//Fill the VM environment
	{
		eei_environment_struct * full_env = (eei_environment_struct *)environment;
		eei_vmmake_data pointers;

		//Setup the VM tables memory
		char * ptr = eei_environment_calculate_pointers(
				&pointers,
				&full_env->offsets,
				(char *)&full_env->data[0]);

		vm_environment.constants = pointers.constants;
		vm_environment.variables = pointers.variables;
		vm_environment.functions = pointers.functions;
		vm_environment.instructions = pointers.instructions;
		vm_environment.stack = (ee_variable_type*)ptr;
		vm_environment.instruction_count = full_env->instruction_count;
	}

	if (vm_environment.instruction_count == 0)
	{
		//This is an empty expression that can not return any result

		if (result)
			//A result was expected - notify the caller
			return ee_evaluator_empty;

		//No result was expected anyhow - all is well
		return ee_evaluator_ok;
	}

	ee_evaluator_reply reply = eei_vm_execute(&vm_environment);

	//Extract the top of the stack and return it as the result
	if (result)
	{
		//A result is expected but the stack is empty
		if (reply == ee_evaluator_empty)
			return ee_evaluator_stack_underflow;

		*result = *vm_environment.stack;
	}
	else if (reply == ee_evaluator_ok)
		return ee_evaluator_stack_extra;

	return reply;
}

//System verification
//-------------------

//This comes to make sure we don't get any wierd surprises on esoteric systems
#if __CHAR_BIT__ < 8
#	error Non conformat system! A char must be, at least, 8 bits wide.
#endif

//This structure is just a clever way to make sure the sizes of the basic data types are exactly what we expect them to be.
//When a check fails the compilation will halt with an error of: "check type" declared as an array with a negative size
struct check_type_sizes
{
	//Enumeration sentinel based checks
	int not_enough_bits_for_token_type[((1 << eei_token_bits_type_size) >= eei_token_sentinel) ? 1 : -1];
	int not_enough_bits_for_handler[((1 << eei_rule_bits_handler_size) >= eei_rule_handle_sentinel) ? 1 : -1];
	int not_enough_bits_for_precedence2[( ((1 << eei_rule_bits_precedence_size)-1) >= eei_precedence_sentinel) ? 1 : -1];

	//Bit-width based checks
	int not_enough_bits_for_token[(sizeof(eei_token) * __CHAR_BIT__ >= eei_token_bits_size) ? 1 : -1];
	int not_enough_bits_for_rule[(sizeof(eei_rule) * __CHAR_BIT__ >= eei_rule_bits_rule_size) ? 1 : -1];
	int not_enough_bits_for_precedence[(1 << ((sizeof(eei_precedence) * __CHAR_BIT__) >= eei_rule_bits_precedence_size)) ? 1 : -1];
	int not_enough_bits_for_rule_description[(sizeof(eei_rule_description) * __CHAR_BIT__ >= eei_rule_bits_total_size) ? 1 : -1];
};
