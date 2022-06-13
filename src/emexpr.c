/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "emexpr.h"

#include <stddef.h>

//Notation and formalism:
// All implementation details have a prefix of "eei" - Embedded Expression Implementation
// All externally visible definition have a prefix of "ee" - Embedded Expression


//Lexer
//-----

typedef enum
{
	eei_token_error,
	eei_token_eof,

	eei_token_identifier,
	eei_token_number,
	eei_token_delimiter,
	eei_token_operator,

	eei_token_sof,
	eei_token_group,

	//Sentinel value to count the number of enumeraions
	//The _check_type_sizes struct uses this to validate all values fit in the data type used for the token
	//DO NOT use it in the actual tokens since it may not fit in the allocated bits!
	eei_token_sentinel
} eei_token_type;

int eei_is_internal_token(eei_token_type token)
{
	return
			(token == eei_token_error)
			|| (token == eei_token_eof)
			|| (token == eei_token_sof)
			|| (token == eei_token_group);
}

typedef char * tExpressionItem;

typedef struct
{
	const ee_char_type * start;
	const ee_char_type * head;

	int number_integer;
	int number_fractional;
	int number_exponent;
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

eei_token_type eei_lexer_consume_identifier(eei_lexer_state * state)
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

	return eei_token_identifier;
}

eei_token_type eei_lexer_consume_number(eei_lexer_state * state)
{
	//Numbers are scanned piece-wise and later processed by the parser
	state->number_integer = 0;
	state->number_fractional = 0;
	state->number_exponent = 0;

	//Scan the integer part
	state->number_integer = eei_lexer_consume_digits(state);

	//Scan the fractional part
	if (*state->head == '.')
	{
		state->head++;

		//A number MUST follow
		if (!eei_lexer_is_number(*state->head))
			return eei_token_error;

		state->number_fractional = eei_lexer_consume_digits(state);
	}

	if (eei_lexer_is_alpha(*state->head))
	{
		//No other letter except 'e' is allowed adjacent to a number
		if (*state->head != 'e')
			return eei_token_error;
	}

	//Scan the exponent part
	if (*state->head == 'e')
	{
		state->number_exponent = 1;

		state->head++;

		//A number or sign MUST follow

		if (*state->head == '+')
		{
			state->head++;
		}
		else if (*state->head != '-')
		{
			state->number_exponent = -1;
			state->head++;
		}
		else if (!eei_lexer_is_number(*state->head))
			return eei_token_error;

		state->number_exponent *= eei_lexer_consume_digits(state);
	}

	return eei_token_number;
}

eei_token_type eei_lexer_next_token(eei_lexer_state * state)
{
	//Parse the input stream for the next token

	//Skip spaces
	while (eei_lexer_is_space(*state->head)) state->head++;

	state->start = state->head;

	if (eei_lexer_is_eof(*state->head))
		return eei_token_eof;

	//The first character determines the token class

	if (eei_lexer_is_number(*state->head))
		return eei_lexer_consume_number(state);

	if (eei_lexer_is_alpha(*state->head))
		return eei_lexer_consume_identifier(state);

	if (eei_lexer_is_delimiter(*state->head))
	{
		//Delimiters are always single character
		state->head++;
		return eei_token_delimiter;
	}

	if (eei_lexer_is_operator(*state->head))
	{
		//Only single-character operators are currently supported
		state->head++;
		return eei_token_operator;
	}

	return eei_token_error;
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


//Holds the token type and the symbol
//A token has two parts:
//	A type, as enumerated in eei_token_type
//	A symbol, used to differentiate between the various delimiters and operators
typedef unsigned short int eei_token;

//Holds a parsing rule
//A rule consists of a token and the rule type, as enumerated in eei_rule_type
typedef unsigned short int eei_rule;

//Holds a rule precedence
typedef int eei_precedence;

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

//A rule table item to hold rules and their handler functions
typedef struct
{
	//Rule description
	eei_rule_description rule;

	//Handler function for the rule
	eei_rule_handler handler;
} eei_rule_item;


//Parser rule handler forward declarations
//----------------------------------------
ee_parser_reply eei_rule_handler_number(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_variable(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_group(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_prefix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_infix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_postfix(eei_parser * parser, const eei_parser_node * node);
ee_parser_reply eei_rule_handler_function(eei_parser * parser, const eei_parser_node * node);

//Parser rules
//------------

//Field sizes of the various parts for the token, rule & rule description
typedef enum
{
	//Token symbol
	eei_rule_bits_token_char_size = sizeof(ee_char_type) * __CHAR_BIT__,

	//Token type. Must accomodate eei_token_type (without the sentinel)
	eei_rule_bits_token_type_size = 3,

	//Rule type. Must accomodate eei_rule_type
	eei_rule_bits_type_size = 2,

	//Rule description precedence. Must be wide enough to alllow for the highest used precedence
	eei_rule_bits_precedence_size = 8,

	//Rule description accosiativity. Must accomodate eei_rule_associativity
	eei_rule_bits_accosiativity_size = 1,

	//Rule description end-delimited flag. Single bit.
	eei_rule_bits_end_delimiter_size = 1,

	//Rule description delayed fold flag. Single bit.
	eei_rule_bits_delayed_fold_size = 1,

	//Compound size of a token
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the token
	eei_rule_bits_token_size = eei_rule_bits_token_char_size + eei_rule_bits_token_type_size,

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

	//Offset for the symbol part of a token
	eei_rule_bits_token_char_offset = eei_rule_bits_start_offset,

	//Offset for the type part of a token
	eei_rule_bits_token_type_offset = eei_rule_bits_token_char_offset + eei_rule_bits_token_char_size,

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
	eei_rule_bits_delayed_fold_offset = eei_rule_bits_end_delimiter_offset + eei_rule_bits_end_delimiter_size,

	//Total size of a rule description, in bits
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the rule description
	eei_rule_bits_total_size = eei_rule_bits_delayed_fold_offset + eei_rule_bits_delayed_fold_size
} eei_rule_offsets;


//Create a bitmask of specified width and offset
#define BITMASK(width) ((( (1ULL<<((width) - 1)) - 1) << 1) | 1 )
#define BITMASKS(width, offset) (BITMASK(width) << (offset))

#define MAKE_PART_BITS(data, offset, size) ( ((data) & BITMASK(size)) << (offset) )
#define GET_PART_BITS(data, offset, size) ( ((data) >> (offset)) & BITMASK(size) )

//Extract a token from a rule description
#define GET_TOKEN(rule_description) \
	((eei_token)GET_PART_BITS(rule_description, eei_rule_bits_token_offset, eei_rule_bits_token_size))

//Create a token from its parts
#define MAKE_TOKEN(token_type, token_symbol) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size) |\
	MAKE_PART_BITS(token_symbol, eei_rule_bits_token_char_offset, eei_rule_bits_token_char_size) \
	)

//Create a simple token that does not use the symbol
#define MAKE_SIMPLE_TOKEN(token_type) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size) |\
	MAKE_PART_BITS('\0', eei_rule_bits_token_char_offset, eei_rule_bits_token_char_size) \
	)

//Create a rule from its parts
#define MAKE_RULE(token, type) \
	(eei_rule)(\
	MAKE_PART_BITS(token, eei_rule_bits_token_offset, eei_rule_bits_token_size) |\
	MAKE_PART_BITS(type, eei_rule_bits_type_offset, eei_rule_bits_type_size) \
	)

//Create a rule description from its parts
#define MAKE_RULE_DESCRIPTION(rule, precedence, next, accosiativity, end_delimiter, delayed_fold) \
	(eei_rule_description)(\
	MAKE_PART_BITS(rule, eei_rule_bits_rule_offset, eei_rule_bits_rule_size) |\
	MAKE_PART_BITS(precedence, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size) |\
	MAKE_PART_BITS(next, eei_rule_bits_next_offset, eei_rule_bits_next_size) |\
	MAKE_PART_BITS(accosiativity, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size) |\
	MAKE_PART_BITS(end_delimiter, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size) |\
	MAKE_PART_BITS(delayed_fold, eei_rule_bits_delayed_fold_offset, eei_rule_bits_delayed_fold_size)\
	)

//Make a prefix rule description
#define MAKE_PREFIX_RULE(token, end_delimiter, delayed_fold, next) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_prefix), 0, next, eei_rule_right, end_delimiter, delayed_fold)

//Make a default prefix rule description
#define MAKE_DEFAULT_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 0, 0, eei_rule_prefix)

//Make a leaf prefix rule description
#define MAKE_TERMINAL_PREFIX_RULE(token, delayed_fold) MAKE_PREFIX_RULE(token, 0, delayed_fold, eei_rule_infix)

//Make a delimited prefix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 1, 0, eei_rule_prefix)

//Make a infix rule description
#define MAKE_INFIX_RULE(token, precedence, accosiativity, end_delimiter) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_infix), precedence, eei_rule_prefix, accosiativity, end_delimiter, 0)

//Make a default infix rule description
#define MAKE_DEFAULT_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_left, 0)

//Make a delimited infix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_right, 1)

//Make a default postfix rule description
#define MAKE_POSTFIX_RULE(token, precedence) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_postfix), precedence, eei_rule_infix, eei_rule_left, 0, 0)

//Make an end rule to match a delimited rule description
#define MAKE_END_RULE(token) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_end), 0, eei_rule_prefix, eei_rule_left, 0, 0)

//Make a sentinel invalid rule to mark the end of a rules table
#define MAKE_SENTINEL_RULE() MAKE_DEFAULT_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))
#define MAKE_SENTINEL_END_RULE() MAKE_END_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))

//Make a special rule that marks parsing groups
#define MAKE_GROUP_RULE() MAKE_DELIMITED_INFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_group),0)

//Create a rule than only represents state
#define STATE(rule) {rule, (eei_rule_handler)NULL}

//Create a rule with a handler
#define HANDLE(rule, handler) {rule, handler}

//Macros to simplify working with a ruleset

//Test if the rule is valid
#define IS_RULE_VALID(rule) ((rule) != MAKE_SENTINEL_RULE())

#define GET_RULE_TYPE(rule) (eei_rule_type)GET_PART_BITS(rule, eei_rule_bits_type_offset, eei_rule_bits_type_size)
#define GET_RULE_PRECEDENCE(rule) (eei_precedence)GET_PART_BITS(rule, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size)
#define GET_RULE_NEXT(rule) (eei_rule)GET_PART_BITS(rule, eei_rule_bits_next_offset, eei_rule_bits_next_size)
#define GET_RULE_ACCOSIATIVITY(rule) (eei_rule_associativity)GET_PART_BITS(rule, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size)
#define GET_RULE_ENDDELIMITER(rule) GET_PART_BITS(rule, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)
#define GET_RULE_DELAYEDFOLD(rule) GET_PART_BITS(rule, eei_rule_bits_delayed_fold_offset, eei_rule_bits_delayed_fold_size)
#define GET_TOKEN_TYPE(rule) (eei_token_type)GET_PART_BITS(rule, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size)

//Parser rule tables
//------------------

//Operator precedence
enum
{
	eei_precedence_comma = 2,
	eei_precedence_logical_or = 3,
	eei_precedence_logical_and = 4,
	eei_precedence_compare = 5,
	eei_precedence_power1 = 6,
	eei_precedence_power2 = 7,
	eei_precedence_power3 = 8,
	eei_precedence_function = 10,
	eei_precedence_postfix = 11,
};

static const eei_rule_item eei_parser_prefix_rules[] =
{
	//Expression start
	STATE(MAKE_DELIMITED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_sof))),

	HANDLE(MAKE_TERMINAL_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_number),0), eei_rule_handler_number),
	HANDLE(MAKE_TERMINAL_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier),1), eei_rule_handler_variable),

	//Grouping parens
	HANDLE(MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'(')), eei_rule_handler_group),

	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'!')), eei_rule_handler_prefix),
	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'~')), eei_rule_handler_prefix),
	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'&')), eei_rule_handler_prefix),
	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'|')), eei_rule_handler_prefix),
	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'+')), eei_rule_handler_prefix),
	HANDLE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'-')), eei_rule_handler_prefix),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_item eei_parser_infix_rules[] =
{
	//Sequence delimiter
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,','), eei_precedence_comma)),

	//Function call
	HANDLE(MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function), eei_rule_handler_function),

	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'|'), eei_precedence_logical_or), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'&'), eei_precedence_logical_and), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'='), eei_precedence_compare), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'<'), eei_precedence_compare), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'>'), eei_precedence_compare), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'+'), eei_precedence_power1), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'-'), eei_precedence_power1), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'*'), eei_precedence_power2), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'/'), eei_precedence_power2), eei_rule_handler_infix),
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'%'), eei_precedence_power2), eei_rule_handler_infix),
	HANDLE(MAKE_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'^'), eei_precedence_power3, eei_rule_right, 0), eei_rule_handler_infix),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_item eei_parser_postfix_rules[] =
{
	HANDLE(MAKE_POSTFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier), eei_precedence_postfix), eei_rule_handler_postfix),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_description eei_parser_end_rules[][2] =
{
	//Expression end
	{
		MAKE_DELIMITED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_sof)),
		MAKE_END_RULE(MAKE_SIMPLE_TOKEN(eei_token_eof))
	},

	//Grouping parens
	{
		MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'(')),
		MAKE_END_RULE(MAKE_TOKEN(eei_token_delimiter,')'))
	},

	//Function call
	{
		MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function),
		MAKE_END_RULE(MAKE_TOKEN(eei_token_delimiter,')'))
	},

	{MAKE_SENTINEL_RULE(),MAKE_SENTINEL_END_RULE()}
};

//A group rule description for internal use
static const eei_rule_item eei_parser_group_rule = STATE(MAKE_DEFAULT_INFIX_RULE(eei_token_group,0));

//A sentinel rule description to use as a return value when needed
static const eei_rule_item eei_parser_sentinel_rule = STATE(MAKE_SENTINEL_RULE());


const eei_rule_item * eei_find_rule(eei_token token, eei_rule_type expected)
{
	//Find the rule of the expected type matching the given token
	//Will return the sentinel rule on failure

	const eei_rule_item * table;

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
			return &eei_parser_sentinel_rule;
	}

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it
	while (table->rule != MAKE_SENTINEL_RULE())
	{
		if (token == GET_TOKEN(table->rule))
			break;
		table++;
	}

	return table;
}

int eei_find_end_rule(eei_token token, eei_rule_description rule)
{
	//Find the end rule with the given token for the requested rule
	//Will return the sentinel end rule on failure

	const eei_rule_description (* table)[2] = eei_parser_end_rules;

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it
	while (table[0][0] != MAKE_SENTINEL_RULE())
	{
		if ((rule == table[0][0]) && (token == GET_TOKEN(table[0][1])))
			break;

		table++;
	}

	return table[0][0] != MAKE_SENTINEL_RULE();
}


//Parser stack
//------------

struct eei_parser_node_
{
	//The rule being processed
	const eei_rule_item * rule;

	//The source token for the rule being processed
	const ee_char_type * token_start;
	const ee_char_type * token_end;

	//The token type expected next
	eei_rule_type next;

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
	dst->token_start = src->token_start;
	dst->token_end = src->token_end;
	dst->next = src->next;
	dst->precedence = src->precedence;
	dst->stack_top = src->stack_top;

	return ee_parser_ok;
}

ee_parser_reply eei_stack_push(eei_parser_stack * stack, const eei_parser_node * node)
{
	//Push a node to the top of the stack
	//Return on-zero on error

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

	eei_stack_copynode(node, &stack->stack[stack->top - 1]);

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
};

typedef struct
{
	ee_variable_type * constants;
	const ee_variable_type ** variables;
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

	if (vm->current.instructions + data + 1 > vm->max.instructions)
		return ee_parser_instrictions_overflow;

	int i = total;
	while (i)
	{
		vm->data.instructions[vm->current.instructions++] =
				((immediate >> (eei_vm_immediate_bits * i)) & ((1 << eei_vm_immediate_bits) - 1)
				<< eei_vm_immediate_shift)
				| eei_vm_insturction_immediate;

		i--;
	}

	vm->data.instructions[vm->current.instructions++] =
			(immediate & ((1 << eei_vm_immediate_bits) - 1)
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
		const ee_variable_type * variable)
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
		//Nothing found, we need to add a new constant to the table
		if (vm->current.functions >= vm->max.functions)
			return ee_parser_functions_overflow;

		vm->data.functions[index] = function;
		vm->current.functions++;
	}

	//Update stack usage
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

//Parser
//------

typedef struct
{
	eei_token token;
	const ee_char_type * start;
	const ee_char_type * end;
} eei_parser_token;

typedef struct
{
	//The externally supplied data
	const ee_compilation_data * foreign;

	//Counts
	int variables;
	int functions;
} eei_parser_symboltable;

struct eei_parser_
{
	eei_parser_stack stack;
	eei_vmmake_environment vm;
	eei_parser_symboltable symboltable;

	eei_parser_token error_token;
	ee_parser_reply status;

	//Stack index of the current group
	//This points to the actual rule and not the synthetic group rule
	int currentGroup;
};

ee_parser_reply eei_parse_set_error(
		eei_parser * parser,
		ee_parser_reply error,
		const eei_parser_token * token)
{
	parser->status = error;

	parser->error_token.token = token->token;
	parser->error_token.start = token->start;
	parser->error_token.end = token->end;

	return error;
}

//Parser utility functions
//------------------------

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
			parser->error_token.start = token->start;
			parser->error_token.end = token->end;
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
		parser->error_token.start = token->start;
		parser->error_token.end = token->end;
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
		const eei_rule_item * rule,
		const eei_rule_type next,
		const eei_precedence precedence,
		const eei_parser_token * token
		)
{
	eei_parser_node node;

	node.token_start = token->start;
	node.token_end = token->end;
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

	node.token_start = token->start;
	node.token_end = token->end;
	node.rule = &eei_parser_group_rule;
	node.next = eei_rule_prefix;
	node.precedence = 0;
	node.stack_top = parser->vm.current.stack;

	//Update the current group to the point the current stack top
	//	since that must be the rule that created this group
	parser->currentGroup = parser->stack.top - 1;

	return eei_parse_error(parser, eei_stack_push(&parser->stack, &node), token);
}

//Parser symbol table handling
//----------------------------

ee_parser_reply eei_parse_symbols_init(
		eei_parser * parser,
		const ee_compilation_data *data)
{
	parser->symboltable.foreign = data;

	//This is the naive implementation that will just perform a linear search
	//	over all available symbol.
	//For that to work we need to count them all.

	parser->symboltable.variables = 0;
	if (data->variables.meta.names && data->variables.data)
	{
			const ee_char_type * const * names = data->variables.meta.names;
			ee_variable const * values = data->variables.data;

			if (data->variables.meta.count == 0)
				//No actual limit
				while (*names && *values)
				{
					parser->symboltable.variables++;
					names++;
					values++;
				}
			else
				while (*names && *values && (parser->symboltable.variables < data->variables.meta.count))
				{
					parser->symboltable.variables++;
					names++;
					values++;
				}
	}


	parser->symboltable.functions = 0;
	if (data->functions.meta.names && data->functions.data)
	{
			const ee_char_type * const * names = data->functions.meta.names;
			ee_compilation_data_function * values = data->functions.data;

			if (data->functions.meta.count == 0)
				//No actual limit
				while (*names && values->function)
				{
					parser->symboltable.functions++;
					names++;
					values++;
				}
			else
				while (*names && values->function && (parser->symboltable.functions < data->functions.meta.count))
				{
					parser->symboltable.functions++;
					names++;
					values++;
				}
	}

	return ee_parser_ok;
}

static inline int eei_parse_symbols_compare_node(
		const ee_char_type * name,
		const ee_char_type * token_start,
		const ee_char_type * token_end)
{
	//Compare token text with a name
	//Return false is not equal

	const char * ptr = token_start;
	const ee_char_type * cmp = name;

	while (*cmp && (ptr != token_end))
		if (*cmp++ != *ptr++)
			return 0;

	return *cmp == '\0';
}

static inline int eei_parse_symbols_get_name(
		const ee_char_type * const * names,
		int start_index,
		int end_index,
		const ee_char_type * token_start,
		const ee_char_type * token_end)
{
	//Returns the index of the nodes' token inside the names

	for (int i = start_index; i < end_index; ++i)
		if (eei_parse_symbols_compare_node(names[i], token_start, token_end))
			return i;

	return end_index;
}

ee_variable_type * eei_parse_symbols_get_variable(
		eei_parser * parser,
		const eei_parser_node * node)
{
	//Returns the variable pointed to by the node, or NULL on error.
	const ee_char_type * token_start = node->token_start;
	const ee_char_type * token_end = node->token_end;

	int index =
			eei_parse_symbols_get_name(
				parser->symboltable.foreign->variables.meta.names,
				0,
				parser->symboltable.variables,
				token_start,
				token_end);

	if (index < parser->symboltable.variables)
		return parser->symboltable.foreign->variables.data[index];

	return 0;
}

ee_function eei_parse_symbols_get_function(
		eei_parser * parser,
		int arity,
		const eei_parser_node * node,
		int * wrong_arity)
{
	//Returns the function pointed to by the node, with a compatible arity, or NULL on error.
	const ee_char_type * token_start = node->token_start;
	const ee_char_type * token_end = node->token_end;

	int seen = 0;
	int index = 0;

	while (index < parser->symboltable.functions)
	{
		index =
				eei_parse_symbols_get_name(
					parser->symboltable.foreign->functions.meta.names,
					index,
					parser->symboltable.functions,
					token_start,
					token_end);

		if (index < parser->symboltable.functions)
		{
			seen = 1;

			//Test for correct arity
			int found_arity = parser->symboltable.foreign->functions.data[index].arity;

			if (found_arity >= 0)
			{
				//This is regular function that must get an exact number of parameters
				if (found_arity != arity)
				{
					index++;
					continue;
				}
			}
			else
			{
				//We're looking at a variadic function
				//Calculate the number of mandatory parameters
				found_arity = -found_arity - 1;

				if (found_arity > arity)
				{
					index++;
					continue;
				}
			}

			//We found it!
			return parser->symboltable.foreign->functions.data[index].function;
		}

		index++;
	}

	if (wrong_arity)
		*wrong_arity = seen;

	return NULL;
}


//Parser core functions
//---------------------

ee_parser_reply eei_parse_done_node(eei_parser * parser, const eei_parser_node * node)
{
	//Test for a handler function
	if (node->rule->handler)
	{
		const ee_parser_reply reply = node->rule->handler(parser, node);
		if (reply != ee_parser_ok)
		{
			eei_parser_token token;
			token.token = GET_TOKEN(node->rule->rule);
			token.start = node->token_start;
			token.end = node->token_end;
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
		const eei_rule_item * rule,
		const eei_parser_token * token)
{
	//Push the rule itself
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule->rule),
				GET_RULE_PRECEDENCE(eei_stack_top(&parser->stack, 0)->rule->rule),
				token);

	if (GET_RULE_ENDDELIMITER(rule->rule))
		//Push a special group to reset the precedence inside the delimited group
		//	without affecting the precedence processing of the tokens that will
		//	follow the group.
		eei_parse_pushGroupRule(parser, token);
}

static inline void eei_parse_parseInfix(
		eei_parser * parser,
		const eei_rule_item * rule,
		const eei_parser_token * token)
{
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule->rule),
				(GET_RULE_ACCOSIATIVITY(rule->rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule->rule)
				: GET_RULE_PRECEDENCE(rule->rule) - 1,
				token);

	if (GET_RULE_ENDDELIMITER(rule->rule))
		//Push a special group to reset the precedence inside the delimited group
		//	without affecting the precedence processing of the tokens that will
		//	follow the group.
		eei_parse_pushGroupRule(parser, token);
}

void eei_parse_parsePostfix(
		eei_parser * parser,
		const eei_rule_item * rule,
		const eei_parser_token * token)
{
	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule->rule),
				(GET_RULE_ACCOSIATIVITY(rule->rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule->rule)
				: GET_RULE_PRECEDENCE(rule->rule) - 1,
				token);
}

void eei_parse_foldPrefix(eei_parser * parser, int delay)
{
	//Fold immediately preceeding prefix nodes.
	//This will stop on any non-prefix node, any special node and,
	//	also, on any node that expects an end token
	while (parser->stack.top)
	{
		if (eei_is_internal_token( GET_TOKEN_TYPE(eei_stack_top(&parser->stack, 0)->rule->rule) ))
			break;

		if (GET_RULE_ENDDELIMITER(eei_stack_top(&parser->stack, 0)->rule->rule))
			break;

		if (GET_RULE_TYPE(eei_stack_top(&parser->stack, 0)->rule->rule) != eei_rule_prefix)
			break;

		if (delay && GET_RULE_DELAYEDFOLD(eei_stack_top(&parser->stack, 0)->rule->rule))
			break;

		eei_parse_done(parser);
	}
}

void eei_parse_foldPrecedence(eei_parser * parser, const eei_rule_item * rule)
{
	const eei_precedence precedence = GET_RULE_PRECEDENCE(rule->rule);

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

	//Additional sanity check to make sure the stack was build correctly
	if (!GET_RULE_ENDDELIMITER(parser->stack.stack[parser->currentGroup].rule->rule))
	{
		eei_parse_set_error(parser, ee_parser_error, token);
		return;
	}

	//Test the end-delimited rule that created this group
	const int matched = eei_find_end_rule(
							token->token,
							parser->stack.stack[parser->currentGroup].rule->rule);

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
	//This will fold the synthetic gropup rule as well
	++group;
	while (parser->stack.top > group)
		eei_parse_done(parser);

	//Before folding the rule itself we need to update the current group
	//	to the previous one, since the rule that created the current group is,
	//	in itself, an element of the previous one, and must be folded as part of that.

	group = parser->stack.top - 1;

	//Special handling for the SOF token
	if (!group)
	{
		parser->currentGroup = 0;
		eei_parse_done(parser);
		return;
	}

	//Walk the stack back and find the previous group.
	//Make sure that, at least, the initial group remains.
	//Pre-decrementing skips the current top, that we know is not a group rule
	while (--group > 0)
		if (parser->stack.stack[group].rule == &eei_parser_group_rule)
			break;

	//Sanity check
	if (group <= 0)
	{
		eei_parse_set_error(parser, ee_parser_error, token);
		return;
	}

	//We need the rule that created the previous group, not the synthetic group itself.
	parser->currentGroup = group - 1;

	//Fold the rule that created the group.
	eei_parse_done(parser);

	//Sanity check
	if (!parser->stack.top)
	{
		eei_parse_error(parser, ee_parser_stack_underflow, token);
		return;
	}

	//The token is an end delimiter - that was just completely processed.
	//This means the node can be treated as a postfix -
	//	so an infix must follow
	eei_stack_top(&parser->stack,0)->next = eei_rule_infix;
}


void eei_parse_token(eei_parser * parser, eei_parser_token * token)
{
	eei_rule_type expected = eei_stack_top(&parser->stack, 0)->next;
	const eei_rule_item * rule = eei_find_rule(token->token, expected);
	int foundExpected = IS_RULE_VALID(rule->rule);

	if (!foundExpected && (expected != eei_rule_postfix))
	{
		//A postfix can appear when not expected - so look for it
		rule = eei_find_rule(token->token, eei_rule_postfix);
		foundExpected = IS_RULE_VALID(rule->rule);
		expected = eei_rule_postfix;
	}

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
				eei_parse_foldPrefix(parser, GET_RULE_ENDDELIMITER(rule->rule));

				eei_parse_foldPrecedence(parser, rule);
				eei_parse_parseInfix(parser, rule, token);
				break;

			case eei_rule_postfix:
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
//		eei_parse_foldPrefix(parser);
		eei_parse_foldEndDilimiter(parser, token);
	}
}

void eei_parse_init(eei_parser * parser, const ee_char_type * expression)
{
	eei_parser_node node;
	eei_parser_token token;

	parser->status = ee_parser_ok;
	parser->error_token.token = MAKE_SIMPLE_TOKEN(eei_token_sof);
	parser->error_token.start = expression;
	parser->error_token.end = expression;

	node.token_start = expression;
	node.token_end = expression;
	node.precedence = 0;
	node.next = eei_rule_prefix;
	node.stack_top = 0;

	//Push a "start" rule - this will simplify things during processing
	node.rule = eei_find_rule(MAKE_SIMPLE_TOKEN(eei_token_sof), eei_rule_prefix);
	eei_stack_push(&parser->stack, &node);

	//Start a group - this allows to remove many tests for an empty stack
	token.token = MAKE_SIMPLE_TOKEN(eei_token_group);
	token.start = expression;
	token.end = expression;
	eei_parse_pushGroupRule(parser, &token);
}

void eei_parse_expression(eei_parser * parser, const ee_char_type * expression)
{
	eei_lexer_state lexer_state;
	lexer_state.head = expression;

	eei_parse_init(parser, expression);

	while ((parser->status == ee_parser_ok) && (parser->stack.top))
	{
		eei_parser_token token;
		const eei_token_type token_type = eei_lexer_next_token(&lexer_state);

		//Create a complete token
		token.start = lexer_state.start;
		token.end = lexer_state.head;

		if ((token_type == eei_token_delimiter) || (token_type == eei_token_operator))
			token.token = MAKE_TOKEN(token_type, *lexer_state.start);
		else
			token.token = MAKE_SIMPLE_TOKEN(token_type);

		eei_parse_token(parser, &token);
	}

	if (parser->stack.top && (parser->status == ee_parser_ok))
		parser->status = ee_parser_error;
}


//Parser handler functions
//------------------------

ee_parser_reply eei_rule_handler_number(eei_parser * parser, const eei_parser_node * node)
{
	eei_lexer_state state;
	state.head = node->token_start;

	if (eei_lexer_consume_number(&state) == eei_token_error)
		return ee_parser_expression_not_a_constant;

	return eei_vmmake_load_constant(&parser->vm, state.number_integer);
}

ee_parser_reply eei_rule_handler_variable(eei_parser * parser, const eei_parser_node * node)
{
	ee_variable_type * var = eei_parse_symbols_get_variable(parser, node);

	//Look for a zero-arity function with the same name
	ee_function op = eei_parse_symbols_get_function(parser, 0, node, NULL);

	if (var && op)
		//We do not allow both to be defined to avoid confusion
		return ee_parser_varfunction_duplicate;

	if (!var && !op)
		//Assume this should have been a variable
		return ee_parser_unknown_variable;

	if (var)
		return eei_vmmake_load_variable(&parser->vm, var);

	if (op)
		return eei_vmmake_execute_functions(&parser->vm, op, 0);

	return ee_parser_error;
}

ee_parser_reply eei_rule_handler_group(eei_parser * parser, const eei_parser_node * node)
{
	//A group must end with exactly ONE new value on the runtime stack
	const int items = parser->vm.current.stack - node->stack_top;

	if (items == 1)
		return ee_parser_ok;

	return ee_parser_expression_empty_group;
}

ee_parser_reply eei_rule_handler_prefix(eei_parser * parser, const eei_parser_node * node)
{
	ee_function op = eei_parse_symbols_get_function(parser, 1, node, NULL);

	if (!op)
		return ee_parser_prefix_not_implemented;

	return eei_vmmake_execute_functions(&parser->vm, op, 1);
}

ee_parser_reply eei_rule_handler_infix(eei_parser * parser, const eei_parser_node * node)
{
	ee_function op = eei_parse_symbols_get_function(parser, 2, node, NULL);

	if (!op)
		return ee_parser_infix_not_implemented;

	return eei_vmmake_execute_functions(&parser->vm, op, 2);
}

ee_parser_reply eei_rule_handler_postfix(eei_parser * parser, const eei_parser_node * node)
{
	ee_function op = eei_parse_symbols_get_function(parser, 1, node, NULL);

	if (!op)
		return ee_parser_postfix_not_implemented;

	return eei_vmmake_execute_functions(&parser->vm, op, 1);
}

ee_parser_reply eei_rule_handler_function(eei_parser * parser, const eei_parser_node * node)
{
	//An identifier must be on the stack at this point
	eei_parser_node identifier;

	ee_parser_reply reply =
			eei_parse_popT(
				parser,
				&identifier,
				&((eei_parser_token){GET_TOKEN(node->rule->rule), node->token_start, node->token_end}));

	if (reply != ee_parser_ok)
		return reply;

	if (GET_TOKEN_TYPE(identifier.rule->rule) != eei_token_identifier)
		return eei_parse_set_error(
					parser,
					ee_parser_expression_identifier_expected,
					&((eei_parser_token){GET_TOKEN(identifier.rule->rule), identifier.token_start, identifier.token_end}));

	const int arity = parser->vm.current.stack - identifier.stack_top;
	int wrong_arity;
	ee_function op = eei_parse_symbols_get_function(parser, arity, &identifier, &wrong_arity);

	if (!op)
		return eei_parse_set_error(
					parser,
					wrong_arity
					? ee_parser_function_wrong_arity
					: ee_parser_function_not_implemented,
					&((eei_parser_token){GET_TOKEN(identifier.rule->rule), identifier.token_start, identifier.token_end}));

	return eei_vmmake_execute_functions(&parser->vm, op, arity);
}

//Virtual machine execute
//-----------------------

//Holds the VM environment data
typedef struct
{
	//The constants table
	const ee_variable_type * constants;

	//The (pointers-to) variables table
	const ee_variable_type * const * variables;

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
				//The variables table holds pointers to the variables
				//	so the requested index needs to be dereferenced to access
				//	the actual user variable.
				*rt.stack_top++ =
						*vm_environment->variables[rt.accumulator];

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
		}
	};

	//The stack top hold the result of the evaluation
	if (rt.stack_top == vm_environment->stack)
		return ee_evaluator_empty;

	return ee_evaluator_ok;
}


//External API utility
//--------------------

//Helper macro to calculate alignment of a type
#define alignof(type) ((ptrdiff_t)&((struct { char c; type d; } *)0)->d)

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
	pointers->variables = (const ee_variable_type**)(base + offsets->variables);
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
		token_type = eei_lexer_next_token(&state);

		switch (token_type)
		{
			case eei_token_identifier:
				identifiers++;
				break;

			case eei_token_number:
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


	//Guestimate the number of elements

	size->constants = numbers;
	size->variables = identifiers;
	size->functions = identifiers + operators;
	size->instructions = numbers + identifiers + operators;
	size->instructions *= size->instructions;
	size->compilation_stack = 2 + numbers + identifiers + operators * 2 + groups * 4;
	size->runtime_stack = numbers + actuals + groups * 2 + operators + identifiers;

	eei_guestimate_calculate_sizes(size);

	return (token_type == eei_token_error) ? ee_parser_error : ee_parser_ok;
}

ee_parser_reply ee_compile(
		const ee_char_type * expression,
		ee_data_size * size,
		ee_compilation_header * compilation,
		ee_environment environment,
		const ee_compilation_data *data)
{
	eei_environment_struct * full_env = (eei_environment_struct *)environment;
	eei_parser parser;

	//Setup the parser stack memory

	eei_compilation_struct * full_compilation = (eei_compilation_struct *)compilation;

	char * ptr = (char *)&full_compilation->data[0];
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

	parser.vm.max.constants = size->constants;
	parser.vm.max.variables = size->variables;
	parser.vm.max.functions = size->functions;
	parser.vm.max.instructions = size->instructions;

	//Calculate the VM tables memory locations
	eei_environment_calculate_offsets(&full_env->offsets,(char *)&full_env->data[0],size);
	full_env->instruction_count = parser.vm.max.instructions;

	//Setup the VM tables memory
	eei_environment_calculate_pointers(
			&parser.vm.data,
			&full_env->offsets,
			(char *)&full_env->data[0]);

	//Reset the stack info since the parser will count the actual stack usage
	parser.vm.max.stack = 0;
	parser.vm.current.stack = 0;

	eei_parse_symbols_init(&parser, data);
	eei_parse_expression(&parser, expression);

	//Fill back data

	//Setup the new sizes
	size->constants = parser.vm.current.constants;
	size->variables = parser.vm.current.variables;
	size->functions = parser.vm.current.functions;
	size->instructions = parser.vm.current.instructions;

	//Fill the actually used compilation stack size
	size->compilation_stack = parser.stack.high;

	//Fill the calculate maximum runtime stack size
	size->runtime_stack = parser.vm.max.stack;

	full_env->instruction_count = parser.vm.current.instructions;

	//TODO: Allow to disable the compaction
	eei_environment_compact(full_env, size, &parser);
	eei_guestimate_calculate_sizes(size);

	if ((parser.status == ee_parser_ok) && (parser.vm.current.instructions == 0))
		compilation->reply = ee_parser_empty;
	else
		compilation->reply = parser.status;
	compilation->error_token_start = parser.error_token.start;
	compilation->error_token_end = parser.error_token.end;

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
		vm_environment.variables = (const ee_variable_type**)pointers.variables;
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
	int not_enough_bits_for_token_type[((1 << eei_rule_bits_token_type_size) >= eei_token_sentinel) ? 1 : -1];
	int not_enough_bits_for_token[(sizeof(eei_token) * __CHAR_BIT__ >= eei_rule_bits_token_size) ? 1 : -1];
	int not_enough_bits_for_rule[(sizeof(eei_rule) * __CHAR_BIT__ >= eei_rule_bits_rule_size) ? 1 : -1];
	int not_enough_bits_for_rule_description[(sizeof(eei_rule_description) * __CHAR_BIT__ >= eei_rule_bits_total_size) ? 1 : -1];
};
