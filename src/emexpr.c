/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "emexpr.h"

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
	int number = 0;

	do
	{
		number *= 10;
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
ee_parser_reply eei_rule_handler_comma(eei_parser * parser, const eei_parser_node * node);

//Parser rules
//------------

//Field sizes of the various parts for the token, rule & rule description
enum
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
enum
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

	//Total size of a rule description, in bits
	//The _check_type_sizes struct uses this to validate all parts fit in the data type used for the rule description
	eei_rule_bits_total_size = eei_rule_bits_end_delimiter_offset + eei_rule_bits_end_delimiter_size
} eei_rule_offsets;


//Create a bitmask of specified width and offset
#define BITMASK(width) ((( (1ull<<(width - 1)) - 1) << 1) | 1 )
#define BITMASKS(width, offset) (BITMASK(width) << (offset))

#define MAKE_PART_BITS(data, offset, size) ( ((data) & BITMASK(size)) << offset )
#define GET_PART_BITS(data, offset, size) ( ((data) >> offset) & BITMASK(size) )

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
#define MAKE_RULE_DESCRIPTION(rule, precedence, next, accosiativity, end_delimiter) \
	(eei_rule_description)(\
	MAKE_PART_BITS(rule, eei_rule_bits_rule_offset, eei_rule_bits_rule_size) |\
	MAKE_PART_BITS(precedence, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size) |\
	MAKE_PART_BITS(next, eei_rule_bits_next_offset, eei_rule_bits_next_size) |\
	MAKE_PART_BITS(accosiativity, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size) |\
	MAKE_PART_BITS(end_delimiter, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)\
	)

//Make a prefix rule description
#define MAKE_PREFIX_RULE(token, end_delimiter, next) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_prefix), 0, next, eei_rule_right, end_delimiter)

//Make a default prefix rule description
#define MAKE_DEFAULT_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 0, eei_rule_prefix)

//Make a delimited prefix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_PREFIX_RULE(token) MAKE_PREFIX_RULE(token, 1, eei_rule_prefix)

//Make a infix rule description
#define MAKE_INFIX_RULE(token, precedence, accosiativity, end_delimiter) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_infix), precedence, eei_rule_prefix, accosiativity, end_delimiter)

//Make a default infix rule description
#define MAKE_DEFAULT_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_left, 0)

//Make a delimited infix rule description
//The matching rule MUST appear in the end-rules table
#define MAKE_DELIMITED_INFIX_RULE(token, precedence) MAKE_INFIX_RULE(token, precedence, eei_rule_right, 1)

//Make a default postfix rule description
#define MAKE_POSTFIX_RULE(token, precedence) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_postfix), precedence, eei_rule_infix, eei_rule_left, 0)

//Make an end rule to match a delimited rule description
#define MAKE_END_RULE(token) \
	MAKE_RULE_DESCRIPTION(MAKE_RULE(token, eei_rule_end), 0, eei_rule_prefix, eei_rule_left, 0)

//Make a sentinel invalid rule to mark the end of a rules table
#define MAKE_SENTINEL_RULE() MAKE_DEFAULT_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))
#define MAKE_SENTINEL_END_RULE() MAKE_END_RULE(MAKE_SIMPLE_TOKEN(eei_token_error))

//Make a special rule that marks parsing groups
#define MAKE_GROUP_RULE() MAKE_DELIMITED_INFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_group),0)

//Create a rule than only represents state
#define STATE(rule) {rule, (eei_rule_handler)0}

//Create a rule with a handler
#define HANDLE(rule, handler) {rule, handler}

//Macros to simplify working with a ruleset

//Test if the rule is valid
#define IS_RULE_VALID(rule) ((rule) != MAKE_SENTINEL_RULE())

#define GET_RULE_TYPE(rule) GET_PART_BITS(rule, eei_rule_bits_type_offset, eei_rule_bits_type_size)
#define GET_RULE_PRECEDENCE(rule) GET_PART_BITS(rule, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size)
#define GET_RULE_NEXT(rule) GET_PART_BITS(rule, eei_rule_bits_next_offset, eei_rule_bits_next_size)
#define GET_RULE_ACCOSIATIVITY(rule) GET_PART_BITS(rule, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size)
#define GET_RULE_ENDDELIMITER(rule) GET_PART_BITS(rule, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)
#define GET_TOKEN_TYPE(rule) GET_PART_BITS(rule, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size)

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

	HANDLE(MAKE_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_number),0,eei_rule_infix), eei_rule_handler_number),
	STATE(MAKE_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier),0,eei_rule_infix)),

	//Grouping parens
	STATE(MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('))),

	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'!'))),
	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'~'))),
	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'&'))),
	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'|'))),
	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'+'))),
	STATE(MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'-'))),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_item eei_parser_infix_rules[] =
{
	//Sequence delimiter
	HANDLE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,','), eei_precedence_comma), eei_rule_handler_comma),

	//Function call
	STATE(MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function)),

	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'|'), eei_precedence_logical_or)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'&'), eei_precedence_logical_and)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'='), eei_precedence_compare)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'<'), eei_precedence_compare)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'>'), eei_precedence_compare)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'+'), eei_precedence_power1)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'-'), eei_precedence_power1)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'*'), eei_precedence_power2)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'/'), eei_precedence_power2)),
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'%'), eei_precedence_power2)),
	STATE(MAKE_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'^'), eei_precedence_power3, eei_rule_right, 0)),

	STATE(MAKE_SENTINEL_RULE())
};

static const eei_rule_item eei_parser_postfix_rules[] =
{
	STATE(MAKE_POSTFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier), eei_precedence_postfix)),

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

	return table[0][1] != MAKE_SENTINEL_RULE();
}


//Parser stack
//------------

typedef struct eei_parser_node_
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

	//For rules creating a group this holds the number
	//	of direct items in said group.
	int group_items;
} eei_parser_node;

//Holds management data for the parser stack that is used instead
//	of functional recursion to hold the parser data
typedef struct
{
	//Location of the stack
	eei_parser_node * stack;

	//Total allocated size of the stack
	int size;

	//Location of the current stack top
	//This always points to the element ABOVE the top
	int top;
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
	dst->group_items = src->group_items;

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
	eei_vm_mask_instruction = ((1u << eei_vm_insturction_bits) - 1) << eei_vm_insturction_shift,

	//Mask for the immediate values for each instruction
	eei_vm_mask_immediate_shifted = ((1u << eei_vm_immediate_bits) - 1),
	eei_vm_mask_immediate = eei_vm_mask_immediate_shifted << eei_vm_immediate_shift,



	//Add immediate to runtime accumulator
	eei_vm_insturction_immediate = (0x00) << eei_vm_insturction_shift,

	//Push a constant to the execution stack
	eei_vm_insturction_constant = (0x01) << eei_vm_insturction_shift,

	//Read and push a variable to the execution stack
	eei_vm_insturction_variable = (0x02) << eei_vm_insturction_shift,

	//Execute a function
	eei_vm_insturction_function = (0x03) << eei_vm_insturction_shift,

	//Set the arity of the nearest function to execute
	eei_vm_insturction_arity = (0x04) << eei_vm_insturction_shift,

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
	int constants;
	int variables;
	int functions;
	int instructions;

	//This tracks the run-time stack usage
	int stack;
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
	int data = immediate;
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
			| (instruction << eei_vm_insturction_shift);

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

typedef struct eei_parser_
{
	eei_parser_stack stack;
	eei_vmmake_environment vm;
	const ee_compilation_data * data;

	eei_parser_token error_token;
	ee_parser_reply status;

	//Stack index of the current group
	//This points to the actual rule and not the synthetic group rule
	int currentGroup;
} eei_parser;

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
	node.group_items = 0;

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

static inline ee_parser_reply eei_parse_test_pop(eei_parser * parser, int distance)
{
	//Test a pop can be performed
	if (parser->stack.top > distance)
		return ee_parser_ok;

	if (parser->status == ee_parser_ok)
		parser->status = ee_parser_stack_underflow;

	return ee_parser_stack_underflow;
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
	node.group_items = 0;

	//Update the current group to the point the current stack top
	//	since that must be the rule that created this group
	parser->currentGroup = parser->stack.top - 1;

	return eei_parse_error(parser, eei_stack_push(&parser->stack, &node), token);
}


//Parser core functions
//---------------------

ee_parser_reply eei_parse_done_node(eei_parser * parser, const eei_parser_node * node)
{
	//Test for a handler function
	if (node->rule->handler)
		return node->rule->handler(parser, node);

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
	eei_parser_node node;

	if (eei_parse_popT(parser, &node, token) != ee_parser_ok)
		return;

	eei_parse_push(
				parser,
				rule,
				GET_RULE_NEXT(rule->rule),
				(GET_RULE_ACCOSIATIVITY(rule->rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule->rule)
				: GET_RULE_PRECEDENCE(rule->rule) - 1,
				token);

	eei_parse_done_node(parser, &node);

	if (GET_RULE_TYPE(rule->rule) == eei_token_delimiter)
		eei_parse_pushGroupRule(parser, token);
}

void eei_parse_parsePostfix(
		eei_parser * parser,
		const eei_rule_item * rule,
		const eei_parser_token * token)
{
	eei_parser_node node;

	node.token_start = token->start;
	node.token_end = token->end;
	node.rule = rule;
	node.next = GET_RULE_NEXT(rule->rule);
	node.precedence = GET_RULE_PRECEDENCE(rule->rule);

	eei_parse_done_node(parser, &node);

	if (eei_parse_test_pop(parser, 0) != ee_parser_ok)
		return;

	//Since this node is never pushed we need to adjust
	//	the stack top with the correct next
	eei_stack_top(&parser->stack, 0)->next = node.next;
}

void eei_parse_foldPrefix(eei_parser * parser)
{
	//Fold immediately preceeding prefix nodes.
	//This will stop on any non-prefix node, any special node and,
	//	also, on any node that expects an end token
	while (parser->stack.top > 1)
	{
		if (eei_is_internal_token( GET_TOKEN_TYPE(eei_stack_top(&parser->stack, 0)->rule->rule) ))
			break;

		if (GET_RULE_ENDDELIMITER(eei_stack_top(&parser->stack, 0)->rule->rule))
			break;

		if (GET_RULE_TYPE(eei_stack_top(&parser->stack, 1)->rule->rule) != eei_rule_prefix)
			break;

		eei_parse_done(parser);
	}
}

void eei_parse_foldPrecedence(eei_parser * parser, const eei_rule_item * rule)
{
	const eei_precedence precedence = GET_RULE_PRECEDENCE(rule->rule);

	if (
		parser->stack.top
		&& (eei_stack_top(&parser->stack, 0)->precedence >= precedence))
	{
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
			   && (eei_stack_top(&parser->stack, 1)->precedence >= precedence))
			eei_parse_done(parser);
	}
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

	group = parser->stack.top;

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
	const eei_rule_type expected = eei_stack_top(&parser->stack, 0)->next;
	const eei_rule_item * rule = eei_find_rule(token->token, expected);
	int foundExpected = IS_RULE_VALID(rule->rule);

	if (foundExpected && (expected == eei_rule_prefix))
		//This is an expected prefix token
		eei_parse_parsePrefix(parser, rule, token);
	else
	{
		//This is either a infix or postfix
		//Postfix binds strongest of them all, so it is searched for first

		//Do not perform an additional search if a postfix was actually expected
		const eei_rule_item * postRule =
				(expected != eei_rule_postfix)
				? eei_find_rule(token->token, eei_rule_postfix)
				: rule;

		if (IS_RULE_VALID(postRule->rule))
		{
			//A postfix was found
			foundExpected = 1;

			eei_parse_foldPrefix(parser);
			eei_parse_foldPrecedence(parser, postRule);
			eei_parse_parsePostfix(parser, postRule, token);
		}
		else if (foundExpected && (expected != eei_rule_postfix))
		{
			//A postfix was not found - but we weren't expecting one anyway.
			//This will not be executed if a postfix was actually expected!

			//Fold all prefixes before going any further
			//	since they bind stonger than the infixes.
			eei_parse_foldPrefix(parser);

			eei_parse_foldPrecedence(parser, rule);
			eei_parse_parseInfix(parser, rule, token);
		}
	}

	//The token was processed
	if (foundExpected)
		return;

	//The current token is not any expected token - it must be some END delimiter
	eei_parse_foldEndDilimiter(parser, token);
}

void eei_parse_init(eei_parser * parser, const ee_char_type * expression)
{
	eei_parser_node node;
	eei_parser_token token;

	node.token_start = expression;
	node.token_end = expression;
	node.precedence = 0;
	node.next = eei_rule_prefix;

	//Push a "start" rule - this will simplify things during processing
	node.rule = eei_find_rule(MAKE_SIMPLE_TOKEN(eei_token_sof), eei_rule_prefix);
	eei_stack_push(&parser->stack, &node);

	//Start a group - this allows to remove many tests for an empty stack
	token.token = MAKE_SIMPLE_TOKEN(eei_token_group);
	token.start = expression;
	token.end = expression;
	eei_parse_pushGroupRule(parser, &token);

	parser->status = ee_parser_ok;
	parser->error_token.token = MAKE_SIMPLE_TOKEN(eei_token_sof);
	parser->error_token.start = expression;
	parser->error_token.end = expression;
}

int eei_parse_expression(eei_parser * parser, const ee_char_type * expression)
{
	eei_lexer_state lexer_state;
	lexer_state.head = expression;

	eei_parse_init(parser, expression);

	while (parser->status == ee_parser_ok)
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

	if (parser->stack.top || (parser->status != ee_parser_ok))
		return -1;

	return 0;
}


//Parser handler functions
//------------------------

ee_parser_reply eei_rule_handler_number(eei_parser * parser, const eei_parser_node * node)
{

}

ee_parser_reply eei_rule_handler_comma(eei_parser * parser, const eei_parser_node * node)
{

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
	int arity;

	//Pointer to the one-past stack top
	//When pushing this address is the new write address
	ee_variable_type * stack_top;

	//The VM bytecode to execute next
	const eei_vm_bytecode * instruction;

	//One-past the last instruction to execute
	const eei_vm_bytecode * instruction_end;
} eei_vm_runtime;

//Exeucute the VM environment
int eei_vm_execute(const eei_vm_environment * vm_environment)
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

			case eei_vm_insturction_function:
			{
				int error;
				ee_variable_type function_result;

				//Adjust the stack by the arity so the function would receive
				//	the correct actuals
				rt.stack_top -= rt.arity;

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

			case eei_vm_insturction_arity:
				//Set the arity for the next function to execute
				rt.arity = rt.accumulator;

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;
		}

	};

	return 0;
}


//External API
//------------

//Helper macro to calculate alignment of a type
#define alignof(type) ((int)&((struct { char c; type d; } *)0)->d)

//Helper structure to simplify alignment management
typedef struct
{
	ee_environment_header header;

	//Counts of the various elements
	int constants;
	int variables;
	int functions;

	ee_environment_element data[1];
} ee_environment_struct;


//Initialize the VM from the environemt
int ee_vm_init(
		const ee_environment_struct * environment,
		eei_vm_environment * vm_environment,
		eei_vm_runtime * vm_runtime)
{
	vm_runtime->stack_top = environment->header.stack;

}


ee_parser_reply ee_guestimate(const ee_char_type * expression, ee_data_size * size)
{
	int identifiers = 0;
	int numbers = 0;
	int actuals = 0;
	int groups = 0;
	int operators = 0;
	int estimate = 0;

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
	size->compilation_stack = numbers + identifiers + operators * 2 + groups * 2;
	size->runtime_stack = actuals + groups * 2 + operators + identifiers;

	//Calculate the sizes

	size->compilation_size =
			sizeof(ee_compilation_header)
			+ alignof(eei_parser) + sizeof(eei_parser)
			+ alignof(eei_parser_node) + sizeof(eei_parser_node) * size->compilation_stack;

	size->environment_size =
			sizeof(ee_environment_struct)
			+ alignof(ee_variable_type) + sizeof(ee_variable_type) * size->constants
			+ alignof(ee_variable_type) + sizeof(ee_variable_type) * size->variables
			+ alignof(ee_function) + sizeof(ee_function) * size->functions
			+ alignof(eei_vm_bytecode) + sizeof(eei_vm_bytecode) * size->instructions;

	size->stack_size =
			sizeof(ee_variable_type) * size->runtime_stack;

	size->full_environment_size =
			size->environment_size
			+ alignof(ee_variable_type) + size->stack_size;

	return (token_type == eei_token_error) ? ee_parser_error : ee_parser_ok;
}

ee_parser_reply ee_compile(
		const ee_char_type * expression,
		ee_data_size *size,
		ee_compilation_header * compilation,
		ee_environment environment,
		const ee_compilation_data *data)
{
	//TODO: Prepare symbol tables
	//TODO: Correctly setup the parser structure from the environment
	eei_parser_node stack[64];
	eei_parser parser;
	parser.stack.stack = stack;
	parser.stack.size = 64;
	parser.stack.top = 0;

	return eei_parse_expression(&parser, expression);
}

int ee_evaluate(ee_environment environment, ee_variable result)
{
	eei_vm_environment vm_environment;
	int error;

	//TODO: Fill the VM environment

	error = eei_vm_execute(&vm_environment);

	//Extract the top of the stack and return it as the result
	*result = *vm_environment.stack;

	return error;
}

//System verification
//-------------------

//This comes to make sure we don't get any wierd surprises on esoteric systems
#if __CHAR_BIT__ < 8
#	error Non conformat system! A char must be, at least, 8 bits wide.
#endif

//This structure is just a clever way to make sure the sizes of the basic data types are exactly what we expect them to be.
//When a check fails the compilation will halt with an error of: "check type" declared as an array with a negative size
struct _check_type_sizes
{
	int not_enough_bits_for_token_type[((1 << eei_rule_bits_token_type_size) >= eei_token_sentinel) ? 1 : -1];
	int not_enough_bits_for_token[(sizeof(eei_token) * __CHAR_BIT__ >= eei_rule_bits_token_size) ? 1 : -1];
	int not_enough_bits_for_rule[(sizeof(eei_rule) * __CHAR_BIT__ >= eei_rule_bits_rule_size) ? 1 : -1];
	int not_enough_bits_for_rule_description[(sizeof(eei_rule_description) * __CHAR_BIT__ >= eei_rule_bits_total_size) ? 1 : -1];
};
