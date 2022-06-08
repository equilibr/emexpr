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


//Parser rules
//------------

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

//Holds a parsing rule description
//A description consists of:
//	A rule - when this token is expected
//	Its associativity & precedence - how to combine it with other rules
//	Delimited flag - when set this rule must have a matching end-dilimiter rule
//	Next - the rule type to expected next
typedef unsigned int eei_rule_description;

typedef void (*eei_rule_handler)(void);

//A rule table item to hold rules and their handler functions
typedef struct
{
	//Rule description
	eei_rule_description rule;

	//Handler function for the rule
	eei_rule_handler handler;
} eei_rule_item;

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

	STATE(MAKE_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_number),0,eei_rule_infix)),
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
	STATE(MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,','), eei_precedence_comma)),

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

eei_rule_description eei_find_end_rule(eei_token token, eei_rule_description rule)
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

	return table[0][1];
}


//Parser stack
//------------

typedef struct
{
	//The rule being processed
	const eei_rule_item * rule;

	//The source token for the rule being processed
	const ee_char_type * token_start;
	const ee_char_type * token_end;

	//The token type expected next
	eei_rule_type next;

	//The precedence of the current node
	char precedence;
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


int eei_stack_push(eei_parser_stack * stack, const eei_parser_node * node)
{
	//Push a node to the top of the stack
	//Return on-zero on error

	if (stack->top >= stack->size)
		return 1;

	stack->stack[stack->top].rule = node->rule;
	stack->stack[stack->top].token_start = node->token_start;
	stack->stack[stack->top].token_end = node->token_end;
	stack->stack[stack->top].next = node->next;
	stack->stack[stack->top].precedence = node->precedence;

	stack->top++;

	return 0;
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



//Parser
//------

typedef struct
{
	eei_parser_stack stack;
	const ee_compilation_data * data;
	eei_token current_token;
} eei_parser;

int eei_parse_token(eei_parser * parser)
{

}

int eei_parse_expression(eei_parser * parser, const ee_char_type * expression)
{
	eei_lexer_state lexer_state;

	lexer_state.head = expression;

	//TODO: Push a fake "start" rule - this will simplify things during processing

	//TODO: Start a group - this allows to remove many tests for an empty stack

	while (parser->stack.top > 1)
	{
		const eei_token_type current_token = eei_lexer_next_token(&parser);

		//TODO: Create a complete token
		//parser->current_token = ...;

		while (!eei_parse_token(parser));
	}

	//This should never happen and indicates an undetected discrepancy during parsing
	if (parser->stack.top != 1)
		return -1;

	return 0;
}


//External API
//------------

//Helper structure to simplify alignment management
typedef struct
{
	ee_environment_header header;

	//Counts of the various elements
	int constants;
	int variables;
	int functions;

	ee_environment_element data[0];
} ee_environment_struct;


//Initialize the VM from the environemt
int ee_vm_init(
		const ee_environment_struct * environment,
		eei_vm_environment * vm_environment,
		eei_vm_runtime * vm_runtime)
{
	vm_runtime->stack_top = environment->header.stack;

}


int ee_guestimate(const ee_char_type * expression, ee_environment_header *header)
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

	header->constants = numbers;

	//Runtime maximum stack size
	header->max_stack = actuals + groups * 2 + operators + identifiers;

	//Concrete syntax tree maximum depth
	header->internal = identifiers * 2 + groups;

	estimate = sizeof(ee_environment_header);
	estimate += sizeof(ee_variable_type) * numbers;
	estimate += sizeof(ee_variable_type*) * identifiers;
	estimate += sizeof(ee_variable_type) * header->max_stack;
	estimate += sizeof(ee_environment_element) * header->internal;

	return (token_type == eei_token_error) ? -estimate : estimate;
}

int ee_compile(const ee_char_type * expression,
		ee_environment environment,
		const ee_compilation_data *data)
{
	//TODO: Prepare symbol tables
	//TODO: Correctly setup the parser structure from the environment
	eei_parser parser;

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
