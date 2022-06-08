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
	//DO NOT use it in the actual tokens since it may not fit in the allocated bits!
	eei_token_sentinel
} eei_token_type;

typedef struct
{
	const char * start;
	const char * head;

	int number_integer;
	int number_fractional;
	int number_exponent;
} eei_lexer_state;

static inline int eei_lexer_is_eof(const char c)
{
	return c == '\0';
}

static inline int eei_lexer_is_space(const char c)
{
	return
			(c == ' ') || (c == '\t') || (c == '\r') || (c == '\n');
}


static inline int eei_lexer_is_alpha(const char c)
{
	return
			((c >= 'a') && (c <= 'z'))
			|| ((c >= 'A') && (c <= 'Z'))
			|| ( c == '_');
}

static inline int eei_lexer_is_number(const char c)
{
	return (c >= '0') && (c <= '9');
}

static inline int eei_lexer_is_delimiter(const char c)
{
	return
			(c == ',')
			|| (c == '(') || (c == ')')
			|| (c == '[') || (c == ']')
			|| (c == '{') || (c == '}');
}

static inline int eei_lexer_is_operator(const char c)
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

eei_token_type eei_lexer_next_token(eei_lexer_state * state)
{
	//Parse the input stream for the next token

	//Skip spaces
	while (eei_lexer_is_space(*state->head)) state->head++;

	state->start = state->head;

	if (eei_lexer_is_eof(*state->head))
		return eei_token_eof;

	//The first character determines the token class

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

	if (eei_lexer_is_alpha(*state->head))
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

	if (eei_lexer_is_number(*state->head))
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
typedef unsigned short int eei_token;

//Holds a parsing rule
typedef unsigned short int eei_rule;

//Holds a parsing rule description
typedef unsigned int eei_rule_description;

enum
{
	//Field sizes
	eei_rule_bits_token_char_size = __CHAR_BIT__,
	eei_rule_bits_token_type_size = 3,
	eei_rule_bits_type_size = 2,
	eei_rule_bits_precedence_size = 8,
	eei_rule_bits_accosiativity_size = 1,
	eei_rule_bits_end_delimiter_size = 1,

	eei_rule_bits_token_size = eei_rule_bits_token_char_size + eei_rule_bits_token_type_size,
	eei_rule_bits_rule_size = eei_rule_bits_token_size + eei_rule_bits_type_size,
	eei_rule_bits_next_size = eei_rule_bits_type_size,
	eei_rule_bits_start_offset = 0,

	//Token
	eei_rule_bits_token_char_offset = eei_rule_bits_start_offset,
	eei_rule_bits_token_type_offset = eei_rule_bits_token_char_offset + eei_rule_bits_token_char_size,
	eei_rule_bits_token_offset = eei_rule_bits_start_offset,

	//Rule
	eei_rule_bits_type_offset = eei_rule_bits_token_offset + eei_rule_bits_token_size,
	eei_rule_bits_rule_offset = eei_rule_bits_token_offset,

	//Rule description
	eei_rule_bits_precedence_offset = eei_rule_bits_rule_offset + eei_rule_bits_rule_size,
	eei_rule_bits_next_offset = eei_rule_bits_precedence_offset + eei_rule_bits_precedence_size,
	eei_rule_bits_accosiativity_offset = eei_rule_bits_next_offset + eei_rule_bits_next_size,
	eei_rule_bits_end_delimiter_offset = eei_rule_bits_accosiativity_offset + eei_rule_bits_accosiativity_size,

	//Value used to validate the size does not overflows the data type used for the rule
	eei_rule_bits_total_size = eei_rule_bits_end_delimiter_offset + eei_rule_bits_end_delimiter_size
} eei_rule_bits;

#define MAKE_PART_BITS(data, offset, size) (((data) & ((1 << size)-1)) << offset)
#define GET_PART_BITS(data, offset, size) (((data) >> offset) & ((1 << size)-1))

#define GET_TOKEN(rule_description) \
	((eei_token)GET_PART_BITS(rule_description, eei_rule_bits_token_offset, eei_rule_bits_token_size))

#define MAKE_TOKEN(token_type, token_symbol) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size) |\
	MAKE_PART_BITS(token_symbol, eei_rule_bits_token_char_offset, eei_rule_bits_token_char_size) \
	)

#define MAKE_SIMPLE_TOKEN(token_type) \
	(eei_token)(\
	MAKE_PART_BITS(token_type, eei_rule_bits_token_type_offset, eei_rule_bits_token_type_size) |\
	MAKE_PART_BITS('\0', eei_rule_bits_token_char_offset, eei_rule_bits_token_char_size) \
	)

#define MAKE_RULE(token, type) \
	(eei_rule)(\
	MAKE_PART_BITS(token, eei_rule_bits_token_offset, eei_rule_bits_token_size) |\
	MAKE_PART_BITS(type, eei_rule_bits_type_offset, eei_rule_bits_type_size) \
	)

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

static const eei_rule_description eei_parser_prefix_rules[] =
{
	//Expression start
	MAKE_DELIMITED_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_sof)),

	MAKE_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_number),0,eei_rule_infix),
	MAKE_PREFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier),0,eei_rule_infix),

	//Grouping parens
	MAKE_DELIMITED_PREFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'(')),

	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'!')),
	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'~')),
	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'&')),
	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'|')),
	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'+')),
	MAKE_DEFAULT_PREFIX_RULE(MAKE_TOKEN(eei_token_operator,'-')),

	MAKE_SENTINEL_RULE()
};

static const eei_rule_description eei_parser_infix_rules[] =
{
	//Sequence delimiter
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,','), eei_precedence_comma),

	//Function call
	MAKE_DELIMITED_INFIX_RULE(MAKE_TOKEN(eei_token_delimiter,'('), eei_precedence_function),

	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'|'), eei_precedence_logical_or),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'&'), eei_precedence_logical_and),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'='), eei_precedence_compare),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'<'), eei_precedence_compare),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'>'), eei_precedence_compare),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'+'), eei_precedence_power1),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'-'), eei_precedence_power1),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'*'), eei_precedence_power2),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'/'), eei_precedence_power2),
	MAKE_DEFAULT_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'%'), eei_precedence_power2),
	MAKE_INFIX_RULE(MAKE_TOKEN(eei_token_operator,'^'), eei_precedence_power3, eei_rule_right, 0),

	MAKE_SENTINEL_RULE()
};

static const eei_rule_description eei_parser_postfix_rules[] =
{
	MAKE_POSTFIX_RULE(MAKE_SIMPLE_TOKEN(eei_token_identifier), eei_precedence_postfix),

	MAKE_SENTINEL_RULE()
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


eei_rule_description eei_find_rule(eei_token token, eei_rule_type expected)
{
	//Find the rule of the expected type matching the given token
	//Will return the sentinel rule on failure

	const eei_rule_description * table;

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
	while (*table != MAKE_SENTINEL_RULE())
	{
		if (token == GET_TOKEN(*table))
			break;
		table++;
	}

	return *table;
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
	eei_rule_description rule;
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


int ee_guestimate(const char * expression, ee_environment_header *header)
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
	header->max_stack = actuals * 2 + groups + operators;
	header->internal = identifiers * 2 + operators;

	estimate = sizeof(ee_environment_header);
	estimate += sizeof(ee_variable_type) * header->constants;
	estimate += sizeof(ee_variable_type) * header->max_stack;
	estimate += sizeof(ee_environment_element) * header->internal;

	return (token_type == eei_token_error) ? -estimate : estimate;
}

int ee_compile(const char * expression,
		ee_environment environment,
		const ee_compilation_data *data)
{
	eei_lexer_state lexer_state;
	eei_token_type current_token = eei_token_error;



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
