/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "emexpr.h"
#include "eei_rules.h"


//User-selectable definitions
//---------------------------

//Allow the constant scanner/parser to look for a fractional part.
#define EEI_CONSTANT_FRACT 1

//Allow using prefix operators with functional syntax
#define EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS 1

//Allow calling functions by name
#define EEI_ALLOW_FUNCTIONS 1

//Allow assigning into variables
#define EEI_ALLOW_ASSIGN 1

//Allow comparison operators into variables
#define EEI_ALLOW_COMPARE 1

//Allow the selection ?: operator
#define EEI_ALLOW_SELECT 1

//Allow logical operations
#define EEI_ALLOW_LOGIC 1

//Allow simple mathematical operators
#define EEI_ALLOW_SIMPLE_MATH 1

//Allow chaning(N-ary) operators
#define EEI_ALLOW_CHAIN_OP 1

//Allow the modulus operator.
#define EEI_ALLOW_MATH_MOD 1

//Use the default implementation of the modulus operator.
//Will only compile for integer variables.
#define EEI_USE_MATH_MOD 0

//Allow bit manipulation operators.
#define EEI_ALLOW_BIT 1

//Allow the xor(^) operator when bit manupulations are allowed.
#define EEI_ALLOW_BIT_XOR 1

//Use the default implementation of the bit manipulation operators.
//Will only compile for integer variables.
#define EEI_USE_BIT 0

//Allow the power operator. No default implementation.
#define EEI_ALLOW_POWER_MATH 0

//Allow parsing of postfix operators.
//Even when enabled none are actually defined in the default library.
#define EEI_ALLOW_POSTFIX 1


//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_CONSTANT_SCANNER) && defined(EE_USER_CONSTANT_PARSER)
#	define EEI_DEFAULT_CONSTANT 0
#else
#	define EEI_DEFAULT_CONSTANT 1
#endif

#if defined(EE_USER_FUNCTION_LIBRARY)
#	define EEI_DEFAULT_LIBRARY 0
#else
#	define EEI_DEFAULT_LIBRARY 1
#endif

#if defined(EE_USER_PARSER_RULES)
#	define EEI_DEFAULT_PARSER_RULES 0
#else
#	define EEI_DEFAULT_PARSER_RULES 1
#endif

//Disallow the xor & power at the same time since they use the same operator token
#if EEI_ALLOW_BIT && EEI_ALLOW_BIT_XOR && EEI_ALLOW_POWER_MATH
#	undef EEI_ALLOW_POWER_MATH
#	define EEI_ALLOW_POWER_MATH 0
#endif


//This provides the library defaults.
//Theese are implementation defaults of things the user can modify/override.
//--------------------------------------------------------------------------


#if EEI_DEFAULT_CONSTANT

//Lexer constant scanner/parser
//-----------------------------

const ee_char_type * eei_constant_scanner(const ee_char_type * start)
{
	//A naive base-10 scanner

	while ( (*start >= '0') && (*start <= '9') )
		++start;

#	if EEI_CONSTANT_FRACT
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
#	endif

	return start;
}

int eei_constant_parser(const ee_char_type * start, const ee_char_type * end, ee_variable_type * result)
{
	//A naive base-10 parser
	enum {numeric_base = 10};

	ee_variable_type integer = 0;

#	if EEI_CONSTANT_FRACT
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
#	else
	while (start < end)
	{
		integer *= numeric_base;
		integer += *start - '0';
		start++;
	}

	*result = integer;
#	endif

	return 0;
}

#endif

#if EEI_DEFAULT_LIBRARY

//The default operators library.
//To be used in conjunctions with the default parser rules.
//---------------------------------------------------------

#if EEI_ALLOW_SIMPLE_MATH
int eei_operator_subneg(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	switch (arity)
	{
		case 1:
			*result = -actuals[0];
			return 0;

		case 2:
			*result = actuals[0] - actuals[1];
			return 0;

		default:
			return 1;
	}
}

int eei_operator_plus(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
#	if EEI_ALLOW_CHAIN_OP
	ee_variable_type r = actuals[0];
	for (int i = 1; i < arity; ++i)
		r += actuals[i];
	*result = r;
#	else
	(void)arity;
	*result = actuals[0] + actuals[1];
#	endif
	return 0;
}

int eei_operator_mul(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
#	if EEI_ALLOW_CHAIN_OP
	ee_variable_type r = actuals[0];
	for (int i = 1; i < arity; ++i)
		r *= actuals[i];
	*result = r;
#	else
	(void)arity;
	*result = actuals[0] * actuals[1];
#	endif
	return 0;
}

int eei_operator_div(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] / actuals[1];
	return 0;
}
#endif

#if EEI_ALLOW_MATH_MOD && EEI_USE_MATH_MOD
int eei_operator_mod(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] % actuals[1];
	return 0;
}
#endif

#if EEI_ALLOW_COMPARE
int eei_operator_equal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] == actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_greater(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] > actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_less(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] < actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_notequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] != actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_greaterequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] >= actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_lessequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] <= actuals[1]) ? 1 : 0;
	return 0;
}
#endif

#if EEI_ALLOW_SELECT
int eei_operator_select(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] != 0) ? actuals[1] : actuals[2];
	return 0;
}
#endif

#if EEI_ALLOW_LOGIC
int eei_operator_fold_and(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	int r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r != 0); ++i)
		r &= (actuals[i] != 0) ? 1 : 0;

	*result = (r & 1) ? 1 : 0;
	return 0;
}

int eei_operator_fold_or(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	int r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r == 0); ++i)
		r |= (actuals[i] != 0) ? 1 : 0;

	*result = (r & 1) ? 1 : 0;
	return 0;
}

int eei_operator_fold_xor(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	int r = (actuals[0] != actuals[1]) ? 1 : 0;

	for (int i = 2; i < arity; ++i)
		r ^= (actuals[i] != 0) ? 1 : 0;

	*result = (r & 1) ? 1 : 0;
	return 0;
}

int eei_operator_not(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] == 0) ? 1 : 0;
	return 0;
}
#endif

#if EEI_ALLOW_BIT && EEI_USE_BIT
int eei_operator_fold_bitand(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = actuals[0];

	for (int i = 1; (i < arity) && (r != 0); ++i)
		r &= actuals[i];

	*result = r;
	return 0;
}

int eei_operator_fold_bitor(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = actuals[0];

	for (int i = 1; (i < arity) && (r == 0); ++i)
		r |= actuals[i];

	*result = r;
	return 0;
}

#if EEI_ALLOW_BIT_XOR
int eei_operator_fold_bitxor(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = actuals[0] ^ actuals[1];

	for (int i = 2; i < arity; ++i)
		r ^= actuals[i];

	*result = r;
	return 0;
}
#endif

int eei_operator_bitinv(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = ~actuals[0];
	return 0;
}
#endif

#if EEI_ALLOW_CHAIN_OP
enum { ChainArity = -3 };
#else
enum { ChainArity = 2 };
#endif

const ee_symboltable_function eei_operators_library[] =
{
#	if EEI_ALLOW_SIMPLE_MATH
	{eei_operator_subneg,"-",1,ee_function_flag_prefix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_subneg,"-",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_plus,"+",ChainArity,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_mul,"*",ChainArity,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_div,"/",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

#	if EEI_ALLOW_MATH_MOD && EEI_USE_MATH_MOD
	{eei_operator_mod,"%",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

#	if EEI_ALLOW_COMPARE
	{eei_operator_equal,"==",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greater,">",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_less,"<",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_notequal,"!=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greaterequal,">=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_lessequal,"<=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

#	if EEI_ALLOW_SELECT
	{eei_operator_select,"?",3,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

#	if EEI_ALLOW_LOGIC
	{eei_operator_fold_and,"&&",-2, ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_or,"||",-2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_xor,"^^",-3,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_not,"!",1, ee_function_flag_prefix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

#	if EEI_ALLOW_BIT && EEI_USE_BIT
	{eei_operator_fold_bitand,"&",-2, ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_bitor,"|",-2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	if EEI_ALLOW_BIT_XOR
	{eei_operator_fold_bitxor,"^",-3,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
#	endif
	{eei_operator_bitinv,"~",1, ee_function_flag_prefix | ee_function_flag_operator | ee_function_flag_pure},
#	endif

	{0,0,0,0}
};

#endif

#if EEI_DEFAULT_PARSER_RULES

//Parser rule tables
//------------------

static const eei_rule_description eei_parser_prefix_rules[] =
{
	HANDLE(TERMINAL(SIMPLE_TOKEN(eei_token_constant)),eei_rule_handle_constant),

	//Identifier folding is delayed since they can be both varaible and function names
	HANDLE(DELAYED(TERMINAL(SIMPLE_TOKEN(eei_token_identifier))),eei_rule_handle_variable),

#	if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
	//Catch-all for all operators
	//Prefix-operators folding is delayed to have the same stack layout as regular functions when folding
	HANDLE(DELAYED(PREFIX(SIMPLE_TOKEN(eei_token_operator))),eei_rule_handle_prefix),

	//Prefix-operator call
	LOOKBEHIND(PREFIX(TOKEN(eei_token_delimiter,'('))),
#	else
#	if EEI_ALLOW_SIMPLE_MATH
	HANDLE(PREFIX(TOKEN(eei_token_operator,'-')),eei_rule_handle_prefix),
#	endif
#	if EEI_ALLOW_LOGIC
	HANDLE(PREFIX(TOKEN(eei_token_operator,'!')),eei_rule_handle_prefix),
#	endif
#	if EEI_ALLOW_BIT
	HANDLE(PREFIX(TOKEN(eei_token_operator,'~')),eei_rule_handle_prefix),
#	endif
#	endif

	//Grouping parens
	DELIMITED(PREFIX(TOKEN(eei_token_delimiter,'('))),

	//Closing parens. Will produce different rules depending on the current group
	GROUPED(TERMINAL(TOKEN(eei_token_delimiter,')'))),

	//Expression end for an empty expression. Only allowed inside the SOF group
	LOOKBEHIND(GROUPED(PREFIX(EOF_TOKEN()))),

	SENTINEL_RULE()
};

static const eei_rule_description eei_parser_infix_rules[] =
{
#	if EEI_ALLOW_ASSIGN
	//Variable assignment, only allowed in the SOF group
	HANDLE(GROUPED(NOFOLD(INFIX(TOKEN(eei_token_operator,'='), eei_precedence_assign))),eei_rule_handle_assign),
#	endif

#	if EEI_ALLOW_SELECT
	//Select operator
	ERROR(INFIX(TOKEN(eei_token_operator,'?'), eei_precedence_select)),
	ERROR(INFIX(TOKEN(eei_token_operator,':'), eei_precedence_select_else)),
#	endif

	//Sequence delimiter
	HANDLE(GROUPED(INFIX(TOKEN(eei_token_delimiter,','), eei_precedence_comma)),eei_rule_handle_delimiter),

#	if EEI_ALLOW_FUNCTIONS
	//Function call
	DELIMITED(INFIX(TOKEN(eei_token_delimiter,'('), eei_precedence_function)),
#	endif

	//Closing parens for groups and functions calls. Will produce different rules depending on the current group.
	//The group folded by this rule could be considered as a post-fix, thus an infix must follow.
	GROUPED(INFIX(TOKEN(eei_token_delimiter,')'), eei_precedence_group)),

	//Normal operators

#	if EEI_ALLOW_LOGIC
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_or), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_xor), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_and), eei_precedence_logical_and), eei_rule_handle_infix),
#	endif

#	if EEI_ALLOW_COMPARE
	HANDLE(INFIX(TOKEN(eei_token_operator,'<'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'>'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_eq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_neq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_gte), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_lte), eei_precedence_compare), eei_rule_handle_infix),
#	endif

#	if EEI_ALLOW_SIMPLE_MATH
	HANDLE(INFIX(TOKEN(eei_token_operator,'+'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'-'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'*'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'/'), eei_precedence_power2), eei_rule_handle_infix),
#	endif

#	if EEI_ALLOW_BIT
	HANDLE(INFIX(TOKEN(eei_token_operator,'|'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'&'), eei_precedence_power2), eei_rule_handle_infix),
#	if EEI_ALLOW_BIT_XOR
	HANDLE(INFIX(TOKEN(eei_token_operator,'^'), eei_precedence_power2), eei_rule_handle_infix),
#	endif
#	endif

#	if EEI_ALLOW_MATH_MOD
	HANDLE(INFIX(TOKEN(eei_token_operator,'%'), eei_precedence_power2), eei_rule_handle_infix),
#	endif

#	if EEI_ALLOW_POWER_MATH
	//A right-binding operator
	HANDLE(RIGHTINFIX(TOKEN(eei_token_operator,'^'), eei_precedence_power3), eei_rule_handle_infix),
#	endif

	//Expression end for an normal expression. Will fold everything before closing the group.
	GROUPED(INFIX(EOF_TOKEN(), eei_precedence_group)),

	SENTINEL_RULE()
};

static const eei_rule_description eei_parser_postfix_rules[] =
{
#	if EEI_ALLOW_POSTFIX
	HANDLE(POSTFIX(SIMPLE_TOKEN(eei_token_identifier)), eei_rule_handle_postfix),
#	endif

	SENTINEL_RULE()
};

static const eei_conditional_table_item eei_parser_lookbehind_rules[] =
{
#	if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
	//Prefix-operator call
	{
		PREFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		PREFIX_RULE(SIMPLE_TOKEN(eei_token_operator)),
		DELIMITED(INFIX(TOKEN(eei_token_delimiter,'('), eei_precedence_function)),
	},
#	endif

	{
		PREFIX_RULE(EOF_TOKEN()),
		GROUP_RULE(),
		COPY_RULE()
	},

	{SENTINEL_RULE(),SENTINEL_RULE(),SENTINEL_RULE()}
};

static const eei_conditional_table_item eei_parser_group_rules[] =
{
	//Prefix closing parens of grouping parens, group of group or error
	{
		PREFIX_RULE(TOKEN(eei_token_delimiter,')')),
		PREFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		END_RULE(eei_rule_handle_group)
	},

	//Infix closing parens of grouping parens
	{
		INFIX_RULE(TOKEN(eei_token_delimiter,')')),
		PREFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		END_RULE(eei_rule_handle_group)
	},

	//Prefix closing parents of a function call, 0-arity or error
	{
		PREFIX_RULE(TOKEN(eei_token_delimiter,')')),
		INFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		END_RULE(eei_rule_handle_function)
	},

	//Infix closing parents of a function call
	{
		INFIX_RULE(TOKEN(eei_token_delimiter,')')),
		INFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		END_RULE(eei_rule_handle_function)
	},

	//Sequence delimiter of a function call
	{
		INFIX_RULE(TOKEN(eei_token_delimiter,',')),
		INFIX_RULE(TOKEN(eei_token_delimiter,'(')),
		COPY_RULE()
	},

#	if EEI_ALLOW_ASSIGN
	//Variable assignment, only allowed in the SOF group
	{
		INFIX_RULE(TOKEN(eei_token_operator,'=')),
		PREFIX_RULE(SOF_TOKEN()),
		COPY_RULE()
	},
#	endif

	//Normal expression end
	{
		INFIX_RULE(EOF_TOKEN()),
		PREFIX_RULE(SOF_TOKEN()),
		END_RULE(eei_rule_handle_none)
	},

	//Empty expression or unexpected end
	{
		PREFIX_RULE(EOF_TOKEN()),
		PREFIX_RULE(SOF_TOKEN()),
		END_RULE(eei_rule_handle_none)
	},

	{SENTINEL_RULE(),SENTINEL_RULE(),SENTINEL_RULE()}
};

static const eei_conditional_table_item eei_parser_fold_rules[] =
{
#	if EEI_ALLOW_CHAIN_OP
	{
		INFIX_RULE(TOKEN(eei_token_operator,'+')),
		INFIX_RULE(TOKEN(eei_token_operator,'+')),
		COPY_RULE()
	},
	{
		INFIX_RULE(TOKEN(eei_token_operator,'*')),
		INFIX_RULE(TOKEN(eei_token_operator,'*')),
		COPY_RULE()
	},
#	endif

#	if EEI_ALLOW_SELECT
	//Second half of a select operator
	{
		INFIX_RULE(TOKEN(eei_token_operator,':')),
		INFIX_RULE(TOKEN(eei_token_operator,'?')),
		HANDLE(TYPE_RULE(INFIX(TOKEN(eei_token_operator,'?'), eei_precedence_select), eei_rule_end),eei_rule_handle_infix)
	},
	#	endif

	{SENTINEL_RULE(),SENTINEL_RULE(),SENTINEL_RULE()}
};

const ee_parser_rules eei_parser_rules =
{
	eei_parser_prefix_rules,
	eei_parser_infix_rules,
	eei_parser_postfix_rules,
	eei_parser_lookbehind_rules,
	eei_parser_group_rules,
	eei_parser_fold_rules
};

#endif //#if EEI_DEFAULT_RULES
