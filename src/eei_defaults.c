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

//Allow using prefix operators with functional syntax
#define EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS 1

//Allow assigning into variables
#define EEI_ALLOW_ASSIGN 1


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

#if EEI_DEFAULT_LIBRARY

//The default operators library.
//To be used in conjunctions with the default parser rules.
//---------------------------------------------------------

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
	(void)arity;
	*result = actuals[0] + actuals[1];
	return 0;
}

int eei_operator_mul(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] * actuals[1];
	return 0;
}

int eei_operator_div(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] / actuals[1];
	return 0;
}

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

int eei_operator_fold_and(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r != 0); ++i)
		r *= (actuals[i] != 0) ? 1 : 0;

	*result = r;
	return 0;
}

int eei_operator_fold_or(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r == 0); ++i)
		r += (actuals[i] != 0) ? 1 : 0;

	*result = (r != 0) ? 1 : 0;
	return 0;
}

int eei_operator_fold_xor(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != actuals[1]) ? 1 : 0;

	for (int i = 2; i < arity; ++i)
		r = (actuals[i] != r) ? 1 : 0;

	*result = r;
	return 0;
}

int eei_operator_not(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] == 0) ? 1 : 0;
	return 0;
}

const ee_symboltable_function eei_operators_library[] =
{
	{eei_operator_subneg,"-",1,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_subneg,"-",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_plus,"+",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_mul,"*",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_div,"/",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},

	{eei_operator_equal,"==",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greater,">",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_less,"<",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_notequal,"!=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greaterequal,">=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_lessequal,"<=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},

	{eei_operator_fold_and,"&&",-2,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_or,"||",-2,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_xor,"^^",-3,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_not,"!",1, ee_function_flag_prefix | ee_function_flag_operator | ee_function_flag_pure},
	{0,0,0,0}
};

#endif

#if EEI_DEFAULT_PARSER_RULES

//Parser rule tables
//------------------

static const eei_rule_description eei_parser_prefix_rules[] =
{
	HANDLE(TERMINAL(SIMPLE_TOKEN(eei_token_constant)),eei_rule_handle_constant),
	HANDLE(DELAYED(TERMINAL(SIMPLE_TOKEN(eei_token_identifier))),eei_rule_handle_variable),

#	if EEI_ALLOW_PREFIX_OPERATOR_FUNCTIONS
	//Catch-all for all operators
	HANDLE(DELAYED(PREFIX(SIMPLE_TOKEN(eei_token_operator))),eei_rule_handle_prefix),

	//Prefix-operator call
	LOOKBEHIND(PREFIX(TOKEN(eei_token_delimiter,'('))),
#	else
	HANDLE(PREFIX(TOKEN(eei_token_operator,'-')),eei_rule_handle_prefix),
	HANDLE(PREFIX(TOKEN(eei_token_operator,'!')),eei_rule_handle_prefix),
#	endif

	//Grouping parens
	DELIMITED(PREFIX(TOKEN(eei_token_delimiter,'('))),

	//Closing parens. Will produce different rules depending on the current group
	GROUPED(TERMINAL(TOKEN(eei_token_delimiter,')'))),

	//Expression end for an empty expression. Only allowed inside the SOF group
	GROUPED(PREFIX(EOF_TOKEN())),

	SENTINEL_RULE()
};

static const eei_rule_description eei_parser_infix_rules[] =
{
#	if EEI_ALLOW_ASSIGN
	//Variable assignment, only allowed in the SOF group
	HANDLE(GROUPED(NOFOLD(INFIX(TOKEN(eei_token_operator,'='), eei_precedence_assign))),eei_rule_handle_assign),
#	endif

	//Sequence delimiter
	HANDLE(GROUPED(INFIX(TOKEN(eei_token_delimiter,','), eei_precedence_comma)),eei_rule_handle_delimiter),

	//Function call
	DELIMITED(INFIX(TOKEN(eei_token_delimiter,'('), eei_precedence_function)),

	//Closing parens for groups and functions calls. Will produce different rules depending on the current group.
	//The group folded by this rule could be considered as a post-fix, thus an infix must follow.
	GROUPED(INFIX(TOKEN(eei_token_delimiter,')'), eei_precedence_group)),

	//Normal operators
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_or), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_xor), eei_precedence_logical_or), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_and), eei_precedence_logical_and), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'<'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'>'), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_eq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_neq), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_gte), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,token_symbol_op_lte), eei_precedence_compare), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'+'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'-'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'|'), eei_precedence_power1), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'*'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'&'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'/'), eei_precedence_power2), eei_rule_handle_infix),
	HANDLE(INFIX(TOKEN(eei_token_operator,'%'), eei_precedence_power2), eei_rule_handle_infix),

	//A right-binding operator
	HANDLE(RIGHTINFIX(TOKEN(eei_token_operator,'^'), eei_precedence_power3), eei_rule_handle_infix),

	//Expression end for an normal expression. Will fold everything before closing the group.
	GROUPED(INFIX(EOF_TOKEN(), eei_precedence_group)),

	SENTINEL_RULE()
};

static const eei_rule_description eei_parser_postfix_rules[] =
{
	HANDLE(POSTFIX(SIMPLE_TOKEN(eei_token_identifier)), eei_rule_handle_postfix),

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

const ee_parser_rules eei_parser_rules =
{
	eei_parser_prefix_rules,
	eei_parser_infix_rules,
	eei_parser_postfix_rules,
	eei_parser_lookbehind_rules,
	eei_parser_group_rules
};

#endif //#if EEI_DEFAULT_RULES
