/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022-2024 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "emexpr.h"
#include "eei_rules.h"
#include "eei_symboltable.h"
#include "eei_vm.h"

#include <stddef.h>

//Notation and formalism:
// All implementation details have a prefix of "eei" - Embedded Expression Implementation
// All externally visible definition have a prefix of "ee" - Embedded Expression


//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_CONSTANT_SCANNER) && defined(EE_USER_CONSTANT_PARSER)
#	define EEI_CONSTANT_SCANNER EE_USER_CONSTANT_SCANNER
#	define EEI_CONSTANT_PARSER EE_USER_CONSTANT_PARSER
#else
#	define EEI_CONSTANT_SCANNER eei_constant_scanner
#	define EEI_CONSTANT_PARSER eei_constant_parser
#endif

#if defined(EE_USER_PARSER_RULES)
#	define EEI_PARSER_RULES EE_USER_PARSER_RULES
#else
#	define EEI_PARSER_RULES eei_parser_rules
#endif


//Prototypes for externally provided functions and data

const ee_char_type * EEI_CONSTANT_SCANNER(const ee_char_type * start);
int EEI_CONSTANT_PARSER(const ee_char_type * start, const ee_char_type * end, ee_variable_type * result);
extern const ee_parser_rules EEI_PARSER_RULES;



//Lexer
//-----

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
			(c == '+') || (c == '-') || (c == '*') || (c == '/')
			|| (c == '=') ||(c == '>') || (c == '<')
			|| (c == '?') || (c == ':') || (c == '\'') || (c == '.')
			|| (c == '%') ||(c == '^') || (c == '&') || (c == '|')
			|| (c == '~') ||(c == '!');
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

static eei_token eei_lexer_next_token(eei_lexer_state * state)
{
	//Parse the input stream for the next token

	//Skip spaces
	while (eei_lexer_is_space(*state->head)) state->head++;

	state->start = state->head;

	if (eei_lexer_is_eof(*state->head))
		return EOF_TOKEN();

	//The first character determines the token class

	if (eei_lexer_is_number(*state->head))
	{
		state->head = EEI_CONSTANT_SCANNER(state->head);

		if (state->head != state->start)
			return SIMPLE_TOKEN(eei_token_constant);
	}

	if (eei_lexer_is_alpha(*state->head))
	{
		eei_lexer_consume_identifier(state);
		return SIMPLE_TOKEN(eei_token_identifier);
	}

	if (eei_lexer_is_delimiter(*state->head))
	{
		//Delimiters are always single character
		state->head++;
		return TOKEN(eei_token_delimiter, *state->start);
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

			return TOKEN(eei_token_operator, op);
		}

		return TOKEN(eei_token_operator, *state->start);
	}

	return ERROR_TOKEN();
}


//Macros to simplify working with a ruleset
//-----------------------------------------

//Test if the rule is valid
#define IS_RULE_VALID(rule) ((rule) != SENTINEL_RULE())

#define GET_RULE_TYPE(rule) (eei_rule_type)GET_PART_BITS(rule, eei_rule_bits_type_offset, eei_rule_bits_type_size)
#define GET_RULE_PRECEDENCE(rule) (eei_precedence)GET_PART_BITS(rule, eei_rule_bits_precedence_offset, eei_rule_bits_precedence_size)
#define GET_RULE_NEXT(rule) (eei_rule)GET_PART_BITS(rule, eei_rule_bits_next_offset, eei_rule_bits_next_size)
#define GET_RULE_ACCOSIATIVITY(rule) (eei_rule_associativity)GET_PART_BITS(rule, eei_rule_bits_accosiativity_offset, eei_rule_bits_accosiativity_size)
#define GET_RULE_ENDDELIMITER(rule) GET_PART_BITS(rule, eei_rule_bits_end_delimiter_offset, eei_rule_bits_end_delimiter_size)
#define GET_RULE_FOLD(rule) (eei_rule_fold)GET_PART_BITS(rule, eei_rule_bits_fold_offset, eei_rule_bits_fold_size)
#define GET_RULE_LOOK_BEHIND(rule) GET_PART_BITS(rule, eei_rule_bits_look_behind_offset, eei_rule_bits_look_behind_size)
#define GET_RULE_GROUPED(rule) GET_PART_BITS(rule, eei_rule_bits_grouped_offset, eei_rule_bits_grouped_size)
#define GET_RULE_CONDITIONAL(rule) GET_PART_BITS(rule, eei_rule_bits_conditional_offset, eei_rule_bits_conditional_size)
#define GET_RULE_HANDLER(rule) GET_PART_BITS(rule, eei_rule_bits_handler_offset, eei_rule_bits_handler_size)

//Extract a token and data from a rule description
#define GET_RULE_TOKEN(rule_description) ((eei_token)GET_PART_BITS(rule_description, eei_rule_bits_token_offset, eei_rule_bits_token_size))
#define GET_RULE_TOKEN_TYPE(rule_description) GET_TOKEN_TYPE(GET_RULE_TOKEN(rule_description))
#define GET_RULE_TOKEN_SYMBOL(rule_description) GET_TOKEN_SYMBOL(GET_RULE_TOKEN(rule_description))

//Clear the token part of a rule
#define CLEAR_RULE_TOKEN(rule_description) ((rule_description) & ~BITMASKS(eei_rule_bits_token_size, eei_rule_bits_token_offset))


//Parser rules
//------------

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
	return (GET_RULE_TYPE(a) == GET_RULE_TYPE(b)) && eei_compare_tokens(a, b);
}

//Return a morphed rule descption given the original
static inline eei_rule_description eei_conditional_morph(eei_rule_description rule, eei_rule_description morphed)
{
	if (morphed == SENTINEL_RULE())
		return SENTINEL_RULE();

	if (GET_RULE_TOKEN(morphed) == COPY_TOKEN())
		//A copy rule - used only for group filtering of regular rules
		return rule;

	if ((GET_RULE_TOKEN(morphed) != END_TOKEN()) && (GET_RULE_TOKEN(morphed) != DELIMIT_TOKEN()))
		//A regular morphed rule description - use as-is
		return morphed;

	//Create a franken-rule
	eei_rule_description result = rule;

	//The token is always from the original rule
	result &= ~BITMASKS(eei_rule_bits_token_size,eei_rule_bits_token_offset);
	result |= MAKE_PART_BITS(GET_RULE_TOKEN(rule), eei_rule_bits_token_offset, eei_rule_bits_token_size);

	//The handler is always from the morphed rule
	result &= ~BITMASKS(eei_rule_bits_handler_size,eei_rule_bits_handler_offset);
	result |= MAKE_PART_BITS(GET_RULE_HANDLER(morphed), eei_rule_bits_handler_offset, eei_rule_bits_handler_size);

	if (GET_RULE_TOKEN(morphed) == END_TOKEN())
	{
		//The end rule overrides the rule type
		result &= ~BITMASKS(eei_rule_bits_type_size,eei_rule_bits_type_offset);
		result |= MAKE_PART_BITS(eei_rule_end, eei_rule_bits_type_offset, eei_rule_bits_type_size);
	}

	return result;
}

static inline eei_rule_description eei_conditional_find(
		const eei_conditional_table_item * cond_table,
		eei_rule_description rule,
		eei_rule_description condition)
{
	//We need to compare the conditional table with the previous(condition) token
	const eei_conditional_table_item * table = cond_table;

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it
	while (table->current != SENTINEL_RULE())
	{
		if (eei_compare_rules(rule, table->current) && eei_compare_rules(condition, table->condition))
			//We found a match in the conditional table - return the new rule
			return eei_conditional_morph(rule,table->morphed);

		table++;
	}

	//The look-behind did not match anything - keep searching
	return SENTINEL_RULE();
}

static eei_rule_description eei_find_rule(
		const ee_parser_rules * tables,
		eei_token token,
		eei_rule_type expected,
		eei_rule_description previous,
		eei_rule_description group)
{
	//Find the rule of the expected type matching the given token
	//Will return the sentinel rule on failure

	const eei_rule_description * table = NULL;

	switch (expected)
	{
		case eei_rule_prefix:
			table = tables->prefix;
			break;

		case eei_rule_infix:
			table = tables->infix;
			break;

		case eei_rule_postfix:
			table = tables->postfix;
			break;

		case eei_rule_end:
			//No one expects the end rule!
			return SENTINEL_RULE();
	}

	//Use a simple linear search since the tables are small and the
	// overhead of a binary search is not worth it.
	//Moreover, some behaviour below depends on certain rules being in sequence.
	for (; *table != SENTINEL_RULE(); ++table)
	{
		eei_rule_description tested = *table;

		if (!eei_compare_tokens(GET_RULE_TOKEN(tested), token))
			//The tokens don't match, no more tests needed
			continue;

		//Test for conditions, if exist.
		//For this to work properly, for a rule that has both conditional and unconditional forms,
		// the unconditional one must appear AFTER the conditional ones.

		if (GET_RULE_LOOK_BEHIND(tested))
		{
			//Compare the look-behind table with the previous token
			//Use the returned rule as the new rule for further tests
			tested = eei_conditional_find(tables->lookbehind, tested, previous);

			if (tested == SENTINEL_RULE())
				//The look-behind did not match anything - keep searching
				continue;
		}

		if (GET_RULE_GROUPED(tested))
		{
			//We need to compare the group table with the current group
			tested = eei_conditional_find(tables->group, tested, group);

			if (tested == SENTINEL_RULE())
				//The group did not match anything - keep searching
				continue;
		}

		//This rule has no additional constraints - we found what we're looking for!
		return tested;
	}

	return SENTINEL_RULE();
}


//Parser stack
//------------

typedef struct
{
	ee_element_count start;
	ee_element_count end;
} eei_text_location;

typedef struct
{
	//The rule being processed
	eei_rule_description rule;

	//The source token for the rule being processed
	eei_text_location text;

	//Count of elements inside this group (0 otherwise)
	ee_element_count elements;
} eei_parser_node;

//Holds management data for the parser stack that is used instead
//	of functional recursion to hold the parser data
typedef struct
{
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

static inline ee_parser_reply eei_stack_copynode(eei_parser_node * dst, const eei_parser_node * src)
{
	if (!dst || !src)
		return ee_parser_stack_error;

	dst->rule = src->rule;
	dst->text.start = src->text.start;
	dst->text.end = src->text.end;
	dst->elements = src->elements;

	return ee_parser_ok;
}

static inline ee_parser_reply eei_stack_push(eei_parser_stack * stack, const eei_parser_node * node)
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

static inline ee_parser_reply eei_stack_pop(eei_parser_stack * stack, eei_parser_node * node)
{
	if (stack->top == 0)
		return ee_parser_stack_underflow;

	if (eei_stack_copynode(node, &stack->stack[stack->top - 1]) != ee_parser_ok)
		return ee_parser_stack_error;

	stack->top--;
	return ee_parser_ok;
}

static inline eei_parser_node * eei_stack_top(eei_parser_stack * stack, int distance)
{
	//Get the element 'distance' from the stack top.
	//Returns stack bottom on error.

	if (distance >= stack->top)
		return &stack->stack[0];

	return &stack->stack[stack->top - distance - 1];
}


//Parser
//------

typedef struct
{
	eei_token token;
	eei_text_location text;
} eei_parser_token;

typedef struct
{
	eei_parser_stack stack;
	eei_vmmake_environment vm;
	eei_symboltable symboltable;
	ee_parser_rules rules;

	const ee_char_type * expression;

	eei_parser_token error_token;
	ee_parser_reply status;

	//Stack index of the current group
	//This points to the actual rule and not the synthetic group rule
	int currentGroup;

	//The token type expected next
	eei_rule_type next;
} eei_parser;


//Parser utility functions
//------------------------

static ee_parser_reply eei_parse_error(
		eei_parser * parser,
		ee_parser_reply reply,
		const eei_parser_token * token)
{
	//Quick path for the normal situation
	if (reply == ee_parser_ok)
		return ee_parser_ok;

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

static inline ee_parser_reply eei_parse_push(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_precedence precedence,
		const eei_parser_token * token,
		const ee_element_count elements
		)
{
	//Update the next expected rule type
	parser->next = GET_RULE_NEXT(rule);

	eei_parser_node node;

	node.text.start = token->text.start;
	node.text.end = token->text.end;
	node.rule = PRECEDENCE_RULE(rule, precedence);
	node.elements = elements;

	return eei_parse_error(parser, eei_stack_push(&parser->stack, &node), token);
}

static inline ee_parser_reply eei_parse_pop(
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
	//Update the current group to the current stack top
	//	since that must be the rule that created this group
	parser->currentGroup = parser->stack.top - 1;

	return eei_parse_push(parser, GROUP_RULE(), eei_precedence_group, token, 0);
}

//Parser symbol table helper functions
//------------------------------------

static ee_symboltable_reply eei_symboltable_get_variable(
		const eei_symboltable * symboltable,
		const ee_char_type * start,
		const ee_element_count length,
		ee_variable_type ** result)
{
	eei_symboltable_index index;

	ee_symboltable_reply reply =
			eei_symboltable_find_text(
				symboltable,
				start,
				length,
				&index);

	if (reply != ee_symboltable_ok)
		return reply;

	reply =
			eei_symboltable_get(
				symboltable,
				&index,
				0,
				0,
				0,
				~ee_function_flag_invalid);

	if (reply != ee_symboltable_ok)
		return reply;

	*result = symboltable->third.locations[index.third].variable;
	return ee_symboltable_ok;
}

static ee_symboltable_reply eei_symboltable_get_function(
		const eei_symboltable * symboltable,
		const ee_char_type * start,
		const ee_element_count length,
		const ee_arity arity,
		const ee_function_flags any_flags,
		const ee_function_flags all_flags,
		const ee_function_flags not_flags,
		ee_function * result)
{
	eei_symboltable_index index;

	ee_symboltable_reply reply =
			eei_symboltable_find_text(
				symboltable,
				start,
				length,
				&index);

	if (reply != ee_symboltable_ok)
		return reply;

	reply =
			eei_symboltable_get(
				symboltable,
				&index,
				arity,
				any_flags,
				all_flags,
				not_flags);

	if (reply != ee_symboltable_ok)
		return reply;

	*result = symboltable->third.locations[index.third].function;
	return ee_symboltable_ok;
}

//Parser handler functions
//------------------------

//Rule handler
typedef ee_parser_reply (*eei_rule_handler)(eei_parser * parser, const eei_parser_node * node);

static ee_parser_reply eei_rule_handler_error(eei_parser * parser, const eei_parser_node * node)
{
	//Set the error token to the folded node, since thats the one with the error
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};
	return eei_parse_error(parser, ee_parser_expression_unexpected, &error);
}

static ee_parser_reply eei_rule_handler_constant(eei_parser * parser, const eei_parser_node * node)
{
	ee_variable_type constant;

	if (EEI_CONSTANT_PARSER(
			&parser->expression[node->text.start],
			&parser->expression[node->text.end],
			&constant) != 0)
		return ee_parser_expression_not_a_constant;

	return eei_vmmake_load_constant(&parser->vm, constant);
}

static ee_parser_reply eei_rule_handler_variable(eei_parser * parser, const eei_parser_node * node)
{
	const ee_char_type * token_start = &parser->expression[node->text.start];
	const ee_element_count token_length = node->text.end - node->text.start;

	ee_variable_type * var;
	const ee_symboltable_reply reply_var =
			eei_symboltable_get_variable(
				&parser->symboltable,
				token_start,
				token_length,
				&var);

	ee_function op;
	const ee_symboltable_reply reply_op =
			eei_symboltable_get_function(
				&parser->symboltable,
				token_start,
				token_length,
				0,
				0,
				ee_function_flag_prefix,
				0,
				&op);

	if ((reply_var == ee_symboltable_ok) && (reply_op == ee_symboltable_ok))
		//Do not allow both to be defined to avoid confusion
		return ee_parser_varfunction_duplicate;

	if ((reply_var != ee_symboltable_ok) && (reply_op != ee_symboltable_ok))
		//None found - assume this should have been a variable
		return ee_parser_unknown_variable;

	if (reply_var == ee_symboltable_ok)
		return eei_vmmake_load_variable(&parser->vm, var);
	else
		return eei_vmmake_execute_functions(&parser->vm, op, 0);
}

static ee_parser_reply eei_rule_handler_delimiter(eei_parser * parser, const eei_parser_node * node)
{
	(void)node;

	//Add another element to the group count.
	//The count is added to the synthetic group.
	parser->stack.stack[parser->currentGroup+1].elements++;
	return ee_parser_ok;
}

static ee_parser_reply eei_rule_handler_group(eei_parser * parser, const eei_parser_node * node)
{
	//The group is folded over the node contaning the group end rule.
	//That node holds information about the number of elements in the group.

	//Set the error token to the folded node for now
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};

	//The group start rule must be on the stack at this point.
	eei_parser_node head;

	//Pop it since its no longer needed
	ee_parser_reply reply = eei_parse_pop(parser, &head, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Now that the group start is available change the possible error to that.
	//Only the text start is modified since its end is already correctly set from the folded-over node.
	error.token = GET_RULE_TOKEN(head.rule);
	error.text.start = head.text.start;

	//A group must have exactly one element

	if (node->elements == 1)
		return ee_parser_ok;

	if (node->elements > 1)
		return eei_parse_error(parser, ee_parser_expression_overfull_group, &error);
	else
		return eei_parse_error(parser, ee_parser_expression_empty_group, &error);
}

static inline ee_parser_reply eei_rule_handler_operator(
		eei_parser * parser,
		const eei_parser_node * node,
		ee_function_flag flag,
		ee_arity arity,
		ee_parser_reply error
		)
{
	ee_function op;
	const ee_symboltable_reply reply =
			eei_symboltable_get_function(
				&parser->symboltable,
				&parser->expression[node->text.start],
				node->text.end - node->text.start,
				arity,
				((GET_RULE_TOKEN_TYPE(node->rule) == eei_token_operator) ? ee_function_flag_operator : 0),
				flag,
				0,
				&op);

	if (reply != ee_symboltable_ok)
	{
		const eei_parser_token token = {GET_RULE_TOKEN(node->rule), node->text};
		return eei_parse_error(
					parser,
					(reply == ee_symboltable_filtered)
					? ee_parser_function_wrong_arity
					: error,
					&token);
	}

	return eei_vmmake_execute_functions(&parser->vm, op, arity);
}

static ee_parser_reply eei_rule_handler_prefix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_prefix, node->elements, ee_parser_prefix_not_implemented);
}

static ee_parser_reply eei_rule_handler_infix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_infix, node->elements, ee_parser_infix_not_implemented);
}

static ee_parser_reply eei_rule_handler_postfix(eei_parser * parser, const eei_parser_node * node)
{
	return eei_rule_handler_operator(parser, node, ee_function_flag_postfix, node->elements, ee_parser_postfix_not_implemented);
}

static ee_parser_reply eei_rule_handler_function(eei_parser * parser, const eei_parser_node * node)
{
	//A function is folded over the node contaning the parameter group end rule.
	//That node holds information about the number of elements in the group.

	//Set the error token to the folded node, since we don't have the correct identifier yet
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};

	//A parameter group head must be on the stack at this point
	eei_parser_node head;

	//Pop it since its no longer needed
	ee_parser_reply reply = eei_parse_pop(parser, &head, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Now that the head is available change the possible error to that
	error.token = GET_RULE_TOKEN(head.rule);
	error.text = head.text;

	//An identifier must be on the stack at this point
	eei_parser_node identifier;

	//Pop it since we're going to use it as the function name
	reply = eei_parse_pop(parser, &identifier, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Now that the identifier is available change the possible error to that
	error.token = GET_RULE_TOKEN(identifier.rule);
	error.text = identifier.text;

	//Make sure this is acutally an indentifier or an operator, as theese are the only things curently supported
	if (
		(GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_identifier)
		&& (GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_operator)
		)
		return eei_parse_error(
					parser,
					ee_parser_expression_identifier_expected,
					&error);

	//The arity is the amount of elements in the parameter group of the function
	const ee_arity arity = (ee_arity)(node->elements);

	ee_function func;
	const ee_symboltable_reply reply_func =
			eei_symboltable_get_function(
				&parser->symboltable,
				&parser->expression[identifier.text.start],
				identifier.text.end - identifier.text.start,
				arity,
				ee_function_flag_infix,
				((GET_RULE_TOKEN_TYPE(identifier.rule) == eei_token_operator) ? ee_function_flag_operator : 0),
				0,
				&func);

	//In case of an error make sure we report exactly what happened
	if (reply_func != ee_symboltable_ok)
		return eei_parse_error(
					parser,
					(reply_func == ee_symboltable_filtered)
					? ee_parser_function_wrong_arity
					: ee_parser_function_not_implemented,
					&error);

	return eei_vmmake_execute_functions(&parser->vm, func, arity);
}

static ee_parser_reply eei_rule_handler_assign(eei_parser * parser, const eei_parser_node * node)
{
	//Set the error token to the folded node, since we don't have the correct identifier yet
	eei_parser_token error = {GET_RULE_TOKEN(node->rule), node->text};

	//An identifier must be on the stack at this point
	eei_parser_node identifier;

	//Pop it since we're going to use it as the LHS identifier
	ee_parser_reply reply = eei_parse_pop(parser, &identifier, &error);

	if (reply != ee_parser_ok)
		return reply;

	//Use all the content of the current group before the assign operator as the error
	error.text.start = parser->stack.stack[parser->currentGroup].text.start;
	error.text.end = node->text.start;

	//Make sure this is acutally an indentifier as this is the only thing curently supported
	if (GET_RULE_TOKEN_TYPE(identifier.rule) != eei_token_identifier)
		return eei_parse_error(
					parser,
					ee_parser_expression_identifier_expected,
					&error);

	//Get the actual variable
	ee_variable_type * var;
	const ee_symboltable_reply reply_var =
			eei_symboltable_get_variable(
				&parser->symboltable,
				&parser->expression[identifier.text.start],
				identifier.text.end - identifier.text.start,
				&var);

	//In case of an error make sure we report what happened
	if (reply_var != ee_symboltable_ok)
	{
		//Now that the identifier is available change the possible error to that
		error.token = GET_RULE_TOKEN(identifier.rule);
		error.text = identifier.text;

		return eei_parse_error(
					parser,
					ee_parser_unknown_variable,
					&error);
	}

	return eei_vmmake_store_variable(&parser->vm, var);
}


//Parser core functions
//---------------------

static inline ee_parser_reply eei_parse_done_node(eei_parser * parser, const eei_parser_node * node)
{
	static const eei_rule_handler handlers[eei_rule_handle_sentinel] =
	{
		NULL,
		eei_rule_handler_error,
		eei_rule_handler_constant,
		eei_rule_handler_variable,
		eei_rule_handler_delimiter,
		eei_rule_handler_group,
		eei_rule_handler_prefix,
		eei_rule_handler_infix,
		eei_rule_handler_postfix,
		eei_rule_handler_function,
		eei_rule_handler_assign
	};

	if (GET_RULE_CONDITIONAL(node->rule))
	{
		//The folded rule has a fold condition.
		//Look inside the fold table for a match against the stack top.
		const eei_rule_description fold =
				eei_conditional_find(
					parser->rules.fold,
					node->rule,
					eei_stack_top(&parser->stack, 0)->rule);

		if (fold == SENTINEL_RULE())
		{
			//The condition failed to match.
			const eei_parser_token token = {GET_RULE_TOKEN(node->rule), node->text};
			return eei_parse_error(parser, ee_parser_expression_unexpected, &token);
		}

		//Replace the stack top with the morphed rule.
		//This allows to modify the behaviour of subsequent folds.
		eei_stack_top(&parser->stack, 0)->rule = fold;
	}

	//Test for a handler function
	const eei_rule_handler handler = handlers[GET_RULE_HANDLER(node->rule)];

	if (handler)
	{
		const ee_parser_reply reply = handler(parser, node);
		if (reply != ee_parser_ok)
		{
			const eei_parser_token token = {GET_RULE_TOKEN(node->rule), node->text};
			return eei_parse_error(parser, reply, &token);
		}

		//After the handler is done the stack top holds the node who will
		// be folding over the element created by the handler.
		eei_stack_top(&parser->stack, 0)->elements++;
	}
	else
		//No handler was executed, pass all elements to the node that will
		// be folding over them next.
		eei_stack_top(&parser->stack, 0)->elements += node->elements;

	return ee_parser_ok;
}

static inline ee_parser_reply eei_parse_done(eei_parser * parser)
{
	//Process the top node

	eei_parser_node node;
	const ee_parser_reply reply = eei_parse_pop(parser, &node, NULL);

	if (reply != ee_parser_ok)
		return reply;

	return eei_parse_done_node(parser, &node);
}

static inline void eei_parse_parsePrefix(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_rule_description previous,
		const eei_parser_token * token)
{
	//Inherit the current precedence.
	//No elements are yet folded for the prefix.
	eei_parse_push(
				parser,
				rule,
				GET_RULE_PRECEDENCE(previous),
				token,
				0);

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
	//This is only called when the first(left) operand is already folded.
	//Since it was folded in the previous context its count was
	// added to the current stack top. We steal it to ourselves.
	eei_stack_top(&parser->stack,0)->elements--;

	//Set the precedence according to the associativity.
	//Mark we already have one element.
	eei_parse_push(
				parser,
				rule,
				(GET_RULE_ACCOSIATIVITY(rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule)
				: GET_RULE_PRECEDENCE(rule) - 1,
				token,
				1);

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
	//This is only called when the first(left) operand is already folded.
	//Since it was folded in the previous context its count was
	// added to the current stack top. We steal it to ourselves.
	eei_stack_top(&parser->stack,0)->elements--;

	//Set the precedence according to the associativity.
	//Mark we already have one element.
	eei_parse_push(
				parser,
				rule,
				(GET_RULE_ACCOSIATIVITY(rule) == eei_rule_left)
				? GET_RULE_PRECEDENCE(rule)
				: GET_RULE_PRECEDENCE(rule) - 1,
				token,
				1);
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

		if (GET_RULE_TOKEN_TYPE(rule) == eei_token_internal)
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

	//Fold until the top node is of a lower precedence.
	while (
		   (parser->stack.top > 1)
		   && (GET_RULE_PRECEDENCE(eei_stack_top(&parser->stack, 0)->rule) >= precedence))
	{
		eei_parse_done(parser);
	}
}

static void eei_parse_foldEndDilimiter(
		eei_parser * parser,
		const eei_rule_description rule,
		const eei_parser_token * token)
{
	//Sanity check
	if (parser->stack.top <= (parser->currentGroup+1))
	{
		eei_parse_error(parser, ee_parser_stack_underflow, token);
		return;
	}

	//Additional sanity check to make sure the stack was built correctly
	if (!GET_RULE_ENDDELIMITER(parser->stack.stack[parser->currentGroup].rule))
	{
		eei_parse_error(parser, ee_parser_error, token);
		return;
	}

	//This rule closes the current group.
	//Replace the current synthetic group rule with the actual closing rule of the group.
	//This allows to just fold this node in sequence with the others while, also,
	// keeping the original group starting node just below it on the stack.
	//This allows the closing rule handler to modify its behaviour based on the original
	// group openning rule.
	//This also avoids the need for handling the synthetic group rule since its never actually folded.

	const int currentGroup = parser->currentGroup;
	int group = currentGroup + 1;

	eei_parser_node node;
	node.text.start = token->text.start;
	node.text.end = token->text.end;
	node.rule = rule;

	//The element count must be perserved since it was kept in the, about to be replaced, synthetic group rule.
	node.elements = parser->stack.stack[group].elements;

	eei_stack_copynode(&parser->stack.stack[group], &node);


	//Fold up to, but not including, the group end rule (former synthetic group)
	++group;
	while (parser->stack.top > group)
	{
		eei_parse_done(parser);
		if (parser->status != ee_parser_ok)
			return;
	}


	//Before folding the rule itself we need to update the current group
	//	to the previous one, since the rule that created the current group is,
	//	in itself, an element of the previous group, and must be folded as part of that.

	//Special handling for the SOF token to avoid many check in the following code.
	if (parser->stack.top == 2)
	{
		//This code should only be reached when a EOF token was encountered that correctly matched the
		// SOF delimited rule that exists at the very bottom of the stack.
		if (
			(parser->stack.stack[0].rule != SOF_RULE()) ||
			(GET_RULE_TOKEN(parser->stack.stack[1].rule) != EOF_TOKEN()))
		{
			eei_parse_error(parser, ee_parser_error, token);
			return;
		}

		//The current group is now a lie but we can stil fold this rule, thus completely clearing the stack.
		parser->currentGroup = 0;
		eei_parse_done(parser);
		eei_parse_done(parser);
		return;
	}

	//Account for the stack top pointing one element ABOVE the actual top.
	group = parser->stack.top - 1;

	//Walk the stack back and find the previous group.
	//Pre-decrementing skips the current top, that we know is not a group rule!
	while (--group > 0)
		if (parser->stack.stack[group].rule == GROUP_RULE())
			break;

	//Sanity check
	if (group <= 0)
	{
		eei_parse_error(parser, ee_parser_error, token);
		return;
	}

	//We need the rule that created the previous group, not the synthetic group itself.
	parser->currentGroup = group - 1;

	//At this point the current group is the one the group start rule was created in so we can
	//	fold the rule that created the just-folded group.
	//Since the behaviour of the processed nodes is unknown the below loop makes sure
	// the stack is left in the expected state, just after folding the original group opening rule.
	group = currentGroup;
	while (parser->stack.top > group)
	{
		eei_parse_done(parser);
		if (parser->status != ee_parser_ok)
			return;
	}

	//Sanity check
	if (!parser->stack.top)
	{
		eei_parse_error(parser, ee_parser_stack_underflow, token);
		return;
	}

	//The token is an end delimiter - that was just completely processed.
	//This means the folded node can be treated as a postfix -
	//	so an infix must follow
	parser->next = eei_rule_infix;
}

static void eei_parse_rule_error(eei_parser * parser, eei_parser_token * token)
{
	eei_parser_token error;

	if (token->token == EOF_TOKEN())
	{
		error.token = GET_RULE_TOKEN(parser->stack.stack[parser->currentGroup].rule);
		error.text.start = parser->stack.stack[parser->currentGroup].text.start;
		error.text.end = token->text.end;
		eei_parse_error(parser, ee_parser_expression_unmatched_end, &error);
		return;
	}

	eei_parse_error(parser, ee_parser_expression_unexpected, token);
}

static inline void eei_parse_token(eei_parser * parser, eei_parser_token * token)
{
	const eei_parser_node * top = eei_stack_top(&parser->stack, 0);
	const eei_rule_description previous = top->rule;
	eei_rule_type expected =  parser->next;

	eei_rule_description rule =
			eei_find_rule(
				&parser->rules,
				token->token,
				expected,
				previous,
				parser->stack.stack[parser->currentGroup].rule);
	int found = IS_RULE_VALID(rule);

	if (!found && (expected == eei_rule_infix))
	{
		//A postfix can appear when an infix is expected - so look for it
		rule = eei_find_rule(
				&parser->rules,
				token->token,
				eei_rule_postfix,
				previous,
				parser->stack.stack[parser->currentGroup].rule);
		found = IS_RULE_VALID(rule);
	}

	if (!found)
	{
		eei_parse_rule_error(parser, token);
		return;
	}

	//Use the actual found rule, since it might be not the one expected
	switch (GET_RULE_TYPE(rule))
	{
		case eei_rule_prefix:
			eei_parse_parsePrefix(parser, rule, previous, token);
			break;

		case eei_rule_infix:
			//If allowed fold all prefixes before going any further
			//	since they bind stonger than the infixes.
			if (GET_RULE_FOLD(rule) != eei_rule_no_fold)
				eei_parse_foldPrefix(parser, GET_RULE_ENDDELIMITER(rule));

			eei_parse_foldPrecedence(parser, rule);
			eei_parse_parseInfix(parser, rule, token);
			break;

		case eei_rule_postfix:
			//If allowed fold all prefixes before going any further
			//	since they bind stonger than the infixes.
			if (GET_RULE_FOLD(rule) != eei_rule_no_fold)
				eei_parse_foldPrefix(parser, 0);

			eei_parse_foldPrecedence(parser, rule);
			eei_parse_parsePostfix(parser, rule, token);
			break;

		case eei_rule_end:
			eei_parse_foldEndDilimiter(parser,rule,token);
			break;
	}
}

static void eei_parse_init(eei_parser * parser)
{
	parser->status = ee_parser_ok;
	parser->error_token.token = SOF_TOKEN();
	parser->error_token.text.start = 0;
	parser->error_token.text.end = 0;

	//Inject a SOF token into the parser
	eei_parse_parsePrefix(parser, SOF_RULE(), 0, &parser->error_token);
}

static void eei_parse_expression(eei_parser * parser)
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

		if (token.token == ERROR_TOKEN())
		{
			eei_parse_error(parser, ee_parser_bad_input, &token);
			return;
		}

		eei_parse_token(parser, &token);
	}

	if (parser->stack.top && (parser->status == ee_parser_ok))
		parser->status = ee_parser_error;
}


//External API semi-opaque structures
//-----------------------------------

//Internal direct data held inside a compilation data structure
typedef struct
{
	//Placeholder for start of dynamic data
	eei_parser_node stack[1];
} eei_compilation_struct;

//Internal direct data held inside an execution environment
typedef struct
{
	//Pointers to data for avoiding re-calculation on each execution
	eei_vm_environment environment;

	//Placeholder for start of dynamic data
	char data[1];
} eei_environment_struct;


//External API utility
//--------------------

//Helper macro to calculate alignment of a type
#define alignof(type) ((ptrdiff_t)&((struct { char c; type d; } *)0)->d)

//Helper macro to calculate misalignment of a pointer to a type
#define ptr_misalign(type, ptr) ((alignof(type) - ((ptrdiff_t)(ptr) % alignof(type))) % alignof(type))

//Helper macro to calculate an aligned pointer
#define aligned_ptr(type, ptr) ((type *)((char *)(ptr) + ptr_misalign(type, (ptr)) ))
#define aligned_const_ptr(type, ptr) ((type *)((const char *)(ptr) + ptr_misalign(type, (ptr)) ))


//Environment API utility
//-----------------------

static void * eei_environment_calculate(
		void * data,
		eei_vm_environment * environment,
		const ee_data_size * sizes)
{
	//Setup the environment pointers according to sizes based on the data pointer

	environment->constants		= aligned_ptr(ee_variable_type,	data);
	environment->variables		= aligned_ptr(ee_variable,		environment->constants + sizes->constants);
	environment->functions		= aligned_ptr(ee_function,		environment->variables + sizes->variables);
	environment->instructions	= aligned_ptr(ee_vm_bytecode,	environment->functions + sizes->functions);
	environment->stack			= aligned_ptr(ee_variable_type,	environment->instructions + sizes->instructions);

	environment->instruction_count = sizes->instructions;
	return environment->stack + sizes->runtime_stack;
}

static void eei_environment_copy(
		const eei_environment_struct * source,
		eei_environment_struct * destination,
		const ee_data_size * destination_size)
{
	//Currently this function only handles same-size copy and compaction.
	//TODO: Handle expansion when interrupted parsing is implemented.

	//Calculate the destination pointers.
	//This is first done locally since the source and destination could be the same.
	eei_vm_environment dst;
	eei_environment_calculate(destination->data, &dst, destination_size);

	//Copy over the data

	if (dst.constants != source->environment.constants)
		for (int i = 0; i < destination_size->constants; ++i)
			((ee_variable_type *)dst.constants)[i] = source->environment.constants[i];

	if (dst.variables != source->environment.variables)
		for (int i = 0; i < destination_size->variables; ++i)
			((ee_variable_type **)dst.variables)[i] = source->environment.variables[i];

	if (dst.functions != source->environment.functions)
		for (int i = 0; i < destination_size->functions; ++i)
			((ee_function *)dst.functions)[i] = source->environment.functions[i];

	if (dst.instructions != source->environment.instructions)
		for (int i = 0; i < destination_size->instructions; ++i)
			((ee_vm_bytecode *)dst.instructions)[i] = source->environment.instructions[i];

	//Copy over the new pointers
	destination->environment = dst;
}

static void eei_guestimate_calculate_sizes(ee_data_size * size)
{
	size->compilation_size = (ee_memory_size)(sizeof(eei_parser_node) * size->compilation_stack);
	size->stack_size = (ee_memory_size)(sizeof(ee_variable_type) * size->runtime_stack);

	//Assume aligned buffers!
	eei_vm_environment env;
	const char * env_end = eei_environment_calculate((void*)(sizeof(eei_vm_environment)), &env, size);

	size->environment_size = (ee_memory_size)((char*)env.stack - (char*)0);
	size->full_environment_size = (ee_memory_size)(env_end - (char*)0);
}


//External API
//------------

ee_parser_reply ee_estimate(ee_memory_size length, ee_data_size * size)
{
	//Assume any character can be any element, at the same time!
	//This will grossly over-estimate the needed size but will make
	// sure the allocated memory would be enough to parse and evaluate
	// any expression of the given length.

	//The below items always must be separated by, at least, one other symbol.
	//Thus no more than half the expression can consist of any of them.
	size->constants = length / 2 + 1;
	size->variables = length / 2 + 1;
	size->functions = length / 2 + 1;

	//Worst case:
	// An expression consisting of single character, the group start delimiters.
	size->compilation_stack = 4 + length * 2;

	//Worst case:
	// An expression consisting of single character operators, each using an extra immediate.
	size->instructions = length * 2;

	//Worst case:
	// A complete RPN expression.
	size->runtime_stack = length;

	eei_guestimate_calculate_sizes(size);

	return ee_parser_ok;
}

ee_parser_reply ee_guestimate(const ee_char_type * expression, ee_data_size * size)
{
	int identifiers = 0;
	int numbers = 0;
	int actuals = 0;
	int groups = 0;
	int operators = 0;

	eei_lexer_state state;
	eei_token token = SOF_TOKEN();

	state.head = expression;
	do
	{
		token = eei_lexer_next_token(&state);

		switch (GET_TOKEN_TYPE(token))
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

	} while (GET_TOKEN_TYPE(token) != eei_token_internal);


	//Guestimate the number of elements

	size->constants = numbers;
	size->variables = identifiers;
	size->functions = identifiers + operators;
	size->instructions = numbers + identifiers*2 + operators*2;
	size->compilation_stack = 2 + numbers + identifiers + operators * 2 + groups * 2;
	size->runtime_stack = numbers + actuals + groups * 2 + operators + identifiers;

	eei_guestimate_calculate_sizes(size);

	return (token == ERROR_TOKEN()) ? ee_parser_error : ee_parser_ok;
}

ee_parser_reply ee_compile(
		const ee_char_type * expression,
		ee_data_size * size,
		const ee_symboltable_header * symboltable,
		const ee_compilation * compilation,
		ee_location * error,
		ee_evaluation * evaluation)
{
	//Calculate the correctly aligned pointers
	eei_symboltable_struct * full_symboltable = (eei_symboltable_struct *)symboltable;


	//Compilation data
	//----------------

	//Calculate the correctly aligned pointer
	eei_compilation_struct * full_compilation = aligned_ptr(eei_compilation_struct, compilation->data);

	//Calculate how much (aligned) stack elements the compilation data buffer can actually hold.
	const ptrdiff_t compilation_misalign = (const char*)full_compilation - (const char*)compilation->data;
	const int stack_elements = (compilation->size - compilation_misalign) / sizeof(eei_parser_node);

	//Make sure the data buffer is big enough to hold the expected stack.
	//Note that this would still work if the expected stack size is not calculated (zero).
	if (stack_elements < size->compilation_stack)
		return ee_parser_memory;


	//Evaluation data
	//---------------

	//Calculate the correctly aligned pointer
	eei_environment_struct * full_env = aligned_ptr(eei_environment_struct, evaluation->data);

	//Calculate the misalign. Used to find actual available space.
	const ptrdiff_t environment_misalign = (const char*)full_env - (const char*)evaluation->data;

	//Make sure the data buffer is big enough to hold, at least, the minimal environment.
	if ((evaluation->size - environment_misalign) < (ptrdiff_t)sizeof(eei_environment_struct))
		return ee_parser_memory;

	//Setup the evaluation environment
	const char * env_end = (const char *)eei_environment_calculate(full_env->data, &full_env->environment, size);

	//Make sure the data buffer is big enough to hold the complete environment.
	const ptrdiff_t env_size = env_end - (const char *)evaluation->data;
	if ((evaluation->size - environment_misalign) < env_size)
		return ee_parser_memory;


	//Parser setup
	//------------

	eei_parser parser;

	parser.stack.stack = full_compilation->stack;
	parser.stack.size = stack_elements;
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

	//Convert to VM-make compatible types
	parser.vm.data.constants = (ee_variable_type*)full_env->environment.constants;
	parser.vm.data.variables = (ee_variable_type**)full_env->environment.variables;
	parser.vm.data.functions = (ee_function*)full_env->environment.functions;
	parser.vm.data.instructions = (ee_vm_bytecode*)full_env->environment.instructions;

	eei_symboltable_calculate_pointers(&parser.symboltable, full_symboltable);
	parser.rules = eei_parser_rules;

	parser.expression = expression;

	//---Parse the expression---
	eei_parse_expression(&parser);

	//Fill back data

	//Setup the new sizes
	size->constants = parser.vm.current.constants;
	size->variables = parser.vm.current.variables;
	size->functions = parser.vm.current.functions;
	size->instructions = parser.vm.current.instructions;

	//Fill the actually used instructions count
	full_env->environment.instruction_count = parser.vm.current.instructions;

	//Fill the actually used compilation stack size
	size->compilation_stack = parser.stack.high;

	//Fill the calculated maximum runtime stack size
	size->runtime_stack = parser.vm.max.stack;

	//Recalculate new sizes
	eei_guestimate_calculate_sizes(size);

	//Compact the evaluation environment in-place
	//TODO: Allow to disable the compaction
	eei_environment_copy(full_env, full_env, size);

	//Detect special conditions on correctly parsed code
	if (parser.status == ee_parser_ok)
	{
		if (parser.vm.current.instructions == 0)
			//No instructions were generated
			parser.status = ee_parser_empty;
		else if (parser.vm.current.stack == 0)
		{
			//The runtime variable stack would be empty after evaluation

			if (parser.vm.max.stack > 0)
				//The stack had data that was (most likely) stored
				parser.status = ee_parser_store;
			else
				//The stack never had any data, thus surely none was stored,
				//	and this is an actual error.
				parser.status = ee_parser_noresult;
		}
	}

	if (error)
	{
		error->start = &parser.expression[parser.error_token.text.start];
		error->end = &parser.expression[parser.error_token.text.end];
	}

	return parser.status;
}

ee_evaluator_reply ee_evaluate(const ee_evaluation * evaluation, ee_variable_type * result)
{
	//Calculate the correctly aligned evaluation data pointer
	const eei_vm_environment * environment =
			&aligned_const_ptr(const eei_environment_struct, evaluation->data)->environment;

	if (environment->instruction_count == 0)
	{
		//This is an empty expression that can not return any result

		if (result)
			//A result was expected - notify the caller
			return ee_evaluator_empty;

		//No result was expected anyhow - all is well
		return ee_evaluator_ok;
	}

	const ee_evaluator_reply reply = eei_vm_execute(environment);

	//Handle user-generated errors
	if (reply <= ee_evaluator_user_base)
		return reply;

	//Extract the top of the stack and return it as the result
	if (result)
	{
		//A result is expected but the stack is empty
		if (reply == ee_evaluator_empty)
			return ee_evaluator_stack_underflow;

		*result = *environment->stack;
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
