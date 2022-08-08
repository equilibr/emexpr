#include <stdio.h>
#include <string.h>

#include "src/emexpr.h"

//Globally allocated data pools
enum { pool_bytes = 10240 };

union
{
	ee_compilation_header header;
	char data[pool_bytes];
} global_parser;

union
{
	ee_environment_header header;
	char data[pool_bytes];
} global_environment;


int mega(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] * 1000;
	return 0;
}

int milli(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] / 1000;
	return 0;
}

int pi(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	(void)actuals;
	*result = 3.141592653589;
	return 0;
}

int unity(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0];
	return 0;
}

int arity(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)actuals;
	*result = arity;
	return 0;
}

int acuum(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type sum = 0;
	for (int i = 0; i < arity; ++i)
		sum += actuals[i];

	*result = sum;
	return 0;
}

//Global handling functions

ee_compilation_data_function funcData[] = {
	{mega,1},
	{milli,1},
	{pi, 0},
	{unity,1},
	{arity,-1},
	{acuum, -1},
	{0,0}
};

const char * funcNames[] = {"M","m","pi","unity","arity","acuum"};

ee_variable_type var1, var2;

ee_variable const varData[] = {&var1, &var2, 0};
const char * varNames[] = {"a","b"};

ee_compilation_data global_compilation_data =
{
	.variables =
	{
		varData,
		{varNames, 0}
	},
	.functions =
	{
		funcData,
		{funcNames, 0}
	}
};

void test_print_header()
{
	printf(
				"%16s   %8s %16s %10s "
				"%5s %4s %5s %5s %3s %3s %2s %5s   "
				"%5s %4s %5s %5s %3s %3s %2s %5s   "
				"%5s %5s %s\n",
				"expression","result","compile","eval",
				"parse","exec","cstk","const","var","fun","vm","stack",
				"parse","exec","cstk","const","var","fun","vm","stack",
				"start","len.","text");
}

void test_print_sizes(const ee_data_size * sizes)
{
	printf(
				"%5d %4d %5d %5d %3d %3d %2d %5d   ",
				sizes->compilation_size,
				sizes->full_environment_size,
				sizes->compilation_stack,
				sizes->constants,
				sizes->variables,
				sizes->functions,
				sizes->instructions,
				sizes->runtime_stack);
}

void test_diff_sizes(ee_data_size * dst, const ee_data_size * left, const ee_data_size * right)
{
	dst->compilation_size = left->compilation_size - right->compilation_size;
	dst->full_environment_size = left->full_environment_size - right->full_environment_size;
	dst->constants = left->constants - right->constants;
	dst->variables = left->variables - right->variables;
	dst->functions = left->functions - right->functions;
	dst->instructions = left->instructions - right->instructions;
	dst->compilation_stack = left->compilation_stack - right->compilation_stack;
	dst->runtime_stack = left->runtime_stack - right->runtime_stack;
}

void test_print_location(const char * expression, const ee_compilation_header * header)
{
	int length = header->error_token_end - header->error_token_start;
	int position = header->error_token_start - expression;
	printf("%5d %5d ",position, length);
	if (length)
		printf("%.*s",length, header->error_token_start);
	else if (!*header->error_token_start)
		printf("END");
	else
		printf("START");
}

const char * parser_status_string(ee_parser_reply reply)
{
	static const char * strings[] =
	{
		"ok",
		"empty",
		"error",
		"stack error",
		"stack underflow",
		"memory",
		"stack overflow",
		"ins. overflow",
		"const. overflow",
		"var. overflow",
		"func. overflow",
		"unknown var",
		"dup. varfunc",
		"no func.",
		"wrong arity",
		"no prefix",
		"no infix",
		"no postfix",
		"expression",
		"rouge end",
		"not constants",
		"not identifier",
		"empty group",
		"over. group"
	};

	return strings[reply];
}

const char * eval_status_string(ee_evaluator_reply reply)
{
	static const char * strings[] =
	{
		"ok",
		"empty",
		"stack und.",
	};

	return strings[reply];
}

int test_expression(const char * expression)
{
	ee_parser_reply reply;

	ee_data_size sizes;
	ee_data_size sizes_guess;
	ee_data_size sizes_delta;

	memset(&global_parser.header, 0, sizeof(ee_compilation_header));
	memset(&global_environment.header, 0, sizeof(ee_environment_header));

	printf("%16s = ",expression);

	reply = ee_guestimate(expression, &sizes);
	memcpy(&sizes_guess, &sizes, sizeof(ee_data_size));

	reply = ee_compile(expression, &sizes, &global_parser.header, &global_environment.header, &global_compilation_data);
	test_diff_sizes(&sizes_delta, &sizes_guess, &sizes);

	if (reply >= ee_parser_error)
	{
		printf("%8s ", " ");
		printf("%16s %10s ",parser_status_string(reply)," ");
		test_print_sizes(&sizes);
		test_print_sizes(&sizes_delta);
		test_print_location(expression, &global_parser.header);
		printf("\n");
		return 1;
	}

	ee_variable_type result = 0;
	ee_evaluator_reply ereply = ee_evaluate(&global_environment.header, &result);
	if (reply || ereply)
	{
		printf("%8s ", " ");
		printf("%16s %10s ",parser_status_string(reply), eval_status_string(ereply));
		test_print_sizes(&sizes);
		test_print_sizes(&sizes_delta);
		printf("\n");
		return 1;
	}
	else
	{
			printf("%8g ", result);
			printf("%16s %10s ",parser_status_string(reply), eval_status_string(ereply));
			test_print_sizes(&sizes);
			test_print_sizes(&sizes_delta);
			printf("\n");
	}

	return 0;
}

int main()
{
	test_print_header();

	test_expression("1 m");
	test_expression("1 M * 1 m");
	test_expression("2 + 1 M * 1 m");

	test_expression("pi()");
	test_expression("pi");
	test_expression("unity(pi())");
	test_expression("unity(1+1)");
	test_expression("arity()");
	test_expression("arity(0)");
	test_expression("arity(0,0)");
	test_expression("acuum()");
	test_expression("acuum(1)");
	test_expression("acuum(1+2,-3)");
	test_expression("acuum(pi(),pi)");

	var1 = 1;
	test_expression("a");
	test_expression("a + 1");
	var1 = 10;
	test_expression("a");
	test_expression("a * 2");

	test_expression("2 * 2");
	test_expression("2 * -2");
	test_expression("-2 * -2");
	test_expression("2 * 3 + 4");
	test_expression("2 * (3 + 4)");
	test_expression("2 + 3 * 4");
	test_expression("(2 + 3) * 4");

	test_expression("-1");
	test_expression("1-1");
	test_expression("1 - -1");
	test_expression("1 - (-1)");
	test_expression("1 - (-1--1)");

	test_expression("&2");
	test_expression("&(2)");
	test_expression("&(2,3)");
	test_expression("&(2,0,3)");
	test_expression("2&3");

	test_expression("1");
	test_expression("(1)");
	test_expression("((1))");

	test_expression("0.1");
	test_expression("1.23");
	test_expression("0.2 - 1/5");
	test_expression("1/5 + 0.8");

	test_expression("2 == 1 + 1");
	test_expression("1 + 1 != 2");
	test_expression("2 > 1");
	test_expression("-1 >= 1");
	test_expression("!=(1,2)");

	test_expression("-1");
	test_expression("-(1)");
	test_expression("-(1,0)");

	//Refuse wrong arity
	test_expression("-(1,0,1)");

	//Mark empty expression during parsing!
	test_expression("");
	test_expression("()");
	test_expression("(())");

	//Refuse incorrect grammar
	test_expression("(1+");
	test_expression(")1+");
	test_expression("1+(");
	test_expression("1+)");
	test_expression("novar");
	test_expression("unity()");
	test_expression("arity(1+(),(2))");

	return 0;
}
