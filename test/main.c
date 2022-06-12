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


int subneg(int arity, const ee_variable actuals, ee_variable result)
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

int plus(int arity, const ee_variable actuals, ee_variable result)
{
	*result = actuals[0] + actuals[1];
	return 0;
}

int mul(int arity, const ee_variable actuals, ee_variable result)
{
	*result = actuals[0] * actuals[1];
	return 0;
}

int mega(int arity, const ee_variable actuals, ee_variable result)
{
	*result = actuals[0] * 1000;
	return 0;
}

int milli(int arity, const ee_variable actuals, ee_variable result)
{
	*result = actuals[0] / 1000;
	return 0;
}

int pi(int arity, const ee_variable actuals, ee_variable result)
{
	*result = 3.141592653589;
	return 0;
}

int unity(int arity, const ee_variable actuals, ee_variable result)
{
	*result = actuals[0];
	return 0;
}

int arity(int arity, const ee_variable actuals, ee_variable result)
{
	*result = arity;
	return 0;
}

//Global handling functions

ee_compilation_data_function funcData[] = {
	{subneg,1},
	{subneg,2},
	{plus,2},
	{mul,2},
	{mega,1},
	{milli,1},
	{pi, 0},
	{unity,1},
	{arity,-1},
	{0,0}
};

const char * funcNames[] = {"-","-","+","*","M","m","pi","unity","arity"};

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
				"%16s   %8s %16s %10s %5s %5s %5s %5s %5s %5s %5s %5s %5s %s\n",
				"expression","result","compile","eval",
				"parse","exec","const","var","func","vm","stack","start","len.","text");
}

void test_print_sizes(const ee_data_size * sizes)
{
	printf(
				"%5d %5d %5d %5d %5d %5d %5d ",
				sizes->compilation_size,
				sizes->full_environment_size,
				sizes->constants,
				sizes->variables,
				sizes->functions,
				sizes->instructions,
				sizes->runtime_stack);
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
		"no prefix",
		"no infix",
		"no postfix",
		"expression",
		"rouge end",
		"not constants",
		"not identifier"
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

	memset(&global_parser.header, 0, sizeof(ee_compilation_header));
	memset(&global_environment.header, 0, sizeof(ee_environment_header));

	printf("%16s = ",expression);

	reply = ee_guestimate(expression, &sizes);
//	printf("guesstimate status: %d; ",reply);
//	test_print_sizes(&sizes);

	reply = ee_compile(expression, &sizes, &global_parser.header, &global_environment.header, &global_compilation_data);
	if (reply >= ee_parser_error)
	{
		printf("%8s ", " ");
		printf("%16s %10s ",parser_status_string(reply)," ");
		test_print_sizes(&sizes);
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
		printf("\n");
		return 1;
	}
	else
	{
			printf("%8g ", result);
			printf("%16s %10s ",parser_status_string(reply), eval_status_string(ereply));
			test_print_sizes(&sizes);
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
//	return 0;

	test_expression("pi()");
	test_expression("pi");
	test_expression("unity()");
	test_expression("unity(0)");
	test_expression("unity(1+1)");
	test_expression("arity()");
	test_expression("arity(0)");
	test_expression("arity(0,0)");

	var1 = 1;
	test_expression("a");
	test_expression("a + 1");
	var1 = 10;
	test_expression("a");
	test_expression("a * 2");
//	return 0;


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

	test_expression("1");
	test_expression("(1)");

	test_expression("((1))");

	//TODO: Mark empty expression during parsing!
	test_expression("");
	test_expression("()");
	test_expression("(())");

	//Refuse incorrect grammar
	test_expression("(1+");
	test_expression(")1+");
	test_expression("1+(");
	test_expression("1+)");
	test_expression("novar");

	return 0;
}
