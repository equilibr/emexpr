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

//Global handling functions

ee_compilation_data_function funcData[] = {
	{subneg,1},
	{subneg,2},
	{mul,2},
	{mega,1},
	{milli,1},
	{0,0}
};

const char * funcNames[] = {"-","-","*","M","m"};

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

void test_print_sizes(const ee_data_size * sizes)
{
	printf(
				"compile: %04d; execute: %04d; const: %04d; var: %04d; func: %04d; vm: %04d; stack: %04d;\n",
				sizes->compilation_size,
				sizes->full_environment_size,
				sizes->constants,
				sizes->variables,
				sizes->functions,
				sizes->instructions,
				sizes->runtime_stack);
}

int test_expression(const char * expression)
{
	ee_parser_reply reply;

	ee_data_size sizes;

	memset(&global_parser.header, 0, sizeof(ee_compilation_header));
	memset(&global_environment.header, 0, sizeof(ee_environment_header));

	printf("%20s = ",expression);

	reply = ee_guestimate(expression, &sizes);
//	printf("guesstimate status: %d; ",reply);
//	test_print_sizes(&sizes);

	reply = ee_compile(expression, &sizes, &global_parser.header, &global_environment.header, &global_compilation_data);
	if (reply)
	{
		printf("compilation status: %d; ",reply);
		test_print_sizes(&sizes);
		return 1;
	}
//	printf("compilation status: %d; ",reply);
//	test_print_sizes(&sizes);

	ee_variable_type result = 0;
	ee_evaluator_reply ereply = ee_evaluate(&global_environment.header, &result);
	if (ereply)
	{
		printf("\ncompilation status: %d; ",reply);
		test_print_sizes(&sizes);
		printf(
					"evaluation  status: %d; result: %g\n",
					ereply,
					result);
	}
	else
			printf("%g\t\t(%d %d)\n", result, reply, ereply);

	return 0;
}

int main()
{
//	test_expression("");

//	test_expression("2 * 2");
//	test_expression("2 * -2");
//	test_expression("-2 * -2");

//	return 0;

	test_expression("1 m");
	return 0;

	test_expression("-1");
	test_expression("1-1");
	test_expression("1 - -1");
	test_expression("1 - (-1)");
	test_expression("1 - (-1--1)");

	test_expression("1");
	test_expression("()");
	test_expression("(1)");
	test_expression("(())");
	test_expression("((1))");


	test_expression("(1+");
	test_expression(")1+");

	return 0;
}
