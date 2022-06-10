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

//Global handling functions

ee_compilation_data_function funcData[] = {
	{subneg,1},
	{subneg,2}
};

const char * funcNames[] = {"-","-"};

ee_variable_type var1, var2;

ee_variable const varData[] = {&var1, &var2};
const char * varNames[] = {"a","b"};

ee_compilation_data global_compilation_data =
{
	.variables =
	{
		varData,
		{varNames, 2}
	},
	.functions =
	{
		funcData,
		{funcNames, 2}
	}
};


int test_subneg()
{
	static const char * expression = "";
	ee_parser_reply reply;

	ee_data_size sizes;

	memset(&global_parser.header, 0, sizeof(ee_compilation_header));
	memset(&global_environment.header, 0, sizeof(ee_environment_header));

	//Setup compilation data


	ee_compilation_data compilation_data =
	{
		.variables =
		{
			varData,
			{varNames, 2}
		},
		.functions =
		{
			funcData,
			{funcNames, 2}
		}
	};


	reply = ee_guestimate(expression, &sizes);
	printf("status: %d; size: %04d; # %s\n",reply,sizes.full_environment_size, expression);

	reply = ee_compile(expression, &sizes, &global_parser.header, &global_environment.header, &compilation_data);
	printf("status: %d; size: %04d\n",reply,sizes.full_environment_size);

	return 0;
}

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

	printf("===Expression: %s\n",expression);

	reply = ee_guestimate(expression, &sizes);
	printf("guesstimate status: %d; ",reply);
	test_print_sizes(&sizes);

	reply = ee_compile(expression, &sizes, &global_parser.header, &global_environment.header, &global_compilation_data);
	printf("compilation status: %d; ",reply);
	test_print_sizes(&sizes);

	ee_variable_type result = 0;
	ee_evaluator_reply ereply = ee_evaluate(&global_environment.header, &result);
	printf(
				"evaluation  status: %d; result: %g\n",
				ereply,
				result);

	return 0;
}

int main()
{
	test_expression("1");
	test_expression("-1");
	test_expression("1-1");

	test_expression("");
	test_expression("1");
	test_expression("()");
	test_expression("(1)");
	test_expression("(())");
	test_expression("((1))");

	test_expression("(1+");
	test_expression(")1+");

	return 0;
}
