#include <stdio.h>
#include <string.h>

#include "src/emexpr.h"

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

int main()
{
	static const char * expression = "1 + 2";

	ee_data_size sizes;
	ee_environment_header header;

	ee_parser_reply reply = ee_guestimate(expression, &sizes);
	printf("%d : %s\n",sizes.full_environment_size, expression);

	//Locally allocated data pool
	union
	{
		ee_environment_header header;
		char data[1024];
	} environment;

	ee_variable_type var1, var2;

	ee_variable const varData[] = {&var1, &var2};
	const char * varNames[] = {"a","b"};

	ee_compilation_data_function funcData[] = {
		{subneg,1},
		{subneg,2}
	};
	const char * funcNames[] = {"-","-"};

	ee_compilation_data data =
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

	memset(&environment.header, 0, sizeof(ee_environment_header));

	int compiled = ee_compile(expression, &sizes, &environment.header, &data);
	printf("Compiled: %d\n", compiled);

	ee_variable_type result;
	int evaluated = ee_evaluate(&environment.header, &result);
	printf("Evaluated: %d; Result: %f\n", evaluated, result);

	return 0;
}
