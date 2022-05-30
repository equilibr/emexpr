#include <stdio.h>
#include <string.h>

#include "src/emexpr.h"

int main()
{
	static const char * expression = "1 + 2";
	ee_environment_header header;

	int estimate = ee_guestimate(expression, &header);
	printf("%d : %s\n",estimate, expression);

	//Locally allocated data pool
	union
	{
		ee_environment_header header;
		char data[1024];
	} environment;

	ee_compilation_data data;

	memset(&environment.header, 0, sizeof(ee_environment_header));

	int compiled = ee_compile(expression, &environment.header, &data);
	printf("Compiled: %d\n", compiled);

	ee_variable_type result;
	int evaluated = ee_evaluate(&environment.header, &result);
	printf("Evaluated: %d; Result: %f\n", evaluated, result);

	return 0;
}
