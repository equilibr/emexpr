#include "src/emexpr.h"

#include <time.h>
#include <stdio.h>
#include <math.h>

//Simple test function
static int compare_simple(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	(void)actuals;
	*result = 1;
	return 0;
}

static int compare_overwhelm(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	(void)actuals;
	*result = 0;
	for (int i =0; i < 100; ++i)
		*result = pow(fabs(hypot(sin(*result + 0.1), atan(*result + 0.2))), -1e-3);
	return 0;
}

enum
{
	MeasureTime = 1,
	MeasureCounts = 3
};


static long double measure_direct(ee_function f)
{
	ee_variable_type result;
	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		f(0, NULL, &result);
		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_evaluation(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables,
		const char * expression)
{
	//Globally allocated data pools
	enum { pool_bytes = 1024 };

	static union
	{
		ee_symboltable_header header;
		char data[pool_bytes];
	} symboltable;

	static union
	{
		ee_compilation_header header;
		char data[pool_bytes];
	} parser;

	static union
	{
		ee_environment_header header;
		char data[pool_bytes];
	} environment;

	symboltable.header.flags = 0;
	symboltable.header.size = sizeof(symboltable);
	ee_symboltable_add(&symboltable.header, functions, variables);

	ee_data_size sizes;
	parser.header.flags = 0;

	ee_guestimate(expression, &sizes);
	if (ee_compile(expression, &sizes, &symboltable.header, &parser.header, &environment.header) >= ee_parser_error)
		return 0;

	if (ee_evaluate(&environment.header, NULL) > ee_evaluator_stack_extra)
		return 0;

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		ee_evaluate(&environment.header, NULL);
		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_compilation(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables,
		const char * expression)
{
	//Globally allocated data pools
	enum { pool_bytes = 1024 };

	static union
	{
		ee_symboltable_header header;
		char data[pool_bytes];
	} symboltable;

	static union
	{
		ee_compilation_header header;
		char data[pool_bytes];
	} parser;

	static union
	{
		ee_environment_header header;
		char data[pool_bytes];
	} environment;

	symboltable.header.flags = 0;
	symboltable.header.size = sizeof(symboltable);
	ee_symboltable_add(&symboltable.header, functions, variables);

	ee_data_size sizes;

	ee_guestimate(expression, &sizes);
	if (ee_compile(expression, &sizes, &symboltable.header, &parser.header, &environment.header) >= ee_parser_error)
		return 0;

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		parser.header.flags = 0;

		ee_guestimate(expression, &sizes);
		ee_compile(expression, &sizes, &symboltable.header, &parser.header, &environment.header);

		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_symbol(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables)
{
	//Globally allocated data pools
	enum { pool_bytes = 1024 };

	static union
	{
		ee_symboltable_header header;
		char data[pool_bytes];
	} symboltable;

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		symboltable.header.flags = 0;
		symboltable.header.size = sizeof(symboltable);
		ee_symboltable_add(&symboltable.header, functions, variables);

		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (elapsed / CLOCKS_PER_SEC);

	return ips;
}

static const ee_symboltable_function funcData[] =
{
	{compare_simple,"f_with_long_name",-1,ee_function_flag_anywhere},
	{compare_simple,"f",-1,ee_function_flag_anywhere},
	{compare_overwhelm,"g",-1,ee_function_flag_anywhere},
	{0,0,0,0}
};

static ee_variable_type var1;
static ee_variable_type var2;

static const ee_symboltable_variable varData[] =
{
	{&var1, "a"},
	{&var2, "b"},
	{0,0}
};

static const ee_function direct[MeasureCounts] =
{
	compare_simple,
	compare_simple,
	compare_overwhelm,
};

static const char * eval[] =
{
	"f_with_long_name()",
	"f()",
	"g()",
	"f(f(f(f(f(f())))))",

	"",
	"a",
	"!a",
	"a + b",
	"a - b",
	"a = b",
	"a == b",
	NULL
};

void compare()
{
	double measurement;

	printf("%20s  ", "-= iter/sec =-");
	printf("%16s ","direct [M]");
	printf("%16s ","eval [M]");
	printf("%16s ","compile [K]");
	printf("%16s ","full [K]");
	printf("\n");

	for (int i = 0; eval[i]; ++i)
	{
		printf("%20s =", eval[i]);
		if (i < MeasureCounts)
		{
			measurement = measure_direct(direct[i]);
			printf("%16.3f ", measurement / 1e6);
		}
		else
			printf("%16s ", " ");

		measurement = measure_evaluation(funcData, varData, eval[i]);
		printf("%16.3f ", measurement / 1e6);

		measurement = measure_compilation(funcData, varData, eval[i]);
		printf("%16.0f ", measurement / 1e3);

		if (i < 1)
		{
			measurement = measure_symbol(funcData, varData);
			printf("%16.0f ", measurement / 1e3);
		}
		else
			printf("%16s ", " ");

		printf("\n");
	}
}
