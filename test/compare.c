#include "src/emexpr.h"

#include <time.h>
#include <stdio.h>
#include <math.h>

//Simple test functions
static int compare_one(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	(void)actuals;
	*result = 1;
	return 0;
}

static int compare_unity(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0];
	return 0;
}

static int compare_acuum(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type sum = 0;
	for (int i = 0; i < arity; ++i)
		sum += actuals[i];

	*result = sum;
	return 0;
}

static int compare_sqrt(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = sqrt(actuals[0]);
	return 0;
}

enum
{
	MeasureTime = 1,
	MeasureCounts = 4,
	MeasureArities = 2
};


static long double measure_direct(ee_function f, int arity, ee_variable_type * inputs)
{
	ee_variable_type result;
	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		f(arity, inputs, &result);
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
	ee_compile(expression, &sizes, &symboltable.header, &parser.header, &environment.header);

	ee_variable_type result;
	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		ee_evaluate(&environment.header, &result);
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

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		ee_data_size sizes;
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
	{compare_one,"one",0,ee_function_flag_anywhere | ee_function_flag_static},
	{compare_unity,"unity",1,ee_function_flag_anywhere | ee_function_flag_pure},
	{compare_acuum, "acuum",-1,ee_function_flag_anywhere | ee_function_flag_pure},
	{0,0,0,0}
};

ee_variable_type var1, var2;

static const ee_symboltable_variable varData[] =
{
	{&var1, "a"},
	{&var2, "b"},
	{0,0}
};

static const ee_function direct[MeasureCounts] =
{
	compare_one,
	compare_unity,
	compare_acuum,
	compare_sqrt
};

static const char * eval[] =
{
	"one()",
	"unity(0)",
	"acuum(0,0)",
	"sqrt(1)",

	"",
	"a = 1",
	"a",
	"a = 0",
	"!a",
	"a = pi() / pi - 1",
	"2M * (1/2)m",
	"1 * 1",
	"-(1 * -1)",
	"-1 * -1",
	"2 * 3 + 4 == 10",
	"14 == 2 * (3 + 4)",
	">(15,2 + 3 * 4)",
	"(2 + 3) * 4 / 20",
	"1 - (-1--1)",
	"||(a,1)",
	"^^(0,a,1)",
	"!^^(0,1,1)",
	"1.01 - 0.1/10",
	NULL
};

void compare_measure()
{
	ee_variable_type inputs[MeasureArities];
	double measurement;

	for (int i = 0; i < MeasureArities; ++i)
		inputs[i] = i + 1;

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
			measurement = measure_direct(direct[i], MeasureArities, inputs);
			printf("%16.1f ", measurement / 1e6);
		}
		else
			printf("%16s ", " ");

		measurement = measure_evaluation(funcData, varData, eval[i]);
		printf("%16.1f ", measurement / 1e6);

		measurement = measure_compilation(funcData, varData, eval[i]);
		printf("%16.1f ", measurement / 1e3);

		if (i < MeasureCounts)
		{
			measurement = measure_symbol(funcData, varData);
			printf("%16.1f ", measurement / 1e3);
		}
		else if (i == MeasureCounts)
		{
			measurement = measure_symbol(NULL, varData);
			printf("%16.1f ", measurement / 1e3);
		}
		else
			printf("%16s ", " ");

		printf("\n");
	}
}
