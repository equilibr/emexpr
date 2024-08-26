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

static const double MeasureTime = 0.1;

enum
{
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
	ips /= (1.0 * elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_evaluation(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables,
		const char * expression)
{
	//Globally allocated data pools
	enum { pool_bytes = 10240 };

	static union
	{
		ee_symboltable_header header;
		char data[pool_bytes];
	} symboltable;

	char parser_data[pool_bytes];
	char environment_data[pool_bytes];

	symboltable.header._.flags = 0;
	symboltable.header.size = sizeof(symboltable);
	ee_symboltable_add(&symboltable.header, functions, variables);

	ee_data_size sizes;
	ee_compilation parser;
	ee_evaluation eval;

	parser.data = parser_data;
	parser.size = pool_bytes;

	eval.data = environment_data;
	eval.size = pool_bytes;

	ee_guestimate(expression, &sizes);
	if (ee_compile(expression, &sizes, &symboltable.header, &parser, NULL, &eval) >= ee_parser_error)
		return 0;

	if (ee_evaluate(&eval, NULL) > ee_evaluator_stack_extra)
		return 0;

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		ee_evaluate(&eval, NULL);
		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (1.0 * elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_compilation(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables,
		const char * expression)
{
	//Globally allocated data pools
	enum { pool_bytes = 10240 };

	static union
	{
		ee_symboltable_header header;
		char data[pool_bytes];
	} symboltable;

	char parser_data[pool_bytes];
	char environment_data[pool_bytes];

	symboltable.header._.flags = 0;
	symboltable.header.size = sizeof(symboltable);
	ee_symboltable_add(&symboltable.header, functions, variables);
	ee_symboltable_add(&symboltable.header, NULL, NULL);

	ee_data_size sizes;
	ee_compilation parser;
	ee_evaluation eval;

	parser.data = parser_data;
	parser.size = pool_bytes;

	eval.data = environment_data;
	eval.size = pool_bytes;

	ee_guestimate(expression, &sizes);
	if (ee_compile(expression, &sizes, &symboltable.header, &parser, NULL, &eval) >= ee_parser_error)
		return 0;

	long long int counts = 0;
	long long elapsed = 0;
	const clock_t start = clock();
	do
	{
		ee_guestimate(expression, &sizes);
		ee_compile(expression, &sizes, &symboltable.header, &parser, NULL, &eval);

		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (1.0 * elapsed / CLOCKS_PER_SEC);

	return ips;
}

static long double measure_symbol(
		const ee_symboltable_function * functions,
		const ee_symboltable_variable * variables)
{
	//Globally allocated data pools
	enum { pool_bytes = 1024*2 };

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
		symboltable.header._.flags = 0;
		symboltable.header.size = sizeof(symboltable);
		ee_symboltable_add(&symboltable.header, functions, variables);
		ee_symboltable_add(&symboltable.header, NULL, NULL);

		elapsed = clock() - start;
		counts++;
	} while (elapsed < (MeasureTime * CLOCKS_PER_SEC));

	double ips = counts;
	ips /= (1.0 * elapsed / CLOCKS_PER_SEC);

	return ips;
}

static const ee_symboltable_function funcData[] =
{
	{compare_simple,"f_with_long_name",-1,ee_function_flag_anywhere},
	{compare_simple,"f",-1,ee_function_flag_anywhere},
	{compare_simple,"|",-1,ee_function_flag_anywhere},
	{compare_overwhelm,"g",-1,ee_function_flag_anywhere},
	{compare_overwhelm,"%",-1,ee_function_flag_anywhere},
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

	"|a",
	"a|a",
	"|a|a",
	"a||a",
	"|(a)",
	"|(a,a)",
	"|(a,a,a)",
	"a|a|a|a|a|a",
	"a|(a|(a|(a|(a|a))))",

	"%a",
	"a%a%a%a%a%a%a%a%a%a",

	NULL
};

void compare()
{
	double measurement;

	printf("%20s  ", "-= iter/sec =-");
	printf("%11s ","direct [M]");
	printf("%11s ","symb [K]");
	printf("%11s ","parse [K]");
	printf("%11s ","parse [nS]");
	printf("%11s ","eval [M]");
	printf("%11s ","eval [nS]");
	printf("%11s ","delta [nS]");
	printf("\n");

	//Measure evaluation of an empty expression and use it as a baseline
	//	for time deltas
	double empty =1.0e9 /  measure_evaluation(funcData, varData, "");

	for (int i = 0; eval[i]; ++i)
	{
		printf("%20s =", eval[i]);
		if (i < MeasureCounts)
		{
			measurement = measure_direct(direct[i]);
			printf("%11.3f ", measurement / 1e6);
		}
		else
			printf("%11s ", " ");

		if (i < 1)
		{
			measurement = measure_symbol(funcData, varData);
			printf("%11.0f ", measurement / 1e3);
		}
		else
			printf("%11s ", " ");

		measurement = measure_compilation(funcData, varData, eval[i]);
		printf("%11.0f ", measurement / 1e3);
		printf("%11.0f ", 1.0e9 / measurement);

		measurement = measure_evaluation(funcData, varData, eval[i]);
		printf("%11.3f ", measurement / 1e6);
		printf("%11.0f ", 1.0e9 / measurement);
		printf("%11.0f ", 1.0e9 / measurement - empty);

		printf("\n");
	}
}
