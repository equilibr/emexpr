#include <stdio.h>
#include <string.h>

#include "src/emexpr.h"
#include "src/eelib/errors.h"

//Globally allocated data pools
enum { pool_bytes = 10240 };

union
{
	ee_symboltable_header header;
	char data[pool_bytes];
} global_symboltable;

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

static const ee_symboltable_function funcData[] =
{
	{mega,"M",1,ee_function_flag_postfix | ee_function_flag_pure},
	{milli,"m",1, ee_function_flag_postfix | ee_function_flag_pure},
	{pi, "pi",0,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_static},
	{unity,"unity",1,ee_function_flag_infix | ee_function_flag_pure},
	{arity,"arity",-1,ee_function_flag_infix | ee_function_flag_pure},
	{acuum, "acuum",-1,ee_function_flag_infix | ee_function_flag_pure},
	{0,0,0,0}
};

ee_variable_type var1, var2;

static const ee_symboltable_variable varData[] =
{
	{&var1, "a"},
	{&var2, "b"},
	{0,0}
};

void test_print_header()
{
	printf(
				"%20s   %8s %16s %10s "
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

void test_symboltable()
{
	memset(&global_symboltable.header, 0, sizeof(ee_symboltable_header));
	global_symboltable.header.size = sizeof(global_symboltable);

	printf("Symbol table size: %d", global_symboltable.header.size);
	ee_symboltable_add(&global_symboltable.header, funcData, varData);
	ee_symboltable_add(&global_symboltable.header, NULL, NULL);
	printf(" -> %d\n", global_symboltable.header.size);
}

int test_expression(const char * expression)
{
	ee_data_size sizes;
	ee_data_size sizes_guess;
	ee_data_size sizes_delta;

	memset(&global_parser.header, 0, sizeof(ee_compilation_header));
	memset(&global_environment.header, 0, sizeof(ee_environment_header));

	printf("%20s = ",expression);

	ee_guestimate(expression, &sizes);
	memcpy(&sizes_guess, &sizes, sizeof(ee_data_size));

	ee_parser_reply reply = ee_compile(expression, &sizes, &global_symboltable.header, &global_parser.header, &global_environment.header);
	test_diff_sizes(&sizes_delta, &sizes_guess, &sizes);

	if (reply >= ee_parser_error)
	{
		printf("%8s ", " ");
		printf("%16s %10s ",eelib_compile_status_string_short(reply)," ");
		test_print_sizes(&sizes);
		test_print_sizes(&sizes_delta);
		test_print_location(expression, &global_parser.header);
		printf("\n");
		return 1;
	}

	ee_variable_type result = 0;
	ee_evaluator_reply ereply =
			ee_evaluate(
				&global_environment.header,
				(reply != ee_parser_store) ? &result : NULL);

	if (reply || ereply)
	{
		printf("%8s ", " ");
		printf("%16s %10s ",eelib_compile_status_string_short(reply), eelib_evaluate_status_string_short(ereply));
		test_print_sizes(&sizes);
		test_print_sizes(&sizes_delta);
		printf("\n");
		return 1;
	}
	else
	{
			printf("%8g ", result);
			printf("%16s %10s ",eelib_compile_status_string_short(reply), eelib_evaluate_status_string_short(ereply));
			test_print_sizes(&sizes);
			test_print_sizes(&sizes_delta);
			printf("\n");
	}

	return 0;
}

int main()
{
	test_symboltable();
	test_print_header();
	var1 = 0;

	//Mark empty expression during parsing!
	test_expression("");

	test_expression("a = 1");
	test_expression("a");
	test_expression("a = 0");
	test_expression("!a");
	test_expression("a = pi() / pi - 1");
	test_expression("2M * (1/2)m");
	test_expression("1 * 1");
	test_expression("-(1 * -1)");
	test_expression("-1 * -1");
	test_expression("2 * 3 + 4 == 10");
	test_expression("14 == 2 * (3 + 4)");
	test_expression(">(15,2 + 3 * 4)");
	test_expression("(2 + 3) * 4 / 20");
	test_expression("1 - (-1--1)");
	test_expression("||(a,1)");
	test_expression("^^(0,a,1)");
	test_expression("!^^(0,1,1)");
	test_expression("1.01 - 0.1/10");

	//Extra stack values
	//TODO: Something is definetely wrong with this one...
	test_expression("a = 0,a");
	test_expression("a");

	//Refuse assign to non-variable
	test_expression("0 = 0");
	test_expression("a+0 = 0");
	test_expression("0+a = 0");
	test_expression("a,a = 0");
	test_expression("a = c");
	test_expression("c = a");
	test_expression("arity() = 0");

	//Refuse wrong arity
	test_expression("-(1,0,1)");
	test_expression("&&()");

	//Refuse no function
	test_expression("&2");
	test_expression("&(2)");
	test_expression("2&3");

	//Refuse incorrect grammar
	test_expression("a =");
	test_expression("()");
	test_expression("(())");
	test_expression("(1+");
	test_expression(")1+");
	test_expression("1+(");
	test_expression("1+)");
	test_expression("novar");
	test_expression("nofunc()");

	return 0;
}
