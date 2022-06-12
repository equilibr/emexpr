/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EMEXPR_H
#define EMEXPR_H

#ifdef __cplusplus
extern "C" {
#endif

//The underlying type of an expression character
typedef char ee_char_type;

//The type of the variable handled by the engine
//This type must be copyable by the compiler using the assignment(=) syntax!
typedef double ee_variable_type;

//A pointer to a user visible variable
typedef ee_variable_type * ee_variable;

//A user defined function
//The arity is the number of parameters passed to the function.
//The actuals is an array of the parameters themselves, in order.
//The result should be written by this function, if it returns any value.
//A non-zero value should be returned if any error was encountered during execution.
//It is posible to use the same function under different names and arities.
typedef int (*ee_function)(int arity, const ee_variable actuals, ee_variable result);

//Describes a single function
typedef struct
{
	//The function itself.
	//This can be NULL when used to signal the end of a function array.
	//It is assumed all functions are pure (have no visible side effets).
	ee_function function;

	//The count of parameters accepted by this function.
	//Arity can be zero.
	//A negative number denotes a variadic function that accepts, at least,
	//	the negative of one less the given arity.
	//That is, an arity of -1 denotes a function that accept any number of optional parameters.
	//An arity of -2 denotes a function that expects a single mandatory
	//	parameter and any number of additional, optional, ones, and so on...
	//The actual number of parameters will be provided to the function upon invocation.
	//
	//Functions with an arity of 0 can also be invoked without the usual call syntax (as a variable reference).
	//This can be useful to supply various constants or other data, disguising it as a variable reference.
	//Functions with zero arity take shadow over variables with the same name.
	//
	//Functions with an arity of 1 can be invoked as a post-fix operator without the usual call syntax.
	//This can be useful for suppling scaling or conversions, such as "k/M/G".
	//Function with an arity of one can have the same name as a variable when used ONLY as a post-fix operator.
	const int arity;

} ee_compilation_data_function;

//This structure provides meta-data for ee_compilation_data variables and functions.
//See its comment for explanations.
typedef struct
{
	const ee_char_type * const * names;
	int count;
} ee_compilation_data_meta;

//This structure provides complete variables data for ee_compilation_data.
//See its comment for explanations.
typedef struct
{
	ee_variable const * data;
	ee_compilation_data_meta meta;
} ee_compilation_data_variables;

//This structure provides complete functions data for ee_compilation_data.
//See its comment for explanations.
typedef struct
{
	ee_compilation_data_function * data;
	ee_compilation_data_meta meta;
} ee_compilation_data_functions;

//This structure is used to supply external hooks to emexpr.
//The referenced variables and functions must remain at the given addresses
//	until the environment compiled with this data in no longer needed.
//The arrays used for referencing, naming, arity, the names and this structure itself
//	can be deleted/reused right away after running ee_compile.
//Both variables and functions use the same mechanism for providing the
//	hooks, their names and determining their count.
//
//The variables/functions::data members - the data member:
//	An array of pointers to the variables or function desctiptions;
//	Can be NULL is there are no elements at all.
//	The last element of the array can, optionally, itself be null.
//	For functions the function member is the one to NULL-ify since
//		an arity if zero is, in itself, valid.
//
//The variables/functions::meta::names members - the names member:
//	An array of names accosiated with the elements from the data member, in order.
//	Must have the same number of elements as the data member.
//	Ignored if the data member is NULL and must be valid otherwise.
//	The last element of the array can, optionally, itself be null.
//
//The variables/functions::meta::count members - the count member:
//	Holds the number of elements in the previous members.
//	Can be zero if any of the above arrays is NULL terminated and
//		MUST be non-zero if both the above arrays are not NULL-terminated.
//	When zero it is ignored during calculations of element counts.
//
//The count of the elements is performed as follows:
//	Both the data and names members are walked in parallell.
//	As soon as a NULL element is encountered in any (or both) arrays, or the count
//		reaches the number supplied in the count member (if non zero) - the count is terminated.
typedef struct
{
	ee_compilation_data_variables variables;
	ee_compilation_data_functions functions;
} ee_compilation_data;

//Status returned by the parser
typedef enum
{
	//All is well
	ee_parser_ok = 0,

	//A generic, unspedicied, error
	ee_parser_error,

	//Generic stack related error
	ee_parser_stack_error,

	//Algorithmic error causing a prematurely empty stack
	ee_parser_stack_underflow,

	//A general memory problem
	//All enums up to ee_parser_expression indicate that
	//	the expression was too complex for the allocated memory.
	ee_parser_memory,

	//Run out of stack space
	ee_parser_stack_overflow,

	//Run out of bytecode space
	ee_parser_instrictions_overflow,

	//Run out of constants space
	ee_parser_constants_overflow,

	//Run out of variables space
	ee_parser_variables_overflow,

	//Run out of functions space
	ee_parser_functions_overflow,

	//A variable named by the expression is not provided
	ee_parser_unknown_variable,

	//A function used by the expression is not implemented
	ee_parser_function_not_implemented,

	//A prefix operator used by the expression is not implementation
	ee_parser_prefix_not_implemented,

	//An infix operator used by the expression is not implementation
	ee_parser_infix_not_implemented,

	//An postfix operator used by the expression is not implementation
	ee_parser_postfix_not_implemented,

	//A malformed expression was parsed with an unspecified error.
	//Any enumerator after this indicates only various expression errors,
	//	thus the parser reply could be tested with ">" to differentiate between
	//	erroneous expressions and problems with the parser.
	ee_parser_expression,

	//The expression has an unmatched end delimiter
	ee_parser_expression_unmatched_end,

	//Something that looked like a constant, but was not an actual constant, was encountered
	ee_parser_expression_not_a_constant,

	//An identifier (usually function name) was expected
	ee_parser_expression_identifier_expected,
} ee_parser_reply;

//Status returned by the evaluator
typedef enum
{
	//All is well
	ee_evaluator_ok = 0,

	//All is well but the expression was empty
	//	and did not return a result.
	ee_evaluator_empty,

	ee_evaluator_stack_underflow
} ee_evaluator_reply;

//Holds size calculations from the guestimator and compilator
typedef struct
{
	//Maximum sizes, in bytes, for allocations
	//It is assumed that memory is allocated with
	//	the correct alignment for each datum

	//Data for the compilation, used only by ee_compile.
	//Aligned as ee_compilation_header.
	//Calculated by ee_guestimate.
	int compilation_size;

	//Data for the execution environment, used by ee_compile and ee_evaluate.
	//Aligned as ee_environment_header.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	int environment_size;

	//Data for the runtime stack, used only by ee_evaluate.
	//Aligned as ee_variable, when allocated separately.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	//This can be allocated as part of the execution environment or separately
	int stack_size;

	//Data for the execution environment, used by ee_compile and ee_evaluate.
	//Aligned as ee_environment_header.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	//This can be used instead of environment_size & stack_size when
	//	the runtime stack is allocated inside the execution environment.
	//This is just the sum of environment_size & stack_size taking into account alignment of the latter.
	int full_environment_size;


	//Counts of the various elements
	//Used and updated by ee_compile

	int constants;
	int variables;
	int functions;
	int instructions;
	int compilation_stack;
	int runtime_stack;
} ee_data_size;

//Basic allocation element for the environment
typedef int ee_environment_element;

//Semi-transparent header of the compilation environment
typedef struct
{
	int flags;

	ee_parser_reply reply;
	const ee_char_type * error_token_start;
	const ee_char_type * error_token_end;

	//Start of internal data. Used to simplify alignment.
	ee_environment_element internal[1];
} ee_compilation_header;

//Semi-transparent header of the environment
typedef struct
{
	int flags;

	//Pre-allocated runtime stack
	//This can be modified to any correctly aligned pointer
	//	to a suitable memory before executing ee_evaluate
	ee_variable_type * stack;

	//Max runtime stack depth, in count of of ee_variable_type elements
	int max_stack;

	//Start of internal data. Used to simplify alignment.
	ee_environment_element internal[1];
} ee_environment_header;

//Typedef to simply the interface
typedef ee_environment_header * ee_environment;


//Anayze the expression and estimate the upper bound of needed memory, in bytes.
ee_parser_reply ee_guestimate(
		const ee_char_type * expression,
		ee_data_size * size);

//Compile the expression, using the pre-allocated environment.
//Will calculate the actual memory used and update "size" accordingly.
//Memory requierements will NEVER grow and an error will be returned in case
//	the allocated memory is not enough.
//The environment can be resized and moved in memory as needed.
ee_parser_reply ee_compile(
		const ee_char_type * expression,
		ee_data_size * size,
		ee_compilation_header * compilation,
		ee_environment environment,
		const ee_compilation_data * data);

//Evaluate the compiled environment
ee_evaluator_reply ee_evaluate(ee_environment environment, ee_variable result);

#ifdef __cplusplus
}
#endif

#endif // EMEXPR_H
