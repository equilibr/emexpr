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
	ee_parser_ok,

	//A generic, unspedicied, error
	ee_parser_error,

	//Generic stack related error
	ee_parser_stack_error,

	//Run out of stack space
	ee_parser_stack_overflow,

	//Algorithmic error causing a prematurely empty stack
	ee_parser_stack_underflow,

	//A malformed expression was parsed with an unspecified error
	ee_parser_expression,

	//The expression has an unmatched end delimiter
	ee_parser_expression_unmatched_end,
} ee_parser_reply;


//A semi-opaque type to hold compiled data and the execution environment
//------

//Basic allocation element for the environment
typedef int ee_environment_element;

//Transparent header of the environment
typedef struct
{
	//Number of constants, of ee_variable_type elements
	//TODO: Move this to eei_vm_environment
	int constants;

	//Pre-allocated runtime stack
	//This can be modified to any correctly aligned pointer
	//	to a suitable memory before executing ee_evaluate
	ee_variable_type * stack;

	//Max runtime stack depth, in count of of ee_variable_type elements
	int max_stack;

	//Additional internal data, in ee_environment_element
	int	internal;
} ee_environment_header;

typedef ee_environment_header * ee_environment;


//Anayze the expression and return an (upper bound) estimate of needed memory, in bytes.
//Returns non-positive in case of any error
//A negative number is the estimate up until the error was encountered
int ee_guestimate(const ee_char_type * expression, ee_environment_header * header);

//Compile the expression, using the pre-allocated environment
//Returns the actual memory used, in ee_environment_element.
//The environment can be resized and moved in memory as needed.
int ee_compile(
		const ee_char_type * expression,
		ee_environment environment,
		const ee_compilation_data * data);

//Evaluate the compiled environment
//Returns non-zero on error.
int ee_evaluate(ee_environment environment, ee_variable result);

#ifdef __cplusplus
}
#endif

#endif // EMEXPR_H
