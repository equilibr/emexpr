/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022-2024 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EMEXPR_H
#define EMEXPR_H

#ifdef __cplusplus
extern "C" {
#endif


//User modifiable types
//---------------------

//The type of the variable handled by the engine
//This type must be copyable by the compiler using the assignment(=) syntax!
typedef double ee_variable_type;


//User modifiable API types
//-------------------------

//These are the types used by the EmExpr API functions.
//They can be changed by the user, if needed, to tweak performance and resource usage.

//The underlying type of an expression character
typedef char ee_char_type;

//The type used for counting memory, in bytes
typedef signed short ee_memory_size;

//The type used for counting various elements of the parsing and execution environment
typedef unsigned char ee_element_count;

//The type used for counting function parameters
typedef signed char ee_arity;

//Type used for holding function flags
typedef unsigned char ee_function_flags;

//Type used to hold a single VM bytecode
typedef unsigned char ee_vm_bytecode;


//User modifiable API pointer types
//---------------------------------

//There are the pointers to the above user modifiably type.
//They can be changed by the user, if needed, to facilitate different memory types and allocation strategies.

//A pointer to a user visible variable
typedef ee_variable_type * ee_variable;

//A pointer to a user visible expression character
typedef ee_char_type * ee_char_ptr;


//API types
//---------

//The following two prototypes are for compiling the library with user-defined constant parsers.
//Two functions with the below prototypes should be written.
//The names of the functions should be supplied using two pre-processor #define macros,
//	EE_USER_CONSTANT_SCANNER & EE_USER_CONSTANT_PARSER. Theese macros should be visible during
//	the compilation of emexpr.c.
//This can be achieved by either writing them directly in this header, providing them at the command line,
//	or any other appropriate mechanism.

//A user defined scanner of constants
//On success it should return the pointer to the first character AFTER the scanned constant
//On failure it should return 'start'
typedef const ee_char_type * (*ee_constant_scanner)(const ee_char_type * start);

//A user defined parser of constants
//It must scan the expression from start to one-before end, read-only
//On success it should return zero and fill the result with the parsed value
//On failure it should return non-zero, the result will be ignored
typedef int (*ee_constant_parser)(const ee_char_type * start, const ee_char_type * end, ee_variable_type * result);

//A user defined function.
//Functions of this type are used to provide implementations for all implemented functions and operators.
//The arity is the number of parameters passed to the function, as parsed from the expression.
//The actuals is an array of the parameters themselves, in order.
//The result must be written by this function on success.
//A zero value should be returned on success.
//A negative value should be returned if any error was encountered during execution.
//The value ee_evaluator_user_base from the ee_evaluator_reply enumeration shold be used as base, if possible.
//In case of an error the result will be ignored and the execution would halt, floating the returned value up to the caller.
//It is posible to use the same function under different names and arities.
typedef int (*ee_function)(ee_element_count arity, const ee_variable_type * actuals, ee_variable_type * result);


typedef enum
{
	//The function always returns the same value
	ee_function_flag_const = 1 << 0,

	//The function has no side effects
	ee_function_flag_pure = 1 << 1,

	//The function can be used an as operator
	ee_function_flag_operator = 1 << 2,

	//The function can be used from a prefix rule
	ee_function_flag_prefix = 1 << 3,

	//The function can be used from a infix rule
	ee_function_flag_infix = 1 << 4,

	//The function can be used from a postfix rule
	ee_function_flag_postfix = 1 << 5,


	//No function can exist with this flags combination
	ee_function_flag_invalid = 0,

	//The function can be invoked at any time
	ee_function_flag_static = ee_function_flag_const | ee_function_flag_pure,

	//The function can be used in any location
	ee_function_flag_anywhere =
	ee_function_flag_operator | ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_postfix,

	//The ideal function that can fill all roles and be completely folded during compilation
	ee_function_flag_ideal = ee_function_flag_static | ee_function_flag_anywhere
} ee_function_flag;

//Describes a single user function
typedef struct
{
	//The function itself.
	//This can be NULL when used to signal the end of a function array.
	//It is assumed all functions are pure (have no visible side effets).
	const ee_function item;

	//The name of the function, as it should be referenced inside an expresiion
	const ee_char_type * name;

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
	const ee_arity arity;

	//Flags. See the comments inside the ee_function_flag enumeration.
	const ee_function_flags flags;
} ee_symboltable_function;

//Describes a single bound variable
typedef struct
{
	//A pointer to the bound variable
	ee_variable_type * item;

	//The name of the variable, as it should be referenced inside an expresiion
	const ee_char_type * name;
} ee_symboltable_variable;

//This vectors are used to supply variable binding and user functions to emexpr.
//The referenced variables and functions must remain at the given addresses
//	until the environment compiled with this data in no longer needed.
//The vecors themselves, including the used names,
//	can be deleted/reused right away after running ee_compile.
//End of vector is marked by the item and/or name being NULL.
typedef const ee_symboltable_function * ee_symboltable_functions;
typedef const ee_symboltable_variable * ee_symboltable_variables;

typedef enum
{
	//All is well
	ee_symboltable_ok,

	//Compacted structure can not be modified
	ee_symboltable_compacted,

	//Not enough memory
	ee_symboltable_memory,

	//An index is outside the used count
	ee_symboltable_out_of_bounds,

	//The requested name was not found
	ee_symboltable_no_name,

	//The requested type was not found
	ee_symboltable_no_type,

	//All possible candidates were filtered out
	ee_symboltable_filtered

} ee_symboltable_reply;

//Status returned by the parser
typedef enum
{
	//All is well
	ee_parser_ok = 0,

	//All is well but the expression is empty
	//	because the last value on the stack is stored in a variable
	ee_parser_store,

	//All is well but the expression is empty
	//	and will not return a result.
	ee_parser_empty,

	//A generic, unspedicied, error
	ee_parser_error,

	//The expression will not return a result
	//	although one is expected.
	ee_parser_noresult,

	//A bad input was encountered
	ee_parser_bad_input,

	//Generic stack related error
	ee_parser_stack_error,

	//Algorithmic error causing a prematurely empty stack
	ee_parser_stack_underflow,

	//Algorithmic error that would result in runtime stack underflow
	ee_parser_stack_runtime_underflow,

	//A general memory problem
	//All enums up to ee_parser_binding indicate that
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

	//A general problem with the binding
	//All enums up to ee_parser_expression indicate a problem with
	//	a variable or function binding.
	ee_parser_binding,

	//A variable named by the expression is not provided
	ee_parser_unknown_variable,

	//A variable and a zero-arity function of the same name are defined at the same time
	ee_parser_varfunction_duplicate,

	//A function used by the expression is not implemented
	ee_parser_function_not_implemented,

	//A function used by the expression is not implemented with the requested arity
	ee_parser_function_wrong_arity,

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

	//An unexpected token was encountered
	ee_parser_expression_unexpected,

	//The expression has an unmatched end delimiter
	ee_parser_expression_unmatched_end,

	//Something that looked like a constant, but was not an actual constant, was encountered
	ee_parser_expression_not_a_constant,

	//An identifier (usually function name) was expected
	ee_parser_expression_identifier_expected,

	//An empty group (enclosed in parens) was specified
	ee_parser_expression_empty_group,

	//An group (enclosed in parens) with too many elements was specified
	ee_parser_expression_overfull_group,

	//Assignment from an empty expression
	ee_parser_expression_empty_assign

} ee_parser_reply;

//Status returned by the evaluator
typedef enum
{
	//User defined functions (ee_function) should return
	// this value, or *smaller*, in case of an internal error.
	ee_evaluator_user_base = -1,

	//All is well
	ee_evaluator_ok = 0,

	//All is well but the expression was empty
	//	and did not return a result.
	ee_evaluator_empty,

	//There are more values than expected left on the stack
	ee_evaluator_stack_extra,

	//A result was expected on the stack but there was none
	ee_evaluator_stack_underflow,
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
	ee_memory_size compilation_size;

	//Data for the execution environment, used by ee_compile and ee_evaluate.
	//Aligned as ee_environment_header.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	ee_memory_size environment_size;

	//Data for the runtime stack, used only by ee_evaluate.
	//Aligned as ee_variable, when allocated separately.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	//This can be allocated as part of the execution environment or separately
	ee_memory_size stack_size;

	//Data for the execution environment, used by ee_compile and ee_evaluate.
	//Aligned as ee_environment_header.
	//Calculated by ee_guestimate.
	//ee_compile will recalculate this and shrink to the actual size used.
	//This can be used instead of environment_size & stack_size when
	//	the runtime stack is allocated inside the execution environment.
	//This is just the sum of environment_size & stack_size taking into account alignment of the latter.
	ee_memory_size full_environment_size;


	//Counts of the various elements
	//Used and updated by ee_compile

	ee_element_count constants;
	ee_element_count variables;
	ee_element_count functions;
	ee_element_count instructions;
	ee_element_count compilation_stack;
	ee_element_count runtime_stack;
} ee_data_size;

//Semi-transparent header of the symbol-table binder
typedef struct
{
	union
	{
		void * align;
		int flags;
	} _;

	ee_memory_size size;
} ee_symboltable_header;

//Text location in the parsed expression
typedef struct
{
	//The first character
	const ee_char_type * start;

	//The last character
	const ee_char_type * end;
} ee_location;

//Compilation data
typedef struct
{
	//Data buffer
	void * data;

	//Size of the data buffer
	ee_memory_size size;
} ee_compilation;

//Evaluation data
typedef struct
{
	//Data buffer
	void * data;

	//Size of the data buffer
	ee_memory_size size;
} ee_evaluation;


//Add
//	all the functions from the vector of functions and
//	all the variables from the vector of variables to the symbol table.
//If the table is not big enough ee_symboltable_memory will be returned and
//	the "size" will hold the requested size, in bytes.
//If the 'size' is zero the needed space will be calculated and returned back in 'size'.
//When both the vectors are NULL will return the minimal size needed (including for any library data).
//Before the first invocation set 'flags' to zero and 'size' to the total allocated size.
//After adding all needed data call once again with both vectors NULL to finalize the symboltable.
//On exit the 'size' field will hold the size taken by the data, after compaction, so the table
//	could be resized to reclaim memory, if needed.
ee_symboltable_reply ee_symboltable_add(
		ee_symboltable_header * symboltable,
		const ee_symboltable_functions functions,
		const ee_symboltable_variables variables);

//Estimate the upper bound of needed memory, in bytes, based on the expression length.
//The calculated memory will accomodate any expression of the given length.
ee_parser_reply ee_estimate(
		ee_memory_size length,
		ee_data_size * size);

//Anayze the expression and guestimate the upper bound of needed memory, in bytes.
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
		const ee_symboltable_header * symboltable,
		const ee_compilation * compilation,
		ee_location * error,
		ee_evaluation * evaluation);

//Evaluate the compiled environment
ee_evaluator_reply ee_evaluate(const ee_evaluation * evaluation, ee_variable_type * result);

#ifdef __cplusplus
}
#endif

#endif // EMEXPR_H
