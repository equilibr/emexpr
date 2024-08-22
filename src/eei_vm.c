/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022-2024 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "eei_vm.h"


//User-selectable definitions
//---------------------------

//Enables reusing constant/variable/function values already in the table.
//When enabled might make building slightly slower at the expence of higher memory usage
// and possible slower execution(due to not being able to use optimized instructions).
#define EEI_REUSE_TABLES 1


//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_VARIBALE_LOAD)
#	define EEI_VARIABLE_LOAD EE_USER_VARIBALE_LOAD
#else
#	define EEI_VARIABLE_LOAD(dst,src) *dst = *src
#endif

#if defined(EE_USER_VARIBALE_STORE)
#	define EEI_VARIABLE_STORE EE_USER_VARIBALE_STORE
#else
#	define EEI_VARIABLE_STORE(dst,src) *dst = *src
#endif


//The VM instruction is decoded using the following definitions.
enum
{
	//Shift value for the immediate bits
	eei_vm_immediate_shift = 0,

	//Bits allocated to the instrictions
	eei_vm_instruction_bits = 3,

	//Shift value for the instruction bits
	eei_vm_instruction_shift = 8 - eei_vm_instruction_bits,

	//Bits allocated to the immediate
	eei_vm_immediate_bits = eei_vm_instruction_shift,


	//Mask for the instruction
	eei_vm_mask_instruction = ((1U << eei_vm_instruction_bits) - 1) << eei_vm_instruction_shift,

	//Mask for the immediate values for each instruction
	eei_vm_mask_immediate_shifted = ((1U << eei_vm_immediate_bits) - 1),
	eei_vm_mask_immediate = eei_vm_mask_immediate_shifted << eei_vm_immediate_shift,



	//Add immediate to runtime accumulator
	eei_vm_instruction_immediate = (0x00) << eei_vm_instruction_shift,

	//Push a constant to the execution stack
	eei_vm_instruction_constant = (0x01) << eei_vm_instruction_shift,

	//Read and push a variable to the execution stack
	eei_vm_instruction_variable = (0x02) << eei_vm_instruction_shift,

	//Set the arity of the nearest function to execute
	eei_vm_instruction_arity = (0x03) << eei_vm_instruction_shift,

	//Execute a function with an arity of 1 or 2
	eei_vm_instruction_function2 = (0x04) << eei_vm_instruction_shift,

	//Execute a function
	eei_vm_instruction_function = (0x05) << eei_vm_instruction_shift,

	//Pop a variable from the stack and store it
	eei_vm_instruction_store = (0x06) << eei_vm_instruction_shift,
};


//Virtual machine building
//------------------------

ee_parser_reply eei_vmmake_append_instruction(
		eei_vmmake_environment * vm,
		ee_vm_bytecode instruction,
		unsigned int immediate)
{
	//Insert an instruction with an immediate

	//Count number of immediate opcodes needed
	int total = -1;
	unsigned int data = immediate;
	do
	{
		total++;
		data >>= eei_vm_immediate_bits;
	} while (data);

	if (vm->current.instructions + total + 1 > vm->max.instructions)
		return ee_parser_instrictions_overflow;

	int i = total;
	while (i)
	{
		vm->data.instructions[vm->current.instructions++] =
				(((immediate >> (eei_vm_immediate_bits * i)) & eei_vm_mask_immediate_shifted)
				<< eei_vm_immediate_shift)
				| eei_vm_instruction_immediate;

		i--;
	}

	vm->data.instructions[vm->current.instructions++] =
			((immediate & eei_vm_mask_immediate_shifted)
			<< eei_vm_immediate_shift)
			| instruction;

	return ee_parser_ok;
}

ee_parser_reply eei_vmmake_load_constant(
		eei_vmmake_environment * vm,
		const ee_variable_type constant)
{
	//Add a constant load bytecode

	//Linear search through the constants table for a match
	//We trade speed during parsing for lower memory usage of the final environment
	int index;
#	if EEI_REUSE_TABLES
	for (index = 0; index < vm->current.constants; ++index)
		if (vm->data.constants[index] == constant)
			break;
#	else
	index = vm->current.constants;
#	endif

	if (index == vm->current.constants)
	{
		//Nothing found, we need to add a new constant to the table

		if (vm->current.constants >= vm->max.constants)
			return ee_parser_constants_overflow;

		vm->data.constants[index] = constant;
		vm->current.constants++;
	}

	//Update stack usage
	vm->current.stack++;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	return eei_vmmake_append_instruction(vm, eei_vm_instruction_constant, index);
}

ee_parser_reply eei_vmmake_load_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable)
{
	//Add a variable load bytecode

	//Sanity check for non-NULL pointer
	if (!variable)
		return ee_parser_binding;

	//Linear search through the variables table for a match
	//We trade speed during parsing for lower memory usage of the final environment
	int index;
#	if EEI_REUSE_TABLES
	for (index = 0; index < vm->current.variables; ++index)
		if (vm->data.variables[index] == variable)
			break;
#	else
	index = vm->current.variables;
#	endif

	if (index == vm->current.variables)
	{
		//Nothing found, we need to add a new variable to the table

		if (vm->current.variables >= vm->max.variables)
			return ee_parser_variables_overflow;

		vm->data.variables[index] = variable;
		vm->current.variables++;
	}

	//Update stack usage
	vm->current.stack++;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	return eei_vmmake_append_instruction(vm, eei_vm_instruction_variable, index);
}

ee_parser_reply eei_vmmake_execute_functions(
		eei_vmmake_environment * vm,
		ee_function function,
		int arity)
{
	//Add a function execute bytecode

	//Sanity check for non-NULL pointer
	if (!function)
		return ee_parser_binding;

	//Linear search through the functions table for a match
	//We trade speed during parsing for lower memory usage of the final environment
	int index;
#	if EEI_REUSE_TABLES
	for (index = 0; index < vm->current.functions; ++index)
		if (vm->data.functions[index] == function)
			break;
#	else
	index = vm->current.functions;
#	endif

	if (index == vm->current.functions)
	{
		//Nothing found, we need to add a new function to the table

		if (vm->current.functions >= vm->max.functions)
			return ee_parser_functions_overflow;

		vm->data.functions[index] = function;
		vm->current.functions++;
	}

	//Make sure the stack will hold enough elements
	if (vm->current.stack < arity)
		return ee_parser_stack_runtime_underflow;

	//Update stack usage: "arity" elements will be popped and a single result pushed
	vm->current.stack += 1 - arity;
	if (vm->max.stack < vm->current.stack)
		vm->max.stack = vm->current.stack;

	if (
		((arity == 1) || (arity == 2))
		&& (index <= (((1 << (eei_vm_instruction_bits-1)) - 1))) )
	{
		//Use the special command
		return eei_vmmake_append_instruction(
					vm,
					eei_vm_instruction_function2,
					index | ( (arity == 1) ? 0 : (1 << (eei_vm_immediate_bits - 1)) ));
	}

	if (arity != 0)
	{
		const ee_parser_reply reply =
				eei_vmmake_append_instruction(vm, eei_vm_instruction_arity, arity);

		if (reply != ee_parser_ok)
			return reply;
	}

	return eei_vmmake_append_instruction(vm, eei_vm_instruction_function, index);
}

ee_parser_reply eei_vmmake_store_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable)
{
	//Add a variable store bytecode

	//Sanity check for non-NULL pointer
	if (!variable)
		return ee_parser_binding;

	//Make sure there is something on the stack
	if (vm->current.stack == 0)
		return ee_parser_expression_empty_assign;

	//Linear search through the variables table for a match
	//We trade speed during parsing for lower memory usage of the final environment
	int index;
#	if EEI_REUSE_TABLES
	for (index = 0; index < vm->current.variables; ++index)
		if (vm->data.variables[index] == variable)
			break;
#	else
	index = vm->current.variables;
#	endif

	if (index == vm->current.variables)
	{
		//Nothing found, we need to add a new variable to the table

		if (vm->current.variables >= vm->max.variables)
			return ee_parser_variables_overflow;

		vm->data.variables[index] = variable;
		vm->current.variables++;
	}

	//Update stack usage
	vm->current.stack--;

	return eei_vmmake_append_instruction(vm, eei_vm_instruction_store, index);
}


//Virtual machine execute
//-----------------------

//Holds VM runtime data
typedef struct
{
	//Command parameter accumulator
	unsigned int accumulator;

	//Function arity accumulator
	ee_element_count arity;

	//Pointer to the one-past stack top
	//When pushing this address is the new write address
	ee_variable_type * stack_top;

	//The VM bytecode to execute next
	const ee_vm_bytecode * instruction;

	//One-past the last instruction to execute
	const ee_vm_bytecode * instruction_end;
} eei_vm_runtime;

//Exeucute the VM environment
ee_evaluator_reply eei_vm_execute(const eei_vm_environment * vm_environment)
{
	eei_vm_runtime rt =
	{
		0,
		0,
		vm_environment->stack,
		vm_environment->instructions,
		vm_environment->instructions + vm_environment->instruction_count
	};

	//We assume the environment was correctly constructed
	//	and all needed data is allocated and valid, thus
	//	no overflow, underflow or null pointer tests are being made.

	while (rt.instruction != rt.instruction_end)
	{
		//Update the accumulator from the immediate bits
		rt.accumulator <<= eei_vm_immediate_bits;
		rt.accumulator |=
				((*rt.instruction) >> eei_vm_immediate_shift)
				& eei_vm_mask_immediate_shifted;

		//Decode and advance the current instruction
		switch ((*rt.instruction++) & eei_vm_mask_instruction)
		{
			case eei_vm_instruction_immediate:
				//Do nothing
				//This allows using this instruction to accumulate any value
				break;

			case eei_vm_instruction_constant:
				//The constant table holds the values directly so just
				//	copy the one at the requested index.
				*rt.stack_top++ =
						vm_environment->constants[rt.accumulator];

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;

			case eei_vm_instruction_variable:
				//Use a user-modifiable load.
				//At this point the destination holds garbage and can be safely ignored.
				//The variables table holds pointers to the variables
				//	so the requested index needs to be dereferenced to access
				//	the actual user variable.
				EEI_VARIABLE_LOAD(rt.stack_top++, vm_environment->variables[rt.accumulator]);

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;

			case eei_vm_instruction_arity:
				//Set the arity for the next function to execute
				rt.arity = rt.accumulator;

				//Test if we can go directly to the "function" case since
				//	this is the expected sequence when the function table is small.
				//The instruction was already advanced in the switch header
				if (((*rt.instruction) & eei_vm_mask_instruction) != eei_vm_instruction_function)
				{
					//Clear the accumulator in preparation for the next instruction
					rt.accumulator = 0;
					break;
				}

				//Set the accumulator from the immediate bits and advance the instruction
				rt.accumulator =
						((*rt.instruction++) >> eei_vm_immediate_shift)
						& eei_vm_mask_immediate_shifted;

				//fall through

			case eei_vm_instruction_function2:
				if (rt.arity == 0)
				{
					//Only execute this if we did not come directly from the case above

					//Set the arity according to the high bit of the accumulator
					rt.arity =
							(rt.accumulator & (1 << (eei_vm_immediate_bits - 1)))
							? 2
							: 1;

					//Clear that bit to use the rest as the index
					rt.accumulator &= ~(1 << (eei_vm_immediate_bits - 1));
				}

				//fall through

			case eei_vm_instruction_function:
			{
				int error;
				ee_variable_type function_result;

				//Adjust the stack by the arity so the function would receive
				//	the correct actuals
				rt.stack_top -= rt.arity;

				if (rt.stack_top < vm_environment->stack)
					return ee_evaluator_stack_underflow;

				//Call the function
				error =
						vm_environment->functions[rt.accumulator]
						(
							rt.arity,
							rt.stack_top,
							&function_result
						);

				//Push the result to the stack top
				*rt.stack_top++ = function_result;

				//Clear the arity for the next function to execute
				rt.arity = 0;

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;

				//Any function can return a non-zero error code.
				//We do not analyze its meaning but simply halt and return
				//	it as it is.
				if (error != 0)
					return error;

				break;
			}

			case eei_vm_instruction_store:
				//Use a user-modifiable store.
				//After the store the source is popped of the stack and no longer used.
				//The variables table holds pointers to the variables
				//	so the requested index needs to be dereferenced to access
				//	the actual user variable.
				EEI_VARIABLE_STORE(vm_environment->variables[rt.accumulator], --rt.stack_top);

				//Clear the accumulator in preparation for the next instruction
				rt.accumulator = 0;
				break;
		}
	};

	//The stack top holds the result of the evaluation
	const int stack = rt.stack_top - vm_environment->stack;

	if (stack == 0)
		return ee_evaluator_empty;

	if (stack > 1)
		return ee_evaluator_stack_extra;

	return ee_evaluator_ok;
}
