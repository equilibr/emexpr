/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022-2024 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EEI_VM_H
#define EEI_VM_H

#include "emexpr.h"

//Virtual machine building
//------------------------

typedef struct
{
	ee_variable_type * constants;
	ee_variable_type ** variables;
	ee_function * functions;
	ee_vm_bytecode * instructions;
} eei_vmmake_data;

typedef struct
{
	ee_element_count constants;
	ee_element_count variables;
	ee_element_count functions;
	ee_element_count instructions;

	//This tracks the run-time stack usage
	ee_element_count stack;
} eei_vmmake_counters;

typedef struct
{
	//Pointers to the data tables
	eei_vmmake_data data;

	//Allocated/maximum size for each datum
	eei_vmmake_counters max;

	//Currently used size for each datum
	eei_vmmake_counters current;

} eei_vmmake_environment;


ee_parser_reply eei_vmmake_load_constant(
		eei_vmmake_environment * vm,
		const ee_variable_type constant);

ee_parser_reply eei_vmmake_load_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable);

ee_parser_reply eei_vmmake_execute_functions(
		eei_vmmake_environment * vm,
		ee_function function,
		int arity);

ee_parser_reply eei_vmmake_store_variable(
		eei_vmmake_environment * vm,
		ee_variable_type * variable);


//Virtual machine execute
//-----------------------

//Holds the VM environment data
typedef struct
{
	//The constants table
	const ee_variable_type * constants;

	//The (pointers-to) variables table
	ee_variable_type * const * variables;

	//The functions table
	const ee_function * functions;

	//Start of the VM bytecode stream
	const ee_vm_bytecode * instructions;

	//Pointer to the runtime stack
	ee_variable_type * stack;

	//Count of instructions to execute
	ee_element_count instruction_count;
} eei_vm_environment;


ee_evaluator_reply eei_vm_execute(const eei_vm_environment * vm_environment);


#endif // EEI_VM_H
