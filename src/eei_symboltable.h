/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#ifndef EEI_SYMBOLTABLE_H
#define EEI_SYMBOLTABLE_H

#include "emexpr.h"

//Internal API for the symbol table
//---------------------------------
//This API is used to query the symbol table for identifiers.

//We handle only printable characters in the lower ASCII region
enum
{
	eei_symboltable_first_symbol = '!',
	eei_symboltable_last_symbol = '~',
	eei_symboltable_total_symbols = eei_symboltable_last_symbol - eei_symboltable_first_symbol + 1
};

typedef ee_element_count eei_symboltable_element_count;

typedef struct
{
	//These are sizes, in elements.

	//Total number of second-level elements
	ee_memory_size second_level;

	//Total number of third-level elements
	ee_memory_size third_level;

	//Size of the textbook
	ee_memory_size textbook;
} eei_symboltable_usage_data;

typedef struct
{
	//Offsets for the various elements
	//The elements are defined here in the order they appear
	//	inside the symbol table memory, not in the order of usage by the code.
	//These are offsets, in byts.

	// second.book
	ee_memory_size second_level;

	//second.next
	ee_memory_size second_level_next;

	//third.data
	ee_memory_size third_level;

	//third.locations
	ee_memory_size locations;

	//second.textbook
	ee_memory_size textbook;
} eei_symboltable_offsets;

//Holds basic data about a function
typedef struct
{
	ee_function_flags flags;
	ee_arity arity;
} eei_symboltable_function_data;

typedef struct
{
	//Counts of next-level elements at each index for this element
	eei_symboltable_element_count count;

	//Start index in the next-level table for this element
	eei_symboltable_element_count index;
} eei_symboltable_level;

typedef struct
{
	//The count of elements at this level is exactly eei_symboltable_total_symbols

	//Indexes into the second-level search tables based on the first symbol
	//Counts of elements in the second-level search table for each first symbol
	eei_symboltable_level * next;
} eei_symboltable_first_level;

typedef struct
{
	//Index into the textbook where the text, from the second character of the symbol, starts.
	//Length of each symbol, including the first character.
	eei_symboltable_level * book;

	//The textbook holding symbols, from the second character forward
	ee_char_type * textbook;

	//Index into the third-level table where function flag/arity combinations for a symbol start.
	eei_symboltable_element_count * next;
} eei_symboltable_second_level;

typedef union
{
	//A field used to handle the location without differenting between the held types.
	void * ptr;

	//A bound variable
	ee_variable variable;

	//A user-functions
	ee_function function;
} eei_symboltable_location;

typedef struct
{
	//The arity/flag combinations for a samely-named function.
	//A flag of ee_function_flag_invalid denotes a variable.
	eei_symboltable_function_data * data;

	//Locations in memory of the variable/function
	eei_symboltable_location * locations;
} eei_symboltable_third_level;

typedef struct
{
	//This does not stores the actual data, only pointers to it,
	//	since this is all that is needed to search through the symbol table.

	//Amount of data actually allocated in memory
	eei_symboltable_usage_data * allocated;

	//Count of elements actually used
	eei_symboltable_usage_data * used;

	//The first level is the first symbol only of each element
	//The indexes point into the second level
	eei_symboltable_first_level first;

	//Second level data is used for comparing the elements(from the second symbol) with
	//	the data stored in the textbook.
	//The indexes point into the third level
	eei_symboltable_second_level second;

	//Third level data is used for finding the requested items.
	eei_symboltable_third_level third;
} eei_symboltable;

//Internal direct data held inside a symbol table data structure
typedef struct
{
	ee_symboltable_header header;

	//Byte offsets from "data" for the various tables
	eei_symboltable_offsets offsets;

	//Used element counts for each of the items in 'offsets'
	eei_symboltable_usage_data used;

	//Allocated element counts for each of the items in 'offsets'
	eei_symboltable_usage_data allocated;

	//Requested element counts for each of the items in 'offsets'
	eei_symboltable_usage_data requested;

	//This data is defined directly here since its size is constant
	eei_symboltable_level first_level[eei_symboltable_total_symbols];

	char data[1];
} eei_symboltable_struct;

typedef struct
{
	int first;
	int second;
	int third;
} eei_symboltable_index;

ee_symboltable_reply eei_symboltable_find_text(
		const eei_symboltable * st,
		const ee_char_type * start,
		const ee_element_count length,
		eei_symboltable_index * index);

ee_symboltable_reply eei_symboltable_get(
		const eei_symboltable * st,
		eei_symboltable_index * index,
		ee_arity arity,
		ee_function_flags any_flags,
		ee_function_flags all_flags,
		ee_function_flags not_flags);

void eei_symboltable_calculate_pointers(
		eei_symboltable * pointers,
		eei_symboltable_struct * full);

#endif // EEI_SYMBOLTABLE_H
