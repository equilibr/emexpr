/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

#include "eei_symboltable.h"
#include "emexpr.h"
#include <stddef.h>


//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_FUNCTION_LIBRARY)
#	define EEI_DEFAULT_LIBRARY 0
#	define EEI_FUNCTION_LIBRARY EE_USER_FUNCTION_LIBRARY
#else
#	define EEI_DEFAULT_LIBRARY 1
#	define EEI_FUNCTION_LIBRARY eei_operators_library
#endif

//Default implementations of user modifiable items
//------------------------------------------------

//This provides the library default implementation of things the user can modify/override.
//The implementation is at the top of the code to make sure it only uses declarations visible
//	to the external user of the library, e.g. our header file.

#if EEI_DEFAULT_LIBRARY

int eei_operator_subneg(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	switch (arity)
	{
		case 1:
			*result = -actuals[0];
			return 0;

		case 2:
			*result = actuals[0] - actuals[1];
			return 0;

		default:
			return 1;
	}
}

int eei_operator_plus(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] + actuals[1];
	return 0;
}

int eei_operator_mul(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] * actuals[1];
	return 0;
}

int eei_operator_div(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = actuals[0] / actuals[1];
	return 0;
}

int eei_operator_equal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] == actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_greater(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] > actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_less(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] < actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_notequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] != actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_greaterequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] >= actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_lessequal(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] <= actuals[1]) ? 1 : 0;
	return 0;
}

int eei_operator_fold_and(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r != 0); ++i)
		r *= (actuals[i] != 0) ? 1 : 0;

	*result = r;
	return 0;
}

int eei_operator_fold_or(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != 0) ? 1 : 0;

	for (int i = 1; (i < arity) && (r == 0); ++i)
		r += (actuals[i] != 0) ? 1 : 0;

	*result = (r != 0) ? 1 : 0;
	return 0;
}

int eei_operator_fold_xor(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	ee_variable_type r = (actuals[0] != actuals[1]) ? 1 : 0;

	for (int i = 2; i < arity; ++i)
		r = (actuals[i] != r) ? 1 : 0;

	*result = r;
	return 0;
}

int eei_operator_not(ee_element_count arity, const ee_variable_type * actuals, ee_variable result)
{
	(void)arity;
	*result = (actuals[0] == 0) ? 1 : 0;
	return 0;
}

static const ee_symboltable_function eei_operators_library[] =
{
	{eei_operator_subneg,"-",1,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_subneg,"-",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_plus,"+",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_mul,"*",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_div,"/",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},

	{eei_operator_equal,"==",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greater,">",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_less,"<",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_notequal,"!=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_greaterequal,">=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_lessequal,"<=",2,ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},

	{eei_operator_fold_and,"&&",-2,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_or,"||",-2,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_fold_xor,"^^",-3,ee_function_flag_prefix | ee_function_flag_infix | ee_function_flag_operator | ee_function_flag_pure},
	{eei_operator_not,"!",1, ee_function_flag_prefix | ee_function_flag_operator | ee_function_flag_pure},
	{0,0,0,0}
};

#endif


//Symbol table lookup
//-------------------

void eei_symboltable_invalidate_index(eei_symboltable_index * index, int level)
{
	if (level <= 1)
		index->first = -1;

	if (level <= 2)
		index->second = -1;

	if (level <= 3)
		index->third = -1;

	if (level <= 4)
		index->data = -1;
}

ee_symboltable_reply eei_symboltable_find_text(
		const eei_symboltable * st,
		const ee_char_type * start,
		const ee_element_count length,
		eei_symboltable_index * index)
{
	//Finds the text given by (start, length) inside the symbol table.
	//Returns the index into st->second.next since it holds both
	//	the index into the third level where this items starts as well as the
	//	count of relevant third-level items.
	//Will retrun <0 on failure

	//We assume length is always > 0 and that
	//	the text to search is valid (non-zero) for it's entire length.

	eei_symboltable_invalidate_index(index, 0);

	//Select first level index based on the first symbol
	const int index1 = *start - eei_symboltable_first_symbol;

	index->first = index1;

	//If there is no second level data for this symbol...
	if (st->first.next[index1].count == 0)
		//...there is nothing more to be done
		return ee_symboltable_no_name;

	//Advance to the next symbol to simplify all following code
	start++;

	//The starting index
	int index2 = st->first.next[index1].index;

	//The stopping index
	int index2_stop = index2 + st->first.next[index1].count;

	for (; index2 < index2_stop; ++index2)
	{
		//If the text lenghts don't match...
		if (st->second.book[index2].count != length)
			//...no use in further testing
			continue;

		//A matching length is found, see if the textbook also matches

		if (length == 2)
		{
			//At length of 2 the second symbol is stored inside the *INDEX*
			if (st->second.book[index2].index != start[0])
				//Textbook mismatch
				continue;
		}
		else if (length > 2)
		{
			//Only look inside the textbook when the data is actually stored there

			const int adjusted_length = length - 1;

			//Make sure the comparison will not read past the end of the textbook
			if ((st->second.book[index2].index + adjusted_length) > st->used->textbook)
				//This should never happen, but it did...
				return ee_symboltable_out_of_bounds;

			const ee_char_type * textbook =
					&st->second.textbook[ st->second.book[index2].index ];

			int i = 0;
			for (; i < adjusted_length; ++i)
				if (textbook[i] != start[i])
					break;

			if (i != adjusted_length)
				//Textbook mismatch
				continue;
		}

		//This is it!
		index->second = index2;
		return ee_symboltable_ok;
	}

	//No match was found, return failure
	return ee_symboltable_no_name;
}

ee_symboltable_reply eei_symboltable_get_variable(
		const eei_symboltable * st,
		eei_symboltable_index * index)
{
	//Returns a pointer to a variable given an index returned by eei_symboltable_find_text

	eei_symboltable_invalidate_index(index, 3);

	//Early break when nothing was found
	if (index->second < 0)
		return ee_symboltable_no_name;

	//Make sure a variable actually exists at that index in the third level
	int index3 = st->second.next[index->second].index;
	int index3_stop = index3 + st->second.next[index->second].count;

	for (; index3 < index3_stop; ++index3)
		if (st->third.data[index3].flags == ee_function_flag_invalid)
			break;

	if (index3 == index3_stop)
		//There is no variable at this index
		return ee_symboltable_no_type;

	index->third = index3;

	//Find the index into the final vector
	int accumulator = 0;
	for (int i = 0; i < index3; ++i)
		accumulator +=
				(st->third.data[i].flags == ee_function_flag_invalid)
				? 1
				: 0;

	if (accumulator >= st->used->variables)
		//Something went terribly wrong!
		return ee_symboltable_out_of_bounds;

	index->data = accumulator;
	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_get_function(
		const eei_symboltable * st,
		eei_symboltable_index * index,
		ee_arity arity,
		ee_function_flags any_flags,
		ee_function_flags all_flags,
		ee_function_flags not_flags)
{
	//Returns a pointer to a function given an index returned by eei_symboltable_find_text
	//This takes into account the requested arity and all flag masks

	eei_symboltable_invalidate_index(index, 3);

	//Early break when nothing was found
	if (index->second < 0)
		return ee_symboltable_no_name;

	//Find the matching function at that index in the third level
	int index3 = st->second.next[index->second].index;
	int index3_stop = index3 + st->second.next[index->second].count;

	int seen_function = 0;

	for (; index3 < index3_stop; ++index3)
	{
		seen_function |= st->third.data[index3].flags != ee_function_flag_invalid;

		//If set, at least one of any_flags must be present
		if (any_flags && ((st->third.data[index3].flags & any_flags) == 0))
			continue;

		//If set, all of all_flags must be present
		if (all_flags && ((st->third.data[index3].flags & all_flags) != all_flags))
			continue;

		//If set, none from not_flags must be present
		if (not_flags && ((st->third.data[index3].flags & not_flags) != 0))
			continue;

		//Test for correct arity
		ee_arity found_arity = st->third.data[index3].arity;

		if (found_arity >= 0)
		{
			//This is regular function that must get an exact number of parameters
			if (found_arity != arity)
				continue;
		}
		else
		{
			//We're looking at a variadic function
			//Calculate the number of mandatory parameters
			found_arity = (ee_arity)(-(found_arity + 1));

			if (found_arity > arity)
				continue;
		}

		//All tests passed - we found what we're looking for!
		break;
	}

	if (index3 == index3_stop)
		//There is no matching function at this index
		return seen_function ? ee_symboltable_filtered : ee_symboltable_no_type;

	index->third = index3;

	//Find the index into the final vector
	int accumulator = 0;
	for (int i = 0; i < index3; ++i)
		accumulator +=
				(st->third.data[i].flags != ee_function_flag_invalid)
				? 1
				: 0;

	if (accumulator >= st->used->functions)
		//Something went terribly wrong!
		return ee_symboltable_out_of_bounds;

	index->data = accumulator;
	return ee_symboltable_ok;
}

//Symbol table building
//---------------------

ee_symboltable_reply eei_symboltable_add_textbook(
		eei_symboltable * st,
		const ee_char_type * start,
		ee_element_count length,
		eei_symboltable_element_count * book_index)
{
	//Add the text into the textbook and fill the data according to the index
	//Returns the textbook idnex in book_index

	if (length == 1)
	{
		//This is a trivial case when nothing is stored in the textbook
		*book_index = 0;
		return ee_symboltable_ok;
	}

	if (length == 2)
	{
		//A length of 2 is handled specially
		//The second symbol is stored inside the *INDEX* element thus avoiding the need
		//	to actually use the textbook itself.
		//This allows for all symbols of length 2 not to take any space in the textbook.
		//Currently this includes, at least, all recognised operators.
		*book_index = start[1];
		return ee_symboltable_ok;
	}

	//This is a long symbol and it should reside inside the textbook.
	//If the symbol already exists in the textbook we should reuse that data.
	//If the symbols' prefix exists at the textbooks' end we should resuse what we can
	//	and extend the textbook to include the rest.
	//Otherwise the symbol is just appended to the end of the textbook.

	//The following algorithm is a naive O(n^2) scan.
	//Given the symbol table is usually appended only once in the lifetime of the program
	//	simplicity if preferred over performance.

	//Advance to the next symbol to simplify all following code
	start++;
	length--;

	//Make sure we can always perform a full comparison. The left-over will be handled separately.
	int stop_index = st->used->textbook - length + 1;
	int start_index;
	int matched = 0;

	for (start_index = 0; !matched && (start_index < stop_index); ++start_index)
	{
		int i = 0;
		for (; i < length; ++i)
			if (st->second.textbook[start_index + i] != start[i])
				break;

		matched = i == length;
	}

	if (matched)
	{
		//The symbol exists in the textbook in it's entirety, so nothing more needs to be done
		*book_index = start_index;
		return ee_symboltable_ok;
	}

	//We need to search for the symbols' prefix inside the textbooks' suffix.
	stop_index = st->used->textbook;

	for (; !matched && (start_index < stop_index); ++start_index)
	{
		const int prefix = stop_index - start_index;

		int i = 0;
		for (; i < prefix; ++i)
			if (st->second.textbook[start_index + i] != start[i])
				break;

		matched = i == prefix;
	}

	//At this point start_index is is the best possible match, even if it's past the textbooks' end
	//Calculate how much data we need to append to the textbook
	const int leftover = length - (stop_index - start_index);

	//Get more space
	st->used->textbook += leftover;

	if (st->used->textbook > st->allocated->textbook)
		//We're out of free space!
		return ee_symboltable_memory;

	//Copy the data to the textbook.
	//We simply copy all of the text to avoid doing funky calculations for edge cases that would
	//	not really save us much processing.
	for (int i = 0; i < length; ++i)
		st->second.textbook[start_index + i] = start[i];

	*book_index = start_index;
	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_add_text(
		eei_symboltable * st,
		const ee_char_type * start,
		ee_element_count length,
		eei_symboltable_index * index)
{
	//Verify there is enough space to add text
	if (st->used->second_level >= st->allocated->second_level)
		return ee_symboltable_memory;

	//Try to insert this symbol into the textbook as early as possible
	//	 since the outcome is hard to predict.
	eei_symboltable_element_count book_index;
	{
		const ee_symboltable_reply treply = eei_symboltable_add_textbook(st,start,length,&book_index);
		if (treply != ee_symboltable_ok)
			return treply;
	}

	//At this point the text is already inside the textbook
	//Now we need to add it to the first 2 levels

	//Find the second-level insertion point.
	const int insertion =
			(st->first.next[index->first].count == 0)

			//Adding to an empty first-level is trivial: Just use the space at the end
			? (st->used->second_level)

			//Otherwise add at the end of the existing elements for this first level
			: (st->first.next[index->first].index + st->first.next[index->first].count);

	if (st->first.next[index->first].count == 0)
		//Only update a new first-level with the index
		index->second = insertion;
	else
		//Save this to allow restoring it later since it might be modified by the index-update loop
		index->second = st->first.next[index->first].index;

	if (insertion != st->used->second_level)
	{
		//We need to move everything past the insertion point to preserve continuity

		for (int destination = st->used->second_level; destination > insertion; --destination)
		{
			st->second.book[destination].index = st->second.book[destination-1].index;
			st->second.book[destination].count = st->second.book[destination-1].count;
			st->second.next[destination].index = st->second.next[destination-1].index;
			st->second.next[destination].count = st->second.next[destination-1].count;
		}

		//We also need to update all first-level indexes that pointed into the moved elements
		//	of the second level.

		for (int i = 0; i < eei_symboltable_total_symbols; ++i)
			if (st->first.next[i].index >= insertion)
				st->first.next[i].index++;
	}

	//(re)set the index
	st->first.next[index->first].index = index->second;
	st->first.next[index->first].count++;
	st->used->second_level++;

	//Write the textbook data into the element
	st->second.book[insertion].count = length;
	st->second.book[insertion].index = book_index;

	//This is a new symbol so it has no third level data
	st->second.next[insertion].count = 0;

	//The caller should set the index once it is knwon
	index->second = insertion;

	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_add_third(
		eei_symboltable * st,
		eei_symboltable_index * index,
		const ee_char_type * start,
		const ee_element_count length)
{
	//Verify there is enough space to add new data
	if (st->used->third_level >= st->allocated->third_level)
		return ee_symboltable_memory;

	if (index->second == -1)
	{
		//The symbol needs to be added
		const ee_symboltable_reply reply =
				eei_symboltable_add_text(st, start, length, index);

		if (reply != ee_symboltable_ok)
			return reply;
	}

	//At this point the first two levels are filled and the index points to them

	//Find the third-level insertion point.
	const int insertion =
			(st->second.next[index->second].count == 0)

			//Adding to an empty second-level is trivial: Just use the space at the end
			? (st->used->third_level)

			//Otherwise add at the end of the existing elements for this second level
			: (st->second.next[index->second].index + st->second.next[index->second].count);

	if (st->second.next[index->second].count == 0)
		//Only update a new first-level with the index
		index->third = insertion;
	else
		//Save this to allow restoring it later since it might be modified by the index-update loop
		index->third = st->second.next[index->second].index;

	if (insertion != st->used->third_level)
	{
		//We need to move everything past the insertion point to preserve continuity

		for (int destination = st->used->third_level; destination > insertion; --destination)
		{
			st->third.data[destination].flags = st->third.data[destination-1].flags;
			st->third.data[destination].arity = st->third.data[destination-1].arity;
		}

		//We also need to update all second-level indexes that pointed into the moved elements
		//	of the third level.

		for (int i = 0; i < st->used->second_level; ++i)
			if (st->second.next[i].index >= insertion)
				st->second.next[i].index++;
	}

	//(re)set the index
	st->second.next[index->second].index = index->third;
	st->second.next[index->second].count++;
	st->used->third_level++;

	//The caller should fill the data
	index->third = insertion;

	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_add_variable(
		eei_symboltable * st,
		const ee_char_type * start,
		const ee_element_count length,
		ee_variable_type * item)
 {
	 eei_symboltable_index index;
	 ee_symboltable_reply reply;

	 //First we try to find if a varaible with the same name already exists.
	 eei_symboltable_find_text(st, start, length, &index);
	 reply = eei_symboltable_get_variable(st, &index);

	 //If so the existing value will simply be re-bound to the new variable.
	 if (reply == ee_symboltable_ok)
	 {
		 st->third.variables[index.data] = item;
		 return ee_symboltable_ok;
	 }

	 //This is new data that needs to be added

	 //Make sure there is space in the data vector
	 if (st->used->variables >= st->allocated->variables)
		 return ee_symboltable_memory;

	 //Add third-level data for a variable
	 reply = eei_symboltable_add_third(st,&index,start,length);
	 if (reply != ee_symboltable_ok)
		 return reply;

	 //Write the data into the third-level
	 st->third.data[index.third].flags = ee_function_flag_invalid;
	 st->third.data[index.third].arity = 0;

	 //Find the index for the new item
	 {
		 int accumulator = 0;
		 for (int i = 0; i < index.third; ++i)
			 accumulator +=
					 (st->third.data[i].flags == ee_function_flag_invalid)
					 ? 1
					 : 0;

		 if (accumulator > st->used->variables)
			 //Something went terribly wrong!
			 return ee_symboltable_out_of_bounds;

		 index.data = accumulator;
	 }

	 if (index.data < st->used->variables)
	 {
		 //We need to move everything to make space for the new data
		 for (int destination = st->used->variables; destination > index.data; --destination)
			 st->third.variables[destination] = st->third.variables[destination-1];
	 }

	 st->used->variables++;
	 st->third.variables[index.data] = item;
	 return ee_symboltable_ok;
 }

ee_symboltable_reply eei_symboltable_add_function(
		eei_symboltable * st,
		const ee_char_type * start,
		const ee_element_count length,
		ee_function item,
		ee_arity arity,
		ee_function_flags flags)
 {
	 eei_symboltable_index index;
	 ee_symboltable_reply reply;

	 //First we try to find if a function with the same name, arity and flags already exists.
	 eei_symboltable_find_text(st, start, length, &index);
	 reply = eei_symboltable_get_function(st, &index, arity, 0, flags, 0);

	 //If so the existing function will simply be overwritten with the new one.
	 if (reply == ee_symboltable_ok)
	 {
		 st->third.functions[index.data] = item;
		 return ee_symboltable_ok;
	 }

	 //This is new data that needs to be added

	 //Make sure there is space in the data vector
	 if (st->used->functions >= st->allocated->functions)
		 return ee_symboltable_memory;

	 //Add third-level data for a variable
	 reply = eei_symboltable_add_third(st,&index,start,length);
	 if (reply != ee_symboltable_ok)
		 return reply;

	 //Write the data into the third-level
	 st->third.data[index.third].flags = flags;
	 st->third.data[index.third].arity = arity;

	 //Find the index for the new item
	 {
		 int accumulator = 0;
		 for (int i = 0; i < index.third; ++i)
			 accumulator +=
					 (st->third.data[i].flags != ee_function_flag_invalid)
					 ? 1
					 : 0;

		 if (accumulator > st->used->functions)
			 //Something went terribly wrong!
			 return ee_symboltable_out_of_bounds;

		 index.data = accumulator;
	 }

	 if (index.data < st->used->functions)
	 {
		 //We need to move everything to make space for the new data
		 for (int destination = st->used->functions; destination > index.data; --destination)
			 st->third.functions[destination] = st->third.functions[destination-1];
	 }

	 st->used->functions++;
	 st->third.functions[index.data] = item;
	 return ee_symboltable_ok;
 }


//External API semi-opaque structures
//-----------------------------------

enum
{
	//The stucture has allocation count
	eei_symboltable_flag_allocation = 1 << 0,

	//The structure is initialized and can be used
	eei_symboltable_flag_initialized = 1 << 1,

	//The library symbols are loaded
	eei_symboltable_flag_library = 1 << 2,

	//The allocated size was changed and data needs to be moved
	eei_symboltable_flag_reallocated = 1 << 3,

	//The structure is in compacted form and data can not be added
	eei_symboltable_flag_compacted = 1 << 4
};


//Symbol table API utility
//------------------------

//Helper macro to calculate alignment of a type
#define alignof(type) ((ptrdiff_t)&((struct { char c; type d; } *)0)->d)

void eei_symboltable_copy_usagedata(
		eei_symboltable_usage_data * dst,
		const eei_symboltable_usage_data * src)
{
	dst->second_level = src->second_level;
	dst->third_level = src->third_level;
	dst->variables = src->variables;
	dst->functions = src->functions;
	dst->textbook = src->textbook;
}

const char * eei_symboltable_calculate_offsets(
		eei_symboltable_usage_data * offsets,
		const eei_symboltable_struct * full,
		const eei_symboltable_usage_data * size)
{
	//Calculate the memory locations of all tables

	const char * base = &full->data[0];
	const char * ptr = base;

	//Second level data - realign
	if (alignof(eei_symboltable_level))
		while ((ptrdiff_t)ptr % alignof(eei_symboltable_level))
			ptr++;

	offsets->second_level = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_symboltable_level) * size->second_level * 2;

	//Third level data
	while ((ptrdiff_t)ptr % alignof(eei_symboltable_function_data))
		ptr++;

	offsets->third_level = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_symboltable_function_data) * size->third_level;

	//Variables
	while ((ptrdiff_t)ptr % alignof(ee_variable))
		ptr++;

	offsets->variables = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_variable) * size->variables;

	//Functions
	while ((ptrdiff_t)ptr % alignof(ee_function))
		ptr++;

	offsets->functions = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_function) * size->functions;

	//Textbook
	while ((ptrdiff_t)ptr % alignof(ee_char_type))
		ptr++;

	offsets->textbook = (ee_memory_size)(ptr - base);
	ptr += sizeof(ee_char_type) * size->textbook;

	return ptr;
}

void eei_symboltable_calculate_pointers(
		eei_symboltable * pointers,
		eei_symboltable_struct * full)
{
	//Fill working pointers from the data

	//First level is filled directly
	pointers->first.next = full->first_level;

	//Fill all counts first
	pointers->allocated = &full->allocated;
	pointers->used = &full->used;

	char * base = &full->data[0];

	//Second level is daisy chained
	pointers->second.book = (eei_symboltable_level*)(base + full->offsets.second_level);
	pointers->second.next = pointers->second.book + pointers->allocated->second_level;

	pointers->third.data = (eei_symboltable_function_data*)(base + full->offsets.third_level);
	pointers->third.variables = (ee_variable*)(base + full->offsets.variables);
	pointers->third.functions = (ee_function*)(base + full->offsets.functions);
	pointers->second.textbook = (ee_char_type*)(base + full->offsets.textbook);
}

int eei_symboltable_calculate_size(
		const eei_symboltable_struct * full,
		const eei_symboltable_usage_data * size,
		eei_symboltable_usage_data * offsets)
{
	eei_symboltable_usage_data dummy_offsets;

	const char * end =
			eei_symboltable_calculate_offsets(
				offsets ? offsets : &dummy_offsets,
				full,
				size);

	return end - (const char*)full;
}

void eei_symboltable_fill_memory(eei_symboltable_struct * full_symboltable)
{
	//First we calculate the current ratio of textbook/other data and use
	//	that for further estimates.
	const int others =
			full_symboltable->allocated.second_level * sizeof(eei_symboltable_element_count) * 4
			+ full_symboltable->allocated.third_level * sizeof(eei_symboltable_function_data)
			+ full_symboltable->allocated.variables * sizeof(ee_variable)
			+ full_symboltable->allocated.functions * sizeof(ee_function);

	//Always allocate *at least* as much space for the textbook as all other data
	int textbookratio = 1;
	if (full_symboltable->allocated.textbook > others)
		textbookratio = full_symboltable->allocated.textbook / others;

	//Calculate the size of a virtual "element" that will be used to estimate how much
	//	we can fit in.
	//We assume a single third-level for a second level but, at the same time,
	//	overcompensate by assuming both varaibles and function will be present for a symbol.
	const int element =
			sizeof(eei_symboltable_element_count) * 4
			+ sizeof(eei_symboltable_function_data)
			+ sizeof(ee_variable)
			+ sizeof(ee_function);

	//Calculate the size we can allocate for all other elements
	const int totalsize = full_symboltable->header.size - (&full_symboltable->data[0] - (const char*)full_symboltable);
	const int othersize = totalsize	/ (textbookratio+1);
	const int elements = othersize / element;

	eei_symboltable_usage_data allocated;
	allocated.second_level = elements;
	allocated.third_level = elements;
	allocated.variables = elements;
	allocated.functions = elements;
	allocated.textbook = totalsize - (elements+1) * element;

	//Reclaulte the final size needed
	eei_symboltable_usage_data offsets;
	const ee_memory_size newsize =
			eei_symboltable_calculate_size(
				full_symboltable,
				&allocated,
				&offsets);

	if (full_symboltable->header.size >= newsize)
	{
		//We can fit everything, so use it
		eei_symboltable_copy_usagedata(&full_symboltable->allocated, &allocated);
		eei_symboltable_copy_usagedata(&full_symboltable->offsets, &offsets);
	}
}

ee_memory_size eei_symboltable_expand(
		eei_symboltable_struct * full_symboltable,
		eei_symboltable_usage_data * size)
{
	//Calculate the new offset and pointers
	//The offsets are immediately saved to the environment since we don't need them anymore

	//The current location and sizes of the data
	eei_symboltable current;
	eei_symboltable_usage_data lengths;
	eei_symboltable_copy_usagedata(&lengths, &full_symboltable->used);
	eei_symboltable_calculate_pointers(&current, full_symboltable);

	//The new location and size of the data
	eei_symboltable_copy_usagedata(&full_symboltable->allocated, size);
	ee_memory_size newsize =
			eei_symboltable_calculate_size(
				full_symboltable,
				size,
				&full_symboltable->offsets);

	eei_symboltable expanded;
	eei_symboltable_calculate_pointers(&expanded, full_symboltable);


	//Copy over the used parts of the data
	//This must be performed in *reverse memory order*!

	for (int i = lengths.textbook-1; i <= 0; --i)
		expanded.second.textbook[i] = current.second.textbook[i];

	for (int i = lengths.functions-1; i <= 0; --i)
		expanded.third.functions[i] = current.third.functions[i];

	for (int i = lengths.variables-1; i <= 0; --i)
		expanded.third.variables[i] = current.third.variables[i];

	for (int i = lengths.third_level-1; i <= 0; --i)
	{
		expanded.third.data[i].arity = current.third.data[i].arity;
		expanded.third.data[i].flags = current.third.data[i].flags;
	}

	for (int i = lengths.second_level-1; i <= 0; --i)
	{
		expanded.second.next[i].index = current.second.next[i].index;
		expanded.second.next[i].count = current.second.next[i].count;
	}

	for (int i = lengths.second_level-1; i <= 0; --i)
	{
		expanded.second.book[i].index = current.second.book[i].index;
		expanded.second.book[i].count = current.second.book[i].count;
	}

	for (int i = eei_symboltable_total_symbols-1; i <= 0; --i)
	{
		expanded.first.next[i].index = current.first.next[i].index;
		expanded.first.next[i].count = current.first.next[i].count;
	}

	return newsize;
}


ee_memory_size eei_symboltable_compact(
		eei_symboltable_struct * full_symboltable,
		eei_symboltable_usage_data * size)
{
	//Calculate the new offset and pointers
	//The offsets are immediately saved to the environment since we don't need them anymore

	//The current location of the data
	eei_symboltable current;
	eei_symboltable_calculate_pointers(&current, full_symboltable);

	//The new location and size of the data
	eei_symboltable_copy_usagedata(&full_symboltable->allocated, size);
	ee_memory_size newsize =
			eei_symboltable_calculate_size(
				full_symboltable,
				size,
				&full_symboltable->offsets);

	eei_symboltable compacted;
	eei_symboltable_calculate_pointers(&compacted, full_symboltable);


	//Copy over the data
	//This must be performed in *memory order*!

	for (int i = 0; i < eei_symboltable_total_symbols; ++i)
	{
		compacted.first.next[i].count = current.first.next[i].count;
		compacted.first.next[i].index = current.first.next[i].index;
	}

	for (int i = 0; i < size->second_level; ++i)
	{
		compacted.second.book[i].count = current.second.book[i].count;
		compacted.second.book[i].index = current.second.book[i].index;
	}

	for (int i = 0; i < size->second_level; ++i)
	{
		compacted.second.next[i].count = current.second.next[i].count;
		compacted.second.next[i].index = current.second.next[i].index;
	}

	for (int i = 0; i < size->third_level; ++i)
	{
		compacted.third.data[i].flags = current.third.data[i].flags;
		compacted.third.data[i].arity = current.third.data[i].arity;
	}

	for (int i = 0; i < size->variables; ++i)
		compacted.third.variables[i] = current.third.variables[i];

	for (int i = 0; i < size->functions; ++i)
		compacted.third.functions[i] = current.third.functions[i];

	for (int i = 0; i < size->textbook; ++i)
		compacted.second.textbook[i] = current.second.textbook[i];

	return newsize;
}

static inline int eei_symboltable_name_length(const ee_char_type * name)
{
	const ee_char_type * p = name;
	while (*++p) ;
	return p - name;
}

static inline int eei_symboltable_estimate_name(const ee_char_type * name)
{
	const int length = eei_symboltable_name_length(name);

	if (length > 2)
		return length - 1;
	else
		return 0;
}

static inline int eei_symboltable_estimate_functions(const ee_symboltable_functions list, int * textbook)
{
	const ee_symboltable_function * item = list;
	int text = 0;

	while (item->name)
	{
		text += eei_symboltable_estimate_name(item->name);
		item++;
	}

	*textbook += text;
	return item - list;
}

static inline int eei_symboltable_estimate_variables(const ee_symboltable_variables list, int * textbook)
{
	const ee_symboltable_variable * item = list;
	int text = 0;

	while (item->name)
	{
		text += eei_symboltable_estimate_name(item->name);
		item++;
	}

	*textbook += text;
	return item - list;
}

void eei_symboltable_estimate_usage(
		const ee_symboltable_functions functions,
		const ee_symboltable_variables variables,
		int library,
		eei_symboltable_usage_data * usage)
{
	int function_count = 0;
	int variable_count = 0;
	int textbook = 0;

	if (library && (EEI_FUNCTION_LIBRARY != NULL))
		//Calculate the size of the library
		function_count += eei_symboltable_estimate_functions(EEI_FUNCTION_LIBRARY,&textbook);

	if (functions)
		function_count += eei_symboltable_estimate_functions(functions,&textbook);

	if (variables)
		variable_count += eei_symboltable_estimate_variables(variables,&textbook);

	usage->second_level = variable_count + function_count;
	usage->third_level = usage->second_level;
	usage->variables = variable_count;
	usage->functions = function_count;
	usage->textbook = textbook;
}

ee_symboltable_reply eei_symboltable_add_functions(eei_symboltable * st, const ee_symboltable_functions list)
{
	const ee_symboltable_function * item = list;

	while (item->name)
	{
		const int length = eei_symboltable_name_length(item->name);

		const ee_symboltable_reply reply =
				eei_symboltable_add_function(
					st,
					item->name,
					length,
					item->item,
					item->arity,
					item->flags);

		if (reply != ee_symboltable_ok)
			return reply;

		item++;
	}

	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_add_variables(eei_symboltable * st, const ee_symboltable_variables list)
{
	const ee_symboltable_variable * item = list;

	while (item->name)
	{
		const int length = eei_symboltable_name_length(item->name);

		const ee_symboltable_reply reply =
				eei_symboltable_add_variable(
					st,
					item->name,
					length,
					item->item);

		if (reply != ee_symboltable_ok)
			return reply;

		item++;
	}

	return ee_symboltable_ok;
}


//External API
//------------

ee_symboltable_reply ee_symboltable_add(
		ee_symboltable_header * symboltable,
		const ee_symboltable_functions functions,
		const ee_symboltable_variables variables)
{
	eei_symboltable_struct * full_symboltable = (eei_symboltable_struct *)symboltable;

	if (symboltable->size <= (ee_memory_size)sizeof(ee_symboltable_header))
	{
		//This is a request for a size estimate

		//Calculate size requierements
		eei_symboltable_usage_data usage;
		eei_symboltable_estimate_usage(functions, variables, 1, &usage);

		const ee_memory_size newsize =
				eei_symboltable_calculate_size(full_symboltable, &usage, NULL);

		if (symboltable->size >= (ee_memory_size)sizeof(eei_symboltable_struct))
		{
			//There is enough space to store the allocations count
			eei_symboltable_copy_usagedata(&full_symboltable->allocated, &usage);
			full_symboltable->header.flags |= eei_symboltable_flag_allocation;
		}

		if (newsize > symboltable->size)
		{
			symboltable->size = newsize;
			return ee_symboltable_memory;
		}
		else
			return ee_symboltable_ok;
	}

	if (symboltable->size < (ee_memory_size)sizeof(eei_symboltable_struct))
		//Not enough memory to do anything at all
		return ee_symboltable_memory;

	if (!(full_symboltable->header.flags & eei_symboltable_flag_initialized))
	{
		//The offset/usage data is not setup
		//We need to do that before the symbol table can be used at all

		if (!(full_symboltable->header.flags & eei_symboltable_flag_allocation))
		{
			//Allocation data was never calculated - do it now

			//Calculate and store the usage data
			eei_symboltable_estimate_usage(functions, variables, 1, &full_symboltable->allocated);
			full_symboltable->header.flags |= eei_symboltable_flag_allocation;
		}

		//Calculate the size requierement and store the offsets
		const ee_memory_size needed =
				eei_symboltable_calculate_size(
					full_symboltable,
					&full_symboltable->allocated,
					&full_symboltable->offsets);

		//Make sure there is enough space and, if not, report and complain
		if (symboltable->size < needed)
		{
			symboltable->size = needed;
			return ee_symboltable_memory;
		}

		if ((symboltable->size > needed) && ((functions != NULL) || (variables != NULL)))
			//There is over-allocation, and we have user supplied data.
			//We can assume that more data will be supplied in further invocations.
			//Try to use all of the allocated space to reduce the chance of future re-allocations.
			eei_symboltable_fill_memory(full_symboltable);

		//Zero out the global usage and first level data
		//This is enough to mark everything as clean

		full_symboltable->used.second_level = 0;
		full_symboltable->used.third_level = 0;
		full_symboltable->used.variables = 0;
		full_symboltable->used.functions = 0;
		full_symboltable->used.textbook = 0;

		for (int i = 0; i < eei_symboltable_total_symbols; ++i)
			full_symboltable->first_level[i].count = 0;

		full_symboltable->header.flags |= eei_symboltable_flag_initialized;
	}

	if (full_symboltable->header.flags & eei_symboltable_flag_compacted)
		//We don't allow modification on compated data even though,
		//	in the current implementation, this data can still be appended (after expansion)
		return ee_symboltable_compacted;

	int iterations = 2;
	while (--iterations)
	{
		if (full_symboltable->header.flags & eei_symboltable_flag_reallocated)
		{
			//There was a memory re-allocation so data must be expanded in memory

			//First we check that the newly allocated memory is of adequate size

			const ee_memory_size newsize =
					eei_symboltable_calculate_size(
						full_symboltable,
						&full_symboltable->requested,
						NULL);

			if (symboltable->size < newsize)
			{
				//There is not enough space, return the actual size needed
				symboltable->size = newsize;
				return ee_symboltable_memory;
			}

			//We have space to work in. Now we need to expand and move all data vectors.
			eei_symboltable_expand(full_symboltable, &full_symboltable->requested);

			full_symboltable->header.flags &= ~eei_symboltable_flag_reallocated;
		}

		//Adding data

		eei_symboltable st;
		eei_symboltable_calculate_pointers(&st, full_symboltable);

		//Save the currently used counts so we can compare to it, if needed
		eei_symboltable_usage_data current_usage;
		eei_symboltable_copy_usagedata(&current_usage, &full_symboltable->used);

		ee_symboltable_reply reply = ee_symboltable_ok;

		if (!(full_symboltable->header.flags & eei_symboltable_flag_library))
		{
			//The library was not added yet, do it now

			if (EEI_FUNCTION_LIBRARY != NULL)
				reply = eei_symboltable_add_functions(&st, EEI_FUNCTION_LIBRARY);

			if (reply == ee_symboltable_ok)
				full_symboltable->header.flags |= eei_symboltable_flag_library;
		}

		if (functions && (reply == ee_symboltable_ok))
			reply = eei_symboltable_add_functions(&st, functions);

		if (variables && (reply == ee_symboltable_ok))
			reply = eei_symboltable_add_variables(&st, variables);

		//If there was a memory error we try to estimate how much was actually needed
		if (reply == ee_symboltable_memory)
		{
			//Calculate the usage for this invocation
			eei_symboltable_usage_data usage;
			eei_symboltable_estimate_usage(
						functions,
						variables,
						(full_symboltable->header.flags & eei_symboltable_flag_library),
						&usage);

			//Play what-if this was all added correctly
			usage.second_level += current_usage.second_level;
			usage.third_level += current_usage.third_level;
			usage.variables += current_usage.variables;
			usage.functions += current_usage.functions;
			usage.textbook += current_usage.textbook;

			//Save this for the next invocation when we have the requested memory actually provided
			eei_symboltable_copy_usagedata(&full_symboltable->requested, &usage);
			full_symboltable->header.flags |=  eei_symboltable_flag_reallocated;

			//Calculate the size requierements
			const ee_memory_size newsize =
					eei_symboltable_calculate_size(
						full_symboltable,
						&usage,
						NULL);

			if (symboltable->size >= newsize)
			{
				//The data can actually fit in the allocated space, we just need to shuffle things around.
				//This is done by running the re-allocator from here and then adding the data again.
				//Since the symbol table does not actually changes anything when the exactly same
				//	data is added we don't even need to know at what point the current addition failed.
				continue;
			}
			else
			{
				//Report the needed size
				symboltable->size = newsize;

				//Nothing more can be done as the caller must allocate more memory
				return ee_symboltable_memory;
			}
		}


		if (reply != ee_symboltable_ok)
			//There was some error that can not be handled
			return reply;
		else
			//We can proceeed
			break;
	}

	if ((functions == NULL) && (variables == NULL))
	{
		//This is a compaction request

		//Compact and update the new size
		symboltable->size =
				eei_symboltable_compact(full_symboltable, &full_symboltable->used);

		full_symboltable->header.flags |= eei_symboltable_flag_compacted;
	}

	return ee_symboltable_ok;
}
