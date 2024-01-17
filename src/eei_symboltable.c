/*
 * EMEXPR - Expression evaluator for embedded systems
 *
 * Copyright (c) 2022 Sergey Tomilin
 * Under the MIT License
 * For full license text see attached "LICENSE" file
*/

/*
	This is an implementation of a symbol table, a multimap from identifier names to varibales & functions.
	The symbol table is used by the parser to query for previously supplied identifiers.
	During setup the symbol table stores mapping between variable/function names and their memory locations.
	During parsing those locations are provided to the parser, while applying various checks and filters.
	To completely understand the design decision in the implementation an understanding of how EMEXPR handles
	 variables and functions is needed.

	The data is stored in memory using a tiered heirarchy with a supplemental textbook.

	At the first level there is a vector with each element corresponding to the first character of an identifier.
	The elements provide a link into the second level,
	 each storing the start index and count of (contignuous) linked items at the second level.

	At the second level there is a vector of elements, linked from the first level.
	Elements linked from the same first level element are contignous in memory.
	Each element provides two links:
	 * To the textbook where the rest of the characters are stored.
	 * To the third level where the actual memory locations and flags of the functions & variables are stored.
	The textbook link holds the length of the stored identifier and its start index in the textbook.
	When the identifier is only 2 characters long, as an optimization, the textbook is not used and
	 the second character is stored *instead* of the textbook access index.
	 This allows storing all used operators without using any textbook space.
	The third level link holds the start index, at the third level, and a count of consecutive elements
	 that hold variables & functions for the current identifier.

	At the third level there are two vectors, one holding the flags, and the other locations of the varaibles/functions.
	For each element in the flags vector there is a corresponding element in locations vector at the same index.

	Notes and TODO:

	The current design of the third level assumes nothing about pointer sizes.
	If compression is to be utilized keeping the data separate could provide a benefit since memory locations of
	 variables and code are, usually, clumped into two separate locations.

	When storing varaibles the flags vector is wasted, given most its data is irrelevant.
	This could be remedied by storing the flags only for functions and using a bitfield to differentiate variables from functions.
	While saving space this would add to the processing since it, effectively,
	 puts the flags & variable/function vectors at the *fourth* level of the heirarchy.
	This would also prevent saving a "bound" count for the variables (bound checking for indexed variable access).

	The first level might be compacted by adding a bitfield denoting what first level elements are used and, then, storing only those.
	If the second level is to be re-ordered by its first level link then the first level would only need to holds the counts.
	This would half its memory usage at the expece of processing.
	The same method could be applied to the third level, though the relative second-level savings are much less
	 given it also holds the textbook links.

	Given no character is assigned any special meaning at any level the symbol table can be modified to support
	 the complete UTF-8 encoding by simply expanding the first level *only*
	 to include all character that start a valid UTF-8 sequence.

	The textbook is already somewhat compacted by re-using existing data.
	This works best when longer identifiers are provided before shorter ones during setup.
	It is possible to post-process the textbook to look for missed compaction opportunities.
	Given the textbook is only used in conjunction with links from the second level further compression(LZ77, e.g.) seems unnecesarry.

	Since, during queries, the access is sequential across level and, also, inside each level,
	 online decompressions is fesible if the complete symbol table is compressed.
	This allows to only keep the compressed data in memory, while providing low-resource access, especially when using modern
	 entropy compression algorithms such as "Asymmetric numeral systems".
	It seems possible to provide "jump tables" for the decoder to speed up processing by skipping parts of the compressed stream.
	If the level data is stored interleaved, that is first-second-third-... then it is possible to perform queries without
	 the need to processes most of the irrelavant data.
*/

#include "eei_symboltable.h"
#include <stddef.h>


//Auto-selections of compilation options based on external DEFINEs'
//-----------------------------------------------------------------

#if defined(EE_USER_FUNCTION_LIBRARY)
#	define EEI_FUNCTION_LIBRARY EE_USER_FUNCTION_LIBRARY
#else
#	define EEI_FUNCTION_LIBRARY eei_operators_library
#endif

extern const ee_symboltable_function EEI_FUNCTION_LIBRARY[];


//Symbol table lookup
//-------------------

static inline void eei_symboltable_invalidate_index(eei_symboltable_index * index, int level)
{
	if (level <= 1)
		index->first = -1;

	if (level <= 2)
		index->second = -1;

	if (level <= 3)
		index->third = -1;
}

static inline ee_element_count eei_symboltable_count_third(const eei_symboltable * st, int index)
{
	if (index + 1 < st->used->second_level)
		return st->second.next[index+1] - st->second.next[index];
	else
		return st->used->third_level - st->second.next[index];
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

ee_symboltable_reply eei_symboltable_get(
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
	int index3 = st->second.next[index->second];
	int index3_stop = index3 + eei_symboltable_count_third(st,index->second);

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
	return ee_symboltable_ok;
}

//Symbol table building
//---------------------

static inline ee_symboltable_reply eei_symboltable_add_textbook(
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

static inline ee_symboltable_reply eei_symboltable_add_text(
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
		const ee_symboltable_reply treply =
				eei_symboltable_add_textbook(st,start,length,&book_index);

		if (treply != ee_symboltable_ok)
			return treply;
	}

	//At this point the text is already inside the textbook
	//Now we need to add it to the first 2 levels

	//Find the second-level insertion point.
	//The second level data is sorted by the first level index.
	int insertion;

	if (st->first.next[index->first].count == 0)
	{
		//For an empty first level we need to find the next non-empty first-level element and insert there.

		eei_symboltable_element_count next = index->first;

		insertion = st->used->second_level;

		while (++next < eei_symboltable_total_symbols)
			if (st->first.next[next].count != 0)
			{
				insertion = st->first.next[next].index;
				break;
			}

		//Set the new insertion point as the index
		st->first.next[index->first].index = insertion;
	}
	else
		//For an existing first level just add at its own end
		insertion = st->first.next[index->first].index + st->first.next[index->first].count;

	if (insertion != st->used->second_level)
	{
		//We need to move everything past the insertion point to preserve continuity

		for (int destination = st->used->second_level; destination > insertion; --destination)
		{
			st->second.book[destination].index = st->second.book[destination-1].index;
			st->second.book[destination].count = st->second.book[destination-1].count;
			st->second.next[destination] = st->second.next[destination-1];
		}

		//We also need to update all first-level indexes that pointed into the moved elements
		//	of the second level.
		//Since we keep the second level sorted only, and all, first-level elements
		// after the one being updated need an adjustement.

		for (int i = index->first+1; i < eei_symboltable_total_symbols; ++i)
			st->first.next[i].index++;
	}

	st->first.next[index->first].count++;
	st->used->second_level++;

	//Write the textbook data into the element
	st->second.book[insertion].count = length;
	st->second.book[insertion].index = book_index;

	//This is a new symbol so it has no third level data
	st->second.next[insertion] = st->used->third_level;

	//The caller should set the index once it is known
	index->second = insertion;

	return ee_symboltable_ok;
}

static inline ee_symboltable_reply eei_symboltable_add_third(
		eei_symboltable * st,
		eei_symboltable_index * index,
		const ee_char_type * start,
		const ee_element_count length)
{
	//Verify there is enough space to add new data
	if (st->used->third_level >= st->allocated->third_level)
		return ee_symboltable_memory;

	int newelement = 0;
	if (index->second == -1)
	{
		newelement = 1;

		//The symbol needs to be added
		const ee_symboltable_reply reply =
				eei_symboltable_add_text(st, start, length, index);

		if (reply != ee_symboltable_ok)
			return reply;
	}

	//At this point the first two levels are filled and the index points to them

	//Find the third-level insertion point.
	//The third level data is sorted by the second level index.
	const int insertion =
			(index->second + 1 == st->used->second_level)
			//If this is the last second-level index just insert at the end
			? st->used->third_level
			//Otherwise insert at the index of the next second-level.
			: st->second.next[index->second+1];

	if (newelement)
		//Set the new insertion point as the index
		st->second.next[index->second] = insertion;

	if (insertion != st->used->third_level)
	{
		//We need to move everything past the insertion point to preserve continuity

		for (int destination = st->used->third_level; destination > insertion; --destination)
		{
			st->third.data[destination].flags = st->third.data[destination-1].flags;
			st->third.data[destination].arity = st->third.data[destination-1].arity;
			st->third.locations[destination].ptr = st->third.locations[destination-1].ptr;
		}

		//We also need to update all second-level indexes that pointed into the moved elements
		//	of the third level.
		//Since we keep the third level sorted only, and all, second-level elements
		// after the one being updated need an adjustement.

		for (int i = index->second+1; i < st->used->second_level; ++i)
			st->second.next[i]++;
	}

	st->used->third_level++;

	//The caller should fill the data
	index->third = insertion;

	return ee_symboltable_ok;
}

ee_symboltable_reply eei_symboltable_add(
		eei_symboltable * st,
		const ee_char_type * start,
		const ee_element_count length,
		ee_arity arity,
		ee_function_flags all_flags,
		ee_function_flags not_flags,
		int * location)
 {
	 eei_symboltable_index index;
	 ee_symboltable_reply reply;

	 //First we try to find if a function with the same name, arity and flags already exists.
	 eei_symboltable_find_text(st, start, length, &index);
	 reply = eei_symboltable_get(st, &index, arity, 0, all_flags, not_flags);

	 //If so the existing function will simply be overwritten with the new one.
	 if (reply == ee_symboltable_ok)
	 {
		 *location = index.third;
		 return ee_symboltable_ok;
	 }

	 //This is new data that needs to be added

	 //Add third-level data for a variable
	 reply = eei_symboltable_add_third(st,&index,start,length);
	 if (reply != ee_symboltable_ok)
		 return reply;

	 //Write the data into the third-level
	 st->third.data[index.third].flags = all_flags;
	 st->third.data[index.third].arity = arity;

	 *location = index.third;
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
	dst->textbook = src->textbook;
}

void eei_symboltable_copy_offsets(
		eei_symboltable_offsets * dst,
		const eei_symboltable_offsets * src)
{
	dst->second_level = src->second_level;
	dst->second_level_next = src->second_level_next;
	dst->third_level = src->third_level;
	dst->locations = src->locations;
	dst->textbook = src->textbook;
}

const char * eei_symboltable_calculate_offsets(
		eei_symboltable_offsets * offsets,
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
	ptr += sizeof(eei_symboltable_level) * size->second_level;

	//Secon level next
	while ((ptrdiff_t)ptr % alignof(eei_symboltable_element_count))
		ptr++;

	offsets->second_level_next = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_symboltable_element_count) * size->second_level;

	//Third level data
	while ((ptrdiff_t)ptr % alignof(eei_symboltable_function_data))
		ptr++;

	offsets->third_level = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_symboltable_function_data) * size->third_level;

	//Locations
	while ((ptrdiff_t)ptr % alignof(eei_symboltable_location))
		ptr++;

	offsets->locations = (ee_memory_size)(ptr - base);
	ptr += sizeof(eei_symboltable_location) * size->third_level;

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
	pointers->second.next = (eei_symboltable_element_count*)(base + full->offsets.second_level_next);

	pointers->third.data = (eei_symboltable_function_data*)(base + full->offsets.third_level);
	pointers->third.locations = (eei_symboltable_location*)(base + full->offsets.locations);
	pointers->second.textbook = (ee_char_type*)(base + full->offsets.textbook);
}

int eei_symboltable_calculate_size(
		const eei_symboltable_struct * full,
		const eei_symboltable_usage_data * size,
		eei_symboltable_offsets * offsets)
{
	eei_symboltable_offsets dummy_offsets;

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
			full_symboltable->allocated.second_level * sizeof(eei_symboltable_element_count) * 3
			+ full_symboltable->allocated.third_level * sizeof(eei_symboltable_function_data)
			+ full_symboltable->allocated.third_level * sizeof(eei_symboltable_location);

	//Always allocate *at least* as much space for the textbook as all other data
	int textbookratio = 1;
	if (full_symboltable->allocated.textbook > others)
		textbookratio = full_symboltable->allocated.textbook / others;

	//Calculate the size of a virtual "element" that will be used to estimate how much
	//	we can fit in.
	//We over compensate by assuming two third-level elements for a second level one.
	const int element =
			sizeof(eei_symboltable_element_count) * 3
			+ (sizeof(eei_symboltable_function_data) + sizeof(eei_symboltable_location)) * 2;

	//Calculate the size we can allocate for all other elements
	const int totalsize = full_symboltable->header.size - (&full_symboltable->data[0] - (const char*)full_symboltable);
	const int othersize = totalsize	/ (textbookratio+1);
	const int elements = othersize / element;

	eei_symboltable_usage_data allocated;
	allocated.second_level = elements;
	allocated.third_level = elements;
	allocated.textbook = totalsize - (elements+1) * element;

	//Reclaulte the final size needed
	eei_symboltable_offsets offsets;
	const ee_memory_size newsize =
			eei_symboltable_calculate_size(
				full_symboltable,
				&allocated,
				&offsets);

	if (full_symboltable->header.size >= newsize)
	{
		//We can fit everything, so use it
		eei_symboltable_copy_usagedata(&full_symboltable->allocated, &allocated);
		eei_symboltable_copy_offsets(&full_symboltable->offsets, &offsets);
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

	for (int i = lengths.third_level-1; i <= 0; --i)
		expanded.third.locations[i].ptr = current.third.locations[i].ptr;

	for (int i = lengths.third_level-1; i <= 0; --i)
	{
		expanded.third.data[i].arity = current.third.data[i].arity;
		expanded.third.data[i].flags = current.third.data[i].flags;
	}

	for (int i = lengths.second_level-1; i <= 0; --i)
		expanded.second.next[i] = current.second.next[i];

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
		compacted.second.next[i] = current.second.next[i];

	for (int i = 0; i < size->third_level; ++i)
	{
		compacted.third.data[i].flags = current.third.data[i].flags;
		compacted.third.data[i].arity = current.third.data[i].arity;
	}

	for (int i = 0; i < size->third_level; ++i)
		compacted.third.locations[i].ptr = current.third.locations[i].ptr;

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
	usage->textbook = textbook;
}

ee_symboltable_reply eei_symboltable_add_functions(eei_symboltable * st, const ee_symboltable_functions list)
{
	const ee_symboltable_function * item = list;

	while (item->name)
	{
		const int length = eei_symboltable_name_length(item->name);

		int location;

		const ee_symboltable_reply reply =
				eei_symboltable_add(
					st,
					item->name,
					length,
					item->arity,
					item->flags,
					0,
					&location);

		if (reply != ee_symboltable_ok)
			return reply;

		st->third.locations[location].function = item->item;

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

		int location;

		const ee_symboltable_reply reply =
				eei_symboltable_add(
					st,
					item->name,
					length,
					0,
					0,
					~ee_function_flag_invalid,
					&location);

		if (reply != ee_symboltable_ok)
			return reply;

		st->third.locations[location].variable = item->item;

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

	if (symboltable->size <= (ee_memory_size)sizeof(eei_symboltable_struct))
	{
		//Not enough memory to do anything at all.
		//Treat this as a request for a size estimate.

		//Calculate size requierements
		eei_symboltable_usage_data usage;
		eei_symboltable_estimate_usage(functions, variables, 1, &usage);

		symboltable->size =
				eei_symboltable_calculate_size(full_symboltable, &usage, NULL);

		return ee_symboltable_memory;
	}

	if (full_symboltable->header._.flags & eei_symboltable_flag_compacted)
		//Disallow modifications of compacted data since its too much trouble
		//	to revert to a non-compacted state.
		return ee_symboltable_compacted;

	if (!(full_symboltable->header._.flags & eei_symboltable_flag_initialized))
	{
		//The offset/usage data is not setup
		//We need to do that before the symbol table can be used at all

		if (!(full_symboltable->header._.flags & eei_symboltable_flag_allocation))
		{
			//Allocation data was never calculated - do it now

			//Calculate and store the usage data
			eei_symboltable_estimate_usage(functions, variables, 1, &full_symboltable->allocated);
			full_symboltable->header._.flags |= eei_symboltable_flag_allocation;
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
		full_symboltable->used.textbook = 0;

		for (int i = 0; i < eei_symboltable_total_symbols; ++i)
			full_symboltable->first_level[i].count = 0;

		full_symboltable->header._.flags |= eei_symboltable_flag_initialized;
	}

	int iterations = 2;
	while (--iterations)
	{
		if (full_symboltable->header._.flags & eei_symboltable_flag_reallocated)
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

			full_symboltable->header._.flags &= ~eei_symboltable_flag_reallocated;
		}

		//Adding data

		eei_symboltable st;
		eei_symboltable_calculate_pointers(&st, full_symboltable);

		//Save the currently used counts so we can compare to it, if needed
		eei_symboltable_usage_data current_usage;
		eei_symboltable_copy_usagedata(&current_usage, &full_symboltable->used);

		ee_symboltable_reply reply = ee_symboltable_ok;

		if (!(full_symboltable->header._.flags & eei_symboltable_flag_library))
		{
			//The library was not added yet, do it now

			if (EEI_FUNCTION_LIBRARY != NULL)
				reply = eei_symboltable_add_functions(&st, EEI_FUNCTION_LIBRARY);

			if (reply == ee_symboltable_ok)
				full_symboltable->header._.flags |= eei_symboltable_flag_library;
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
						(full_symboltable->header._.flags & eei_symboltable_flag_library),
						&usage);

			//Play what-if this was all added correctly
			usage.second_level += current_usage.second_level;
			usage.third_level += current_usage.third_level;
			usage.textbook += current_usage.textbook;

			//Save this for the next invocation when we have the requested memory actually provided
			eei_symboltable_copy_usagedata(&full_symboltable->requested, &usage);
			full_symboltable->header._.flags |=  eei_symboltable_flag_reallocated;

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

		full_symboltable->header._.flags |= eei_symboltable_flag_compacted;
	}

	return ee_symboltable_ok;
}


//System verification
//-------------------

//This structure is just a clever way to make sure the sizes of the basic data types are exactly what we expect them to be.
//When a check fails the compilation will halt with an error of: "check type" declared as an array with a negative size
struct check_type_sizes
{
		int not_enough_bits_for_symboltable_compaction[( sizeof(eei_symboltable_element_count) >= sizeof(ee_char_type) ) ? 1 : -1];
		int location_variable_longer_than_ptr[(sizeof(ee_variable) >= sizeof(void*)) ? 1 : -1];
		int location_function_longer_than_ptr[(sizeof(ee_function) >= sizeof(void*)) ? 1 : -1];
};
