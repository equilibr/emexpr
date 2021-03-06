

# EmExpr

EmExpr, the Embedded Expression library, is a small, zero-dependency, zero-allocation parser and evaluation engine for mathematical expressions. It is aimed to be used in embedded systems where resources are at a premium and must be tightly controlled.

## Introduction

*The introduction is shamelessly based on the readme from the [tinyexpr](https://github.com/codeplea/tinyexpr) library.*
If you don't really care about call stack depth, dynamic allocations, memory footprint or execution speed go check that project. It might be better suited for your needs and simpler to use.

### Features

- Designed for low-resource embedded systems. 
- Keeps the call stack as small as possible.
- C99 with **no dependencies**. Not even `math.h` and `memory.h`, only `stddef.h`.
- Compiles, warning free, with: -std=c99 -ffreestanding -Wall -Wextra -pedantic-errors
- Single source file and header file for the core functionality.
- Simple and extremely fast.
- Does **not** require usage of heap memory.
- A parsed expression can be evaluated multiple times with no added overhead.
- Implements the expected operators precedence by default.
- Can add custom functions and variables easily.
- Can work with **fixed-point** numbers without any modifications.
- The parsed grammar and precedence can be easily modified.
- Easy to use - the API has 2.5 functions in total.
- Thread-safe - if your code is thread-safe.

### Building

The EmExpr core is self-contained in two files: `emexpr.c` and `emexpr.h`. To use EmExpr, simply add those two files to your project.

If your toolchain is really ancient and/or broken, and does not supply `stddef.h` for some obscure reason, simply erase that line and define `ptrdiff_t` yourself.

### Short Example

Here is a minimal example to evaluate an expression at runtime. Note that this requires adding two more files to your project, `ee_execute.c` and `ee_execute.h`.

```C
#include "eelib/ee_execute.h"
double result = eelib_execute("sin(2 * PI)");
```


## Design philosophy

*The following text is written in the present tense, even during development, to be kept as a reminder of the project goals.*

It has no `#include` directives (other than its own header), not even `memory` nor `math`. This allows it to be used even in bare metal project where there is very limited runtime support and when precise control over memory and processor usage is needed. It can be used in places where there is no `malloc` available, for example, or when the mathematical library is not even implemented.

It is highly configurable, allowing to use any C built-in basic type as storage and performing any operations on it. This can be a float point, integer or even a complex fixed-point arithmetic, for example, with no changes in the expression syntax.

The parsing and execution steps are separated allowing for the lowest possible resource usage and fastest possible reevaluation of parsed expressions. Variables, constants and functions are bound (not copied) during parsing so each evaluation uses the current data from memory.

There is no shared mutable state in the code. Thus, if needed, it can be executed concurrently, given the referenced data and functions also support this.

There are no sanity checks anywhere in the code. It is assumed the provided data is always valid. The validity of the expression depends only on its buffer being allocated, not on its content matching the parsed grammar.

### Expression parser

The parser is implemented as a table-driven, non-recursive, Pratt parser, with support for prefix, infix & postfix operators and controllable precedence.

Being a table-driven parser the parsed language grammar can be easily modified, if needed, without the need to even understand how the code works. For example, changing the precedence of operators is a simple operation of changing the corresponding integers in the table. This also allows for storing the language tables in ROM memory, or another memory bank, if needed, to conserve RAM or speed-up execution using simultaneous memory bank access.

The non-recursive nature of the parser help to keep the call stack depth to a known minimum while parsing expressions of any complexity, as many embedded systems have extreme limitations on the depth and size of the call stack. For less-constrained systems this can also provide protection from call stack overflow when parsing large and/or maliciously constructed user-provided expressions.

### Evaluation environment

The parser generates a semi-opaque structure, the evaluation environment, that is used for evaluating the expression. This allows to, both, discard the expression after parsing to conserve memory and to execute the evaluation multiple times conserving processor usage.

There is a helper estimator function that provides the upper bound on the memory needed for the environment, for a given expression, using a simple algorithm. This is to allow pre-allocating storage for it since the parser does not perform any allocations by itself. The parser will report the actual memory used so it can be trimmed, if needed.

It is also possible to run the parser itself in a special calculation mode. This mode uses the bare minimum of memory and returns a much better estimate for the actual needed memory. Thus a trade-off between processor usage and memory usage during parsing is possible.

This environment structure holds the generated byte code, parsed constants, pointers to user-bound variables and functions and some housekeeping information.  The structure is self contained, never written to by the evaluation engine, can be freely moved in memory, stored on the stack, heap, or any other accessible memory location.

Since the evaluation engine needs a runtime stack of its own to execute the environment storage for this stack can also be allocated inside the environment - to simplify executing the evaluation step.
On the other hand it is possible to keep the environment size to a minimum and provide user allocated memory for the runtime stack. This memory can be located anywhere in RAM, e.g. heap, pre-allocated static memory or on the C runtime stack itself.

### Evaluation engine

The evaluation engine is implemented as a byte-code driven, stack based, virtual machine. This allows for a simple, small, implementation, high execution speed and low memory usage. The implementation uses the shallowest possible call stack and no memory other than provided by the evaluation environment.

All data processing - operators, function calls, custom constant and special variables access - is performed with user provided functions. No functionality other than the four basic operations `+-*/`, also implemented as user functions, is provided by default and even those can be excluded from compilation. This completely decouples the evaluation engine from the data processing keeping it, at the same time, as simple and as flexible as possible.

The engine handles only a single, user defined, type of value. All variables, constants, function parameters and return data, as well as the execution stack itself are of this type. The engine does not care about the actual type, only that it can be copied using the `=` (C assignment) operator, stored in memory and a pointer to it can be taken and de-referenced.  User functions are the only ones that should actually care what this type is since they are the ones who perform operations on it.

## Usage

To obtain the result of evaluating an expression two steps must be performed: parsing and evaluating.

The first step, parsing, analyzes the expression and any additionally user provided data and prepares an evaluation environment. This environment is then used in the second step, evaluation, to actually generate the result.

### Size estimation

Since there is no way to know in advance, before parsing, how much space will be needed for the evaluation environment, an estimator function can be called. This function is given the actual expression and it fills the environment header with size estimation data.
This would look like this:

```C
    ee_environment_header header;
    if (ee_guestimate("40+2",&header))
        return;//There was some error
```

At this point an adequately sized buffer can be allocated.
It is entirely possible to just allocate some space and reject all expressions that require more than that.

### Parsing

No matter the strategy, the next step is performing the actual parsing.
For the example we assume the buffer is pre-allocated to some constant value.

```C
  enum {EnvironmentSize = 64};
  union
  {
     ee_environment_header  header;
     ee_environment_element data[EnvironmentSize];
  }  environment;
  environment.header.size = EnvironmentSize;
  
  ee_compilation_data data;
  const char * expression = "40+2";
  
  int result = ee_compile(expression , &environment.header,  &data);
  if (result)
    //This can be used to display a specific error message
    return result;
```
One very important piece of code is left unexplained, the `ee_compilation_data`. It is extensively discussed as part of the [user interface](#UserInterface).

### Evaluation

If the parsing completed without errors the expression can be evaluated. 
Note that, at this point, the only piece of data that we need to keep is the evaluation environment, `environment`. All the other data used before, as well as the expression itself, are no longer needed.

```C
  //We assume the result of the evaluation is of some interest.
  ee_variable_type result;
  
  int evaluated = ee_evaluate(&environment.header, &result);
  if (evaluated)
    //This can be provided by a user function during evaluation
    return evaluated;
```

This step can be executed as many times as needed, interleaved with other executions.

For example, assume there are two expressions, that were parsed beforehand, that are only needed for the side-effects of the user functions they execute. Code like the following could be used to execute them, repeatedly, until one of the user functions returns some non-zero value.

```C
  static const char * expr1 = "foo()";
  static const char * expr1 = "bar()";

  //Parsing was done here into env1 & env2
  
  ee_variable_type dummy;
  while (!ee_evaluate(&env1, &dummy) && !ee_evaluate(&env2, &dummy)) ;
```

## <a name="UserInterface"></a> User interface

The user interface is composed from variables and functions. The variables are bound during parsing and their value can be used during evaluation. The functions are also bound during parsing and can be executed during evaluation by the evaluation engine.

For the evaluation engine both variables and functions are represented using pointers and their names, both in the source code and in the expression, are irrelevant. Moreover, those names are not even stored in the evaluation environment by the parser.

To conserve memory the parser only stores pointers to those variables and functions that are actually referenced in the expression. This allows for providing to the parser a list of all variables and functions that **might** be used by an expression, e.g. all mathematical functions, without the need to know in advance which of them will actually be used.

Information about the variables and functions is provided to the parser using the `ee_compilation_data` structure. 

### Variables

Variables are provided to the parser using the `variables` field inside the `ee_compilation_data` structure.
For each variable its' address and the name by which it will be known in the expression is provided.

All bound variables must be of the same type, `ee_variable_type`. The value of the variables can be read at any time, in any order, by the execution engine. While inside a user function the execution engine is stopped and, thus, will not access any variables.

Since no synchronization is used for variable access the user must make sure their value is always valid when the execution engine is running.

Following is a diluted example of how to bind two variables to the parser.
```C
  ee_variable_type var1, var2;
  
  ee_variable const varData[] = {&var1,  &var2};
  const char * varNames[] = {"a", "b"};
  ee_compilation_data bindings = { .variables = { varData, {varNames, 2} } };
  
  //Inside an expression variables are accessed directly by the bound name.
  ee_compile("a + b", &environment, &bindings);
  ```

### Functions

Functions are provided to the parser using the `functions` field inside the `ee_compilation_data` structure.
For each function its' address, arity (number of accepted parameters) and the name by which it will be known in the expression is provided.

All functions must be of the same type, `ee_function`. The functions can be called at any time, in any order, by the execution engine. Each function can accept zero or more parameters and returns a value, that is used in its place in the expression, and a result code.

When execution is successful a code of zero should be returned. When the function wants to signal an error it should return some positive number. In this case the evaluation will be halted and the returned number will be propagated, as is, back to the caller of `ee_evaluate`.
This can be used by the user code to handle various conditions and errors without the evaluation engine getting in the way.

Since a single user function can be used to implement several operations and, also, since user function can be declared as variadic, the actual number of parameters is passed to the user function on each invocation.

Functions that accept zero parameters can be invoked using the variable access syntax, without the `()` parenthesis denoting a function call. This allows those functions to act as variables, letting for things such as externally provided constants or dynamically generated values.

For detailed explanation see the comment for the `ee_function` definition and the the `arity` field of the `ee_compilation_data_function` structure.

Following is a diluted example of how to use a single function to implement two operators.


```C
int subneg(int arity, const ee_variable actuals, ee_variable result)
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

  //Somewhere inside the parsing function...
  
  ee_compilation_data_function  funcData[]  =  {
    {subneg,1},
    {subneg,2}
  };
  const  char  *  funcNames[]  =  {"-","-"};

  ee_compilation_data bindings = { .functions= { funcData, {funcNames, 2} } };
  
  ee_compile("-1 - 1", &environment, &bindings);
```

If a function is bound only once, using a non-negative arity, the runtime check for arity can be omitted since the parser makes sure the correct number of parameters is always provided upon invocation.

## Syntax
This describes the default syntax. The syntax can be modified at any scale, from changing the precedence of a single operator to using a completely different grammar.

The syntax is exactly what might be expected inside an expression.

- Various operators with the expected [precedence](#OperatorsTable).
- Parenthesis are used for grouping sub-expressions and invoking functions.
- Spaces are ignored.
- Constants can be provided using scientific notation.
- Postfix functions are supported.

Here are some samples, where each line is a separate expression.
```
a * (b + c)
2 + -f(g(), x)
e^-1
!a | b & c == d
1.1e6 - f(100 K)
```

### Lexer

At the first step the parser converts the expression into a stream of tokens and classifies them into five distinct classes:

* Identifiers
* Numbers
* Delimiters
* Operators
* Spaces

### Identifiers

Identifiers are used to reference variables and functions. They must begin with an character in the range `a` to `z` or `A` to `Z`, or the `_` character. The rest of the identifier can include any of the above characters or a numeric character from `0` to `9`.

### Numbers

Numbers are used to provide constant values directly in the expression. They **must** begin with a numeric character in the range from `0` to `9` to be recognized as numbers. This can be, optionally, followed by `.` and another sequences of characters in the range from `0` to `9`. This, in turn, can be followed by the letter `e`, an optional `+` or `-` character and another sequence of characters in the range `0` to `9`.

Any other character will terminate the number.

### Delimiters

Delimiters are used to, as implied, delimit certain language features.
Delimiters are a character from this list: `,`, `(`, `)`, `[`, `]`, `{`, `}`.
Each delimiter is always a single character. That is, `((` is two delimiters.

### Operators

Operators are used to, well, perform operations.
Operators are a character from the list: `:`, `'`, `.`, `+`, `-`, `*`, `/`, `%`, `^`, `&`, `|`, `~`, `!`, `=`, `>`, `<`.
Each operator is always a single character. That is, `--` are two operators.

### Spaces
Spaces are used to separate between tokens that would, otherwise, be parsed as a single token. *And also to make things pretty and readable.*

The following characters, in C's escape sequence representation, are recognized as space: ` `, `\t`, `\r`, `\n`.

Apart from being used to separate tokens spaces are completely ignored by the parser.

****
After the above step the expression, or rather the generated token stream, is parsed to understand its structure.

The following text describes the default grammar. It can be completely modified, if needed, by changing the parser tables in the source code. No other modifications are necessary.

Note that some delimiters are unused in the default grammar. Also note that **all** operators are implemented using user functions. The default implementation of the four basic operators `+-*/` is also done using user functions. They can be excluded from compilation in case custom functions for them are to be provided by the user.

While the grammar defines many operators actually **using** an operator inside an expression with no provided user function to implement it will result in a parser **error**.

Most importantly, EmExpr imposes **no** semantic meaning to any operator (except the default implementation mentioned above, that can be disabled) and thus *any operator* can be used to mean *any operation*, as is seen fit by the user of the library.

### Operators and precedence

An expression is defined by its operators. All it really does is to apply certain operators to certain operands in a certain order.

Operators come in three flavors (actually four, but we do not handle mix-fix operators): prefix, infix and postfix.
As their names imply they differentiate by where their operands are located in relation to themselves in the expression.

A prefix operator is located before its operand. A classical example of a prefix operator is the *negation*, e.g. `-x`.

A postfix operator is located after its operand. The most known example of this being C's post-increment, `i++`.

And, accordingly, an infix operator is located between its operands. An example of this can be the subtraction operator, `x - y`. Infix operators can have more than two operands, for example `a + b + c` can be treated as a single addition operator with three operands, but this is not handled as such by most parsers, this one not being an exception as well.

It is said that operators bind to their operands. This can be thought of as writing parenthesis around each operator and its operands to unambiguously mark the evaluation order of each operator. Each flavor has its own rules for this binding when several operators are encountered in sequence.

#### Prefix operators

Prefix operators are usually said to bind to the right, that is whatever is to the right of them must be evaluated first. This means that a sequence of prefix operators are executed in reverse order from how they appear in the expression. Given the expression `!-x` the evaluation order is `x`, then `-` and, finally, `!`. Each prefix operator defines what operator flavor must come after it. While regular prefix operator expect another prefix operator after them, there are two exceptions, variable references and function calls. While the latter two are also considered prefix operators and can appear anywhere that is expected they, themselves, must **not** be followed by a prefix operator.

#### Infix operators

In contrast to prefix operators infix ones also have a property called precedence. This property determines, for a sequence of infix operators delimited by non-infix operators, the actual order of bindings. Thus, if `*` has higher precedence than `+` the expression `a + b * c` will be evaluated as if it was written as `a + (b * c)`.

Inside a sequence of operators with the same precedence infix operators can bind left or right. With left binding, the most used one, given the expression `a + b + c` the sub-expression `a + b` is evaluated first, as if it was written `(a + b) + c`. Right binding is usually used for operators such as `^`, mostly used to represent the power function, so that `a ^ b ^ c`is evaluated as `a ^ (b ^ c)`, preserving the order expected from regular mathematical notation using powers.

Infix operators **must** always be surrounded by non-infix operators, so it is invalid to write an expression such as `a / / b`, if there is no prefix/postfix version of `/`.

#### Postfix operators

Postfix operator are a mixture between prefix and infix. They can be chained one after another like the prefix operators and they have a precedence that is also considered during parsing of infix operators. Postfix operators are always left binding, that is, they are executed in the order they appear in the expression.

Postfix operators can be present when an infix operator is expected and must **not** be followed by a prefix operator.

### Grouping

It is beneficial to be able to override the operator precedence. The grouping parenthesis, `(` and `)`, are used for this. They can appear anywhere an infix operator is expected and their content will be evaluated as a complete sub-expression. Thus their content will not bind to the preceding or following operators.

As an example the expression `a * (b + c)` will not bind `b` to `*` and, instead, first evaluate `b + c`, binding its result as the second operand of `*`.

### Variable access

To access a bound variable its name, as set in the parser `ee_compilation_data` structure should be provided.
Variables can appear anywhere a prefix operator is expected.

For example, given two bound variables named `x` and `y`, an expression that calculates their difference could be `x - y` or `x + -y` or `-(-x + y)`.

### User functions

To invoke a user function its name,  as set in the parser `ee_compilation_data` structure should be provided, followed by the `(` call operator, followed by zero or more, comma delimited expressions, concluding with `)`.
Function invocations can appear anywhere a prefix operator is expected.

Follows an example of invoking two functions, `f` and `g`, with different parameters: `f() + f(0) + f(g()) + f(1, 2 * g())`.

The `,` delimiter that is used to separate parameters is actually an operator with a priority lower than any other used operator.
The number of parameters is determined during parsing and a user function with the correct arity must be provided, otherwise the parser will halt with an error.

#### Functions as variables

This is a special case that also invokes a user function.

If a variable name that is not bound to any actual variable is used in an expression the parser will look for a function of the exact same name with an arity of 0 and use that instead. If both a variable and a zero arity function exist with the same name the parser will halt with an error.
The normal function invocation syntax is always available and will **not** result in an error, even if a bound variable with the same name also exists.

This allows to seamlessly harness user functions for dynamic variable lookup or value generation.
This also allows to replace direct variable access for function based access without any modifications to existing expressions.

For example, given a function named `x` which accepts zero parameters, it can be invoked in the following ways: `x() + x`.

#### Functions as postfix operators

When a postfix operator is exactly the name of a user function with an arity of 1 that function is used for the operator. The default grammar does not support any other postfix operators.

For example, this form can be used for implementing SI multipliers.
Given user functions such as `K` and `M` that take a single parameter and multiply it by the correct power of 10, the expression `1 M - 900 K' should evaluate to 100,000.

### <a name="OperatorsTable"></a>Defined operators

There are many operators defined, some are used both as infix and prefix. They are referenced in the following table.

The single, user function based, postfix operator is left binding with a precedence of 11.

The operators `:`, `'` and `.` are not used by the default grammar.

|Operator|Infix precedence|Infix binding|Prefix|
|--|--|--|--|
|`!`|||Yes|
|`~`|||Yes|
|`,`|2|Left|No|
|<code>&#124;</code>|3|Left|Yes|
|`&`|4|Left|Yes|
|`=`|5|Left|No|
|`<`|5|Left|No|
|`>`|5|Left|No|
|`+`|6|Left|Yes|
|`-`|6|Left|Yes|
|`*`|7|Left|No|
|`/`|7|Left|No|
|`%`|7|Left|No|
|`^`|8|Right|No|

## Support code
To simplify the general use case support code is provided, as additional sources, with bindings to functions from`math.h` and `cmath.h`, for several variable types.

To allow for more informative feedback to the user who parses expressions, conversion functions, from the parser error codes to user readable strings, are provided. This can be used in systems where textual user feedback is possible to help with writing correct expressions. This is also provided as an additional source file that can be used only by those who need such a facility.

For users who want to *just use* the library a function is provided that takes an expression as input and returns the result as output. This function has a simplified API for variable binding and it includes the default math functions in the evaluation.

```C
    double myVar = 0;
    double result = eelib_execute("cos(2 * PI * phi)", "phi", &myVar);
```

## Future

This is a list of things to consider after the project is in a working state, in no particular order.

### Digraph and trigraph operators

Add support for operators consisting of two or three characters.
This would allow to greatly increase the usable operators syntax.
Since this is parser-only modification the execution speed and memory usage would stay unchanged.

### Sanity checks

Add various sanity checks during parsing and evaluations.
It must be possible to completely disable them during compilation of the library.

### Variables

Allow using user defined variable types. Currently only the behavior can be changed but the variable must be a C built-in.

### Name lookup

Allow using special user provided functions for variable and function name lookup during parsing instead of the data directly supplied in `ee_compilation_data`.

### Parser scratch space

As currently defined the parser is using the evaluation environment as its scratch space.

Allow for separating the two memory locations, same as can be done for the runtime stack. This would allow using some global scratch buffer, that is generally available in embedded systems, for the parser while keeping the evaluation environment size to an absolute minimum.

### Pre-process parser data

Since the data supplied to the parser, the variables and user functions list, rarely, if never, changes between calls to the parser it would be highly beneficial to perform some pre-processing on this data to convert it to a format better suited for how the parser would operate on it.

While this can somewhat increase the memory usage needed for parsing expressions it can greatly reduce the processing complexity for each parse cycle.

### Environment sharing

Allow sharing of the evaluation environment for different expressions.
This can be done by allowing the parser to append data to an existing environment. It can be used to conserve memory when many expressions share the same set of constants, variables and functions.

### Constant indexing

Remove the separate constants index command and treat them as variables. That is, store a pointer to them inside the variables table, or use any other method for accessing variables.

This would remove a single VM command. This can be useful to reduce the number of bits using for encoding commands. The downside is some added complexity in the parser and, possible, much higher memory usage.

### Direct data

Remove usage of constant/variable/function tables and provide the data/pointers directly in the byte code stream.

This can make the environment sharing redundant and simplify the implementation of the evaluation engine even further. On the other hand this can make execution slower and actually use more memory, especially when the same identifiers are used multiple times in an expression or across different expressions thus negating any possible gains.

### Offset tables

Remove usage of variable/function tables, store a base pointer for each, and provide offsets in the byte code.

This should combine most of the benefits of the direct data approach while keeping the byte code bloat to a minimum, given the process memory space is small enough.

### Optimizations

Any optimization performed should only be done in the parser and must not add any complexity to the execution engine.

This can include things such as common sub-expression elimination and constant folding.
This can also try and automatically estimate if the direct data or offset tables approach, as outlined above, should be used on a per-case basis, if enabled.

Allow for compile-time control over what optimization features should and could be used.
This is not only to keep the parser resource usage to a minimum but also to facilitate system limitations, such as the inability to execute user provided functions during parsing.

### Source separation

Separate the parser and execution engine to different source files to simplify applying different optimization techniques to each, e.g. `-Os` for the parser and `-O3` for the execution engine.

### Beyond expressions

The project should stay **expression** based.
This mostly means not adding execution flow control.

It will be simple to add writing to variables and parsing several expressions in a single evaluation environment, allowing syntax such as `a = ...expression...; b = ...expression...;`. This will not require any change in the API and thus can be added at a later time without interfering with existing users.

It should be noted that this can, currently, be emulated by using user functions to perform the writes, as in `writeA(...expression...) + writeB(...expression...)`.

Even if only a single expression is allowed, writing to variables with a simple syntax can be beneficial for use cases such as dynamically provided user defined variable drivers, simple configuration scripts where each line is treated as an expression, etc...

## Notes

This project came to life after an attempt to remove `malloc` from the [tinyexpr](https://github.com/codeplea/tinyexpr) library. It was soon realized this would require a massive change and would actually defeat the purpose of that project. As a result the only thing shared with that library is the *expr* in the name.

The code for the Pratt parser is a rewrite in C of the same algorithm implemented in C++ from one of my other projects.