


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
- Implements the expected (by someone) operator precedence by default.
- Can add custom functions and variables easily.
- Can work with **fixed-point** numbers without any modifications.
- The parsed grammar and precedence can be easily modified.
- Easy to use - the API has 3.5 functions in total.
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

It has no `#include` directives (other than its own header), not even `memory` nor `math`. This allows it to be used even in bare-metal projects where there is very limited runtime support and when precise control over memory and processor usage is needed. It can be used in places where there is no `malloc` available, for example, or when the mathematical library is not even implemented.

It is highly configurable, allowing to use any C built-in basic type as storage and performing any operations on it. The underlying computations can be a float point, integer or even based on a custom complex-valued fixed-point type, for example, with no changes in the expression syntax nor in the parser code itself.

The parsing and execution steps are separated allowing for the lowest possible resource usage and fastest possible reevaluation of parsed expressions. Variables and functions are bound (not copied) during parsing so each evaluation uses the current data from memory.

There is no shared mutable state in the code. More precisely there is no internal state at all, except the one provided through the API. Thus, if needed, expressions can be parsed and executed concurrently, given the referenced data and functions also support this.

By default there are no sanity checks anywhere in the code. It is assumed the provided data is always valid. Regarding the parsed expression the only requirement is that the input buffer is not modified externally during parsing as any input inside it is considered valid, disregarding if it matches the parsed grammar or not.

### Expression parser

The parser is implemented as a table-driven, non-recursive, Pratt parser, with support for prefix, infix & postfix operators with controllable precedence.

Being a table-driven parser the parsed language grammar can be easily modified, if needed, without the need to even understand how the code works. For example, changing the precedence of operators is a simple operation of changing the corresponding integers in the table. This also allows for storing the language tables in ROM memory, or another memory bank, if needed, to conserve RAM or speed-up execution using simultaneous memory bank access.

The non-recursive nature of the parser help to keep the call stack depth to a known minimum while parsing expressions of any complexity, as many embedded systems have extreme limitations on the depth and size of the call stack. For less-constrained systems this can also provide protection from call stack overflow when parsing large and/or maliciously constructed user-provided expressions. If any problem is encountered during parsing, from incorrect syntax to exhaustion of the working storage, the parsing halts and the relevant information is returned, allowing the user to halt or attempt to remedy the situation.

It is possible to evaluate the parsed expression, or parts thereof, during the parsing stage itself. This ranges from simple constant folding to the evaluation of the whole expression. The possible modes are selected during compile time and the actual mode (if more than one is available) can be chosen when parsing.

### Evaluation environment

If not fully parsing the expression, the parser generates a semi-opaque structure, the evaluation environment, that is used for evaluating the expression. This allows to, both, discard the expression after parsing to conserve memory and to execute the evaluation multiple times conserving processor usage, since it is generated in a form optimized for the evaluation engine.

There is a helper estimator function that provides the upper bound on the memory needed for the environment, for a given expression, using a simple algorithm. This is to allow pre-allocating storage for it since the parser does not perform any allocations by itself. The parser will report the actual memory used so it can be trimmed, if needed. The same function also provides the upper memory bound needed for the parsing itself.

It is also possible to run the parser itself in a special calculation mode. This mode uses the bare minimum of memory and returns a much better estimate for the actual needed memory. Thus a trade-off between processor usage and memory usage during parsing is possible.

This environment structure holds the generated byte code, parsed constants, pointers to user-bound variables and functions and some housekeeping information.  The structure is self contained, never written to (with the exception below) by the evaluation engine, can be freely moved in memory, stored on the stack, heap, or any other accessible memory location.

Since the evaluation engine needs a runtime stack of its own to execute the environment storage for this stack can also be allocated inside the environment, at its tail - to simplify memory management and house-keeping needed for executing the evaluation step.
On the other hand it is possible to keep the environment size to a minimum and provide user allocated memory for the runtime stack. This memory can be located anywhere in RAM, e.g. heap, pre-allocated static memory or on the C runtime stack itself.

### Evaluation engine

The evaluation engine is implemented as a byte-code driven, stack based, virtual machine. This allows for a simple, small, implementation, high execution speed and low memory usage. The implementation uses the shallowest possible C call stack and no memory other than provided by the evaluation environment. Only the runtime stack part of the evaluation environment is written to during evaluation by the engine.

All actual data processing - operators, function calls and special variables access - is performed with user provided functions. No functionality other than reading, writing and copying data, is implemented. This completely decouples the evaluation engine from the data processing keeping it, at the same time, as simple and as flexible as possible.

The engine handles only a single, user defined, type of value. All variables, constants, function parameters and return data, as well as the execution stack itself are of this type. The engine does not care about the actual type, only that it can be copied using the `=` (C assignment) operator (or a special user-provided one), stored in memory and that a pointer to it can be taken and de-referenced.  User functions are the only ones that should actually care what this type is since they are the only ones who perform operations on it.

## Usage

To obtain the result of evaluating an expression three steps must be performed: symbol table preparation, parsing and evaluating.

The first step processes all the available functions and variables into a format that is suitable for usage by the parser. This both reduces the size required for storage and the time it takes to find a symbol name in the data. The second step, parsing, analyzes the expression and prepares the evaluation environment (in case the evaluation is not competed during this step). This environment is then used in the third step, evaluation, to actually generate the result.

### <a name="SymbolTableProcessing"></a>Symbol table processing

The memory usage of the symbol table processing is hard to calculate in advance. The only guarantee is that it will use *no more* than the original input data. Thus three operating modes are available:
* In-place overwrite of the input data
* Size guesstimate
* Processing into pre-allocated space

The space used depends only on the input symbol table and has no dependency on the expressions to be parsed.  Thus, this step can be executed at any time before the actual parsing, including during development on the host machine, saving the resulting data directly into constant storage, such as a ROM or providing it as immediate values inside the source code, or any other method.

As of writing there is no separate API provided for this and it is performed inside the parsing function, every time it is invoked. This also currently requires that the original data is kept around during parsing.

### Size estimation

Since there is no way to know in advance, before parsing, how much space will be needed for the evaluation environment, an estimator function can be called. This function is given the actual expression and it fills the environment header with size estimation data.
This would look like this:

```C
    ee_environment_header header;
    if (ee_guestimate("40+2",&header))
        return;//There was some error
```

At this point an adequately sized buffer can be allocated.
It is entirely possible to just allocate some space and reject all expressions that are reported during parsing as requiring more than that.

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
One very important piece of code is left unexplained, the `ee_compilation_data`, it is the input of the symbol table processor. It is extensively discussed as part of the [user interface](#UserInterface). 

### Evaluation

If the parsing completed without errors the expression can be evaluated. 
Note that, at this point, the only piece of data that we need to keep is the evaluation environment, `environment` (and the `ee_compilation_data`, until that is fixed). All the other data used before, as well as the expression itself, are no longer needed.

```C
  //We assume the result of the evaluation is of some interest.
  ee_variable_type result;
  
  int evaluated = ee_evaluate(&environment.header, &result);
  if (evaluated)
    //This value can be provided by a user function during evaluation
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

Information about the variables and functions is currently provided to the parser using the `ee_compilation_data` structure. See the explanation about the [symbol table](#SymbolTableProcessing) for more details.

### Variables

Variables are provided to the parser using the `variables` field inside the `ee_compilation_data` structure.
For each variable its' address and the name by which it will be known in the expression is provided.

All bound variables must be of a single type, `ee_variable_type`. The value of the variables can be read at any time, in any order, by the execution engine. While inside a user function the execution engine is stopped and, thus, will not access any variables (although other execution engines might).

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
For each function its' address, arity (number of accepted parameters), several flags and the name by which it will be known in the expression is provided.

All functions must be of the single type, `ee_function`. The functions can be called at any time, in any order, by the execution engine. Each function can accept zero or more parameters and returns a value, that is used in its place in the expression, and a result code.

When execution is successful a code of zero should be returned. When the function wants to signal an error it should return some positive number. In this case the evaluation will be halted and the returned value will be propagated, as is, back to the caller of `ee_evaluate`.
This can be used by the user code to handle various conditions and errors without the evaluation engine getting in the way or, in fact, needing any knowledge as to the meaning of those values.

Since a single user function can be used to implement several operations and, also, since user functions can be declared as variadic, the actual number of parameters is passed to the user function on each invocation.

Functions that accept zero parameters can be invoked (if not disabled by a compile-time option) using the variable access syntax, without the `()` parenthesis denoting a function call. This allows those functions to act as variables, letting for things such as externally provided constants or dynamically generated values.

For detailed explanation see the comment for the `ee_function` definition and the the `arity` field of the `ee_compilation_data_function` structure.

Following is a diluted example of how to use a single function to implement two operators.


```C
int subneg(int arity, const  ee_variable_type  *  actuals, ee_variable result)
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

If a function is bound only once, using a non-negative arity, the runtime check for arity can be omitted since the parser makes sure the correct number of parameters is always provided upon invocation, e.g.

```C
int sum(int arity, const  ee_variable_type  *  actuals, ee_variable result)
{
  (void)arity;
  *result = actuals[0] + actuals[1];
  return 0;
}

  //Somewhere inside the parsing function...
  
  ee_compilation_data_function  funcData[]  =  {
    {sum,2}
  };
  const  char  *  funcNames[]  =  {"+"};

  ee_compilation_data bindings = { .functions= { funcData, {funcNames, 1} } };
  
  ee_compile("1 + 1", &environment, &bindings);
```

## Syntax
This describes the default syntax. The syntax can be modified at any scale, from changing the precedence of a single operator to using a completely different grammar.

The syntax is mostly what might be expected inside an expression.

- Various operators with the expected [precedence](#OperatorsTable).
- Parenthesis are used for grouping sub-expressions and invoking functions.
- Spaces are ignored.
- Constants can be provided directly inside the expression.
- Postfix functions are supported.

Here are some samples, where each line is a separate expression.
```
a * (b + c)
2 + -f(g(), x)
e^-1
!a | b & c == d
1.1 - f(100 K)
```

### Lexer

At the first step the parser converts the expression into a stream of tokens and classifies them into five distinct classes:

* Identifiers
* Constants
* Delimiters
* Operators
* Spaces

### Identifiers

Identifiers are used to reference variables and functions. They must not begin with a character that starts any other class. In addition, currently, they must begin with an character in the range `a` to `z` or `A` to `Z`, or the `_` character. The rest of the identifier can include any of the above characters or a numeric character from `0` to `9`.

### Constants

Constants or, more accurately, immediate values, are used to provide data directly in the body of an expression. They **must** begin with a numeric character in the range from `0` to `9` to be recognized as such. A user function can be provided that can consume any number of characters following the initial one and should return a variable with some value, thus allowing constant to support any user-defined semantic. This can include consuming characters that would otherwise be parsed as some other class, thus changing the expression syntax. This, basically, allows creating sub-parsers for a user-define language of any desired complexity. As such it should be used with great care.

If enabled by a compile-time option a default, simple implementation is provided that scans base-10 integers with an optional decimal dot and a fractional part.

### Delimiters

Delimiters are used to, as implied, delimit certain language features.
Delimiters are a character from this list: `,`, `(`, `)`, `[`, `]`, `{`, `}`.
Each delimiter is always a single character. That is, `((` is two delimiters.

### Operators

Operators are used to, well, perform operations.
Operators are a character from the list: `:`, `'`, `.`, `+`, `-`, `*`, `/`, `%`, `^`, `&`, `|`, `~`, `!`, `=`, `>`, `<`.
Operators can consists of more than one character from the list above. By default the following additional, double-character, operators are defined: `==`,`!=`,`>=`,`<=`,`&&`,`||`,`^^`.

### Spaces
Spaces are used to separate between tokens that would, otherwise, be parsed as a single token. *And also to make things pretty and readable.*

The following characters, in C's escape sequence representation, are recognized as space: ` `, `\t`, `\r`, `\n`.

Apart from being used to separate tokens spaces are completely ignored by the parser.

****
After the above step the expression, or rather the generated token stream, is parsed to understand its structure.

The following text describes the default grammar. It can be completely modified, if needed, by changing the parser tables in the source code. No other modifications are necessary.

Note that some delimiters are unused in the default grammar. Also note that operators are implemented using user functions. 

If enabled by a compile-time option default, simple, [implementations](#DefaultOperators) are provided for some operators.

While the grammar defines many operators, actually **using** an operator inside an expression with no provided user function that implements it, will result in a **parser error**. This is also true for variable access and function invocations.

Most importantly, EmExpr imposes **no** semantic meaning to any operator (except the default implementation mentioned above, that can be disabled or overridden) and thus *any operator* can be used to mean *any operation* and have any semantic, as is seen fit by the user of the library.

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

### <a name="VariableAccess"></a>Variable access

To access a bound variable inside an expression information about it, including its name, must be provided in the symbol table.
Variables can appear anywhere a prefix operator is expected.

For example, given two bound variables named `x` and `y`, an expression that calculates their difference could be `x - y` or `x + -y` or `-(-x + y)`.

### User functions

To invoke a user function its name, matching the one set in the symbol table, with all additional flags and conditions, should be provided, followed by the `(` call operator, followed by zero or more, comma delimited expressions, concluding with `)`.
Function invocations can appear anywhere a prefix operator is expected.

Follows an example of invoking two functions, `f` and `g`, with different parameters: `f() + f(0) + f(g()) + f(1, 2 * g())`.

The `,` delimiter that is used to separate parameters is actually an operator with a priority lower than almost any other used operator.
The number of parameters is determined during parsing and a user function with the correct arity must be provided, otherwise the parser will halt with an error.

#### <a name="FunctionVariables"></a>Functions as variables

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

####  <a name="PrefixAsFunctions"></a>Prefix operators as functions

In the default implementation **all** operators can be used as functions, provided an appropriately named user function is provided with the correct arity and flags. This allows, for example, for the following syntax `-(1,2)` that might (or might not, depending on flags) be equivalent to `1-2`.

This functionality can be disable using a compile-time option at the grammar level. That is, when disabled, expressions of this kind would not be recognized as valid even when the appropriate functions are provided in the symbol table.

### Assignment operator
The assignment operator, `=`, when not disabled by a compile-time option, can be used to **write** into variables. A [variable](#VariableAccess) that a value should be stored into must be provided on its left side and an arbitrary complex expression generating said value on it's right. The assigned-to variable must be a simple variable, and not a [function access](#FunctionVariables) through a variable name. 

For example, the following assigns the result of a function call to a variable: `x = f()`.

Expressions that use assignment leave the execution run-time stack empty and, thus, do not return a result.

### <a name="OperatorsTable"></a>Defined operators

There are many operators defined by default, some can be used as infix, and all can be used as prefix.

Prefix operators, i.e. *all* of them, can be also used as [functions](#PrefixAsFunctions), if enabled. The single, user function based, postfix operator is left binding with a precedence of 11. The operators `:`, `'` and `.` are not used by the default grammar. They infix operators are referenced in the table below.

|Operator|Infix precedence|Infix binding|
|--|--|--|
|`=`|1|Left|
|`,`|2|Left|
|<code>&#124;&#124;</code>|3|Left|
|`^^`|3|Left|
|`&&`|4|Left|
|`<`|5|Left|
|`>`|5|Left|
|`==`|5|Left|
|`!=`|5|Left|
|`>=`|5|Left|
|`<=`|5|Left|
|`+`|6|Left|
|`-`|6|Left|
|<code>&#124;</code>|6|Left|
|`*`|7|Left|
|`&`|7|Left|
|`/`|7|Left|
|`%`|7|Left|
|`^`|8|Right|
|`(`|10|Left|

### <a name="DefaultOperators"></a> Default operator implementations

When not disabled by a compile-time option user functions are provided for some operators. While this imposes semantic meaning on the operators the implementations are chosen to be useful in most situations for simple-valued variable types. Users implementing their own variables type would opt-out of compiling thees implementations and provide their own, with any chosen semantics.

Following is a list with short explanation. Note that there is currently no selection based on fixy-ness, only on arity. All comparison and *boolean* operators return `1` as true and `0` as false.

|Operator|Arity|Typical usage|[Alternative](#PrefixAsFunctions) usage|Functionality|
|--|--|--|--|--|
|`-`|1|`-x`||Unary negation|
|`-`|2|`x - y`|`-(x,y)`|Binary subtraction|
|`+`|2|`x + y`|`+(x,y)`|Binary addition|
|`*`|2|`x * y`|`*(x,y)`|Binary multiplication|
|`/`|2|`x / y`|`/(x,y)`|Binary division|
|`==`|2|`x == y`|`==(x,y)`|Comparison, is equal|
|`!=`|2|`x != y`|`!=(x,y)`|Comparison, not equal|
|`>`|2|`x > y`|`>(x,y)`|Comparison, greater|
|`<`|2|`x < y`|`<(x,y)`|Comparison, less|
|`>=`|2|`x >= y`|`>=(x,y)`|Comparison, greater or equal|
|`<=`|2|`x <= y`|`<=(x,y)`|Comparison, less or equal|
|`&&`|1+|`x && y`|`&&x`,`&&(x,y,z)`|Left folding boolean and|
|<code>&#124;&#124;</code>|1+|<code>x &#124;&#124; y</code>|<code>&#124;&#124;x</code>, <code>&#124;&#124;(x,y,z)</code>|Left folding boolean or|
|`^^`|2+|`x ^^ y`|`^^(x,y,z)`|Left folding boolean xor|
|`!`|1|`!x`|`!(x)`|Boolean negation


## Support code
To simplify the general use case support code is provided, as additional sources, with bindings to functions from`math.h` and `cmath.h`, for several variable types.

To allow for more informative feedback to the user who parses expressions, conversion functions, from the parser error codes to user readable strings, are provided. This can be used in systems where textual user feedback is possible to help with writing correct expressions. This is also provided as an additional source file that can be used only by those who need such a facility.

For users who want to *just use* the library a function is provided that takes an expression as input and returns the result as output. This function has a simplified API for variable binding and it includes the default math functions in the evaluation.

```C
    double myVar = 0;
    double result = eelib_execute("cos(2 * PI * phi)", "phi", &myVar);
```
For developers who wish to pre-process symbol tables that are known at compile time the appropriate facilities are provided as separate tools in their own source file.

## Future

This is a list of things to consider after the project is in a working state, in no particular order.

### Unicode

Add support for identifier to be any valid Unicode code point, in UTF-8 encoding.
This is trivially achievable with the addition of user-defined identifier parser functions, that would mirror the constant/immediate user defined parser functions.
While the benefits of this are clear, and, since this is parser-only modification the execution speed and memory usage would stay unchanged, this would have a direct impact on the ability of the symbol table processor to compact it's data.

### Sanity checks

Add various sanity checks during parsing and evaluations.
It must be possible to completely disable them during compilation of the library.

### Variables

Allow using user defined variable types. Currently only the behavior can be changed but the variable must be a C built-in. This must be done to completely decouple the library from the user semantics, also on the (users') implementation level.

### Parser scratch space

As currently defined the parser is using the evaluation environment as its scratch space.

Allow for separating the two memory locations, same as can be done for the runtime stack. This would allow using some global scratch buffer, that is generally available in embedded systems, for the parser while keeping the evaluation environment size to an absolute minimum.

### Name lookup

Allow using special user provided functions for variable and function name lookup during parsing instead of the data directly supplied by the symbol table. This should also be implemented to facilitate a simple switching to a better symbol table provider that should be supplied in an supporting library.

This connects directly to the following item.

### Pre-process parser data

Since the data supplied to the parser, the variables and user functions list, rarely, if never, changes between calls to the parser it would be highly beneficial to perform some pre-processing on this data to convert it to a format better suited for how the parser would operate on it.

This connects directly to the previous item.

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

It will be simple to add parsing several expressions in a single evaluation environment, allowing syntax such as `a = ...expression...; b = ...expression...;`. This will not require any change in the API and thus can be added at a later time without interfering with existing users.

## Notes

This project came to life after an attempt to remove `malloc` from the [tinyexpr](https://github.com/codeplea/tinyexpr) library. It was soon realized this would require a massive change and would actually defeat the purpose of that project. As a result the only thing shared with that library is the *expr* in the name.

The code for the Pratt parser is a rewrite (with modifications and improvements) in C of the same algorithm implemented in C++ from one of my other projects.
