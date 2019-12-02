# Ivy-lang
A programming language written in python.

# Features
* Static Typing (will move to dynamic typing)
* Expressions and Statements
* Binary Operations
* Conditional Control Flow
* Ternary Conditional
* Looping Control Flow
* Functions
* Anonymous Functions
* Classes and constructors
* Methods

# How to use the console
* Run the `ivy` file to initialize the repl or type `python ivy.py -p` in terminal
* To run ivy test files type `python ivy.py -f [filename]` in terminal
e.g. `python ivy.py -f tests/conditional.ivy`
* To tokenize a file run `python ivy.py -t [filename]`

# Example Programs
```
func factorial = function(int n) {
    if n > 1 {
        return n * factorial(n-1);
    }
    return n;
};
print factorial(6);
```

# Technical Specifications
## Features
* Builtin-type objects: Null, Integer, Float, String, Boolean, Collection, Function
* All literals are resolved into a builtin-type object
* Statements are not evaluated to a value
* Expressions are evaluated to a type object

## Grammar
`program := (statement | [function-declaration] | [conditional] | [while-loop] | [for-loop])*
list-statements := (statement)*

conditional := if expression block (elif expression block)* else block
while-loop := while expression block
for-loop := for [iteration] block

statement := ([declaration] | [assignment] | expression | [package] | [import] | break | continue) ";"
assignment := [declaration] = expression
declaration := [type] identifier
function-declaration := func identifier: "(" [list-parameters] ")" ( -> "(" [list-parameters] ")" )? block
package := package identifier
import := import identifier

list-expression := (expression,)*
collection := [ list-expression ]

expression := [binfactor] (and [binfactor])*
binfactor := [binary] (or [binary])*
binary := [term] (( < | <= | > | >= | == | != | === | !== ) [term])?
term := [factor] (( + | - ) [factor])*
factor := [atom] (( * | / | % ) [atom])*
atom := [number] | [string] | [function-call] | [attribute-call] | [index-call] | "(" expression ")"

type := identifier | int | float | str | bool | coll | arr | dict | func
list-parameters := ([declaration],)*
function-block := function ( [list-parameters] ) block
block := { [program] }

variable-call := identifier
function-call := (identifier | [function-block]) "(" [list-expression] ")"
attribute-call := identifier ("." identifier)+
index-call := (identifier | [collection]) "[" expression "]"

range := [integer] .. [integer]
iteration := identifier in ([range] | [collection])

identifier := [a-zA-Z_] ( [a-zA-Z0-9_] )*
number := (+|-| ) [integer] | [float]
integer := [0-9]+ (TokenType.INTEGER_CONSTANT)
float := [0-9]*(.[0-9]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " [.*] " | ' [.*] '`

## TO-DO
- Fix AND/OR binary operations
- Proper syntactical definition for unary not operation (ex. not 16 > 2 etc..)
- Complete object model
- Define binary operations on other objects
- If-Else Conditionals PERMENANT PATCH
- If-Elif conditionals without else blocks
- Parameters implementation and call stack creation for blocks
- Function-name storing for call stack
- Build a return statement for the functions
- Parameters for methods for objects
- Build semantic analyzer and symbol tables
- Type System Error checking
- System framework (system module name)
- Error handling framework based on call stack
- Error processing through trace stack: lexer, parser, interpreter
- Collection Data Type
- Local and global environment references
- Define types in memory, referencing python-implemented objects
- Handle type conversions
- Implement the system package
