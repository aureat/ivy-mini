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
`program := (__statement__ | [function-declaration] | [conditional] | [while-loop] | [for-loop])*
list-statements := (statement)*

conditional := if __expression__ __block__ (elif __expression__ __block__)* else __block__
while-loop := while __expression__ __block__
for-loop := for [iteration] __block__

statement := ([declaration] | [assignment] | __expression__ | [package] | [import] | break | continue) ";"
assignment := [declaration] = __expression__
declaration := [type] __identifier__
function-declaration := func __identifier__: "(" [list-parameters] ")" ( -> "(" [list-parameters] ")" )? __block__
package := package __identifier__
import := import __identifier__

list-expression := (__expression__,)*
collection := [ list-expression ]

expression := [binfactor] (and [binfactor])*
binfactor := [binary] (or [binary])*
binary := [term] (( < | <= | > | >= | == | != | === | !== ) [term])?
term := [factor] (( + | - ) [factor])*
factor := [atom] (( * | / | % ) [atom])*
atom := [number] | [string] | [function-call] | [attribute-call] | [index-call] | "(" __expression__ ")"

type := __identifier__ | int | float | str | bool | coll | arr | dict | func
list-parameters := ([declaration],)*
function-block := function ( [list-parameters] ) __block__
block := { [program] }

variable-call := __identifier__
function-call := (__identifier__ | [function-block]) "(" [list-expression] ")"
attribute-call := __identifier__ ("." __identifier__)+
index-call := (__identifier__ | [collection]) "[" __expression__ "]"

range := [integer] .. [integer]
iteration := __identifier__ in ([range] | [collection])

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
