# Ivy-lang
A programming language written in python.

# Features
* Static Typing
* Expressions and Statements
* Binary Operations
* Conditional Control Flow
* Looping Control Flow
* Functions
* Anonymous Functions
* Classes and constructors
* "Everything is an object"

# Sample Program
```
package system;
import sys;

main: () () {

}
```

# Grammar
package = package \[identifier\]
import = import \[identifier\]

program := (\[statement\] | \[function-declaration\] | \[conditional\] | \[while-loop\] | \[for-loop\])*

statement := (\[declaration\] | \[assignment\] | \[expression\] | \[package\] | \[import\] | break | continue) ";"
list-statements := (statement)*

assignment := \[declaration\] = \[expression\]
declaration := \[type\] \[identifier\]
function-declaration := func \[identifier\]: "(" \[list-parameters\] ")" ( -> "(" \[list-parameters\] ")" )? \[block\]

expression := \[binfactor\] (and \[binfactor\])*
binfactor := \[binary\] (or \[binary\])*
binary := \[term\] ((< | <= | > | >= | == | ===) \[term\])?
term := \[factor\] ((+|-) \[factor\])*
factor := \[atom\] ((*|/|%) \[atom\])*
atom := \[number\] | \[string\] | \[function-call\] | \[attribute-call\] | \[index-call\] | "(" \[expression\] ")"

list-expression := (\[expression\],)*
collection := \[ list-expression \]

type := \[identifier\] | int | float | str | bool | coll | arr | dict | func
list-parameters := (\[declaration\],)*
function-block := function ( \[list-parameters\] ) \[block\]
block := { \[program\] }

variable-call := \[identifier\]
function-call := (\[identifier\] | \[function-block\]) "(" \[list-expression\] ")"
attribute-call := \[identifier\] ("." \[identifier\])+
index-call := (\[identifier\] | \[collection\]) "\[" \[expression\] "\]"

range := \[integer\] .. \[integer\]
iteration := \[identifier\] in (\[range\] | \[collection\])

conditional := if \[expression\] \[block\] (elif \[expression\] \[block\])* else \[block\]
while-loop := while \[expression\] \[block\]
for-loop := for \[iteration\] \[block\]

identifier := \[a-zA-Z_\](\[a-zA-Z0-9_\])*
number := (+|-| ) \[integer\] | \[float\]
integer := \[0-9\]+ (TokenType.INTEGER_CONSTANT)
float := \[0-9\]*(.\[0-9\]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " \[.*\] " | ' \[.*\] '

## TO-DO:
\[\]
