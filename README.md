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
## Statements
program := (__statement__ | [function-declaration] | [conditional] | [while-loop] | [for-loop])* \n
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
binary := [term] ((< | <= | > | >= | == | ===) [term])?
term := [factor] ((+|-) [factor])*
factor := [atom] ((*|/|%) [atom])*
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

identifier := [a-zA-Z_]([a-zA-Z0-9_])*
number := (+|-| ) [integer] | [float]
integer := [0-9]+ (TokenType.INTEGER_CONSTANT)
float := [0-9]*(.[0-9]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " [.*] " | ' [.*] '

## TO-DO:
[]
