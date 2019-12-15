# Ivy-lang
A programming language written in python.

# Features
* Dynamically Typed
* Expressions and Statements
* Object Attributes
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
* To run ivy test files type `python ivy.py -f [filepath]` in terminal
e.g. `python ivy.py -f tests/conditional.ivy`
* To tokenize an ivy file run `python ivy.py -t [filepath]` (note that you can either put the whole path or search for files within the current directory)

# Example Programs
```
nums = [1, 2 , 3, 4];

for (num : nums) {
    print num;
}
```

```
factorial = function(n) {
    if (n > 1) {
        return n * factorial(n-1);
    }
    return n;
};
print factorial(6);
```

```
struct WelcomeMessenger {

    func constructor(name) {
        if name != None {
            self.name = name;
        } else {
            self.name = "World";
        }
        self.length = name.length;
    }

    func say_hello() {
        return "Hello, " + self.name;
    }

}

messenger = new WelcomeMessenger("Altun");
print messenger.say_hello();
```

# Technical Specifications
## Features
* Builtin-type objects: Null, Integer, Float, String, Boolean, Collection, Function
* All literals are resolved into a builtin-type object
* Statements are not evaluated to a value unless they are expressions
* Expressions are evaluated to a type object (which means they are evaluated to a value)

## Grammar
```
program := ([statement] | [function-declaration] | [conditional] | [while-loop] | [for-loop])*
conditional := if [expression] [block] (elif [expression] [block])+ (else [block])?
while-loop := while [expression] [block]
for-loop := for [iteration] [block]
function-declaration := func [identifier]: "(" [list-parameters] ")" ( -> "(" [list-parameters] ")" )? [block]
```

```
statement := ([assignment] | [expression] | [package] | [import] | [return] | break | continue) ";"
assignment := [identifier] = [expression]
return := return (expression)?;
package := package [identifier]
import := import [identifier]
```

```
expression := [binfactor] (and [binfactor])*
binfactor := [binary] (or [binary])*
binary := [term] (( < | <= | > | >= | == | != | === | !== ) [term])?
term := [factor] (( + | - [ ] ) [factor])*
factor := [atom] (( * | / | % ) [atom])*
atom := [number] | [string] | [function-call] | [attribute-call] | [index-call] | "(" [expression] ")"
```

```
type := [identifier] | int | float | str | bool | coll | arr | dict | func
list-parameters := ([declaration],)*
function-[block] := function ( [list-parameters] ) [block]
[block] := { [program] }
```

```
list-expression := ([expression],)*
collection := [ list-[expression] ]
```

```
variable-call := [identifier]
function-call := ([identifier] | [function-[block]]) "(" [list-[expression]] ")"
attribute-call := [identifier] ("." [identifier])+
index-call := ([identifier] | [collection]) "[" [expression] "]"
```

```
range := [integer] .. [integer]
iteration := [identifier] in ([range] | [collection])
```

```
identifier := [a-zA-Z_] ( [a-zA-Z0-9_] )*
number := (+|-| ) [integer] | [float]
integer := [0-9]+ (TokenType.INTEGER_CONSTANT)
float := [0-9]*(.[0-9]+)? (TokenType.FLOAT_CONSTANT)
boolean := true | false
string := " [.*] " | ' [.*] '
```

## TO-DO
- [ ] Complete object model
- [ ] Finish binary operations
- [ ] Callstack needs a fix: fibonacci function not working
- [ ] Build semantic analyzer and symbol tables
- [ ] Define binary operations on other objects
- [ ] If-Else Conditionals PERMENANT PATCH
- [ ] If-Elif conditionals without else blocks
- [ ] Parameters implementation and call stack creation for blocks
- [ ] Parameters for methods for objects
- [ ] System framework (system module name)
- [ ] Local and global environment references
- [ ] Define types in memory, referencing python-implemented objects
- [ ] Handle type conversions
- [ ] Implement the system package
