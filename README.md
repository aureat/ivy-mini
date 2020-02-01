# ivy-lang
Ivy - a programming language with mostly javascript syntax written in python. Ivy Language was developed as a research project to create a modifiable attribute-based object language interpreter. The language highly resembles javascript and has enough features to write most programs.

# Features
* Dynamically Typed
* Expressions and Statements
* Binding and Scoping
* Error Tracebacks
* Object Attributes
* Binary Operations
* Conditional Control Flow
* Looping Control Flow
* Functions and Lambdas
* Classes and constructors
* Atribute-based object model
* Bounded Methods
* Static methods and attributes

# How to use the console
* Run the `ivy` file to initialize the repl or type `python ivy.py` in terminal

* To run ivy test files type `python ivy.py -f [filepath]` in terminal
e.g. `python ivy.py -f tests/factorial.ivy`
* To import an ivy file use the built-in import function e.g. `import('tests/factorial.ivy');`

# Tests
```js
// tests/structs.ivy
struct Person {

    function construct(name, surname, age) {
        self.name = name;
        self.surname = surname;
        self.age = age;
    }

    // call a method to get fullname
    function full_name() {
        return self.name + " " + self.surname;
    }

    // call the static function and use a name and a surname
    static function anyname(name, surname) {
        return name + " " + surname;
    }

    // or the block can initialize an instance variable
    // and call the method
    self.fullname = self.full_name();

}

person = new Person("Altun", "Hasanli", 17);
print(person.type);
print(person.fullname);
print(person.full_name());
print(Person.anyname(person.name, person.surname));
```
```js
// tests/fibonacci.ivy
function fibonacci(n) {
    if(n <= 1) return 1;
    return fibonacci(n-1) + fibonacci(n-2);
}

print(fibonacci(10));
```
```js
// tests/factorial.ivy
function factorial(n) {
    if(n > 1) {
        return n * factorial(n-1);
    }
    return n;
}

print(factorial(6));
```
```js
// tests/accumulator.ivy
function accumulator(start) {
  number = start;
  function adder(amount) {
      outer number;
      number += amount;
      return number;
  }
  return adder;
}

adder = accumulator(20);
adder(40);
adder(50);
print(adder(100));
```
```js
// tests/maxitem.ivy
function findMax(arr) {
    max = arr[0];
    a = 1;
    while (a < length(arr)) {
        if (arr[a] > max) {
            max = arr[a];
        }
        a += 1;
    }
    return max;
}

arr = [1,2,50,4,5];
print(findMax(arr));
```
```js
// tests/while1.ivy
counter = 0;
while(true) {
    if(counter == 10) {
        break;
    }
    print(counter);
    counter += 1;
}
```
