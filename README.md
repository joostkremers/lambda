# Lambda #

If you're reading this, you're probably on the wrong page. Lambda is a simple Lisp based on the code in the book [Build Your Own Lisp](http://www.buildyourownlisp.com/). It lacks all kinds of features that a real programming language should have, so it's not really of use for anything interesting except for yours truly as a way to have some fun and learn something in the process. :-)

## Built-in types ##

Currently, the following built-in types exist:

- Numbers
- Strings
- Boolean
- Symbols
- S-expressions
- Q-expressions
- Functions
- Macros
- Errors


## Numbers and mathematical operators ##

All numbers are treated as double, even though they are printed as integers when they have no fraction part. The following mathematical operators are defined:

- `+`: addition
- `-`: substraction
- `*`: multiplication
- `/`: division
- `mod`: modulo
- `pow`: power

`mod` converts its arguments to long before computing the result, which is then converted back to double.


## Strings ##

Strings are delimited with double quotes and can have the usual escape characters. `puts` prints a string to stdout; `print` displays a string (or any other object) as a Lisp object. `input` prompts for input from the user and returns it as a a string. `read` takes a string and converts it to a Lisp object.


## Boolean and conditionals ##

The boolean type has values `true` and `false`. The conditional operator `if` only accepts a boolean value as its test. Therefore, all comparison operators return a boolean value.

The mathematical comparison operators are `=`, `/=`, `<`, `>`, `<=` and `>=`. They only compare numbers. The function `equal` tests equality for other types.

The format of `if` is:

```
(if <test> <then> <else>)
```

Both `<then>` and `<else>` can only be a single clause. The `<else>` clause is optional and defaults to `false`.


## Symbols ##

Symbols can consist of alphanumerical characters and any of the characters `_+-*/\=<>&%^`. A symbol evaluates to a value, which can be any of the built-in types. A symbol can be bound to a function in the same way as to a value. In essence, this means that Lambda is a Lisp-1.

## Errors ##

Errors are first-class objects, but when a function returns an error, evaluation is interrupted and the error is returned to the REPL. An error can be created with the function `error`, which takes a single argument, a string that functions as error message.


## S-expressions and q-expressions ##

An s-expression is a list and is delimited by parentheses, just as in conventional Lisps. Unlike conventional Lisps, an s-expression is *always* evaluated when it is encountered. This means that its first element must be a function. (More precisely: must be a symbol that is bound to a function).

A q-expression is also a list, but unlike s-expressions, a q-expression is not evaluated, (more precisely: evaluates to itself), except when passed to the function `eval`. A q-expression is written as `'(...)`, i.e., an s-expression preceded by a single quote. Unlike conventional Lisps, this quote is *not* an abbreviation for a function `quote`, it is part of the syntax for q-expressions.

Q-expressions have the same function as macros in conventional Lisps: they delay evaluation. Unlike macros, however, evaluation of a q-expression is not delayed because the first element of the list is a special type of element, but because the list itself is special. With q-expressions, functions can be used to perform the functions that macros perform in conventional Lisps:

```
(def '(a b) 1 2)
```

`def` is a normal function, but is used here to define the symbols `a` and `b` as variables with the values `1` and `2`. Because `'(a b)` is a q-expression, `def` can manipulate the symbols `a` and `b`. Interestingly, the first argument to `def` does not have to be a q-expression. It can also be another type of expression, which must then evaluate to a q-expression. For example:

```
(def (join '(a) '(b)) 1 2)
```

The function `join` takes two q-expressions and combines them into a single q-expression: `(join '(a) '(b)) ==> '(a b)`. This list is of the right type for `def`, so that the symbols `a` and `b` are defined as variables with the values `1` and `2`.

Note that most (though not all) list handling functions in Lambda take q-expressions as arguments and return q-expressions: `(head '(a b c)) ==> '(a)`.


## Functions and macros ##

Functions are first-class objects and can be created with the function `\` ("lambda"). `\` takes two arguments, a list of variables and a body. Both have to be q-expressions. Using `def`, they can be assigned to a variable which can then be used as a function in the traditional manner:

```
lambda> (def '(plus5) (\ '(a) '(+ 5 a)))
>> ()

lambda> (plus5 7)
>> 12
```

However, there is a more convenient way to define functions:

```
(fn (plus5 a) (+ 5 a))
```

`fn` does not require q-expressions, because it is a macro. Although Lambda has q-expressions and doesn't really need macros, there is nonetheless a built-in macro type, primarily for cosmetic reasons: all those single quotes are not very pretty. Macros in Lambda behave slightly differently from conventional Lisps, however: a macro is a kind of function whose arguments are converted into q-expressions before it is called. Because it's a function, it returns a value, not code. (Although the value it returns can be a q-expression.)

The conversion to q-expressions is done according to the following rules:

- Q-expressions are unchanged
- S-expressions are converted to q-expressions
- Anything else is wrapped in a q-expression

So assuming `mymac` is a macro, the call `(mymac a 1 (+ 2 3) '(pow 5 6))` is converted to `(mymac '(a) '(1) '(+ 2 3) '(pow 5 6))` *before* `mymac` is called. That is, the macro itself cannot tell whether an argument was originally an s-expression or a q-expression or even something else, which means that macros are only useful for things that always take q-expressions. As such, `def` and `\` are functions, not macros, because the arguments passed to them usually need to be evaluated.

An anonymous macro can be created with the function `^` ("hat"), which is similar to `\` but returns a macro instead of a function. This can then be assigned to a symbol with `def`. To facilitate macro definitions, a macro `mac` has been defined.

An example of a (pre-defined) macro is `case`, which is similar to a `switch` statement in C:

```
(fn (test n)
    (case n
      (1 "one")
      (2 "two")))
```

If `case` had been a function, a call to `case`would have to be written as follows:


```
(fn (test n)
    (case n
      '(1 "one")
      '(2 "two")))
```

That is, each clause would have to be explicitly marked as a q-expression. Note that it is in fact possible to write the call like this, since `case` expects its arguments to be q-expressions. The s-expressions in the first form are converted to q-expressions before being passed to `case`.

In a macro call, the symbol `!` can be used to force immediate evaluation of the following expression:

```
(fn (test n)
    (case n
      (1 "one")
      !(list 2 "two")))
```

The second clause of the case statement, `(list 2 "two")`, is evaluated before `case` is called. Since `list` returns a q-expression, the call to `case` here is identical to the previous ones.

Immediate evaluation can also apply to variables (symbols):

```
(def '(a) '(2 "two"))

(fn (test n)
    (case n
      (1 "one")
      !a))
```

Here, the variable `a` is evaluated before `case` is called.

Macros are susceptible to variable capture, for which there is currently no decent solution.


## Evaluation ##

Numbers, strings and q-expressions evaluate to themselves. Evaluating a symbol returns the value associated with it in the current environment or, if the symbol is not defined in the current environment, in a parent environment: variables have dynamic scope.

When evaluating an s-expression, a few cases are to be considered. Roughly, evaluation proceeds as follows:

- If the list is empty, it is returned as-is, without evaluation.
- The first element of the list is evaluated. If it evaluates to a macro, the other elements are converted into q-expressions.
- If the first element does not evaluate to a macro, the other elements of the list are evaluated.
- If the list contains only one element, this element is returned.
- If the list contains more than one element, the first element must be a function or a macro, which is then called with the other elements as arguments.

One crucial point is that s-expression that have only one element are *not* considered function calls. This makes it possible to get elements "out of" a q-expression, so to speak:

```
lambda> (eval '("a string"))
>> "a string"
```

Compare:

```
lambda> (eval '("one string" "another string"))
>> Error: Not a function. Got type String instead
```

This also means that it is not possible to define functions that take no arguments. You can evaluate an s-expression with a function as its only element, but it returns the function itself:

```
lambda> (eval (list))
>> <builtin function at 0x14198c0>
```

Strictly speaking, functions that take no arguments are not functions at all, so this state of affairs makes sense.


## Doc strings ##

The function `def` defines a symbol as a variable in the global name space. You can optionally pass a string to `def`, which will be stored as the documentation string for the symbol:

```
lambda> (def '(temp) 18 "Global temperature")
>> ()
```

The doc string can be retrieved again with the `doc` macro:

```
lambda> (doc temp)
>> "Global temperature"
```

The macros for defining variables (`var`), functions (`fn`), and macros (`mac`) all take an optional doc string. For example, the function `split` from the prelude:

```
(fn (split n l)
    "Format: (split n list)
Split <list> at the <n>th element."
    (list (take n l) (drop n l)))
```

The built-in function `doc` can be used to obtain the doc string:

```
lambda> doc split
>> "Format: (split n list)\nSplit <list> at the <n>th element."
```

A better alternative is the macro `describe`, however:

```
lambda> describe split

Format: (split n list)
Split <list> at the <n>th element.

>> ()
```

`fn` and `mac` both have an optional doc string as their second argument, before the body of the function. `var`, on the other hand, has an optional doc string as its third argument, just like `def`:

```
(var nil '() "The empty list")
```


## Built-in functions ##

The following functions and macros are defined as built-ins:

- List functions:
    - `list`: create a list

        `(list 1 2 3 4) ==> '(1 2 3 4)`

    - `head`: return the first element of a list as a q-expression

        `(head '(1 2 3 4)) ==> '(1)`

    - `tail`: return the tail of the list

        `(tail '(1 2 3 4)) ==> '(2 3 4)`

    - `join`: join lists

       `(join '(1) '(2) '(3 4)) ==> '(1 2 3 4)`

    - `init`: return the list without its last element

        `(init '(1 2 3 4)) ==> '(1 2 3)`

    - `cons`: combine an element with a list

        `(cons 1 '(2 3 4)) ==> '(1 2 3 4)`

    - `len`: return the length of the list

        `(len '(1 2 3 4)) ==> 4`

    - `nth`: return the nth element of a list and evaluate

        `nth 0 '(1 2 3 4) ==> 1`

    - `last`: return last element of a list and evaluate

        `last '(1 2 3 4) ==> 4`

    - `eval`: evaluate a q-expression as an s-expression and return the result.

        `(eval '(+ 2 5)) ==> 7`

- Variable functions
    - `def`: define a symbol as a global variable

        `(def '(a) 1)`

        `(def '(temp) 18 "Global temperature")`

    - `set`: set a list of symbols as local variables

        `(set '(a b) 1 2)`

    - `doc`: return the doc string of a symbol

        `(doc temp) ==> "Global temperature"`

- General functions
    - `\`: create an anonymous function

        `(\ '(a) '(* 2 a))`

    - `^`: create an anonymous macro

        `(^ '(a) '(list a))`

    - `exit`: exit the REPL or program

        `(exit 0)`

    - `typeof`: return the type of an expression as a string

        `(typeof '(a)) ==> "Q-expression"`

    - `load`: load a file from disk and evaluate it

        `load "test.l"`

    - `print`: print an object; return `()`

        `print print ==> ()`, displays `<builtin function at 0x14119d0>`

        `print fn ==> ()`, displays `(^ '(name val & doc) '(def name val (if (not (null doc)) (eval (nth 0 doc)))))`

    - `error`: create an error and return it

        `(error "Illegal option") ==> "Error: Illegal option"`

    - `puts`: print a string to stdout, return `()`

        `(puts "Hello.") ==> ()`, displays `Hello.`

    - `read`: read a string as Lambda code and return as q-expression

        `read "(+ a b)" ==> '((+ a b))`

    - `input`: prompt for input

        `(input "Hello, what is your name? ") ==> "John"`

- Mathematical and comparison functions
    - `+ - * / pow mod`: standard mathematical operators; all these functions are multivalued

        `(+ 2 3 4) ==> 9`

    - `= /= < > <= >=`: comparison of numbers only; all these functions are multivalued

        `(< 2 3 4) ==> true`

    - `if`: conditional execution (macro)

        `(if (> 2 1) (list 1 2) (list 2 1)) ==> '(1 2)`

    - `equal`: test equality of any two objects

        `(equal '(a) '(a)) ==> true`

    - `null`: test if argument is the empty list

        `(null '()) ==> true`

- Logical functions
    - `and or not`: standard logical operators; `and` and `or` are macros and take multiple arguments,`not` is a function; all three accept only booleans.


## The prelude ##

The prelude contains a basic set of functions and macros beyond the built-in ones and is loaded automatically upon startup if it is located in the directory from which Lambda is started. See the file `prelude.l` for details.
