# Introduction

Welcome to the Cricket docs! 
Cricket is a tiny language that you could make yourself, 
but aims to still be ergonomic for programming.

Cricket is a functional language.
No one can agree on what that means in practice,
it's more of a general ideology of language design than any specific rules.
In our case, functional means two things:
higher-order functions and no mutable state.
If you're not familiar with these terms,
let me illustrate them with some examples.

Here's a higher-order function in Cricket:
```
def foo(f):
  x-> f(x) + 1
```
`foo` here is a function that takes a function `f` as input.
It then returns a new function that's the same as the old one
except the returned values are 1 higher.
That is, `foo` only makes sense for functions that return integers.
`x -> ...` is the notation for a lambda, or anonymous function.

Here's an example of the immutability of Cricket:
```
let new_object = (old_object <- p: 7) in ...
```
`<- f:` is the notation for adding a field to an object,
but notice that it returns a new object, instead of editing the old one. So `<-` is pronounced "with."

Cricket functions always take exactly one value.
There are three ways to implement functions of multiple arguments,
each with pros and cons. 
The simplest way is to use an object as the argument:
```
def add(args):
  args.p1 + args.p2

add({p1: 7, p2: 8})
```
Cricket has a special syntax for this to make it nicer:
```
add{p1: 7, p2: 8}
```
A benefit of this approach is that the order of parameters doesn't matter,
and parameters are labelled at every callsite.
We can even leave out arguments if we know the function won't use them.
A downside is that this doesn't play well with higher-order functions,
since they often need to know the names of parameters of functions
so they can call those functions.

The second approach is called "currying," 
and is very familiar to those who are used to functional programming.
Functions that need more arguments simply return functions requesting those arguments:
```
def add(a):
  b -> a + b

add(7)(8)
```
Cricket has special syntax for this too:
```
def add(a)(b):
  a + b

add(7)(8)
```
Currying requires a certain argument order, 
and requires every argument be present,
but plays very nicely with higher-order functions,
and has less-verbose callsites.

The final option is "monoid functions."
The details here are a bit much to go into here,
as the scary name suggests.
But it's essentially a variadic function:
```
def sum {
  Has: a-> accumulator-> a + accumulator,
  Empty: 0
}

sum[1, 2, 3]
```
Monoid functions are only useful when 
what you're looking for is essentially a reduce on a hand-written list.
I added monoid functions to Cricket
because I believe they have a high power-to-weight ratio;
the parser just replaces the callsite above with
```
sum.Has(1)(sum.Has(2)(sum.Has(3)(sum.Empty)))
```
and processing and execution continue from there.
However, they unlock things like `list["A", "B", "C", "D"]` 
or `div[text("Hi, "), bold("world"), text("!")]`.
As you can tell, Cricket uses syntax sugar in a few key places to make it more ergonomic.
Otherwise Cricket would be another "Turing Tarpit"-style esolang:
everything *could* be done with its lambdas and objects, 
but it would be very painful to do so.
We will see still more examples of this syntax sugar to come.
