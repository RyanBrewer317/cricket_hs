# Introduction

Welcome to the Cricket docs! 
Cricket is a tiny language that you could make yourself, 
but aims to still be ergonomic for programming.

Here's the hello-world program in Cricket:
```
def main:
  console.write("Hello, world!")
```
(Don't fret, Cricket doesn't care about whitespace :)


### Functional

Cricket is a functional language.
No one can agree on what that means in practice;
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
but notice that it returns a new object, instead of editing the old one. 
So `<-` is pronounced "with."


### Function Calls

Cricket functions always take exactly one value.
There are three ways to implement functions of multiple arguments,
each with pros and cons. 
The simplest way is to use an object as the argument:
```
def add(args):
  args.p1 + args.p2

add({p1: 7, p2: 8})
```
Cricket has a special syntax to make calls nicer:
```
add{p1: 7, p2: 8} // == 15
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

The third and final option is "fold functions."
The details are a bit much to go into here,
as the scary name suggests.
But it's essentially a variadic function:
```
sum[1, 2, 3]
```
But defined as a fold:
```
def sum: {
  Has(a)(accumulator): 
    a + accumulator, // new accumulator
  Empty: 0
}
```
Fold functions are only useful when 
what you're looking for is essentially a reduce ("fold") on a hand-written list.
I added fold functions to Cricket
because I believe they have a high power-to-weight ratio;
the parser just replaces the callsite above with
```
sum.Has(1)(sum.Has(2)(sum.Has(3)(sum.Empty)))
```
and processing and execution continue from there.
But even with that simplicity,
they unlock syntax like `list["A", "B", "C", "D"]` 
or `div[class("my-class")]["Hello, ", strong[]["world"], "!"]`.
As you can tell, Cricket uses syntax sugar in a few key places to make it more ergonomic.
Otherwise it would be another "Turing Tarpit:"
everything *could* be done with its lambdas and objects, 
but it would be very painful to do so.
We will see still more examples of this syntax sugar to come.


### Next Steps

There are a few directions you can go in your Cricket journey.

If you want to learn how to write useful Cricket programs,
I still have more guides to write,
but you can see lots of example code in [main.ct](/main.ct).

If you're excited about the programming language theory, 
check out [theory.md](/docs/theory.md).