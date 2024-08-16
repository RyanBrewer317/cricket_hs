# Cricket

Cricket is a tiny functional language.
The goal is to minimize the implementation size so that the language is easy to implement on every platform.

Cricket makes great use of objects, but it is primarily an impure lazy functional language.

### Laziness

```
let infinite_loop = (x->x(x))(x->x(x)) in
(_ -> let force _ = print(1) in print(2))(infinite_loop)
```
output:
```
1
2
```
Cricket doesn't evaluate anything until they're "forced."
That infinite loop there would hang the program if we started evaluating it,
but it has no influence on the final result, so it never even starts.
When we *want* something to get evaluated without using its result, we use `let force`.
This is a version of `let` that evaluates the value 
regardless of whether or not we ever use the variable later.

### Impure

It's very rare to see a lazy language with simple side effects.
The most famous lazy language is Haskell, and monads are infamously hard to grasp.
There are two major issues laziness faces when it comes to side effects:

1. We often don't care about the returned value from a side effect.
   For example, `print` is generally seen as not really returning anything.
   Laziness sees us not using the result and decides not to start the side effect at all!
   Haskell deals with this by making side effects actually return results that you must use,
   which leads straight into the world of monads.
2. Even if we *do* use the returned value of a side effect,
   laziness may start the effect later than you expect.
   For example, if you request user input a few times,
   they will appear to the user in the order in which you *use the results,*
   not the order you've written the prompts in your code.
   This is painful, and we'd really like to just say that something should happen immediately.

Cricket solves this with the `let force` construct, which evaluates its value whether or not its used later, immediately.
```
let _ = print(1) in print(2)
let force _ = print(1) in print(2)
```
outputs:
```
2
1
2
```
things going wrong:
```
let x = input() in
let y = input() in
let force _ = print(y) in
print(x)
```
outputs:
```
> hello
hello
> world
world
```
things going right:
```
let force x = input() in
let force y = input() in
let force _ = print(y) in
print(x)
```
outputs:
```
> hi
> there
there
hi
```

No monads required! This is much easier to wrap your head around.

### Object Oriented

Instead of ADTs as you might be used to in other functional languages,
Cricket uses "coinductive datatypes" or "codata," which basically just means immutable objects.
The idea is that instead of pattern matching to extract data,
you use the familiar dot-syntax, which is academically called a "projection" or "elimination."

OOP actually has really interesting interactions with laziness.
For example, because of laziness, Cricket doesn't really have object fields. Only getters! [^1]
And don't forget iterators and other lazily-generated datastructures.
There are lots of ways in which OOP principles naturally play well with laziness,
which is mathematically due to the nice relationship between negative types (objects, codata) and a laziness comonad.

[^1]: This is another reason `let force` is a great fit for Cricket: situations where you don't want an object field to get re-evaluated every time its accessed!

### Plans

There are bugs around the inherent recursion of these lazy objects (consider `{this.x: this.x}`), and fixing them is my top priority right now.
Then there's improving the laughable error messages and parsing.

There's certainly a pain point of not having sum-of-products ADTs, for making certain states unrepresentable.
I'd like to explore options for this that aren't just adding ADTs and pattern-matching.
Linear logic has a coinductive disjunction `par` and it'd be cool if I could port it somehow to Cricket's setting but I'm not too optimistic.
Then there's nulls as an option, which are terrible in their own way but expressive with a struct-of-arrays approach.
And we might be able to add the teeniest tiniest little nullability type system if we must.
Finally, in a more boring and potentially tedious way, sum types could just be functions taking an object {A->C,B->C} and returning a C.
I can imagine sugaring that or something, we'll see.

Finally, I want to port the codebase to C, using reference counting. 
The stacks used by the Krivine machine benefit enormously in time-complexity by being immutable (namely, there's O(1) copying).
So the C port would likely keep a lot of the implementation details of the Haskell original, though eagerly evaluated of course.
The main benefits include a more familiar-looking implementation, potential performance improvements, and a smaller dev environment and program binary.
