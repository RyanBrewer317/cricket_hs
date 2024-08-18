# Cricket

Cricket is a tiny functional language.
The goal is to minimize the implementation size so that the language is easy to implement on every platform.

Cricket makes great use of objects, but it is primarily an impure lazy functional language.

As I write this, every demo in this README works perfectly!

### Laziness

```
let infinite_loop = (x->x(x))(x->x(x)) in
(_ -> let force _ = console.write(1) in console.write(2))(infinite_loop)
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
let _ = console.write(1) in console.write(2) // prints 2
```
```
let force _ = console.write(1) in console.write(2) // prints 1, then 2
```
things going wrong (I'll get to the `{}` syntax when I discuss objects later):
```
let x = console.read{} in
let y = console.read{} in
let force _ = console.write(y) in
console.write(x)
```
outputs:
```
> hi
hi
> world
world
```
things going right:
```
let force x = console.read{} in
let force y = console.read{} in
let force _ = console.write(y) in
console.write(x)
```
outputs:
```
> hi
> world
world
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

Object fields are written `x.y: z` or `y: z`, depending on if you want access to `x`, the current object, in `z`.
This is a little like in Go, where methods are written `func (o Obj) method(args) ty { ... }`.
That is, you get to choose the name of your `this`/`self` parameter. I often choose `this`.

Here's a list in Cricket. 
```
let list = {
  e: case -> case.empty,
  p: first-> rest-> case-> case.has(first)(rest)
} in
let l = list.p(1)(list.p(2)(list.p(3)(list.e))) in
// print out all the elements
l{
  empty: 0,
  this.has: first-> rest-> 
    let force _ = console.write(first) in
    rest(this) // anonymous recursion: apply this same pattern match to the tail!
}
```
outputs:
```
1
2
3
```

Let's unpack this. We first make a sort of namespace object, called `list`.
Notice how it has two methods, each taking some object that we call `case` and evaluating one of its fields.
That means that the list we construct, `l`, is a *function!*
In Cricket, object literals can be given to functions without wrapping them in parentheses:
`foo({val: bar})` can be written `foo{val: bar}`.
We give `l` an object this way that handles each of its two possibilities.
If it was constructed with `list.e` ("empty"), we halt with `0`.
If, on the other hand, it was constructed with `list.p` ("prepend"),
we write the first element of the list to the console,
and then apply the current pattern match (which we've named `this`)
to the rest of the list.

This demonstrates how one might write the ADTs (positive types) we know and love from other functional languages.
Note that it's perfectly fine to omit cases if you know they're impossible:

```
let stream_of_ones = {this.val: list.p("hi!")(this.val)}.val in
stream_of_ones{
  has: first-> t-> t{
    has: second-> _->
      let force _ = console.write(first) in
      console.write(second)
  }
}
```
outputs:
```
hi!
hi!
```

Our infinite stream here is lazy, and if we only need the first two values then all's well!

Now let's look at some OOP things:

```
let dog = {bark: "woof!"} in
let cooper = dog <- name: "Cooper" in
console.write(cooper.name + " says: " + cooper.bark)
```
outputs:
```
Cooper says: woof!
```

The `ob <- f: val` syntax returns a new object that is the same as `ob` except it has a field `f` of value `val`.
Here we use it to specialize objects, like a prototypical form of inheritance.



### Plans

I need more than just stdio I/O stuff too, like file IO and networking.
As an embeddable language, great FFI with C is a must.
I also need to add booleans, floats, and strings at the very least.
And `let rec` for recursive bindings.
Booleans (ie `if`/`else`) and recursive bindings would improve things quite a bit, I think.
Nice syntax for lambda methods (like `{this.f(x): ...}` instead of `{this.f: x->...}`) could be good.
And for let bindings too, like `let f(x) = ... in ...`.
We need a better way of doing global definitions, too, like `def f(x) ...`.
I really like back-passing sugar, so it'd be nice to have that too, like `let x <- f in ...` for `f(x -> ...)`.
Then there's improving the laughable error messages and parsing.

There's certainly a pain point of not having sum-of-products ADTs, for making certain states unrepresentable.
I'd like to explore options for this that aren't just adding ADTs and pattern-matching.
Linear logic has a coinductive disjunction `par` and it'd be cool if I could port it somehow to Cricket's setting but I'm not too optimistic.
Then there's nulls as an option, which are terrible in their own way but expressive with a struct-of-arrays approach.
And we might be able to add the teeniest tiniest little nullability type system if we must.
Finally, in a more boring and potentially tedious way, sum types could just be functions taking an object {A->C,B->C} and returning a C.
I can imagine sugaring that or something, we'll see.

Finally, I want to port the codebase to C, using reference counting. 
It's worth thinking about how reference counting would interact with C FFI; I've heard bad things about Python in this regard.
Perhaps another memory management technique would be better, but I need to think of it!
The stacks used by the Krivine machine benefit enormously in time-complexity by being immutable (namely, there's O(1) copying).
So the C port would likely keep a lot of the implementation details of the Haskell original, though eagerly evaluated of course.
The main benefits of the C port are a more familiar-looking implementation, potential performance improvements, and a smaller dev environment and program binary.
