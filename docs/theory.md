# Theory

With Cricket, I wanted to see how far I could get by adding Abadi and Cardelli's Imperative Object Calculus to a lazy lambda calculus. I read that Call-By-Name normalization is a well-behaved evaluation strategy for negative types (functions and objects) and side effects. This is because effectful CBN represents an adjunction that preserves limits (negative types, aka functions and objects) but not colimits (positive types, aka ADTs). I can't say I *fully* understand what that means, just that side effects and laziness mean you have to fill your effectful data with monads like in Haskell, whereas OCaml Call-By-Value side effects are performed before constructors touch them which is nice. Negatively typed languages (like object-oriented ones) use getters and whatnot a bunch, which make CBV side effects annoying to sequence through a program correctly and lazy side effects nice, leading to stuff like iterators. However, most functional languages mostly use positive types (tagged unions and pattern matching, with lamdbdas mostly as the glue around "real" computation). On the other hand, most object-oriented languages use CBV side effects. I figured that a CBN negatively-typed language is an interesting and fairly-unexplored point in the programming language design space. 

I was also encouraged by two things. Firstly, a Krivine machine is absolutely trivial to implement, if you exclude parsing and converting to De Bruijn form. So I knew the interpreter implementation would be very compact, which I like a lot. Secondly, the Imperative Object Calculus is already a little lazy! There are no fields on their objects, only methods, so fields are simulated by getters. Given that real fields would be hard for me to implement in a lazy language anyway, I saw that as an encouraging sign.

What I did not expect was how powerful this combination would actually be!

At some point, while I found it fun that I could use the IOC's self-typed objects to derive lots of OOP things, I was seriously grieving the lack of positive datatypes. That is, tagged unions and pattern matching. But then I realized, *far* too slowly, that I could Church-encode positive datatypes very nicely with lambdas and objects! Cedille wasn't an original inspiration for Cricket at all, but it came to be a huge influence over time. I knew about their efficient encodings, and knew that my self-typed objects could do a lot of the same stuff as their dependent intersections. (Obviously Cricket is dynamically typed, but I still find that thinking about type systems positively impacts my language design choices.)

If you haven't already figured it out, here's how Cricket does inductive datatypes:
```
def Either: {
  Left(x): {
  	case(c): c.Left(x)
  },
  Right(x): {
  	case(c): c.Right(x)
  }
}

...

Either.Left("Ryan").case{
  Left(name): "Hello, " + name,
  Right(name): "Goodbye, " + name
}
```

Even though this is almost entirely objects, this is actually a Church encoding! Here's the corresponding lambdas:
```
def Left: x-> f-> g-> f(x)
def Right: x-> f-> g-> g(x)

Left("Ryan")(name-> "Hello, " + name)(name-> "Goodbye, " + name)
```
So the objects are almost serving a pretty-syntax role, and organizing lambdas better. The lambda-encoded version takes two functions via currying, and the object-encoded version takes an object with two methods. 

There's one other concrete benefit of using objects, which is that an object can be called in multiple different ways! If you want to pattern match, by passing two functions, you call it via the `.case` method. But we could give our `Left` and `Right` constructors other methods too:
```
def Either: {
  Left(x): {
  	case(c): c.Left(x),
  	map_left(f): Left(f(x)),
  	map_right(f): Left(x)
  },
  Right(x): {
  	case(c): c.Right(x),
  	map_left(f): Rigth(x),
  	map_right(f): Right(f(x))
  }
}

...

// this returns `Left("Ryan!")`
Either.Left("Ryan").map_right(x-> x+"?").map_left(x-> x+"!")
```

Objects also make up the Cricket module system. Whenever there's an undefined identifier (that is, one that isn't introduced with `let`) it's translated to a method lookup on the current module object. This is like `window` in JavaScript, but one per module, used for namespacing. So in the above, `Either.Left("Ryan")` is actually in some function `def foo ...` which is actually some module method `$mod.foo ...` in which case the `Either` is translated to `$mod.Either`. One thing that's nice about this is that recursion pops out for free: `{this.rec_foo: this.rec_foo, this.foo: this.rec_foo}.foo` loops forever, and modules with recursive functions are essentially this. All of this comes from the IOC's self-typed objects: object methods are given a parameter (like `this`) that lets them talk about the object itself, and thus the current method definition.

A cute side effect of this is anonymous folds:
```
List[1, 2, 3].case{
  this.Has(first)(rest):
  	let force _ = console.write(first)
  	in rest.case(this),
  Empty: 0
}
```
Here we print each value in the list; the magic is in `rest.case(this)`. What is `this`? The `Has` parameter was introduced as `this.Has`, so `this` is a value in scope referring to the object of which `Has` is a method. That is, the argument of `List[1, 2, 3].case`! So `this` is referring to the current "`case` statement", and `rest.case(this)` means "apply this same `case` statement to the rest." No `foldr` or recursive function required!