# Cricket

Right now, Cricket is a toy untyped lazy functional language. 
At time of writing, the entire interpreter is 200 lines of Haskell! 
It's basically just parsing, pretty printing, and an extended Krivine machine.

The plan is to soon add elements of Abadi and Cardelli's [Imperative Object Calculus](http://lucacardelli.name/Papers/PrimObjImp.pdf). 
I want to explore an impure lazy functional language that defines things coinductively instead of inductively.
Objects are a natural starting point for this, especially the IOC.
The IOC has no fields, only getters and setters, and no types, only mutable methods.
It's kind of prototypical in that sense.
This gives a very small language implementation with potentially decent ergonomics, which I will be tweaking a lot to get right.

With an object system in place, and replacing the I/O interface to use object methods, 
the next and final stage is to port the little language to C.
Or maybe Zig, if I'm up for learning it.
A huge drive for this project is how small it is, implementation-wise, and I want to prioritize that.
Using a Krivine machine means the laziness isn't coming from Haskell's laziness, and I can pull off a decent C port.
All I need to figure out then is memory reclamation, which will probably be reference counting.

![image](https://github.com/user-attachments/assets/2b21894d-eee1-43b7-b520-8dca372c23c1)
