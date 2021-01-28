# Monkey Interpreter in Rust

This project is an implementation of a simple interpreter (tree-walking interpreter with a Pratt parser) in Rust. It can run naive programs written in the Monkey programming language. Run `cargo run --release` to bring up a REPL.

The following are examples of valid Monkey programs:
```
>>> (5 + 10 * 2 + 15 / 3) * 2 + -10

50
```

Conditionals:
```
>>> if (1 > 2) { 10 } else { 20 }

20
```

Arrays and indexing:
```
>>> let x = [1, 2, fn(x) { x }(5)]; 
>>> x[2]

5
```

Strings:
```
>>> let x = "good";
>>> let y = "bye";
>>> let goodbye = fn(x, y) { if (x+y == "goodbye") { 1 } else { 0 }};
>>> goodbye(x, y)

1
```

```
>>> if (10 > 1) {
>>> if (10 > 1) {
>>> return 10;
>>> }
>>> return 1; 
>>> }

10
```

```
>>> let a = 5; let b = 15; let c = a + b; c;

20
```

Functions:
```
>>> let add = fn(x, y) { x + y; }; add(5, add(5, 5));

15
```

```
>>> fn(x) { x; }(5)

5
```

As shown above, the language supports anonymous functions and function literals.
The language doesn't have garbage collection and relies on Rust's ownership
semantics for memory management. When writing code in Monkey, the semantics are
as if everything was allocated on the stack (this is almost necessarily not
true, but is a useful abstraction).

This implementation relies heavily on cloning Rust values. Consider the following example:
```
>>> let v = [1, 2, 3]; 

let superfunc = fn(a) { 
    let b = a; 
    b[0] + b[1] 
}; 

3 == superfunc(v)
```

When `superfunc` is called, `b` is created by cloning the array `a`, but is deallocated when superfunc returns.

Monkey has closures. Thanks to closures, recursion is possible:
```
let factorial = fn(x) {
    if (x == 0) {
        1
    } else {
        x * factorial(x-1)
    }
}

factorial(5)
```

```
let fibonacci = fn(x) { 
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            return 1; 
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    } 
};
            
fibonacci(7)
```

Note that we could've allocated anything within the recursively called
function, and the values would be successfully deallocated on return.