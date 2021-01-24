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
>>> let add = fn(x, y) { x + y; }; add(5, 5);

10
```

```
>>> fn(x) { x; }(5)

5
```

As shown above, the language supports anonymous functions and function literals. However, there's a catch.
Since our implementation is a naive one written in Rust, it inherits some of Rust's peculiarities. As such, every identifier "owns" the underlying value. That means that once an identifier is used, its underlying value is now owned by whatever consumed it. Further, since functions are themselves objects in our system, any function can only **be run once**. On the plus side, garbage collection is unnecessary. On the other hand, we built a language whose memory model conforms to that of Rust, but doesn't have all the features of Rust. In other words, where Rust has ownership, borrowing, and lifetimes, our language only has ownership.

As such, the following are examples of **invalid** code:
```
>>> let x = 5;
>>> let adder = fn(y, z) { y + z };
>>> adder(x, x <-- invalid)
```

```
>>> let x = 5;
>>> let y = x;
>>> let z = x; <-- invalid
```

```
>>> let identity = fn(x) { return x; };
>>> identity(5)
>>> identity(5) <-- invalid
```

All this could of course be fixed with the help of more idiomatic Rust. But
instead of rewriting the interpreter, I'm going to move on to writing a compiler
in Rust and avoid repeating my own mistakes.

The code contains extensive tests.