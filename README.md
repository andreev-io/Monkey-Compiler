# Monkey Compiler in Rust

This project is a single-pass custom bytecode compiler and a stack-based virtual machine that runs code written in Monkey. Under the hood, the parser uses Pratt parsing. 

The language supports first-class functions (with arguments), anonymous functions, closures, return statements, if clauses, strings, integers, booleans, and arrays. 

Run `cargo run --release` to bring up a REPL.

The following are examples of some amazing Monkey programs:
```
>>> let fibonacci = fn(x) {
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

fibonacci(35);
```

```
>>> let wrapper = fn() {
        let countDown = fn(x) {
            if (x == 0) {
                return 0;
            } else {
                countDown(x - 1);
            } 
        };
        countDown(1);
};
wrapper();
```

Arrays and indexing:
```
>>> let x = [1, 2, fn(x) { x }(5)]; 
>>> x[2]

5
```

Nulls and truthy values:
```
>>> !!fn() {}();
false

>>> !!"hi"
true
```


Strings:
```
>>> let x = "good";
>>> let y = "bye";
>>> let goodbye = fn(x, y) { if (x+y == "goodbye") { 1 } else { 0 }};
>>> goodbye(x, y)

1
```

As shown above, the language supports anonymous functions and function literals.
The language doesn't have garbage collection and relies on Rust's ownership
semantics for memory management.

As far as the virtual machine goes, it correctly cleans up the call frame whenever a closure is exited.

```
>>> let v = [1, 2, 3]; 

let superfunc = fn(a) { 
    let b = a; 
    b[0] + b[1] 
}; 

3 == superfunc(v)
```

When `superfunc` is called, `b` is created by cloning the array `a`, but is deallocated when superfunc returns.


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

Note that we could've allocated anything within the recursively called
function, and the values would be successfully deallocated on return.
