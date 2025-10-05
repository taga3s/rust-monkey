let a = "hello";
let b = "world!";
let concat = fn(x, y) { return x + y; };
log(concat(a, concat(" ", b)));
