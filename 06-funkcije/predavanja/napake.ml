let f1 x = x + 10
let f2 x = f1 x
let f3 x = f2 x
let f4 x = f3 x

let g x = f4 "bla" + x
