<!DOCTYPE html>
<html>
<head>
<title>Spremenljive podatkovne strukture</title>
<meta charset="utf-8">
<link rel="stylesheet" href="../../pomozno/prosojnice.css" />
</head>
<body>
<textarea id="source">

class: center, middle

# Spremenljive podatkovne strukture

## Programiranje 1

---

### Seznami v Pythonu so **spremenljivi**

.source[```python
seznam = [1, 2, 3]
def f(x):
    seznam.append(x)
    return len(seznam)
def g(x):
    return f(x) + f(x)
```]

.terminal[```python
>>> g(3)
9
>>> f(3) + f(3)
13
>>> g(3)
17
```]

---

### Seznami v OCamlu so **nespremenljivi**

.source[```
let seznam = [1; 2; 3]
let f x =
    let seznam = x :: seznam in
    List.length seznam
let g x =
    f x + f x
```]

.terminal[```
# g 3;;
- : int = 8
# f 3 + f 3;;
- : int = 8
# g 3;;
- : int = 8
```]

---

### Reference v OCamlu so **spremenljive**

.source[```
let sklic_na_seznam = ref [1; 2; 3]
let f x =
    sklic_na_seznam := x :: !sklic_na_seznam;
    List.length !sklic_na_seznam
let g x =
    f x + f x
```]

.terminal[```
# g 3;;
- : int = 9
# f 3 + f 3;;
- : int = 13
# g 3;;
- : int = 17
```]

---

class: question, center

### Kakšno vrednost vrne program

.left.source[```
let a = 1 in
let b = ref a in
let c = ref b in
let a = 5 in
let d = ref !b in
let e = ref a in
b := 4;
let b = ref !b in
!c := 3;
d := 2;
(a, b, !c, !d, e)
```]

### .spoiler[`(` .spoiler[`5`], .spoiler[`{cnts` `=` .spoiler[`4`]`}`], .spoiler[`{cnts` `=` .spoiler[`3`]`}`], .spoiler[`2`], .spoiler[`{cnts` `=` .spoiler[`5`]`}`]`)`]

</textarea>
<script src="../../pomozno/prosojnice.js"></script>
</body>
</html>
