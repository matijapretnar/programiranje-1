from functools import lru_cache

# Goal: compute the values of the Fibonacci sequence at 100, 500, 1000, 10**5, and 10**6
# For each of the definitions, try how big a value you can compute. Why does it
# fail there?

# First define fib as a recursive function.
# Limitation: Too slow
def fib(n):
    if n <= 1:
        return n
    else:
        x = fib(n-1)
        y = fib(n-2)
        return x + y

# Define fib as a recursive function, but using the lru_cache decorater to
# memoize results.
# Limitation: Exceeds max. recursion depth at ~350
@lru_cache()
def fib_cache(n):
    if n <= 1:
        return n
    else:
        x = fib_cache(n-1)
        y = fib_cache(n-2)
        return x + y

# Draw the call tree for n = 5 and identify which subproblems are repeated.

# Define fib recursively and manually memoize results.
# Limitation: Exceeds max. recursion depth at ~1000
def fib_memo_rec(n):
    fib_0_n = [None] * (max (2, n+1))
    def aux(n):
        if n < 2:
            return n
        else:
            if fib_0_n[n] == None:
                fib_0_n[n] = aux(n-1) + aux(n-2)
            return fib_0_n[n]
    return aux(n)

# Make a new drawing where you merge the repeated nodes in the tree. Which
# subproblems does each call depend on directly?

# Define fib as a dynamic program that fills up the table of results from the bottom.
# Limitation: Runs out of memory at > 10**5
def fib_memo_iter(n):
    fib_0_n = [0] * (max (2, n+1))
    fib_0_n[1] = 1
    for i in range(2, n+1):
        fib_0_n[i] = fib_0_n[i-1] + fib_0_n[i-2]
    return
    return fib_0_n[n]

# Define fib as a dynamic program that only keeps those intermediate results
# around that are needed to compute the next step.
# Limitation: Works for > 10**6
def fib_iter(n):
    if n < 2:
        return n
    else:
        x_2 = 0
        x_1 = 1
        for i in range(2,n+1):
            x = x_1 + x_2
            x_2 = x_1
            x_1 = x
        return x
