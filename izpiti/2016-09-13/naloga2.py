#!/usr/bin/python3 

# Vsak pisar dobi vsaj eno knjigo.

def pisarji(k, p):
    n = len(p)
    dp = [[0 for j in range(k + 1)] for i in range(n + 1)]
    for i in range(1, n + 1):
        dp[i][0] = float('inf')
    for j in range(1, k + 1):
        for i in range(1, n + 1):
            # Prvih i knjig razdelimo med prvih j pisarjev.
            dp[i][j] = min([max(sum(p[k-1:i]), dp[k-1][j-1]) for k in range(1, i + 1)])
    # print(dp)
    return dp[n][k]

print(pisarji(2, [10, 20, 50, 130, 120, 70, 20]))
print(pisarji(3, [10, 20, 50, 130, 120, 70, 20]))
print(pisarji(4, [10, 20, 50, 130, 120, 70, 20]))
