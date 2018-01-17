#  ==========================================================================
#  NALOGA 4.1
#
#
#  ==========================================================================


from functools import lru_cache

test_matrix = [[(x + y)%42 for x in range(100)] for y in range(200)]

def max_points(matrix, max_steps):

    @lru_cache(maxsize=None)
    def jumper(r, c, k):
        val = matrix[r][c]
        #No more steps
        if (k == 0):
            return matrix[r][c]
        #Hit boundaries
        elif (r == len(matrix) - 1):
            #Can't go down
            if (c == len(matrix[r]) - 1):
                #Can't go right
                return val
            else:
                #Can go right
                return val + jumper(r, c+1, k-1)
        else:
            #Can go down
            if (c == len(matrix[r]) - 1):
                #Can't go right
                return val + jumper(r+1, 0, k-1)
            else:
                #Can go right
                return val + max(jumper(r, c+1, k-1), jumper(r+1, 0, k-1))

    #Call function
    return jumper(0,0,max_steps)
