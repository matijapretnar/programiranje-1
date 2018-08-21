from functools import lru_cache

test = [
    [0, 0, 0, 1, 0],
    [1, 0, 1, 0, 0],
    [0, 0, 0, 1, 0],
    [1, 1, 0, 0, 0]]

def possible_moves(x, max_x):
    if x == 0 :
        return [1]
    elif x == max_x:
        return [x - 1]
    else:
        return [x - 1, x + 1]

def available_moves(r, c, maze):
    max_r = len(maze) - 1
    max_c = len(maze[0]) - 1
    moves = [(x, c) for x in possible_moves(r, max_r)] + [(r, y) for y in possible_moves(c, max_c)]
    return [(x, y) for (x, y) in moves if maze[x][y] == 0]

def drop_none(lst):
    return [x for x in lst if x is not None]


def run_the_maze(start, end, n, maze):
    @lru_cache(maxsize=None)
    def runner(r, c, n):
        if (r, c) == end:
            return n
        if n == 0:
            return None
        else:
            moves = available_moves(r, c, maze)
            runs = drop_none([runner(rr, cc, n-1) for (rr, cc) in moves])
            if len(runs) == 0:
                return None
            else:
                return max(runs)
    (r, c) = start
    return runner(r, c, n)

def t():
    print(run_the_maze((0,0),(3,4),9,test))
    print(run_the_maze((0,0),(3,4),5,test))
    print(run_the_maze((0,0),(3,0),9,test))
    print(run_the_maze((0,0),(1,3),20,test))
    return
