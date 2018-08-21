from functools import lru_cache

test = [
    [2, 3, 0, 5, 0],
    [7, 0, 6, 9, 1],
    [0, 2, 0, 0, 0],
    [2, 0, 0, 4, 0]]

def available_moves(r, c, maze):
    moves = []
    if 0 <= r+1 < len(maze):
        moves.append((r+1, c))
    if 0 <= r-1 < len(maze):
        moves.append((r-1, c))
    if 0 <= c+1 < len(maze[0]):
        moves.append((r, c+1))
    if 0 <= c-1 < len(maze[0]):
        moves.append((r, c-1))
    return moves


def drop_none(lst):
    return [x for x in lst if x is not None]


def run_the_maze(start, end, n, maze):
    @lru_cache(maxsize=None)
    def runner(r, c, n):
        if (r, c) == end and n == 0:
            return maze[r][c]
        elif n == 0:
            return None
        else:
            moves = available_moves(r, c, maze)
            runs = drop_none([runner(rr, cc, n-1) for (rr, cc) in moves])
            if len(runs) == 0:
                return None
            else:
                return maze[r][c] + max(runs)
    (r, c) = start
    return runner(r, c, n)

def t():
    print(run_the_maze((0,0),(3,4),9,test))
    print(run_the_maze((0,0),(3,4),10,test))
    print(run_the_maze((0,0),(3,4),5,test))
    print(run_the_maze((0,0),(3,0),9,test))
    print(run_the_maze((0,0),(1,3),200,test))
    return
