from functools import lru_cache

test = [
    [2, 4, 1, 1],
    [3, 2, 0, 5],
    [8, 0, 7, 2]]

def mozni_premiki(r, c, maze):
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


def ovrednoti(start, end, n, maze):
    @lru_cache(maxsize=None)
    def runner(r, c, n):
        if (r, c) == end and n == 0:
            return maze[r][c]
        elif n == 0:
            return None
        else:
            moves = mozni_premiki(r, c, maze)
            runs = drop_none([runner(rr, cc, n-1) for (rr, cc) in moves])
            if len(runs) == 0:
                return None
            else:
                return maze[r][c] + max(runs)
    (r, c) = start
    return runner(r, c, n)

def t():
    print(ovrednoti((0,0),(2,2),10,test))
    print(ovrednoti((0,0),(2,2),11,test))
