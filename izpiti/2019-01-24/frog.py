###############################################################################
# A frog is stuck in a swamp and wants to hop out of it as fast as possible.
# Luckily for the frog, the swamp is full of flies for it to replenish its
# energy, as the frog cannot jump far if it's tired.
#
# We represent the swamp with an array [swamp], where the [k]-th field denotes
# how many flies there are. The frog starts on the field 0, and starts with as
# much energy as it gets by eating the flies on that field. If the frog has [e]
# energy, it can jump any distance shorter than [e] and spends as much energy as
# is the distance of the jump, i.e. if it has 5 energy, it can jump forward 1,
# 2, 3, 4, or 5 fields and if it jumps forward 3 fields, it loses 3 energy. When
# it lands on a field, it eats all the flies in it and regains that much energy.
#
# Calculate the least amount of hops the frog needs to escape the swamp (so that
# it would land in a field outside of the swamp). You may suppose that every
# field has at least one fly on it, so the frog can always jump.
###############################################################################
from functools import lru_cache


def frog_escape(swamp):

    @lru_cache(maxsize=None)
    def escape(k, e):
        if k >= len(swamp):
            return 0
        else:
            e += swamp[k]
            return 1 + min([escape(k + d, e - d) for d in range(1, e + 1)])
    return escape(0, 0)

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5] # Should be 3
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1] # Should be 2

# We can make them return a path as well??
