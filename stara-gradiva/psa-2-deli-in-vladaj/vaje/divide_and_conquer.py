# We will use "hypothesis" for property-based testing à la quickcheck:
# http://hypothesis.readthedocs.io/en/master/quickstart.html
from hypothesis import given
from hypothesis.strategies import text,lists,integers

# Task:
# - fill in the holes marked TODO in the Quicksort I section
# - test your solution using the tests provided in the Testing section
# - repeat for Quicksort II and for Quickselect
# - compare the performance of your implementations using the functions from
#   the Benchmarking section


# Replace all uses of this guy!
def TODO():
    assert (False)

############################################################################
############################### Quicksort - I ##############################

# partition(xs) picks a pivot in xs and returns a triple of elements of xs
# smaller than the pivot, the pivot, and elements (strictly) larger than the
# pivot.
def partition(xs):
    pivot = xs[0]
    le, gt = [], []
    for x in xs[1:]:
        if x <= pivot:
            le.append(x)
        else:
            gt.append(x)
    return (le, pivot, gt)

# quicksort works by picking *a pivot* and partitioning the input into a batch
# of *smaller elements* and a batch of *larger elements*. Once these three data
# are available, the sub-partitions can be sorted recursively, then combined to
# yield the sorted list.
def quicksort(xs):
    if xs == []:
        return xs
    else:
        le, pivot, gt = partition(xs)
        return (quicksort(le) + [pivot] + quicksort(gt))


############################### Testing #################################

# We define a type generic test function for quicksort by comparing it to
# Python's built-in sort method. If everything is fine, it will be silent,
# otherwise it points out what input provoked the error.
def test_quicksort(lst):
    q = quicksort(lst)
    lst.sort()
    assert (q == lst)

# Here we define two instances of this test, one with lists of strings and one
# with lists of integers. Try these!
@given(lists(text()))
def test_quicksort_text(lst):
    test_quicksort(lst)

@given(lists(integers()))
def test_quicksort_int(lst):
    test_quicksort(lst)


###########################################################################
############################### Quicksort - II ############################

# Our first implementation of partition has to allocate two auxiliary lists
# that the original list gets partitioned into, resulting in a space use
# linearly dependent on the size of the input. We say that partition has
# "linear space complexity", or that it "uses O(n) space".

# Partition can be implemented to run using constant memory space by moving
# around the elements of the input list *without allocating auxiliary list*
# instead.
#
# hackers like this kind of thing; they also save space by using shrt fnctn
# names.

def prtn(xs, lo, hi):

    # pick a pivot
    pvt = xs[hi]
    # keep track of the position of the last element less or equal to the pivot
    le = lo - 1

    # walk through the list
    for i in range(lo, hi):
        # if an element is smaller than the pivot
        if xs[i] <= pvt:
            # increment the ≤ counter and exchange the current element with the
            # element at the new counter position
            le += 1
            xs[le], xs[i] = xs[i], xs[le] # swap

    # of course also the pivot is less-or-equal to itself, so we swap it in its
    # place
    le += 1
    xs[le], xs[hi] = xs[hi], xs[le]

    # report the position of the pivot
    return le


# Sort the list xs between the bounds lo and hi
def qsrt_bounded(xs, lo, hi):
    if lo >= hi:
        return
    pvt = prtn(xs, lo, hi)
    qsrt_bounded(xs, lo, pvt-1)
    qsrt_bounded(xs, pvt+1, hi)

# Finally, sort a list by sorting it from beginning to end.
def qsrt(xs):
    qsrt_bounded(xs, 0, len(xs) - 1)



############################### Testing #################################

# tests like for "quicksort"
def test_qsrt(lst):
    q = lst.copy()
    lst.sort()
    qsrt(q)
    assert (q == lst)

@given(lists(text()))
def test_qsrt_text(lst):
    test_qsrt(lst)

@given(lists(integers()))
def test_qsrt_int(lst):
    test_qsrt(lst)



# a function to run all the tests
def run_tests():
    test_quicksort_text()
    test_quicksort_int()
    test_qsrt_text()
    test_qsrt_int()
    test_quickselect_text()
    test_quickselect_int()

##########################################################################
############################### Quickselect ##############################

# The goal of selection is to find the (k-1)th smallest element in a list.

# All we have to do is make good use of the partitioning procedure we devised
# for quicksort.
def quickselect_bounded(xs, k, lo, hi):
    p = prtn(xs, lo, hi)
    if k < p :
        return quickselect_bounded(xs, k, lo, p - 1)
    elif k > p:
        return quickselect_bounded(xs, k, p + 1, hi)
    else:
        return xs[k]

def quickselect(xs, k):
    return quickselect_bounded(xs, k, 0, len(xs) - 1)



############################### Testing #################################

def test_quickselect(xs, k):
    if xs != []:
        k = k % len(xs)
        v = quickselect(xs, k)
        xs.sort()
        assert (v == xs[k])

@given(lists(text()), integers())
def test_quickselect_text(lst, k):
    test_quickselect(lst, k)

@given(lists(integers()), integers())
def test_quickselect_int(lst, k):
    test_quickselect(lst, k)



###############################################################################
############################### Benchmarking ##################################
###############################################################################


# We will compare the execution time of quicksort, qsrt and Python's .sort() by
# running them on random lists and arrays of different sizes.

# To benchmark your implementations, the interesting functions are
# time_qsrt_list, time_quicksort_list, time_builtin_sort_list and run_timers.


from random import randint
import array
import timeit


# Generate n random lists
def gen_lists(n, len, max=2**10):
    return [ [ randint(0,max) for i in range(len) ] for j in range(n) ]

# Same for arrays
def gen_arrays(n, len, max=2**10):
    return [ array.array('i', l) for l in gen_lists(n, len, max) ]

# Generate pairs of random lists together with an index to select
def gen_lists_ks(n, len, max=2**10):
    ls = gen_lists(n, len, max)
    ls_ks = [ (l, randint(0, len - 1)) for l in ls ]
    return ls_ks



# How long does it take the function "sort" to process n_lists of length len,
# as generated by "generator" ?
def timer(sort, n_lists, len, generator):
    return timeit.timeit('[ {}(l) for l in lists ]'.format(sort),
                         setup = 'lists = {}({}, {})'.format(generator, n_lists, str(len)),
                         globals=globals(),
                         # the sorting is effectful code, run only once per generated data!
                         number = 1)

# Use "generator" to generate n_lists random lists/arrays of lengths between 2**minlen
# and 2**maxlen and process them using the function "algo"
def time_algo(algo, generator, n_lists = 1000, minlen = 0, maxlen = 9):
    res = "Algorithm: {}, Random generator: {}\n".format(algo, generator)
    for len in range(minlen, maxlen + 1):
        length = 2**len
        t = timer(algo, n_lists, length, generator)
        res = "{}length: {:4}\ttime: {:.7} s\n".format(res, length, t)
    print(res)

def time_algo_raw(algo, generator, n_lists = 1000, minlen = 0, maxlen = 9):
    label = "Algorithm: {}, Random generator: {}\n".format(algo, generator)
    times = []
    for len in range(minlen, maxlen + 1):
        length = 2**len
        t = timer(algo, n_lists, length, generator)
        times.append((length, t))
    return (label, times)


# The built-in sorting algorithm wrapped as a function for use in time_algo.
# May or may not sort the input, run for side-effects only.
def builtin_sort(xs):
    list(xs).sort()


# for comparison
def insertionsort(xs):
   for i in range(1, len(xs)) :
       v = xs[i]
       j = i
       while j > 0 and xs[j-1] > v :
           xs[j] = xs[j-1]
           j -= 1
       xs[j]= v

# tests like for "qsrt"
def test_insertionsort(lst):
    q = lst.copy()
    lst.sort()
    insertionsort(q)
    assert (q == lst)

@given(lists(text()))
def test_insertionsort_text(lst):
    test_insertionsort(lst)

@given(lists(integers()))
def test_insertionsort_int(lst):
    test_insertionsort(lst)


def quickselect_k(xs_k):
    return quickselect(xs_k[0], xs_k[1])


def time_qsrt_list():
    time_algo("qsrt", "gen_lists")
def time_quicksort_list():
    time_algo("quicksort", "gen_lists")
def time_builtin_sort_list():
    time_algo("builtin_sort", "gen_lists")

def time_quickselect_list():
    time_algo("quickselect_k", "gen_lists_ks")

def time_qsrt_array():
    time_algo("qsrt", "gen_arrays")
def time_quicksort_array():
    time_algo("quicksort", "gen_arrays")
def time_builtin_sort_array():
    time_algo("builtin_sort", "gen_arrays")

def run_timers():
    time_qsrt_list()
    time_quicksort_list()
    time_builtin_sort_list()
    # time_qsrt_array()
    # time_quicksort_array()
    # time_builtin_sort_array()


# If called as a program, run the tests, then the timers.
if __name__ == '__main__' :
    run_tests()
    run_timers()
