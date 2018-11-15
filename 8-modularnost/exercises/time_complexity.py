"""
Experimental exercise: Time complexity tests.
"""

import time                         # Timer
import matplotlib.pyplot as plt     # Graphing


# Function that measures the time needed for algorith on given input data.
def measure(algo, data):
    """
    Measures the time needed for the execution of [algo] on [data].
    """
    before = time.time()
    algo(data)
    after = time.time()
    return (after - before)

# Lists of different sizes used as test inputs.
small = list(range(10**2, 10**3+1, 2*10))
medium = list(range(10**3, 10**4+1, 2*10**2))
large = list(range(10**4, 10**5+1, 2*10**3))
xlarge = list(range(10**5, 10**6+1, 2*10**4))

###############################################################################
# Functions that create a list or dictionary, that includes all elements from
# 1 to n. The function [add_back] adds elements to the back of list,
# [add_front] to the front. We add elements to dictionary as per usual.
###############################################################################


def add_back(n):
    l = []
    for i in range(n):
        l.append(n)
    return


def add_front(n):
    l = []
    for i in range(n):
        l = [n] + l
    return


def add_dict(n):
    d = dict()
    for i in range(n):
        d[i] = i
    return

###############################################################################
# 1.) [test_add_back(test_sizes)] graphs the necessary time to build a list
# by adding elements to the back of the list.
#
# If the time complexity of the whole algorithm is O(n) what is the time
# complexity of a single operation.
#
# Suggested input: large in xlarge
#
###############################################################################


def test_add_back(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    plt.plot(test_sizes, times1, 'r')
    plt.show()

###############################################################################
# 2.) [test_add_compare(test_sizes)] compares the time complexity of adding
# elements to the back and front of the list.
# Remember that the time complexity of adding to the back is not constant as it
# might appear in this test.
#
# Think about the time complexity of a single add operation.
#
# Suggested input: medium
#
###############################################################################


def test_add_compare(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    times2 = []
    for size in test_sizes:
        times2.append(measure(add_front, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()


###############################################################################
# 3.) [test_add_list_vs_dict(test_sizes)] compares the efficiency of building
# a list and a dictionary.
#
# Dictionaries use a so called 'hash' function, that determines where to insert
# the new element. If the hash function is good (it doesnt map multiple
# elements to the same field) it happens fast. If there are too many bad
# mappings (due to new elements) the hash function is changed and the array
# size increased in order to reduce bad mappings.
#
# Suggested input: large
#
###############################################################################


def test_add_list_vs_dict(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    times2 = []
    for size in test_sizes:
        times2.append(measure(add_dict, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()


###############################################################################
# 4.) [test_find_list_vs_dict(test_sizes)] compares the effectivnes of
# searching for an element in the list and dictionary. In the test we save
# number from 0 to n-1 into the data structures and then search for the number
# n, which is the worst case search for the list. We do 1000 searches (can be
# adjusted).
#
# Because the dictionary uses the hash function to determine where to store the
# element, it can be used to quickly check for memebership.
#
# Suggested input: medium
#
# 5.) Change the function to graph only the values for the dictionary.
# What is the time complexity of searching in dictionary.
#
# 6.) Change the function so that you always search for the number 0.
# How does that affect the time complexity. Why is this the best case for
# lists?
#
# 7.) What happens if instead of searching for the number n in the list we
# search for the number at the index n. Does that match the expected complexity
# for searching in lists in OCaml?
#
###############################################################################


def test_find_list_vs_dict(test_sizes, times=1000):
    times1 = []
    times2 = []
    # We do not want to create a list inside the algorithm that we time,
    # so we use a list (and dict) defined outside of the function.
    temp_l = []
    temp_d = dict()

    def find_dict(n):
        for i in range(times):
            # Finds the element (n-1)
            temp_d.get(n-1, 0)
        return

    def find_list(n):
        for i in range(times):
            # Searches for the index of (n-1) which is equivalent
            # to searching if an element is in a list
            temp_l.index(n-1)
        return

    for size in test_sizes:
        # Create a list and dict before timing the search algorithm
        temp_l = [x for x in range(size)]
        temp_d = dict((x, x) for x in range(size))
        times1.append(measure(find_list, size))
        times2.append(measure(find_dict, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()
