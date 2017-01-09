import matplotlib.pyplot as plt  # Risanje grafov
plt.style.use('ggplot') # Lepi grafi

from divide_and_conquer import *

def time_qsrt_raw():
    return time_algo_raw("qsrt", "gen_lists")
def time_quicksort_raw():
    return time_algo_raw("quicksort", "gen_lists")
def time_builtin_sort_raw():
    return time_algo_raw("builtin_sort", "gen_lists")
def time_insertionsort_raw():
    return time_algo_raw("insertionsort", "gen_lists")

def time_quickselect_raw():
    return time_algo_raw("quickselect_k", "gen_lists_ks")


def plot_times():
    for f in [time_builtin_sort_raw, time_insertionsort_raw,
              time_qsrt_raw, time_quicksort_raw, time_quickselect_raw]:
        label, lengths_times = f()
        lengths, times = [], []
        for l_t in lengths_times:
            lengths.append(l_t[0])
            times.append(l_t[1])
        plt.plot(lengths, times, label=label)
    plt.legend()
    plt.show()
