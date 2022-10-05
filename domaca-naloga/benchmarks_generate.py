import csv
import re
import os
import subprocess


def measure_baseline():
    input = f"""
        let n = 1_000_000 in
        let list = List.init n (fun i -> n - i) in
        let before = Sys.time () in
        let _ = List.sort Stdlib.compare list in
        let after = Sys.time () in
        print_float (after -. before);;
    """
    print("Ocenjujem hitrost računalnika... ", end="", flush=True)
    proc = subprocess.run(
        ["ocaml", "-stdin"], input=input.encode("utf-8"), capture_output=True
    )
    print("končano.")
    return float(proc.stdout)


def run(sudoku, timeout):
    with open(sudoku.replace(".sdk", ".out")) as f:
        expected_solution = f.read().strip()
    proc = subprocess.run(
        ["./sudoku.exe", sudoku], check=True, capture_output=True, timeout=timeout
    )
    output = proc.stdout.decode("utf-8")
    match = re.search(
        "Končna rešitev:(?P<solution>.*?)Čas reševanja: (?P<time>.*?) s\.",
        output,
        flags=re.DOTALL,
    )
    result = (
        "CORRECT" if match.group("solution").strip() == expected_solution else "WRONG"
    )
    time = float(match.group("time"))
    return result, time


def add_benchmarks(name, sudokus, csv_name, timeout=60, baseline=None):
    if baseline is None:
        baseline = measure_baseline()
    with open(csv_name, "a") as f:
        writer = csv.writer(f)
        for sudoku in sudokus:
            sudoku_shortname, _ = os.path.splitext(os.path.basename(sudoku))
            print(f"{name} {sudoku_shortname}... ", end="", flush=True)
            try:
                result, time = run(sudoku, timeout)
                time /= baseline
            except subprocess.CalledProcessError:
                result = "ERROR"
                time = None
            except subprocess.TimeoutExpired:
                result = "TIMEOUT"
                time = None
            print(result)
            writer.writerow([name, sudoku_shortname, result, time])


sudokus = [
    os.path.join("sudokuji", filename)
    for filename in sorted(os.listdir("sudokuji"))
    if filename.endswith(".sdk")
]

# add_benchmarks("ImePriimek", sudokus, "benchmarks.csv")
