#!/usr/bin/env python

from glob import glob
from os import listdir, path
from os.path import exists, isfile, splitext
from subprocess import call, check_output
from sys import exit, exc_info, argv
from time import strftime

array_size = 256
fact_input = 1000
matrix_dim = 10
intl_input = 2 ** 15

# Inputs from OOPSLA '15 submission
if len(argv) > 1:
    array_size = 1024
    fact_input = 5000
    matrix_dim = 20
    intl_input = 2 ** 30

tests = ['array1','array2','array3', 'fact1', 'fact2', 'fact3', 'fact4', 'swap', 'matrix1', 'matrix2', 'matrix3', 'array4']
artlibs = ['structural', 'nominal', 'fromscratch']
evals = ['standard']
verbose = True
prefills = [False]

stores = ['trie'] # list is too slow
min_depths = [4]

#reps = 7
reps = 3


results = 'results'

# backup prior results, if any
try:
    def bow_out():
        print "Couldn't backup results for some reason; refusing to clobber paste results."
        exit(1)
    backupcsv = glob(results+'/*.csv')
    backuptxt = glob(results+'/*.txt')
    if len(backupcsv) + len(backuptxt) > 0:
        timestamp = strftime("%Y-%m-%d-%H-%M-%S")
        out = "results/results-"+timestamp+".tgz"
        cmd = ["tar", "-c", "-z", "-f", out]
        cmd.extend(backupcsv)
        cmd.extend(backuptxt)
        if call(cmd) != 0:
            bow_out()
        else:
            print "Backed up old results to "+out
            cmd = ["rm", "-f"]
            cmd.extend(backupcsv)
            cmd.extend(backuptxt)
            if call(cmd) != 0: print "Failed to remove old csv and summary files, you may have stale data."
except:
    print "Unexpected error:", sys.exc_info()[0]
    bow_out()

def run_test(test, artlib, evalu, prefill, store, min_depth):
    cmd = ["./tests.native",
           "--fact", str(fact_input),
           "--array-size", str(array_size),
           "--matrix-dim", str(matrix_dim),
           "--int-log", str(intl_input),
           "--test", test,
           "--artlib", artlib,
           "--eval", evalu,
           "--store", store,
           "--min-depth", str(min_depth),
           "--record"]
    if prefill: cmd.append("--prefill")
    #print " ".join(cmd)
    call(cmd)

# Runs every configuration reps number of times.
for n in range(0, reps):
    for test in tests:
        for artlib in artlibs:
            for evalu in evals:
                for prefill in prefills:
                    for store in stores:
                        if store == "trie":
                            for min_depth in min_depths:
                                run_test(test, artlib, evalu, prefill, store, min_depth)
                        else:
                            run_test(test, artlib, evalu, prefill, store, -1)

prettynames = {'results/arr-ext.csv':'Array Max Extend',
               'results/matrix-mult-ext.csv':'Matrix Mult Extend',
               'results/matrix-mult-swap-while-order.csv':'Matrix Mult Swap 2',
               'results/matrix-mult-swap-assign.csv':'Matrix Mult Swap 1',
               'results/swap-subprograms.csv':'Intlog ; Factorial Swap',
               'results/fact-begin-swap-assign.csv':'Factorial Swap 1',
               'results/fact-end-swap-assign.csv':'Factorial Swap 2',
               'results/fact-mutate-iter-init.csv':'Factorial Extend',
               'results/fact-mutate-unused-init.csv':'Factorial Replace',
               'results/array-max-repl1.csv':'Array Max Replace 1',
               'results/array-max-repl3.csv':'Array Max Replace 2'}

output = '../table2-results.txt'
with open(output, 'w') as out: out.write('')

# Records results in plaintext files.
for f in glob(results+'/*.csv'):
    if f in prettynames:
        cmd = ["./results.R", f]
        resout = check_output(cmd)
        txtpath = splitext(f)[0]+'.txt'
        txtf = open(txtpath, 'w')
        txtf.write(resout)
        printable = 'Results from ' + prettynames[f] + ':' + '\n' + resout
        with open(output, 'a') as results: results.write(printable + '\n')
        print printable
        
