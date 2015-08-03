#!/usr/bin/env python

from glob import glob
from os import listdir, path
from os.path import exists, isfile, splitext
from subprocess import call, check_output
from sys import exit, exc_info
from time import strftime

results = 'results'

# Records results in plaintext files.
for f in glob(results+'/*.csv'):
    cmd = ["./results.R", f]
    resout = check_output(cmd)
    txtpath = splitext(f)[0]+'.txt'
    txtf = open(txtpath, 'w')
    txtf.write(resout)
    print 'Results from '+f+':'
    print resout
