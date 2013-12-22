#!/usr/bin/env python

import sys
import os
import itertools
import datetime
import pprint


PATNUM = 1
ACC = 2
DATE = 4

startdate = datetime.datetime.min

def getrows(lines):

    pi, ai, di = PATNUM, ACC, DATE
    strptime = datetime.datetime.strptime

    for line in lines:
        row = line.rstrip().split('\t')
        row[di] = strptime(row[di], '%Y/%m/%d %H:%M')
        yield row

def minutes(delta):
    return delta.days * 60*24 + delta.seconds/60

def obscure_dates(pgrp):
    di = DATE
    rows = list(pgrp)
    first = min([r[di] for r in rows])
    for row in rows:
        # row[di] = minutes(row[di] - first)
        row[di] = startdate + (row[di] - first)
        yield row

def main():

    infile = sys.argv[1]
    # outfile = sys.stdout
    outfile = open(sys.argv[2],'w')

    pi, ai, di = PATNUM, ACC, DATE

    lines = open(infile)
    headers = lines.next()

    # lines = itertools.islice(lines, 1000)

    # filter non-N,H,U patient ids
    rows = itertools.ifilter(lambda row: row[pi][0] in set(['U','H','N']), getrows(lines))

    # itertools.groupby expects rows sorted by patient id
    rows = sorted(rows, key=lambda row: (row[pi], row[di]))
    print len(rows), 'rows with H-, U-, or N-numbers'

    # sets of patient IDs and accession numbers
    pnums, anums = map(set, zip(*((row[pi],row[ai]) for row in rows)))

    print len(pnums), 'unique patient IDs'
    print len(anums), 'unique accession numbers'

    pdict = {}
    for i, key in enumerate(pnums):
        pdict[key] = key[0] + str(i).zfill(6)

    adict = {}
    for i, key in enumerate(anums):
        adict[key] = 'A' + str(i).zfill(6)

    obfuscated = (obscure_dates(grp) for num, grp in itertools.groupby(rows, lambda x: x[PATNUM]))
    outfile.write(headers)
    for row in itertools.chain.from_iterable(obfuscated):
        row[pi], row[ai] = pdict[row[pi]], adict[row[ai]]
        row[di] = str(row[di])
        outfile.write('\t'.join(row) + '\n')


if __name__ == '__main__':
    main()
