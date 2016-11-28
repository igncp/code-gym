#!/usr/bin/python

"""
This script accepts a CSV file with a shape like
the sample. It will write to StdOut with all the
albums sorted by artist and then title
"""

import sys
import csv

source_file_path = sys.argv[1]

lines = []
with open(source_file_path, 'rb') as csvfile:
    content = csv.reader(csvfile, delimiter=',', quotechar='"')
    for line in content:
        lines.append(line)


def compare(a, b):
    a_artist = a[0].lower()
    b_artist = b[0].lower()
    if a_artist == b_artist:
        a_title = a[1].lower()
        b_title = b[1].lower()
        if a_title == b_title:
            raise ValueError('Two albums with the same artist: ' +
                a[0] + ' and title ' + a[1])
        if a_title < b_title:
            return -1
        else:
            return 1
    if a_artist < b_artist:
        return -1
    else:
        return 1

sorted_lines = [lines[0]] + sorted(lines[1:], cmp=compare)

writer = csv.writer(sys.stdout, delimiter=',',
    quotechar='"', quoting=csv.QUOTE_ALL)
for album in sorted_lines:
    writer.writerow(album)
