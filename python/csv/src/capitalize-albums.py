#!/usr/bin/python

"""
This script accepts a CSV file with a shape like
the sample. It will write to StdOut. It will capitalize
all the words of the artist and the first word of the
title, with some extra cleaverness
"""

import sys
import csv

source_file_path = sys.argv[1]

lines = []
with open(source_file_path, 'rb') as csvfile:
    content = csv.reader(csvfile, delimiter=',', quotechar='"')
    for line in content:
        lines.append(line)


def transform_album(old_album):
    new_album = old_album[:]
    new_album[0] = new_album[0].title()
    new_album[1] = new_album[1].capitalize()
    return new_album

parsed_lines = map(transform_album, lines)

writer = csv.writer(sys.stdout, delimiter=',',
    quotechar='"', quoting=csv.QUOTE_ALL)
for album in parsed_lines:
    writer.writerow(album)
