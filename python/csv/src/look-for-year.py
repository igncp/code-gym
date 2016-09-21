#!/usr/bin/python

"""
This script accepts a CSV file with a shape like
the sample. It will try to guess the release year
of the album and append it as the third column,
otherwise it will add "-". The second argument is
the output file, which will be written entirely each
time a year is found.
"""

import sys
import csv
import discogs_client
import time

source_file_path = sys.argv[1]
target_file_path = sys.argv[2]

lines = []
with open(source_file_path, 'rb') as csvfile:
    content = csv.reader(csvfile, delimiter=',', quotechar='"')
    for line in content:
        lines.append(line)

# generate a user token at https://www.discogs.com/settings/developers

discogs_token = None
with open("discogs-token.txt", "rb") as file:
    discogs_token = file.read().rstrip()

d = discogs_client.Client('LookForAlbumYearApplication/0.1',
    user_token=discogs_token)


def write_file():
    with open(target_file_path, 'w') as csvfile:
        writer = csv.writer(csvfile, delimiter=',',
            quotechar='"', quoting=csv.QUOTE_ALL)
        for album in lines:
            writer.writerow(album)

# requests are throttled to 240 per minute

lines_length = len(lines)
for idx, album in enumerate(lines):
    artist = album[0]
    title = album[1]
    print(len(album))
    if len(album) > 2:
        continue
    results = d.search(artist + " " + title, type='master', per_page="100")
    if len(results) > 0:
        main_release = results[0].main_release
        year = int(main_release.year)
    else:
        year = "-"
    lines[idx] = [artist, title, year]
    time.sleep(2)
    print("Completed " + str(idx + 1) + " of " + str(lines_length))
    print("Writing file ...")
    write_file()
