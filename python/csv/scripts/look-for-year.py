#!/usr/bin/python

"""
Arguments:
    1. Source CSV file path
    2. Target CSV file path
"""

import csv
import os
import sys
import urllib

import discogs_client
import wikipedia

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


def generate_display_and_get_year(summary, year, artist, title):
    os.system('cls' if os.name == 'nt' else 'clear')

    print("Artist: '{0}', Title: '{1}'\n".format(artist, title))
    print(summary)

    print("\nhttp://www.google.com/search?q={0}\n".format(
        urllib.quote(artist + " " + title, safe='')))

    final_year = raw_input("Please, enter the year [{0}]: "
        .format(year)) or year

    percentage = int((idx + 1) * 100 / (lines_length - 1))
    print("\nCompleted {0} of {1} ({2}%)"
        .format(idx + 1, lines_length - 1, percentage))

    return final_year

# requests to discogs are throttled to 240 per minute

lines_length = len(lines)
for idx, album in enumerate(lines[1:]):
    artist = album[0]
    title = album[1]
    if len(album) > 2:
        continue
    results = d.search(artist + " " + title, type='master', per_page="100")
    results_length = len(results)
    if results_length > 0:
        if results_length == 1:
            main_release = results[0].main_release
            year = int(main_release.year)
        else:
            year_a = int(results[0].main_release.year)
            year_b = int(results[1].main_release.year)
            year = min([year_a, year_b])
    else:
        year = "-"

    results = wikipedia.search(artist + " " + title)

    try:
        page = wikipedia.page(results[0])
        summary = page.summary
    except:
        summary = "ERROR"

    final_year = generate_display_and_get_year(summary, year, artist, title)
    lines[idx + 1] = [artist, title, final_year]

    write_file()
