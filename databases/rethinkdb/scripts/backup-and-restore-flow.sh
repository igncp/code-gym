#!/usr/bin/env bash

DIR_NAME=~/backup-and-restore

rm -rf $DIR_NAME
mkdir $DIR_NAME
cd $DIR_NAME

rethinkdb dump -e test &> /dev/null

if [ $? -ne 0 ]; then
  echo "You must have the 'test' table created. Try running one of the examples in /project/examples"
  exit 1
fi

FILE_NAME=$(ls $DIR_NAME | sort -n | head -1)

echo "dumped the 'test' database in the file: $FILE_NAME"

python -c """
import rethinkdb as r

r.connect('localhost', 28015).repl()
r.db_drop('test').run()
""" > /dev/null

echo "dropped the 'test' database"

rethinkdb restore -c localhost:28015 $FILE_NAME  > /dev/null

echo "restored the 'test' database using the file: $FILE_NAME"
