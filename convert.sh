#!/bin/sh
#
# try to remove trailing semicolon from ends of rows
#

if [ $# != 1 ]
then
  echo "Usage: convert.sh <dir where csv files are>"
  echo ""
  echo "Example: convert.sh ./data/"
  exit
fi

DIR=$1
IFS="\n"

if [ -n "$DIR" ]; then
    cd $DIR
    for file in *.csv; do
        echo "$file"
        sed -e "s/;\r/\r/g" $file > "$file"_tmp &&
        mv "$file"_tmp $file
    done
fi

