#!/usr/bin/env bash

# This script defines couple of syntax rules for parsing hx files
# This is a bit oversimplified because HaXe allows field introduction
# which we are interested to collect while not specifying access modifiers
# on the other hand, we aren't interested in finding nested functions...
# It also assumes that declarations always happen on the same line (which is
# common, but is not required). It also may eventually *find* declarations
# inside strings, given they are multiline and they fit the regexps below...
#
# To generate tags for your project do somethig like:
#
# $ ./hxtags.sh ./src
#
# Where `./src' is the location of your project sources.

# some fixes were submitted by Amit Patel (the script would be generating
# only tags for the last source otherwise)
old=$(pwd)

# default to current directory
new=${1:-.}
cd "$new"

# note -- the substitution $filenames will fail if there are spaces in filenames
filenames=$(find "$new" -type f -name "*.hx")

# make sure some files were found before writing TAGS
if [ -z "$filenames" ]; then
    echo "Error: no .hx files found in $new" >&2
else
    etags --lang=none --regex='/[ \t]*class[ \t]+\([^ \t{\/]+\)/\1/' \
        --regex='/[ \t]*interface[ \t]+\([^ \t{\/]+\)/\1/' \
        --regex='/[ \t]*typedef[ \t]+\([^ \t{\/=]+\)/\1/' \
        --regex='/[ \t]*enum[ \t]+\([^ \t{\/]+\)/\1/' \
        --regex='/[ \t]*\(\(public\|private\|static\|override\|inline\)[ \t]\)*function[ \t]\([^ \t(]+\)/\3/' \
        --regex='/[ \t]*\(\(public\|private\|static\|override\|inline\)[ \t]\)*var[ \t]\([^ \t:=]+\)/\3/' \
        $filenames
fi

cd "$old"
