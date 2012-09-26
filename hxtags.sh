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

find $1 -type f -name "*.hx" | \
    etags --lang=none --regex='/[ \t]*class[ \t]+\([^ \t{\/]+\)/\1/' \
    --regex='/[ \t]*typedef[ \t]+\([^ \t{\/=]+\)/\1/' \
    --regex='/[ \t]*enum[ \t]+\([^ \t{\/]+\)/\1/' \
    --regex='/[ \t]*\(\(public\|private\|static\|override\|inline\)[ \t]\)+function[ \t]\([^ \t(]+\)/\3/' \
    --regex='/[ \t]*\(\(public\|private\|static\|override\|inline\)[ \t]\)+var[ \t]\([^ \t:=]+\)/\3/' -