#!/bin/bash
conv=$(echo 'one:o1e;two:t2o;three:t3e;four:f4r;five:f5e;six:s6x;seven:s7n;eight:e8t;nine:n9e' | tr ';' '\n' | awk -F ':' '{print "s/" $1 "/" $2 "/g"}' | tr '\n' ';'); cat in.txt | sed "${conv}s/[^[:digit:]]//g;s/[[:digit:]]/& /g" | awk '{print $1 $(NF)}' | awk '{s += $1} END {print s}'
