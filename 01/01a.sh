#!/bin/bash
cat in.txt | sed 's/[^[:digit:]]//g;s/[[:digit:]]/& /g' | awk '{print $1 $(NF)}' | awk '{s += $1} END {print s}'
