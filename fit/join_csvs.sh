#!/bin/bash
# joins many csv files of a given pattern (followed by a number)
# and stores the result in all_<pattern>
pattern=$1
head -n 1 ${pattern}1.csv > all_${pattern}.csv && tail -n+2 -q ${pattern}*.csv >> all_${pattern}.csv
