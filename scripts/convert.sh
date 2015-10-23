#!/bin/bash

pandoc -f markdown+lhs -t markdown $1.lhs > $1.md

sed -i 's/.literate //g' $1.md

sed -i 's/<!--\n{-# OPTIONS_GHC -Wall #-}\n-->\n//g' $1.md
 
rm $1.lhs
