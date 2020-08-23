#!/bin/bash

#find . -type f | grep *Test.hs$ | xargs runhaskell  

files=`find . -type f | grep -E *Test.hs$`  

for file in $files
do
    echo Target test file name : $file
    runhaskell $file
done

