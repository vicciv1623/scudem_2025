#!/usr/bin/bash

mkdir results
g++ model.cpp -o model
./model
Rscript plotting.R results.txt
