#!/usr/bin/bash

g++ model.cpp -o model
./model
Rscript plotting.R results.txt
