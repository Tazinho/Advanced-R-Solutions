#!/bin/bash

# rename all Rmd files to qmd
for f in *.Rmd; do
    mv -- "$f" "${f%.Rmd}.qmd"
done
