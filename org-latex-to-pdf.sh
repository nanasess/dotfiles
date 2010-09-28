#!/bin/sh
tex=$*
dvi=`/usr/bin/basename "$tex" ".tex"`
PATH=$HOME/Applications/pTeX.app/teTeX/bin:$PATH
platex --kanji=utf8 -interaction nonstopmode $tex
dvipdfmx $dvi
