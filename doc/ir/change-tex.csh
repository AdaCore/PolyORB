#!/bin/csh

sed  s/�/"\\"\'e/g $1-bis.tex > $1.old
sed  s/�/"\\"\`e/g $1.old > $1.tex
sed  s/�/"\\"^e/g $1.tex > $1.old
sed  s/�/"\\"\"e/g $1.old > $1.tex

sed  s/�/"\\"\`a/g $1.tex > $1.old
sed  s/�/"\\"^a/g $1.old > $1.tex

sed  s/�/"\\"^o/g $1.tex > $1.old

sed  s/�/"\\"\`u/g $1.old > $1.tex
sed  s/�/"\\"\"u/g $1.tex > $1.old
sed  s/�/"\\"^u/g $1.old > $1.tex

sed  s/�/"\\"\"i/g $1.tex > $1.old
sed  s/�/"\\"^i/g $1.old > $1.tex

sed  s/�/"\\"c\{c\}/g $1.tex > $1.old

mv $1.old $1.tex

latex $1.tex
latex $1.tex
dvips -o $1.ps $1.dvi
 


