#!/bin/csh

sed  s/é/"\\"\'e/g $1-bis.tex > $1.old
sed  s/è/"\\"\`e/g $1.old > $1.tex
sed  s/ê/"\\"^e/g $1.tex > $1.old
sed  s/ë/"\\"\"e/g $1.old > $1.tex

sed  s/à/"\\"\`a/g $1.tex > $1.old
sed  s/â/"\\"^a/g $1.old > $1.tex

sed  s/ô/"\\"^o/g $1.tex > $1.old

sed  s/ù/"\\"\`u/g $1.old > $1.tex
sed  s/ü/"\\"\"u/g $1.tex > $1.old
sed  s/û/"\\"^u/g $1.old > $1.tex

sed  s/ï/"\\"\"i/g $1.tex > $1.old
sed  s/î/"\\"^i/g $1.old > $1.tex

sed  s/ç/"\\"c\{c\}/g $1.tex > $1.old

mv $1.old $1.tex

latex $1.tex
latex $1.tex
dvips -o $1.ps $1.dvi
 


