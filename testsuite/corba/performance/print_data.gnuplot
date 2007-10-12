set terminal png small
set output 'data.png'

set title 'Benchs PolyORB/CORBA'
set key outside top
set key box
set autoscale
set xlabel 'date'
set ylabel 'Number of requests'

plot 'Azerty.data' using 3 title 'azerty' with lines, \
'echoBoolean.data' using 3 title 'boolean' with lines, \
'echoChar.data' using 3 title 'char' with lines, \
'echoColor.data' using 3 title 'color' with lines, \
'echoDouble.data' using 3 title 'double' with lines, \
'echoFloat.data' using 3 title 'float' with lines, \
'echoLong.data' using 3 title 'long' with lines, \
'echoRainbow.data' using 3 title 'rainbow' with lines, \
'echoShort.data' using 3 title 'short' with lines, \
'echoSixteenKb.data' using 3 title '16kB' with lines, \
'echoString.data' using 3 title 'string' with lines, \
'echoStruct.data' using 3 title 'struct' with lines, \
'echoUnion.data' using 3 title 'union' with lines, \
'echoUsequence.data' using 3 title 'usequence' with lines, \
'echoWchar.data' using 3 title 'wchar' with lines, \
'NoParameter.data' using 3 title 'noparameter' with lines
