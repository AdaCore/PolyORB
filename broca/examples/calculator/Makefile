SHELL=/bin/sh

GENERATED_FILES=calculators-calculator_plus-helper.adb \
calculators-calculator_plus-helper.ads\
calculators-calculator_plus-skel.adb\
calculators-calculator_plus-skel.ads\
calculators-calculator_plus-stream.adb\
calculators-calculator_plus-stream.ads\
calculators-calculator_plus.adb\
calculators-calculator_plus.ads\
calculators-simple_calculator-helper.adb\
calculators-simple_calculator-helper.ads\
calculators-simple_calculator-skel.adb\
calculators-simple_calculator-skel.ads\
calculators-simple_calculator-stream.adb\
calculators-simple_calculator-stream.ads\
calculators-simple_calculator.adb\
calculators-simple_calculator.ads\
calculators.ads\
demo_idl_file.ads



CC = gnatmake
FLAGS = -g -gnata -gnatf 
LIB = -I. -I/home/niebel/adabroker/example/generic -I/home/niebel/adabroker/src \
	-I/home/niebel/adabroker/adasockets/src -I/home/niebel/adabroker/src/ir

first: simple_server server_plus client 

${GENERATED_FILES}: demo.idl
	idlac demo.idl

simple_server: simple_server.adb ${GENERATED_FILES}
	${CC}  ${FLAGS} ${LIB} simple_server.adb

server_plus: server_plus.adb  ${GENERATED_FILES}
	${CC}  ${FLAGS} ${LIB} server_plus.adb

client: client.adb ${GENERATED_FILES}
	${CC} ${FLAGS} ${LIB} client.adb

clean:
	rm *.o *.ali ${GENERATED_FILES} client simple_server server_plus

