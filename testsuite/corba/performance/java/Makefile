
all:
	idlj -fall benchs.idl
	javac benchsServer.java benchsPackage/*.java
#	orbd -ORBInitialPort 1050 -ORBInitialHost localhost
	java benchsServer -ORBInitialPort 1050 -ORBInitialHost localhost

clean:
	rm -rf *.class \
		benchsHelper.java \
		benchs.java \
		benchsPOA.java \
		benchsHolder.java \
		benchsOperations.java \
		_benchsStub.java \
		benchsPackage 
