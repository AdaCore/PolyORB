#Create Channel
create channel c

#Creation Phase
#Create a Sequence PullSupplier
create sequencepullsupplier pls

#Create untyped Consumer
create sequencepushconsumer psc
create sequencepullconsumer plc

#Connection Phase
connect pls to c

connect psc to c

connect plc to c

#Production Phase
produce "sequence pull event : 1" in pls

#Consumption Phase
consume in psc

consume in plc
