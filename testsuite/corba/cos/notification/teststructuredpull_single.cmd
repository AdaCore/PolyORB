#Create Channel
create channel c

#Creation Phase
#Create a Structured PullSupplier
create structuredpullsupplier pls

#Create untyped Consumer
create structuredpushconsumer psc
create structuredpullconsumer plc

#Connection Phase
connect pls to c

connect psc to c

connect plc to c

#Production Phase
produce "structured pull event : 1" in pls

#Consumption Phase
consume in psc

consume in plc
