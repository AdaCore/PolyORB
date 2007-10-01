#Create Channel
create channel c

#Creation Phase
#Create an untyped PushSupplier
create pushsupplier pss

#Create untyped Consumer
create pushconsumer psc
create pullconsumer plc

#Connection Phase
connect pss to c

connect psc to c

connect plc to c

#Production Phase
produce "any push event : 1" in pss
produce "any push event : 2" in pss

#Consumption Phase
consume in psc

consume in plc

tryconsume in psc

tryconsume in plc
