#Create Channel
create channel c

#Creation Phase
#Create a Sequence PushSupplier
create sequencepushsupplier pss

#Create untyped Consumer
create pushconsumer psc
create pullconsumer plc

#Create Structured Consumer
create structuredpushconsumer pstc
create structuredpullconsumer pslc

#Create Sequence Consumer
create sequencepushconsumer psqsc
create sequencepullconsumer psqlc

#Connection Phase
connect pss to c

connect psc to c
connect plc to c

connect pstc to c
connect pslc to c

connect psqsc to c
connect psqlc to c

#Production Phase
produce "sequence push event" in pss

#Consumption Phase
consume in psc
consume in plc

consume in pstc
consume in pslc

consume in psqsc
consume in psqlc
