create channel c
#Creation Phase
create pullsupplier pls
create pullconsumer psc1
create pushconsumer psc2
create sequencepushconsumer psc3
create structuredpullconsumer psc4
create structuredpushconsumer psc5
#Connection Phase
connect pls to c
connect psc1 to c
connect psc2 to c
connect psc3 to c
connect psc4 to c
connect psc5 to c
#Production Phase
produce "any pull event" in pls
#Consumption Phase
consume in psc1
consume in psc2
consume in psc3
consume in psc4
consume in psc5
