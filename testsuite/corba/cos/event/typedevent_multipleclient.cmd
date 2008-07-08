#Creation of TypedEvent Channel
create channel c

#Creation of a TypedPushConsumer
create pushconsumer psc

#Connection of TypedPushConsumer to TypedEventChannel
connect psc to c

#Creation of multiple PushSuppliers
create pushsupplier pss1
create pushsupplier pss2
create pushsupplier pss3
create pushsupplier pss4
create pushsupplier pss5
create pushsupplier pss6
create pushsupplier pss7
create pushsupplier pss8
create pushsupplier pss9
create pushsupplier pss10
create pushsupplier pss11
create pushsupplier pss12
create pushsupplier pss13
create pushsupplier pss14
create pushsupplier pss15
create pushsupplier pss16
create pushsupplier pss17
create pushsupplier pss18
create pushsupplier pss19
create pushsupplier pss20

#Connect Multiple PushSuppliers to the same TypedPush Consumer
#Call operations defined in Mutually Agreed Interface
get_typed_object on pss1 from c
get_typed_object on pss2 from c
get_typed_object on pss3 from c
get_typed_object on pss4 from c
get_typed_object on pss5 from c
get_typed_object on pss6 from c
get_typed_object on pss7 from c
get_typed_object on pss8 from c
get_typed_object on pss9 from c
get_typed_object on pss10 from c
get_typed_object on pss11 from c
get_typed_object on pss12 from c
get_typed_object on pss13 from c
get_typed_object on pss14 from c
get_typed_object on pss15 from c
get_typed_object on pss16 from c
get_typed_object on pss17 from c
get_typed_object on pss18 from c
get_typed_object on pss19 from c
get_typed_object on pss20 from c

#Creation of a TypedPullSupplier
create pullsupplier pls

#Connection of TypedPullSupplier to TypedEventChannel
connect pls to c

#Creation of multiple PullConsumers
create pullconsumer plc1
create pullconsumer plc2
create pullconsumer plc3
create pullconsumer plc4
create pullconsumer plc5
create pullconsumer plc6
create pullconsumer plc7
create pullconsumer plc8
create pullconsumer plc9
create pullconsumer plc10
create pullconsumer plc11
create pullconsumer plc12
create pullconsumer plc13
create pullconsumer plc14
create pullconsumer plc15
create pullconsumer plc16
create pullconsumer plc17
create pullconsumer plc18
create pullconsumer plc19
create pullconsumer plc20

#Connect multiple PullConsumers to same TypedPull Supplier
#Call operations defined in Mutually Agreed Interface
get_typed_object on plc1 from c
get_typed_object on plc2 from c
get_typed_object on plc3 from c
get_typed_object on plc4 from c
get_typed_object on plc5 from c
get_typed_object on plc6 from c
get_typed_object on plc7 from c
get_typed_object on plc8 from c
get_typed_object on plc9 from c
get_typed_object on plc10 from c
get_typed_object on plc11 from c
get_typed_object on plc12 from c
get_typed_object on plc13 from c
get_typed_object on plc14 from c
get_typed_object on plc15 from c
get_typed_object on plc16 from c
get_typed_object on plc17 from c
get_typed_object on plc18 from c
get_typed_object on plc19 from c
get_typed_object on plc20 from c

#Quit the test
quit
