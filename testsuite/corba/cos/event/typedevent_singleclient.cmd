#Creation of TypedEvent Channel
create channel c

#Creation of a TypedPushConsumer
create pushconsumer psc

#Connection of TypedPushConsumer to TypedEventChannel
connect psc to c

#Creation of a PushSupplier
create pushsupplier pss

#Connect PushSupplier to TypedPush Consumer
#Call operations defined in Mutually Agreed Interface
get_typed_object on pss from c

#Creation of a TypedPullSupplier
create pullsupplier pls

#Connection of TypedPullSupplier to TypedEventChannel
connect pls to c

#Creation of a PullConsumer
create pullconsumer plc

#Connect PullConsumer to TypedPull Supplier
#Call operations defined in Mutually Agreed Interface
get_typed_object on plc from c

#Quit the test
quit
