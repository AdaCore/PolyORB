ADA_FULL = \
	   broca-debug.ads \
           broca-exceptions.ads \
           broca-flags.ads \
           broca-giop.ads \
           broca-iiop.ads \
           broca-inet_server.ads \
           broca-ior.ads \
           broca-locks.ads \
           broca-marshalling.ads \
           broca-object.ads \
           broca-orb.ads \
           broca-poa.ads \
           broca-refs.ads \
           broca-repository.ads \
           broca-rootpoa.ads \
           broca-sequences.ads \
           broca-server.ads \
           broca-stream.ads \
           broca-vararray.ads \
           corba-forward.ads \
           corba-object.ads \
           corba-orb.ads \
           corba-sequences-unbounded.ads \
           corba.ads \
           portableserver-adapteractivator-impl.ads \
           portableserver-adapteractivator.ads \
           portableserver-poa.ads \
           portableserver-poamanager.ads \
           portableserver-servantactivator.ads \
           portableserver-servantlocator.ads \
           portableserver-servantmanager-impl.ads \
           portableserver.ads
#           broca-policy.ads \
#           corba-boa.ads \
#           corba-policy.ads \
#           corba-sequences-bounded.ads \
#           portableserver-servantretentionpolicy.ads \
#           portableserver-threadpolicy.ads

ADA_IMPLS = $(ADA_FULL:.ads=.adb)

ADA_SPECS += $(ADA_FULL) \
           broca.ads \
           broca-types.ads \
           corba-sequences.ads \
           portableserver-servantactivator-impl.ads \
           portableserver-servantlocator-impl.ads \
           portableserver-servantmanager.ads

ADA_OBJS = $(ADA_SPECS:.ads=.o)

lib = $(patsubst %,$(LibPattern),broca)

all:: $(lib)

$(lib): $(ADA_OBJS)
	ar rvs $@ $^

$(ADA_OBJS): allsrc

allsrc: $(ADA_SPECS) $(ADA_IMPLS)
	$(GNATMAKE) $(BROCA_FLAGS) allsrc; \

clean::
	$(RM) *.o *.ali *~ *.c
	$(RM) allsrc
	$(RM) $(lib)
