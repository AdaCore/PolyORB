ADA_SRCS = \
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
           broca-types.ads \
           broca-vararray.ads \
           broca.ads \
           corba-forward.ads \
           corba-iop.ads \
           corba-object.ads \
           corba-orb.ads \
           corba-sequences-unbounded.ads \
           corba-sequences.ads \
           corba.ads \
           portableserver-adapteractivator-impl.ads \
           portableserver-adapteractivator.ads \
           portableserver-poa.ads \
           portableserver-poamanager.ads \
           portableserver-servantactivator-impl.ads \
           portableserver-servantactivator.ads \
           portableserver-servantlocator-impl.ads \
           portableserver-servantlocator.ads \
           portableserver-servantmanager-impl.ads \
           portableserver-servantmanager.ads \
           portableserver.ads
#           broca-policy.ads \
#           corba-boa.ads \
#           corba-policy.ads \
#           corba-sequences-bounded.ads \
#           portableserver-servantretentionpolicy.ads \
#           portableserver-threadpolicy.ads \

ADA_OBJS = $(ADA_SRCS:.ads=.o)

lib = $(patsubst %,$(LibPattern),broca)

all:: $(lib)

$(lib): $(ADA_OBJS)
	ar rvs $@ $^

$(ADA_OBJS): allsrc

allsrc: $(ADA_SRCS)
	$(GNATMAKE) $(BROCA_FLAGS) allsrc; \

clean::
	$(RM) *.o *.ali *~ *.c
	$(RM) allsrc
