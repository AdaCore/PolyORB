CXXSRCS = client.cc
# server.cc

DIR_CPPFLAGS = $(CORBA_CPPFLAGS) -D__linux__ -D__x86__ -g -O0
CXXFLAGS+=$(DIR_CPPFLAGS)

CORBA_INTERFACES = all_types

CORBA_STUB_OBJS=all_typesSK.o
CORBA_SKEL_OBJS=all_typesSK.o
CORBA_LIBS=-lomniORB2 -lomniGK_alone

#server=server
client=client

all:: $(server) $(client)

clean::
	$(RM) $(server) $(client)

$(server): server.o $(CORBA_SKEL_OBJS) $(CORBA_LIB_DEPEND)
	#@(libs="$(CORBA_LIB)"; $(CXXExecutable))
	g++ -o $@ $^ $(CORBA_LIBS)

$(client): client.o $(CORBA_STUB_OBJS) $(CORBA_LIB_DEPEND)
	#@(libs="$(CORBA_LIB)"; $(CXXExecutable))
	g++ -o $@ $^ $(CORBA_LIBS)

