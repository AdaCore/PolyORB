include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../naming \
	-I$(srcdir)/../naming \
	-I$(COS_ROOT)/event \
	-I$(COS_ROOT)/naming \
	-I../../src \
	-I$(srcdir)/../../src \
	$(ADASOCKETS_INCLUDE) \
	-g -O2 -m -gnata \
	-largs $(ADASOCKETS_LIBS)

PROGS=test_event

COS_ROOT=../../idls/cos
COS_DIRS=$(COS_ROOT)/event $(COS_ROOT)/naming

IMP_1= \
coseventcomm-pullsupplier-impl.adb \
coseventcomm-pullsupplier-impl.ads \
coseventcomm-pushconsumer-impl.adb \
coseventcomm-pushconsumer-impl.ads \
coseventcomm-pullconsumer-impl.adb \
coseventcomm-pullconsumer-impl.ads \
coseventcomm-pushsupplier-impl.adb \
coseventcomm-pushsupplier-impl.ads

IMP_2= \
coseventchanneladmin-consumeradmin-impl.adb \
coseventchanneladmin-consumeradmin-impl.ads \
coseventchanneladmin-supplieradmin-impl.adb \
coseventchanneladmin-supplieradmin-impl.ads \
coseventchanneladmin-proxypushsupplier-impl.adb \
coseventchanneladmin-proxypushsupplier-impl.ads \
coseventchanneladmin-proxypushconsumer-impl.adb \
coseventchanneladmin-proxypushconsumer-impl.ads \
coseventchanneladmin-proxypullconsumer-impl.adb \
coseventchanneladmin-proxypullconsumer-impl.ads \
coseventchanneladmin-proxypullsupplier-impl.adb \
coseventchanneladmin-proxypullsupplier-impl.ads \
coseventchanneladmin-eventchannel-impl.adb \
coseventchanneladmin-eventchannel-impl.ads

$(PROGS): force $(IMP_1) $(IMP_2)
	$(GNATMAKE_FOR_TARGET) $@ $(GNATMAKE_FLAGS)
