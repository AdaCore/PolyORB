include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../naming \
	-I$(srcdir)/../naming \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

PROGS=test_event

IDL_1=$(srcdir)/CosEventComm.idl
IDL_2=$(srcdir)/CosEventChannelAdmin.idl

GEN_1=\
coseventcomm-helper.adb \
coseventcomm-helper.ads \
coseventcomm-pullconsumer-helper.adb \
coseventcomm-pullconsumer-helper.ads \
coseventcomm-pullconsumer-skel.adb \
coseventcomm-pullconsumer-skel.ads \
coseventcomm-pullconsumer-stream.adb \
coseventcomm-pullconsumer-stream.ads \
coseventcomm-pullconsumer.adb \
coseventcomm-pullconsumer.ads \
coseventcomm-pullsupplier-helper.adb \
coseventcomm-pullsupplier-helper.ads \
coseventcomm-pullsupplier-skel.adb \
coseventcomm-pullsupplier-skel.ads \
coseventcomm-pullsupplier-stream.adb \
coseventcomm-pullsupplier-stream.ads \
coseventcomm-pullsupplier.adb \
coseventcomm-pullsupplier.ads \
coseventcomm-pushconsumer-helper.adb \
coseventcomm-pushconsumer-helper.ads \
coseventcomm-pushconsumer-skel.adb \
coseventcomm-pushconsumer-skel.ads \
coseventcomm-pushconsumer-stream.adb \
coseventcomm-pushconsumer-stream.ads \
coseventcomm-pushconsumer.adb \
coseventcomm-pushconsumer.ads \
coseventcomm-pushsupplier-helper.adb \
coseventcomm-pushsupplier-helper.ads \
coseventcomm-pushsupplier-skel.adb \
coseventcomm-pushsupplier-skel.ads \
coseventcomm-pushsupplier-stream.adb \
coseventcomm-pushsupplier-stream.ads \
coseventcomm-pushsupplier.adb \
coseventcomm-pushsupplier.ads \
coseventcomm-stream.adb \
coseventcomm-stream.ads \
coseventcomm.adb \
coseventcomm.ads \
coseventcomm_idl_file.ads

GEN_2=\
coseventchanneladmin-consumeradmin-helper.adb \
coseventchanneladmin-consumeradmin-helper.ads \
coseventchanneladmin-consumeradmin-skel.adb \
coseventchanneladmin-consumeradmin-skel.ads \
coseventchanneladmin-consumeradmin-stream.adb \
coseventchanneladmin-consumeradmin-stream.ads \
coseventchanneladmin-consumeradmin.adb \
coseventchanneladmin-consumeradmin.ads \
coseventchanneladmin-eventchannel-helper.adb \
coseventchanneladmin-eventchannel-helper.ads \
coseventchanneladmin-eventchannel-skel.adb \
coseventchanneladmin-eventchannel-skel.ads \
coseventchanneladmin-eventchannel-stream.adb \
coseventchanneladmin-eventchannel-stream.ads \
coseventchanneladmin-eventchannel.adb \
coseventchanneladmin-eventchannel.ads \
coseventchanneladmin-helper.adb \
coseventchanneladmin-helper.ads \
coseventchanneladmin-proxypullconsumer-helper.adb \
coseventchanneladmin-proxypullconsumer-helper.ads \
coseventchanneladmin-proxypullconsumer-skel.adb \
coseventchanneladmin-proxypullconsumer-skel.ads \
coseventchanneladmin-proxypullconsumer-stream.adb \
coseventchanneladmin-proxypullconsumer-stream.ads \
coseventchanneladmin-proxypullconsumer.adb \
coseventchanneladmin-proxypullconsumer.ads \
coseventchanneladmin-proxypullsupplier-helper.adb \
coseventchanneladmin-proxypullsupplier-helper.ads \
coseventchanneladmin-proxypullsupplier-skel.adb \
coseventchanneladmin-proxypullsupplier-skel.ads \
coseventchanneladmin-proxypullsupplier-stream.adb \
coseventchanneladmin-proxypullsupplier-stream.ads \
coseventchanneladmin-proxypullsupplier.adb \
coseventchanneladmin-proxypullsupplier.ads \
coseventchanneladmin-proxypushconsumer-helper.adb \
coseventchanneladmin-proxypushconsumer-helper.ads \
coseventchanneladmin-proxypushconsumer-skel.adb \
coseventchanneladmin-proxypushconsumer-skel.ads \
coseventchanneladmin-proxypushconsumer-stream.adb \
coseventchanneladmin-proxypushconsumer-stream.ads \
coseventchanneladmin-proxypushconsumer.adb \
coseventchanneladmin-proxypushconsumer.ads \
coseventchanneladmin-proxypushsupplier-helper.adb \
coseventchanneladmin-proxypushsupplier-helper.ads \
coseventchanneladmin-proxypushsupplier-skel.adb \
coseventchanneladmin-proxypushsupplier-skel.ads \
coseventchanneladmin-proxypushsupplier-stream.adb \
coseventchanneladmin-proxypushsupplier-stream.ads \
coseventchanneladmin-proxypushsupplier.adb \
coseventchanneladmin-proxypushsupplier.ads \
coseventchanneladmin-stream.adb \
coseventchanneladmin-stream.ads \
coseventchanneladmin-supplieradmin-helper.adb \
coseventchanneladmin-supplieradmin-helper.ads \
coseventchanneladmin-supplieradmin-skel.adb \
coseventchanneladmin-supplieradmin-skel.ads \
coseventchanneladmin-supplieradmin-stream.adb \
coseventchanneladmin-supplieradmin-stream.ads \
coseventchanneladmin-supplieradmin.adb \
coseventchanneladmin-supplieradmin.ads \
coseventchanneladmin.adb \
coseventchanneladmin.ads \
coseventchanneladmin_idl_file.ads

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

CLEANFILES += $(GEN_1) $(GEN_2)

$(GEN_1): $(IDL_1) $(IDLAC)
	$(IDLAC) -I$(srcdir) $(IDL_1)

$(GEN_2): $(IDL_2) $(IDLAC)
	$(IDLAC) -I$(srcdir) $(IDL_2)

$(PROGS): force $(GEN_1) $(GEN_2) $(IMP_1) $(IMP_2)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)
