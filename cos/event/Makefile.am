AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/corba/cos

corba_exe=test_event typedtest_event

COS_GNATMAKE_FLAGS = \
	-I$(srcdir) \
	-I../naming \
	-I$(srcdir)/../naming \
	-I$(COS_ROOT)/event \
	-I$(COS_ROOT)/naming \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../idls/cos/naming \
	-I../../idls/cos/event

PROGS=@APPLI_EXES@

FORCE= force

COS_DIRS=../../idls/cos/event ../naming

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

IMP_3= \
typedtest_interface-impl.adb \
typedtest_interface-impl.ads

GEN=\
typedtest_interface-helper.adb \
typedtest_interface-helper.ads \
typedtest_interface-ir_info.adb \
typedtest_interface-ir_info.ads \
typedtest_interface_idl_typedtest_interface-helper.adb \
typedtest_interface_idl_typedtest_interface-helper.ads \
typedtest_interface_idl_typedtest_interface-ir_info.adb \
typedtest_interface_idl_typedtest_interface-ir_info.ads \
typedtest_interface-skel.adb \
typedtest_interface-skel.ads \
typedtest_interface-stream.adb \
typedtest_interface-stream.ads \
typedtest_interface.adb \
typedtest_interface.ads \
typedtest_interface_idl_file.ads

CLEANFILES += $(GEN) stamp-gen

IDL=$(srcdir)/typedtest_interface.idl

$(GEN): stamp-gen
stamp-gen: $(IDL) $(IDLAC)
	$(IDLAC) $(IDL)
	touch stamp-gen

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/cos*.ad[bs] cos*.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done
