SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-giop

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-iiop.ads \
	polyorb-giop_p-exceptions.ads \
	polyorb-protocols-giop-giop_1_0.ads \
	polyorb-protocols-giop-giop_1_1.ads \
	polyorb-protocols-giop-giop_1_2.ads \
	polyorb-protocols-giop.ads \
	polyorb-setup-tcp_access_points-iiop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-giop_p.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-giop.la
libpolyorb_giop_la_SOURCES =
libpolyorb_giop_la_LIBADD = $(ADA_LIBADD)
libpolyorb_giop_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-giop.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS)

ADACOMPILER = ../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali

.adb.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

.ads.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-iiop.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads polyorb-bi di g_data-iiop.ads polyorb-bi di g_data-iiop.adb ./../polyorb-buffers.ads ./../polyorb-compo e ts.ads ./../polyorb-co figuratio .ads ./../polyorb-filters.ads ./../polyorb-filters-slicers.ads ./../polyorb-i itializatio .ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-objects.adb ./../polyorb-opaque.ads ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-tra sport-sockets.ads ./../polyorb-types.ads ./../polyorb-types.adb ./../polyorb-utils.ads ./../polyorb-utils.adb ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-stri gs.ads ./../polyorb-utils-stri gs.adb ./../polyorb-utils-stri gs-lists.ads ./../polyorb-utils-stri gs-lists.adb
polyorb-giop_p-exceptions.lo:./../polyorb.ads ./../polyorb-a y.ads ./../polyorb-exceptio s.ads polyorb-giop_p.ads polyorb-giop_p-exceptio s.ads polyorb-giop_p-exceptio s.adb ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-taski g.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-types.ads ./../polyorb-types.adb ./../polyorb-utils.ads ./../polyorb-utils.adb
polyorb-giop_p.lo:./../polyorb.ads polyorb-giop_p.ads
polyorb-protocols-giop-giop_1_0.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads polyorb-bi di g_data-iiop.ads ./../polyorb-buffers.ads ./../polyorb-compo e ts.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop-giop_1_0.adb ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-stri gs.ads
polyorb-protocols-giop-giop_1_1.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads polyorb-bi di g_data-iiop.ads ./../polyorb-buffers.ads ./../polyorb-compo e ts.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop-giop_1_1.adb ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-stri gs.ads
polyorb-protocols-giop-giop_1_2.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads polyorb-bi di g_data-iiop.ads ./../polyorb-buffers.ads ./../polyorb-compo e ts.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads polyorb-protocols-giop-giop_1_2.ads polyorb-protocols-giop-giop_1_2.adb ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-stri gs.ads
polyorb-protocols-giop.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads ./../polyorb-bi di g_data.adb polyorb-bi di g_data-iiop.ads ./../polyorb-bi di g_data-local.ads ./../polyorb-buffers.ads ./../polyorb-buffers.adb ./../polyorb-compo e ts.ads ./../polyorb-compo e ts.adb ./../polyorb-exceptio s.ads ./../polyorb-filters.ads ./../polyorb-filters-i terface.ads polyorb-giop_p.ads polyorb-giop_p-exceptio s.ads ./../polyorb-i itializatio .ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-log.adb ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-objects.adb ./../polyorb-objects-i terface.ads ./../polyorb-opaque.ads ./../polyorb-opaque.adb ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-orb-i terface.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads polyorb-protocols-giop.adb polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop-giop_1_2.ads ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-types.ads ./../polyorb-types.adb ./../polyorb-utils.ads ./../polyorb-utils.adb ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-stri gs.ads ./../polyorb-utils-stri gs.adb ./../polyorb-utils-stri gs-lists.ads ./../polyorb-utils-stri gs-lists.adb
polyorb-setup-tcp_access_points-iiop.lo:./../polyorb.ads ./../polyorb-a  otatio s.ads ./../polyorb-a y.ads ./../polyorb-a y-exceptio list.ads ./../polyorb-a y- vlist.ads ./../polyorb-asy ch_ev.ads ./../polyorb-bi di g_data.ads polyorb-bi di g_data-iiop.ads ./../polyorb-buffers.ads ./../polyorb-compo e ts.ads ./../polyorb-co figuratio .ads ./../polyorb-filters.ads ./../polyorb-filters-slicers.ads ./../polyorb-i itializatio .ads ./../polyorb-jobs.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chu k_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-giop.ads ./../polyorb-refere ces.ads ./../polyorb-refere ces-ior.ads ./../polyorb-represe tatio s.ads ./../polyorb-represe tatio s-cdr.ads ./../polyorb-requests.ads ./../polyorb-seque ces.ads ./../polyorb-seque ces-u bou ded.ads ./../polyorb-seque ces-u bou ded.adb ./../polyorb-serva ts.ads ./../polyorb-setup.ads ./../polyorb-setup-tcp_access_poi ts.ads polyorb-setup-tcp_access_poi ts-iiop.ads polyorb-setup-tcp_access_poi ts-iiop.adb ./../polyorb-smart_poi ters.ads ./../polyorb-sockets.ads ./../polyorb-task_i fo.ads ./../polyorb-taski g.ads ./../polyorb-taski g-co ditio _variables.ads ./../polyorb-taski g-mutexes.ads ./../polyorb-taski g-threads.ads ./../polyorb-tra sport.ads ./../polyorb-tra sport-sockets.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chai ed_lists.ads ./../polyorb-utils-chai ed_lists.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-stri gs.ads ./../polyorb-utils-stri gs.adb ./../polyorb-utils-stri gs-lists.ads ./../polyorb-utils-stri gs-lists.adb
