include $(srcdir)/../../Makefile.common

CosEventChannelAdmin_IDL=$(srcdir)/CosEventChannelAdmin.idl
CosEventComm_IDL=$(srcdir)/CosEventComm.idl

CosTypedEventChannelAdmin_IDL=$(srcdir)/CosTypedEventChannelAdmin.idl
CosTypedEventComm_IDL=$(srcdir)/CosTypedEventComm.idl

CosEventChannelAdmin_GEN= \
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
CosEventComm_GEN= \
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

CosTypedEventChannelAdmin_GEN= \
  costypedeventchanneladmin-helper.adb \
  costypedeventchanneladmin-helper.ads \
  costypedeventchanneladmin-stream.adb \
  costypedeventchanneladmin-stream.ads \
  costypedeventchanneladmin-typedconsumeradmin-helper.adb \
  costypedeventchanneladmin-typedconsumeradmin-helper.ads \
  costypedeventchanneladmin-typedconsumeradmin-skel.adb \
  costypedeventchanneladmin-typedconsumeradmin-skel.ads \
  costypedeventchanneladmin-typedconsumeradmin-stream.adb \
  costypedeventchanneladmin-typedconsumeradmin-stream.ads \
  costypedeventchanneladmin-typedconsumeradmin.adb \
  costypedeventchanneladmin-typedconsumeradmin.ads \
  costypedeventchanneladmin-typedeventchannel-helper.adb \
  costypedeventchanneladmin-typedeventchannel-helper.ads \
  costypedeventchanneladmin-typedeventchannel-skel.adb \
  costypedeventchanneladmin-typedeventchannel-skel.ads \
  costypedeventchanneladmin-typedeventchannel-stream.adb \
  costypedeventchanneladmin-typedeventchannel-stream.ads \
  costypedeventchanneladmin-typedeventchannel.adb \
  costypedeventchanneladmin-typedeventchannel.ads \
  costypedeventchanneladmin-typedproxypullsupplier-helper.adb \
  costypedeventchanneladmin-typedproxypullsupplier-helper.ads \
  costypedeventchanneladmin-typedproxypullsupplier-skel.adb \
  costypedeventchanneladmin-typedproxypullsupplier-skel.ads \
  costypedeventchanneladmin-typedproxypullsupplier-stream.adb \
  costypedeventchanneladmin-typedproxypullsupplier-stream.ads \
  costypedeventchanneladmin-typedproxypullsupplier.adb \
  costypedeventchanneladmin-typedproxypullsupplier.ads \
  costypedeventchanneladmin-typedproxypushconsumer-helper.adb \
  costypedeventchanneladmin-typedproxypushconsumer-helper.ads \
  costypedeventchanneladmin-typedproxypushconsumer-skel.adb \
  costypedeventchanneladmin-typedproxypushconsumer-skel.ads \
  costypedeventchanneladmin-typedproxypushconsumer-stream.adb \
  costypedeventchanneladmin-typedproxypushconsumer-stream.ads \
  costypedeventchanneladmin-typedproxypushconsumer.adb \
  costypedeventchanneladmin-typedproxypushconsumer.ads \
  costypedeventchanneladmin-typedsupplieradmin-helper.adb \
  costypedeventchanneladmin-typedsupplieradmin-helper.ads \
  costypedeventchanneladmin-typedsupplieradmin-skel.adb \
  costypedeventchanneladmin-typedsupplieradmin-skel.ads \
  costypedeventchanneladmin-typedsupplieradmin-stream.adb \
  costypedeventchanneladmin-typedsupplieradmin-stream.ads \
  costypedeventchanneladmin-typedsupplieradmin.adb \
  costypedeventchanneladmin-typedsupplieradmin.ads \
  costypedeventchanneladmin.adb \
  costypedeventchanneladmin.ads \
  costypedeventchanneladmin_idl_file.ads
CosTypedEventComm_GEN= \
  costypedeventcomm-typedpullsupplier-helper.adb \
  costypedeventcomm-typedpullsupplier-helper.ads \
  costypedeventcomm-typedpullsupplier-skel.adb \
  costypedeventcomm-typedpullsupplier-skel.ads \
  costypedeventcomm-typedpullsupplier-stream.adb \
  costypedeventcomm-typedpullsupplier-stream.ads \
  costypedeventcomm-typedpullsupplier.adb \
  costypedeventcomm-typedpullsupplier.ads \
  costypedeventcomm-typedpushconsumer-helper.adb \
  costypedeventcomm-typedpushconsumer-helper.ads \
  costypedeventcomm-typedpushconsumer-skel.adb \
  costypedeventcomm-typedpushconsumer-skel.ads \
  costypedeventcomm-typedpushconsumer-stream.adb \
  costypedeventcomm-typedpushconsumer-stream.ads \
  costypedeventcomm-typedpushconsumer.adb \
  costypedeventcomm-typedpushconsumer.ads \
  costypedeventcomm.ads \
  costypedeventcomm_idl_file.ads

DEPS=$(CosEventChannelAdmin_GEN) $(CosEventComm_GEN) \
  $(CosTypedEventChannelAdmin_GEN) $(CosTypedEventComm_GEN)

$(CosEventChannelAdmin_GEN): $(CosEventChannelAdmin_IDL)
	$(IDLAC) $(CosEventChannelAdmin_IDL) -I$(srcdir)

$(CosEventComm_GEN): $(CosEventComm_IDL)
	$(IDLAC) $(CosEventComm_IDL) -I$(srcdir)

$(CosTypedEventChannelAdmin_GEN): $(CosTypedEventChannelAdmin_IDL)
	$(IDLAC) $(CosTypedEventChannelAdmin_IDL) -I$(srcdir)

$(CosTypedEventComm_GEN): $(CosTypedEventComm_IDL)
	$(IDLAC) $(CosTypedEventComm_IDL) -I$(srcdir)
