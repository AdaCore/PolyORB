include $(srcdir)/../../Makefile.common

CosNaming_IDL=$(srcdir)/CosNaming.idl
CosNaming_GEN= \
  cosnaming-bindingiterator-helper.adb \
  cosnaming-bindingiterator-helper.ads \
  cosnaming-bindingiterator-skel.adb \
  cosnaming-bindingiterator-skel.ads \
  cosnaming-bindingiterator-stream.adb \
  cosnaming-bindingiterator-stream.ads \
  cosnaming-bindingiterator.adb \
  cosnaming-bindingiterator.ads \
  cosnaming-helper.adb \
  cosnaming-helper.ads \
  cosnaming-namingcontext-helper.adb \
  cosnaming-namingcontext-helper.ads \
  cosnaming-namingcontext-skel.adb \
  cosnaming-namingcontext-skel.ads \
  cosnaming-namingcontext-stream.adb \
  cosnaming-namingcontext-stream.ads \
  cosnaming-namingcontext.adb \
  cosnaming-namingcontext.ads \
  cosnaming-stream.adb \
  cosnaming-stream.ads \
  cosnaming.ads \
  cosnaming_idl_file.ads

DEPS=$(CosNaming_GEN)

$(CosNaming_GEN): $(CosNaming_IDL) $(IDLAC)
	$(IDLAC) $(CosNaming_IDL) -I$(srcdir)
