FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -I.. -gnatf -gnata -m -i client $(FLAGS)
	gnatmake -I.. -gnatf -gnata -m -i server $(FLAGS)


GENERATED_FILES = echo.ad*
GENERATED_FILES += echo-proxy.ad*
GENERATED_FILES += echo-stream.ad*
GENERATED_FILES += echo-skel.ad*
GENERATED_FILES += cosnaming-bindingiterator.ad*
GENERATED_FILES += cosnaming-bindingiterator-proxy.ad*
GENERATED_FILES += cosnaming-bindingiterator-stream.ad*
GENERATED_FILES += cosnaming-bindingiterator-skel.ad*
GENERATED_FILES += cosnaming-bindingiterator_forward.ads
GENERATED_FILES += cosnaming-stream.ad*
GENERATED_FILES += cosnaming-namingcontext.ad*
GENERATED_FILES += cosnaming-namingcontext-proxy.ad*
GENERATED_FILES += cosnaming-namingcontext-stream.ad*
GENERATED_FILES += cosnaming-namingcontext-skel.ad*
GENERATED_FILES += cosnaming-namingcontext_forward.ads
GENERATED_FILES += naming_idl_file.ads
GENERATED_FILES += echo_idl_file.ads
GENERATED_FILES += cosnaming.ads


ada:: echo.ads cosnaming.ads

cosnaming.ads::
	$(EXPORT_TREE)/$(BINDIR)/adabroker Naming.idl

echo.ads::
	$(EXPORT_TREE)/$(BINDIR)/adabroker echo.idl

clean::
	-rm -f *.o *.ali *~ server client $(GENERATED_FILES)


