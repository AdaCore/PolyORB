
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -gnatf -gnata -m -i client $(FLAGS)
	gnatmake -gnatf -gnata -m -i server $(FLAGS)


GENERATED_FILES = echo.ad*
GENERATED_FILES += echo-proxies.ad*
GENERATED_FILES += echo-marshal.ad*
GENERATED_FILES += echo-skeleton.ad*
GENERATED_FILES += corba_initialreferences.ad*
GENERATED_FILES += corba_initialreferences-proxies.ad*
GENERATED_FILES += corba_initialreferences-marshal.ad*
GENERATED_FILES += corba_initialreferences-skeleton.ad*
GENERATED_FILES += cosnaming-bindingiterator.ad*
GENERATED_FILES += cosnaming-bindingiterator-proxies.ad*
GENERATED_FILES += cosnaming-bindingiterator-marshal.ad*
GENERATED_FILES += cosnaming-bindingiterator-skeleton.ad*
GENERATED_FILES += cosnaming-bindingiterator_forward.ads
GENERATED_FILES += cosnaming-marshal.ad*
GENERATED_FILES += cosnaming-namingcontext.ad*
GENERATED_FILES += cosnaming-namingcontext-proxies.ad*
GENERATED_FILES += cosnaming-namingcontext-marshal.ad*
GENERATED_FILES += cosnaming-namingcontext-skeleton.ad*
GENERATED_FILES += cosnaming-namingcontext_forward.ads
GENERATED_FILES += bootstrap_idl_file.ads
GENERATED_FILES += naming_idl_file.ads
GENERATED_FILES += echo_idl_file.ads
GENERATED_FILES += cosnaming.ads


ada::
	omniidl2 -bada echo.idl
	omniidl2 -bada Naming.idl
	omniidl2 -bada bootstrap.idl

clean::
	rm *.o *.ali *~ server client $(GENERATED_FILES)


