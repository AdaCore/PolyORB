corba_exe=test000 test001
idl=echo
include $(srcdir)/../../Makefile.common

CLEANFILES += $(idl)_idl_file.ads $(idl)_idl_file.adb $(idl)_idl_file-helper.ads $(idl)_idl_file-helper.adb $(idl).ads $(idl).adb $(idl)-skel.ads $(idl)-skel.adb $(idl)-stream.ads $(idl)-stream.adb $(idl)-helper.ads $(idl)-helper.adb $(idl)-delegate.ads $(idl)-delegate.adb $(idl)-ir_info.adb $(idl)-ir_info.ads $(idl)_idl_file-ir_info.adb $(idl)_idl_file-ir_info.ads

echo.ads: echo.idl
	$(IDLAC) $(IDLAC_FLAGS) -I $(srcdir) \
	-I $(top_srcdir)/idls/CORBA_IDL -I $(top_srcdir)/idls/CORBA_PIDL \
	-I $(top_srcdir)/idls/Misc -I $(top_srcdir)/idls/Interop $<
