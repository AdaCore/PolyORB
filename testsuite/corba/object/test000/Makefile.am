corba_exe=test000_client test000_server
idl=test_interface
include $(srcdir)/../../../Makefile.common

CLEANFILES += $(idl)_idl_file.ads $(idl)_idl_file.adb $(idl)_idl_file-helper.ads $(idl)_idl_file-helper.adb $(idl).ads $(idl).adb $(idl)-skel.ads $(idl)-skel.adb $(idl)-stream.ads $(idl)-stream.adb $(idl)-helper.ads $(idl)-helper.adb $(idl)-delegate.ads $(idl)-delegate.adb $(idl)-ir_info.adb $(idl)-ir_info.ads $(idl)_idl_file-ir_info.adb $(idl)_idl_file-ir_info.ads

test_interface.ads: test_interface.idl
	$(IDLAC_WRAPPER) --idlac=$(IDLAC_bin) $(IDLAC_FLAGS) -I$(srcdir)  $<
