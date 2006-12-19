corba_exe=test002
IDLS=test002_interface.idl
include $(srcdir)/../../../Makefile.common

CLEANFILES += $(idl)_idl_file.ads $(idl)_idl_file.adb $(idl)_idl_file-helper.ads $(idl)_idl_file-helper.adb $(idl).ads $(idl).adb $(idl)-skel.ads $(idl)-skel.adb $(idl)-stream.ads $(idl)-stream.adb $(idl)-helper.ads $(idl)-helper.adb $(idl)-delegate.ads $(idl)-delegate.adb $(idl)-ir_info.adb $(idl)-ir_info.ads $(idl)_idl_file-ir_info.adb $(idl)_idl_file-ir_info.ads

CLEANFILES += $(idl)-testinterface_idl_file.ads $(idl)-testinterface_idl_file.adb $(idl)-testinterface_idl_file-helper.ads $(idl)-testinterface_idl_file-helper.adb $(idl)-testinterface.ads $(idl)-testinterface.adb $(idl)-testinterface-skel.ads $(idl)-testinterface-skel.adb $(idl)-testinterface-stream.ads $(idl)-testinterface-stream.adb $(idl)-testinterface-helper.ads $(idl)-testinterface-helper.adb $(idl)-testinterface-delegate.ads $(idl)-testinterface-delegate.adb $(idl)-testinterface-ir_info.adb $(idl)-testinterface-ir_info.ads $(idl)-testinterface_idl_file-ir_info.adb $(idl)-testinterface_idl_file-ir_info.ads

IDLAC_FLAGS = \
	-I $(top_srcdir)/idls/CORBA_IDL -I $(top_srcdir)/idls/CORBA_PIDL \
	-I $(top_srcdir)/idls/Misc -I $(top_srcdir)/idls/Interop
