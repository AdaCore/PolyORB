IDL_INTERFACE = all_types
ADA_FLAGS+=-gnatao

GENERATED_FILES =\
all_types_idl_file.ads\
all_types-skel.adb\
all_types-skel.ads\
all_types-stream.adb\
all_types-stream.ads\
all_types.adb\
all_types.ads 

include ../generic/dir.mk
