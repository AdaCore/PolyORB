IDL_INTERFACE = all_functions
ADA_FLAGS+=-gnatao

GENERATED_FILES =\
all_functions_idl_file.ads\
all_functions-skel.adb\
all_functions-skel.ads\
all_functions-stream.adb\
all_functions-stream.ads\
all_functions.adb\
all_functions.ads

include ../generic/dir.mk
