handle SIGWAITING pass noprint nostop
handle SIGABRT pass noprint nostop
b __gnat_raise_constraint_error
