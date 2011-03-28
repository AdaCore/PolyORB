handle SIGWAITING pass noprint nostop
handle SIGABRT pass noprint nostop
handle SIGUSR1 pass noprint nostop
b __gnat_raise
