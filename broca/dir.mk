SUBDIRS = adabroker adasockets src examples

all::
	@$(MakeSubdirs)

clean::
	@$(MakeSubdirs)
