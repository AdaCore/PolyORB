SUBDIRS = echo all_types

all::
	@$(MakeSubdirs)

export::
	@$(MakeSubdirs)

clean::
	@$(MakeSubdirs)
	$(RM) generic/*.ali generic/*.o generic/*~
	$(RM) *.ali *.o

