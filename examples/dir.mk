SUBDIRS = echo all_types all_exceptions multiple_inheritance all_functions \
forward naming_service tasking dii_echo dii_args dii_all_types

all::
	@$(MakeSubdirs)
