###----------------------------------------------------------------------
### File    : makefile
### Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
### Purpose : Makefile for application build_manager.
### Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
###----------------------------------------------------------------------

MAKE = make

ERL = erlc

EBIN = ../ebin

######################################################################

BUILD_MANAGER_HEADER_FILES = build_manager_sup.hrl

BUILD_MANAGER_SOURCE_FILES = \
	build_manager.erl build_manager_sup.erl \
	build_manager_app.erl build_manager_event.erl

BUILD_MANAGER_OBJECT_FILES = $(BUILD_MANAGER_SOURCE_FILES:.erl=.beam)


######################################################################
#
# Transformations 
#

.erl.beam:
	$(ERL) $<

######################################################################



def : build_manager

build_manager: $(BUILD_MANAGER_OBJECT_FILES)
	cp $(BUILD_MANAGER_OBJECT_FILES) $(EBIN)

clean :
	/bin/rm -f $(BUILD_MANAGER_OBJECT_FILES)

$(BUILD_MANAGER_OBJECT_FILES): $(BUILD_MANAGER_HEADER_FILES)

.SUFFIXES : .erl .beam

