###----------------------------------------------------------------------
### File    : makefile
### Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
### Purpose : Makefile for application build_daemon.
### Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
###----------------------------------------------------------------------

MAKE = make

ERL = erlc

EBIN = ../ebin

######################################################################

BUILD_DAEMON_HEADER_FILES = build_daemon_sup.hrl

BUILD_DAEMON_SOURCE_FILES = \
	build_daemon.erl build_daemon_sup.erl \
	build_daemon_app.erl build_daemon_server.erl

BUILD_DAEMON_OBJECT_FILES = $(BUILD_DAEMON_SOURCE_FILES:.erl=.jam)


######################################################################
#
# Transformations 
#

.erl.jam:
	$(ERL) $<

######################################################################



def : build_daemon

build_daemon: $(BUILD_DAEMON_OBJECT_FILES)
	cp $(BUILD_DAEMON_OBJECT_FILES) $(EBIN)

clean :
	/bin/rm -f $(BUILD_DAEMON_OBJECT_FILES)

$(BUILD_DAEMON_OBJECT_FILES): $(BUILD_DAEMON_HEADER_FILES)

.SUFFIXES : .erl .jam

