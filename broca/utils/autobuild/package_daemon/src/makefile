###----------------------------------------------------------------------
### File    : makefile
### Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
### Purpose : Makefile for application package_daemon.
### Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
###----------------------------------------------------------------------

MAKE = make

ERL = erlc

EBIN = ../ebin

######################################################################

PACKAGE_DAEMON_HEADER_FILES = package_daemon_sup.hrl

PACKAGE_DAEMON_SOURCE_FILES = \
	package_daemon.erl package_daemon_sup.erl \
	package_daemon_app.erl package_daemon_server.erl

PACKAGE_DAEMON_OBJECT_FILES = $(PACKAGE_DAEMON_SOURCE_FILES:.erl=.beam)


######################################################################
#
# Transformations 
#

.erl.beam:
	$(ERL) $<

######################################################################



def : package_daemon

package_daemon: $(PACKAGE_DAEMON_OBJECT_FILES)
	cp $(PACKAGE_DAEMON_OBJECT_FILES) $(EBIN)

clean :
	/bin/rm -f $(PACKAGE_DAEMON_OBJECT_FILES)

$(PACKAGE_DAEMON_OBJECT_FILES): $(PACKAGE_DAEMON_HEADER_FILES)

.SUFFIXES : .erl .beam

