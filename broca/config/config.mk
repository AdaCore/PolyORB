BROCA_ADASOCKET = $(BROCA_TOP)/adasockets/lib/adasockets
GNATMAKE = gnatmake
ADA_FLAGS = -O2 -g -i -m -gnata -gnatf -gnatwu -gnatwl -I$(BROCA_ADASOCKET)
BROCA_FLAGS = $(ADA_FLAGS) -I$(BROCA_TOP)/src -I..

ADABROKER = $(BROCA_TOP)/adabroker/adabroker

LibPattern = lib%.a

ifneq ($(wildcard $(BROCA_TOP)/mk/beforedir.mk),)
include $(BROCA_TOP)/mk/beforedir.mk
endif

include dir.mk

ifneq ($(wildcard $(BROCA_TOP)/mk/afterdir.mk),)
include $(BROCA_TOP)/mk/afterdir.mk
endif

