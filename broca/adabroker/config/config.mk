TOP = $(ADABROKER_TOP)/omniORB_2.7.1
CURRENT = ../$(ADABROKER_CURRENT)

ADABROKER_PLATFORM = $(ADABROKER_TOP)/mk/platform

ADABROKER_FLAGS = -largs -ladabroker -lstdc++

include $(TOP)/config/config.mk
