EXAMPLES_ROOT=	..
EXAMPLE=	echo

ERR = $(error PolyORB not configured to generate CORBA applications !)

if ENABLE_CORBA
   ERR=  
endif

EXTRA=		dynclient

IDLAC_FLAGS=	-d

include $(srcdir)/../Makefile.common
