#if ENABLE_ASIS
#CIAO=	ciao
#endif

if ENABLE_SOAP
SOAP_DIR= gnatprfh 
endif

if ENABLE_CORBA
CORBA_DIR= idlac 
endif

SUBDIRS= $(CORBA_DIR) $(SOAP_DIR)
# $(CIAO)
ADACOMPILER=../support/native-adacompiler