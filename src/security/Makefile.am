
SUBDIRS = . gssup x509 tls

include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/security

ADA_SPECS_WITH_BODY = \
	polyorb-asn1.ads \
	polyorb-qos-clients_security.ads \
	polyorb-qos-security_contexts.ads \
	polyorb-qos-targets_security.ads \
	polyorb-qos-transport_contexts.ads \
	polyorb-security-authentication_mechanisms.ads \
	polyorb-security-authority_mechanisms.ads \
	polyorb-security-authorization_elements-unknown.ads \
	polyorb-security-authorization_elements.ads \
	polyorb-security-backward_trust_evaluators.ads \
	polyorb-security-credentials-compound.ads \
	polyorb-security-credentials.ads \
	polyorb-security-exported_names-unknown.ads \
	polyorb-security-exported_names.ads \
	polyorb-security-identities-anonymous.ads \
	polyorb-security-identities-principal_name.ads \
	polyorb-security-identities.ads \
	polyorb-security-security_manager.ads \
	polyorb-security-transport_mechanisms-unprotected.ads \
	polyorb-security-types.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-asn1.ads \
	polyorb-qos-clients_security.ads \
	polyorb-qos-security_contexts.ads \
	polyorb-qos-targets_security.ads \
	polyorb-qos-transport_contexts.ads \
	polyorb-security-authentication_mechanisms.ads \
	polyorb-security-authority_mechanisms.ads \
	polyorb-security-authorization_elements-unknown.ads \
	polyorb-security-authorization_elements.ads \
	polyorb-security-backward_trust_evaluators.ads \
	polyorb-security-connections.ads \
	polyorb-security-credentials-compound.ads \
	polyorb-security-credentials.ads \
	polyorb-security-exported_names-unknown.ads \
	polyorb-security-exported_names.ads \
	polyorb-security-forward_trust_evaluators.ads \
	polyorb-security-identities-anonymous.ads \
	polyorb-security-identities-principal_name.ads \
	polyorb-security-identities.ads \
	polyorb-security-security_manager.ads \
	polyorb-security-transport_mechanisms-unprotected.ads \
	polyorb-security-transport_mechanisms.ads \
	polyorb-security-types.ads \
	polyorb-security.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-security.la

libpolyorb_security_la_SOURCES = polyorb_asn1.c
libpolyorb_security_la_LIBADD = $(ADA_LIBADD)
libpolyorb_security_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
