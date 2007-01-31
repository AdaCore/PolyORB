EXAMPLE=	echo

corba_exe=client server
dsa_exe=
moma_exe=

IDLAC_FLAGS=	-d

include $(srcdir)/../../Makefile.common

noinst_DATA=gssup.conf tls_gssup.conf root.crt polyorb.key polyorb.crt
noinst_SCRIPTS=gssup_example tls_example tls_gssup_example

root.crt polyorb.key polyorb.crt:
	rm -rf .ca
	mkdir .ca
	touch .ca/index.txt
	echo "01" > .ca/serial
	openssl genrsa -out root.key 2048
	openssl req -new -x509 -key root.key -out root.crt \
		-config $(srcdir)/ca_openssl.conf
	openssl genrsa -out polyorb.key 2048
	openssl req -new -key polyorb.key -out polyorb.req \
		-config $(srcdir)/polyorb_openssl.conf
	echo -e "y\ny" | openssl ca -keyfile root.key -cert root.crt \
		-in polyorb.req -out polyorb.crt \
		-config $(srcdir)/ca_openssl.conf || true
	rm -rf .ca
