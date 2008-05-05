all:
	@for i in `cat MANIFEST | grep -v '^#'`; do \
	  ./run-test.sh $$i; \
	done

