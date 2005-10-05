all:
	@for i in `cat MANIFEST | grep -v '^#'`; do \
	  echo $$i; \
	  ./run-test $$i; \
	done

