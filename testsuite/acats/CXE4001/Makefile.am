DSA_SRC=$(srcdir)/../../../src/dsa

dsa_exe=part1 part2

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(srcdir)/../support -I$(DSA_SRC)

part1_FLAGS=	-margs -Apart1.stubs
part2_FLAGS=	-margs -Apart2.stubs

part1.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.adb
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part1.stubs

part1.stubs/cxe4001_partition_a.ali: $(srcdir)/cxe4001_partition_a.adb
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4001_partition_a.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4001_partition_a.o cxe4001_partition_a.ali part1.stubs

part1.stubs/cxe4001_partition_b.ali: $(srcdir)/cxe4001_partition_b.ads
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4001_partition_b.ads $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzc
	-mv cxe4001_partition_b.o cxe4001_partition_b.ali part1.stubs

part2.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part2.stubs

part2.stubs/cxe4001_partition_a.ali: $(srcdir)/cxe4001_partition_a.ads
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4001_partition_a.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv cxe4001_partition_a.o cxe4001_partition_a.ali part2.stubs

part2.stubs/cxe4001_partition_b.ali: $(srcdir)/cxe4001_partition_b.adb
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4001_partition_b.adb $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzr
	-mv cxe4001_partition_b.o cxe4001_partition_b.ali part2.stubs

cxe4001_decl_pure.ali: $(srcdir)/cxe4001_decl_pure.ads
	$(GNATMAKE_FOR_TARGET) -a -u $(srcdir)/cxe4001_decl_pure.ads $(GNATMAKE_FLAGS)

part1: part1.stubs/polyorb-dsa_p-partitions.ali part1.stubs/cxe4001_partition_a.ali part1.stubs/cxe4001_partition_b.ali cxe4001_decl_pure.ali
part2: part2.stubs/polyorb-dsa_p-partitions.ali part2.stubs/cxe4001_partition_a.ali part2.stubs/cxe4001_partition_b.ali cxe4001_decl_pure.ali

clean: clean-stubs clean-am
clean-stubs:
	rm -fr part1.stubs part2.stubs

include $(srcdir)/../../Makefile.common
