DSA_SRC=$(srcdir)/../../../src/dsa

dsa_exe=part1 part2

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(srcdir)/../support -I$(DSA_SRC)

part1_FLAGS=	-margs -Apart1.stubs
part2_FLAGS=	-margs -Apart2.stubs

part1.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.adb
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part1.stubs

part1.stubs/cxe4002_part_a1.ali: $(srcdir)/cxe4002_part_a1.adb
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4002_part_a1.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4002_part_a1.o cxe4002_part_a1.ali part1.stubs

part1.stubs/cxe4002_part_a2.ali: $(srcdir)/cxe4002_part_a2.adb
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4002_part_a2.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4002_part_a2.o cxe4002_part_a2.ali part1.stubs

part2.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part2.stubs

part2.stubs/cxe4002_part_a1.ali: $(srcdir)/cxe4002_part_a1.ads
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4002_part_a1.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc -gnatD
	-mv cxe4002_part_a1.o cxe4002_part_a1.ali part2.stubs

part2.stubs/cxe4002_part_a2.ali: $(srcdir)/cxe4002_part_a2.ads
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4002_part_a2.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv cxe4002_part_a2.o cxe4002_part_a2.ali part2.stubs

part1: part1.stubs/polyorb-dsa_p-partitions.ali part1.stubs/cxe4002_part_a1.ali part1.stubs/cxe4002_part_a2.ali
part2: part2.stubs/polyorb-dsa_p-partitions.ali part2.stubs/cxe4002_part_a1.ali part2.stubs/cxe4002_part_a2.ali

clean: clean-stubs clean-am
clean-stubs:
	rm -fr part1.stubs part2.stubs

include $(srcdir)/../../Makefile.common
