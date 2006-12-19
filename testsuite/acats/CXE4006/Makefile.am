DSA_SRC=$(srcdir)/../../../src/dsa

dsa_exe=part1 part2

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(srcdir)/../support -I$(DSA_SRC)

part1_FLAGS=	-margs -Apart1.stubs
part2_FLAGS=	-margs -Apart2.stubs

part1.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.adb polyorb-dsa_p.ali s-polint.ali
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part1.stubs

part1.stubs/cxe4006_part_a1.ali: $(srcdir)/cxe4006_part_a1.adb polyorb-dsa_p.ali s-polint.ali
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_a1.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4006_part_a1.o cxe4006_part_a1.ali part1.stubs

part1.stubs/cxe4006_part_a2.ali: $(srcdir)/cxe4006_part_a2.adb polyorb-dsa_p.ali s-polint.ali
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_a2.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4006_part_a2.o cxe4006_part_a2.ali part1.stubs

part1.stubs/cxe4006_part_b.ali: $(srcdir)/cxe4006_part_b.ads polyorb-dsa_p.ali s-polint.ali
	@-mkdir part1.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_b.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv cxe4006_part_b.o cxe4006_part_b.ali part1.stubs

part2.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads polyorb-dsa_p.ali s-polint.ali
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv polyorb-dsa_p-partitions.o polyorb-dsa_p-partitions.ali part2.stubs

part2.stubs/cxe4006_part_a1.ali: $(srcdir)/cxe4006_part_a1.ads polyorb-dsa_p.ali s-polint.ali
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_a1.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc -gnatD
	-mv cxe4006_part_a1.o cxe4006_part_a1.ali part2.stubs

part2.stubs/cxe4006_part_a2.ali: $(srcdir)/cxe4006_part_a2.ads polyorb-dsa_p.ali s-polint.ali
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_a2.ads $(GNATMAKE_FLAGS) $(part2_FLAGS) -cargs -gnatzc
	-mv cxe4006_part_a2.o cxe4006_part_a2.ali part2.stubs

part2.stubs/cxe4006_part_b.ali: $(srcdir)/cxe4006_part_b.adb polyorb-dsa_p.ali s-polint.ali
	@-mkdir part2.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/cxe4006_part_b.adb $(GNATMAKE_FLAGS) $(part1_FLAGS) -cargs -gnatzr
	-mv cxe4006_part_b.o cxe4006_part_b.ali part2.stubs

polyorb-dsa_p.ali: $(DSA_SRC)/polyorb-dsa_p.ads $(DSA_SRC)/polyorb-dsa_p.ads
	$(GNATMAKE_FOR_TARGET) -a -u $(DSA_SRC)/polyorb-dsa_p.ads $(GNATMAKE_FLAGS)
s-polint.ali: $(DSA_SRC)/s-polint.ads $(DSA_SRC)/s-polint.adb
	$(GNATMAKE_FOR_TARGET) -a -u $(DSA_SRC)/s-polint.adb $(GNATMAKE_FLAGS) -cargs -gnata
# -gnatg cannot be used as the moment, because s-polint has a dep
# on generated CosNaming code, which does not support it.

part1: part1.stubs/polyorb-dsa_p-partitions.ali part1.stubs/cxe4006_part_a1.ali part1.stubs/cxe4006_part_a2.ali part1.stubs/cxe4006_part_b.ali
part2: part2.stubs/polyorb-dsa_p-partitions.ali part2.stubs/cxe4006_part_a1.ali part2.stubs/cxe4006_part_a2.ali part2.stubs/cxe4006_part_b.ali
part1 part2: s-polint.ali

clean: clean-stubs clean-am
clean-stubs:
	rm -fr part1.stubs part2.stubs

include $(srcdir)/../../Makefile.common
