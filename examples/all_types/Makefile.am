EXAMPLES_ROOT=..
EXAMPLE = all_types

EXTRA = dynclient client_moma $(IR_SERVER)

ADACOMPILER=../../support/adacompiler
# This example might use the IR: some source files need to be processed
# through gnatprep.

if ENABLE_IR
IR_GNATMAKE_FLAGS=	-I../../cos/ir -I$(srcdir)/../../cos/ir
IR_SERVER=	ir_server
endif

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir)/../../cos/ir $(IR_GNATMAKE_FLAGS)

include $(srcdir)/../Makefile.common
