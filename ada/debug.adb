------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                D E B U G                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body Debug is

   ---------------------------------
   -- Summary of Debug Flag Usage --
   ---------------------------------

   --  Debug flags for compiler (GNAT1 and GNATF)

   --  da   Generate messages tracking semantic analyzer progress
   --  db   Show encoding of type names for debug output
   --  dc   List names of units as they are compiled
   --  dd   Dynamic allocation of tables messages generated
   --  de   List the entity table
   --  df   Full tree/source print (includes withed units)
   --  dg   Print source from tree (generated code only)
   --  dh   Generate listing showing loading of name table hash chains
   --  di   Generate messages for visibility linking/delinking
   --  dj   Suppress "junk null check" for access parameter values
   --  dk   Generate GNATBUG message on abort, even if previous errors
   --  dl   Generate unit load trace messages
   --  dm   Allow VMS features even if not OpenVMS version
   --  dn   Generate messages for node/list allocation
   --  do   Print source from tree (original code only)
   --  dp   Generate messages for parser scope stack push/pops
   --  dq
   --  dr   Generate parser resynchronization messages
   --  ds   Print source from tree (including original and generated stuff)
   --  dt   Print full tree
   --  du   Uncheck categorization pragmas
   --  dv   Output trace of overload resolution
   --  dw   Print trace of semantic scope stack
   --  dx   Force expansion on, even if no code being generated
   --  dy   Print tree of package Standard
   --  dz   Print source of package Standard

   --  dA
   --  dB
   --  dC
   --  dD
   --  dE
   --  dF
   --  dG   Compile generics
   --  dH
   --  dI
   --  dJ
   --  dK
   --  dL
   --  dM
   --  dN
   --  dO
   --  dP
   --  dQ
   --  dR
   --  dS
   --  dT
   --  dU
   --  dV
   --  dW
   --  dX
   --  dY
   --  dZ

   --  d1   Error msgs have node numbers where possible
   --  d2   Eliminate error flags in verbose form error messages
   --  d3   Dump bad node in Comperr on an abort
   --  d4   Inhibit automatic krunch of predefined library unit files
   --  d5   Debug output for tree read/write
   --  d6   Default access unconstrained to thin pointers
   --  d7   Do not output version & file time stamp in -gnatv or -gnatl mode
   --  d8   Force opposite endianness in packed stuff
   --  d9

   --  Debug flags for binder (GNATBIND)

   --  da
   --  db
   --  dc  List units as they are chosen
   --  dd
   --  de
   --  df
   --  dg
   --  dh
   --  di
   --  dj
   --  dk
   --  dl
   --  dm
   --  dn  List details of manipulation of Num_Pred values
   --  do
   --  dp
   --  dq
   --  dr
   --  ds
   --  dt
   --  du
   --  dv
   --  dw
   --  dx
   --  dy
   --  dz

   --  d1
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   --  Debug flags used in package Make and its clients (e.g. GNATMAKE)

   --  da
   --  db
   --  dc
   --  dd
   --  de
   --  df
   --  dg
   --  dh
   --  di
   --  dj
   --  dk
   --  dl
   --  dm
   --  dn
   --  do
   --  dp  Prints the contents of the Q used by Make.Compile_Sources
   --  dq  Prints source files as they are enqueued and dequeued
   --  dr
   --  ds
   --  dt
   --  du
   --  dv
   --  dw  Prints the list of units withed by the unit currently explored
   --  dx
   --  dy
   --  dz

   --  d1
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   --------------------------------------------
   -- Documentation for Compiler Debug Flags --
   --------------------------------------------

   --  da   Generate messages tracking semantic analyzer progress. A message
   --       is output showing each node as it gets analyzed, expanded,
   --       resolved, or evaluated. This option is useful for finding out
   --       exactly where a bomb during semantic analysis is occurring.

   --  db   In Exp_Dbug, certain type names are encoded to include debugging
   --       information. This debug switch causes lines to be output showing
   --       the encodings used.

   --  dc   List names of units as they are compiled. One line of output will
   --       be generated at the start of compiling each unit (package or
   --       subprogram).

   --  dd   Dynamic allocation of tables messages generated. Each time a
   --       table is reallocated, a line is output indicating the expansion.

   --  de   List the entity table

   --  df   Full tree/source print (includes withed units). Normally the tree
   --       output (dt) or recreated source output (dg,do,ds) includes only
   --       the main unit. If df is set, then the output in either case
   --       includes all compiled units (see also dg,do,ds,dt). Note that to
   --       be effective, this swich must be used in combination with one or
   --       more of dt, dg, do or ds.

   --  dg   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten this output includes only the
   --       generated code, not the original code (see also df,do,ds,dz).

   --  dh   Generates a table at the end of a compilation showing how the hash
   --       table chains built by the Namet package are loaded. This is useful
   --       in ensuring that the hashing algorithm (in Namet.Hash) is working
   --       effectively with typical sets of program identifiers.

   --  di   Generate messages for visibility linking/delinking

   --  dj   Suppress "junk null check" for access parameters. This flag permits
   --       Ada programs to pass null parameters to access parameters, and to
   --       explicitly check such access values against the null literal.
   --       Neither of these is valid Ada, but both were allowed in versions of
   --       GNAT before 3.10, so this switch can ease the transition process.

   --  dk   Immediate kill on abort. Normally on an abort (i.e. a call to
   --       Comperr.Compiler_Abort), the GNATBUG message is not given if
   --       there is a previous error. This debug switch bypasses this test
   --       and gives the message unconditionally (useful for debugging).

   --  dl   Generate unit load trace messages. A line of traceback output is
   --       generated each time a request is made to the library manager to
   --       load a new unit.

   --  dm   Some features are permitted only in OpenVMS ports of GNAT (e.g.
   --       the specification of passing by descriptor). Normally any use
   --       of these features will be flagged as an error, but this debug
   --       flag allows acceptance of these features in non OpenVMS ports.
   --       Of course they may not have any useful effect, and in particular
   --       attempting to generate code with this flag set may blow up.
   --       The flag also forces the use of 64-bits for Long_Integer.

   --  dn   Generate messages for node/list allocation. Each time a node or
   --       list header is allocated, a line of output is generated. Certain
   --       other basic tree operations also cause a line of output to be
   --       generated. This option is useful in seeing where the parser is
   --       blowing up.;

   --  do   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten, this output includes only the
   --       original code, not the generated code (see also df,dg,ds,dz).

   --  dp   Generate messages for parser scope stack push/pops. A line of
   --       output by the parser each time the parser scope stack is either
   --       pushed or popped. Useful in debugging situations where the
   --       parser scope stack ends up incorrectly synchronized

   --  dr   Generate parser resynchronization messages. Normally the parser
   --       resynchronizes quietly. With this debug option, two messages
   --       are generated, one when the parser starts a resynchronization
   --       skip, and another when it resumes parsing. Useful in debugging
   --       inadequate error recovery situations.

   --  ds   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten this output includes both the
   --       generated code and the original code with the generated code
   --       being enlosed in curly brackets (see also df,do,ds,dz)

   --  dt   Print full tree. The generated tree is output (see also df,dy)

   --  du   Uncheck categorization pragmas. This debug switch causes the
   --       categorization pragmas (Pure, Preelaborate etc) to be ignored
   --       so that normal checks are not made (this is particularly useful
   --       for adding temporary debugging code to units that have pragmas
   --       that are inconsistent with the debugging code added.

   --  dw   Write semantic scope stack messages. Each time a scope is created
   --       or removed, a message is output (see the Sem_Ch8.New_Scope and
   --       Sem_Ch8.Pop_Scope subprograms).

   --  dx   Force expansion on, even if no code being generated. Normally the
   --       expander is inhibited if no code is generated. This switch forces
   --       expansion to proceed normally even if the backend is not being
   --       called. This is particularly useful for debugging purposes when
   --       using the front-end only version of the compiler (which normally
   --       would never do any expansion).

   --  dy   Print tree of package Standard. Normally the tree print out does
   --       not include package Standard, even if the -df switch is set. This
   --       switch forces output of the internal tree built for Standard.

   --  dz   Print source of package Standard. Normally the source print out
   --       does not include package Standard, even if the -df switch is set.
   --       This switch forces output of the source recreated from the internal
   --       tree built for Standard.

   --  dG   Compile generics. Normally generics are not compiled, this switch
   --       causes them to be compiled and treated as first class citizens
   --       from the point of view of binding, elaboration order choice etc.

   --  d1   Error msgs have node numbers where possible. Normally error
   --       messages have only source locations. This option is useful when
   --       debugging errors caused by expanded code, where the source location
   --       does not give enough information.

   --  d2   Suppress output of the error position flags for verbose form error
   --       messages. The messages are still interspersed in the listing, but
   --       without any error flags or extra blank lines. Also causes an extra
   --       <<< to be output at the right margin. This is intended to be the
   --       easiest format for checking conformance of ACVC B tests.

   --  d3   Causes Comperr to dump the contents of the node for which an abort
   --       was detected (normally only the Node_Id of the node is output).

   --  d4   Inhibits automatic krunching of predefined library unit file names.
   --       Normally, as described in the spec of package Krunch, such files
   --       are automatically krunched to 8 characters, with special treatment
   --       of the prefixes Ada, System, and Interfaces. Setting this debug
   --       switch disables this special treatment.

   --  d6   Normally access-to-unconstrained-array types are represented
   --       using fat (double) pointers. Using this debug flag causes them
   --       to default to thin. This can be used to test the performance
   --       implications of using thin pointers, and also to test that the
   --       compiler functions correctly with this choice.

   --  d7   Normally a -gnatl or -gnatv listing includes the time stamp
   --       of the source file. This debug flag suppresses this output,
   --       and also suppresses the message with the version number.
   --       This is useful in certain regression tests.

   --  d8   This forces the packed stuff to generate code assuming the
   --       opposite endianness from the actual correct value. Useful in
   --       testing out code generation from the packed routines.

   ------------------------------------------
   -- Documentation for Binder Debug Flags --
   ------------------------------------------

   --  dc  List units as they are chosen. As units are selected for addition to
   --      the elaboration order, a line of output is generated showing which
   --      unit has been selected.

   --  dn  List details of manipulation of Num_Pred values during execution of
   --      the algorithm used to determine a correct order of elaboration. This
   --      is useful in diagnosing any problems in its behavior.

   ------------------------------------------------------------
   -- Documentation for the Debug Flags used in package Make --
   ------------------------------------------------------------

   --  Please note that such flags apply to all of Make clients,
   --  such as gnatmake.

   --  dp  Prints the Q used by routine Make.Compile_Sources every time
   --      we go around the main compile loop of Make.Compile_Sources

   --  dq  Prints source files as they are enqueued and dequeued in the Q
   --      used by routine Make.Compile_Sources. Useful to figure out the
   --      order in which sources are recompiled.

   --  dw  Prints the list of units withed by the unit currently explored
   --      during the main loop of Make.Compile_Sources.

   ----------------------
   -- Get_Debug_Flag_K --
   ----------------------

   function Get_Debug_Flag_K return Boolean is
   begin
      return Debug_Flag_K;
   end Get_Debug_Flag_K;

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';
      subtype ULet is Character range 'A' .. 'Z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' => Debug_Flag_AA := Val;
            when 'B' => Debug_Flag_BB := Val;
            when 'C' => Debug_Flag_CC := Val;
            when 'D' => Debug_Flag_DD := Val;
            when 'E' => Debug_Flag_EE := Val;
            when 'F' => Debug_Flag_FF := Val;
            when 'G' => Debug_Flag_GG := Val;
            when 'H' => Debug_Flag_HH := Val;
            when 'I' => Debug_Flag_II := Val;
            when 'J' => Debug_Flag_JJ := Val;
            when 'K' => Debug_Flag_KK := Val;
            when 'L' => Debug_Flag_LL := Val;
            when 'M' => Debug_Flag_MM := Val;
            when 'N' => Debug_Flag_NN := Val;
            when 'O' => Debug_Flag_OO := Val;
            when 'P' => Debug_Flag_PP := Val;
            when 'Q' => Debug_Flag_QQ := Val;
            when 'R' => Debug_Flag_RR := Val;
            when 'S' => Debug_Flag_SS := Val;
            when 'T' => Debug_Flag_TT := Val;
            when 'U' => Debug_Flag_UU := Val;
            when 'V' => Debug_Flag_VV := Val;
            when 'W' => Debug_Flag_WW := Val;
            when 'X' => Debug_Flag_XX := Val;
            when 'Y' => Debug_Flag_YY := Val;
            when 'Z' => Debug_Flag_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;
      end if;
   end Set_Debug_Flag;

end Debug;
