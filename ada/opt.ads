------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

--  This package contains global switches set by the initialization
--  routine from the command line and referenced throughout the compiler,
--  the binder or gnatmake. The comments indicate which options are used by
--  which programs (GNAT, GNATF, GNATBIND, GNATMAKE).

with Hostparm;       use Hostparm;
with Types;          use Types;
with System.WCh_Con; use System.WCh_Con;

package Opt is

   Ada_83_Switch : Boolean := False;
   --  GNAT, GNATF
   --  This is the value of the command line switch for Ada 83 mode. At the
   --  start of compiling a unit, Ada_95 and Ada_83 are set from this value
   --  but then they can be subsequently modified by pragmas Ada_83, Ada_95.

   Ada_95 : Boolean := True;
   --  GNAT, GNATF
   --  Set True if operating in Ada 95 mode
   --  Set False if operating in Ada 83 mode

   Ada_83 : Boolean := False;
   --  GNAT, GNATF
   --  Set True if operating in Ada 83 mode
   --  Set False if operating in Ada 95 mode

   All_Errors_Mode : Boolean := False;
   --  GNAT, GNATF
   --  Flag set to force display of multiple errors on a single line and
   --  also repeated error messages for references to undefined identifiers
   --  and certain other repeated error messages.

   All_Sources : Boolean := False;
   --  GNATBIND
   --  Set to True to require all source files to be present. This flag is
   --  directly modified by gnatmake to affect the shared binder routines.

   Assertions_Enabled : Boolean := False;
   --  GNAT, GNATF
   --  Enable assertions made using pragma Assert. Used only by GNATF, ignored
   --  by GNAT1 (actually the pragmas are still processed, but if no code is
   --  generated, they have no effect).

   Bind_Main_Program : Boolean := True;
   --  GNATBIND
   --  Set to False if not binding main Ada program.

   Bind_Alternate_Main_Name : Boolean := False;
   --  GNATBIND
   --  Set to True if main should be called gnat_main.

   Brief_Output : Boolean := False;
   --  GNAT, GNATF, GNATBIND
   --  Force brief error messages to standard error, even if verbose mode is
   --  set (so that main error messages go to standard output).

   Check_Internal_Files : Boolean := False;
   --  GNATMAKE
   --  Set to True to check GNAT internal files during the make process.

   Check_Object_Consistency : Boolean := False;
   --  GNATBIND, GNATMAKE
   --  Set to True to check whether every object file is consistent with
   --  with its corresponding ada library information (ali) file. An object
   --  file is inconsistent with the corresponding ali file if the object
   --  file does not exist or if it has an older time stamp than the ali file.

   Check_Only : Boolean := False;
   --  GNATBIND
   --  Set to True to do checks only, no output of binder file.

   Check_Source_Files : Boolean := True;
   --  GNATBIND
   --  Set to True to enable consistency checking for any source files that
   --  are present (i.e. date must match the date in the library info file).
   --  Set to False for object file consistency check only. This flag is
   --  directly modified by gnatmake, to affect the shared binder routines.

   Compile_Only : Boolean := False;
   --  GNATMAKE
   --  Set to True to skip bind and link step.

   type Distribution_Stub_Mode_Type is
   --  GNAT
     (No_Stubs,
      --  Normal mode, no generation/compilation of distribution stubs

      Compile_Receiver_Stub_Spec,
      --  The unit being compiled is the body of the receiver stubs, and
      --  the corresponding spec will be generated appropriately.

      Compile_Caller_Stub_Spec,
      --  The unit being compiled is the body of the caller stubs, and
      --  the corresponding spec will be generated appropriately

      Generate_Receiver_Stub_Body,
      --  The unit being compiled is the RCI spec, and the compiler will
      --  generate the body for the receiver stubs.

      Generate_Caller_Stub_Body);
      --  The unit being compiled is the RCI spec, and the compiler will
      --  generate the body for the caller stubs.

   subtype Debug_Level_Value is Nat range 0 .. 3;
   Debugger_Level : Debug_Level_Value := 0;
   --  GNATBIND
   --  The value given to the -g parameter.
   --  The default value for -g with no value is 2
   --  This is usually ignored by GNATBIND, except in the VMS version
   --  where it is passed as an argument to __gnat_initialize to trigger
   --  the activation of the remote debugging interface.

   Distribution_Stub_Mode : Distribution_Stub_Mode_Type := No_Stubs;
   --  GNAT
   --  This enumeration variable indicates the five states of distribution
   --  annex stub generation/compilation.

   Dont_Execute : Boolean := False;
   --  GNATMAKE
   --  Set to True if no actual compilations should be undertaken.

   Elab_Dependency_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output complete list of elaboration constraints

   Elab_Order_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output chosen elaboration order

   Elab_Warnings : Boolean := False;
   --  GNAT, GNATF
   --  Set to True to generate full elaboration warnings (-gnatwl)

   Force_Compilations : Boolean := False;
   --  GNATMAKE
   --  Set to force recompilations even when the objects are up-to-date.

   Full_List : Boolean := False;
   --  GNAT, GNATF
   --  Set True to generate full source listing with embedded errors

   type Float_Format_Type is (IEEE, VAX);
   Float_Format : Float_Format_Type :=
                    Float_Format_Type'Val (Boolean'Pos (OpenVMS));
   --  Indicates Float_Format in use. The value is initialized to
   --  IEEE in all environnments except OpenVMS where the default
   --  is VAX. The value can be changed by use of the configuration
   --  pragma Float_Representation (but the value VAX is only possible
   --  on OpenVMS versions of GNAT).

   type Float_Format_Long_Type is (D_Float, G_Float);
   Float_Format_Long : Float_Format_Long_Type := G_Float;
   --  This indicates the representation used on OpenVMS systems for
   --  Stadard.Long_Float if Float_Format is set to VAX (the setting
   --  of this variable is irrelevant if Float_Format is set to IEEE).
   --  The setting may be changed by the use of the configuraqtion
   --  pragma Long_Float.

   Full_Elaboration_Semantics : Boolean := False;
   --  GNATBIND
   --  True if binding with full Ada elaboration semantics (-f switch set)

   GNAT_Mode : Boolean := False;
   --  GNAT, GNATF
   --  True if compiling in GNAT system mode (-g switch set)

   Horrible_Elab_Order : Boolean := False;
   --  GNATBIND
   --  True if horrible elaboration order is to be chosen (-h switch set)

   Identifier_Character_Set : Character;
   --  GNAT, GNATF
   --  This variable indicates the character set to be used for identifiers.
   --  The possible settings are:
   --    '1'  Latin-1
   --    '2'  Latin-2
   --    '3'  Latin-3
   --    '4'  Latin-4
   --    'p'  PC (US, IBM page 437)
   --    '8'  PC (European, IBM page 850)
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada/83 rules)
   --    'w'  Latin-1 plus wide characters allowed in identifiers
   --
   --  The setting affects the set of letters allowed in identifiers and the
   --  upper/lower case equivalences. It does not affect the interpretation of
   --  character and string literals, which are always stored using the actual
   --  coding in the source program. This variable is initialized to the
   --  default value appropriate to the system (in Osint.Initialize), and then
   --  reset if a command line switch is used to change the setting.

   Ignore_Time_Stamp_Errors : Boolean := False;
   --  GNATBIND
   --  Ignore time stamp mismatch errors (treat as warnings only)

   Immediate_Errors : Boolean := False;
   --  GNAT, GNATF
   --  If set to True, then error messages are output as soon as they are
   --  detected (useful for navigating around compiler error situations)

   Inline_Active : Boolean := False;
   --  GNAT, GNATF
   --  Set True to activate pragma Inline processing across modules. Default
   --  for now is not to inline across module boundaries.
   --  Used by GNAT1, ignored by GNATF.

   Inline_All : Boolean := False;
   --  GNAT, GNATF
   --  Set True to activate Inline processing across modules for all
   --  subprograms, regardless of individual pragmas.
   --  Used by GNAT1, ignored by GNATF.

   Keep_Going : Boolean := False;
   --  GNATMAKE
   --  When True signals gnatmake to ignore compilation errors and keep
   --  processing sources until there is no more work.

   List_Units : Boolean := False;
   --  GNAT, GNATF
   --  List units in the active lbrary

   List_Dependencies : Boolean := False;
   --  GNATMAKE
   --  When True gnatmake verifies that the objects are up to date and
   --  outputs the list of object dependencies. This list can be used
   --  directly in a Makefile.

   Locking_Policy : Character := ' ';
   --  GNAT1, GNATF, GNATBIND
   --  Set to ' ' for the default case (no locking policy specified). Reset to
   --  first character (upper case) of locking policy name if a valid pragma
   --  Locking_Policy is encountered. In the binder, this is set to a non-blank
   --  value if any unit specifies a policy.

   Look_In_Primary_Dir : Boolean := True;
   --  GNAT, GNATF, GNATBIND, GNATMAKE
   --  Set to False if a -I- was present on the command line.
   --  When True we are allowed to look in the primary directory to locate
   --  other source or library files.

   Maximum_Errors : Int := 9999;
   --  GNAT, GNATF, GNATBIND
   --  Maximum number of errors before compilation is terminated

   Maximum_File_Name_Length : Int;
   --  GNAT, GNATF, GNATBIND
   --  Maximum number of characters allowed in a file name, not counting the
   --  extension, as set by the appropriate switch. If no switch is given,
   --  then this value is initialized by Osint to the appropriate value.

   Maximum_Processes : Positive := 1;
   --  GNATMAKE
   --  Maximum number of processes that should be spawned to carry out
   --  compilations.

   Normalize_Scalars : Boolean := False;
   --  GNAT
   --  Set true it a pragma Normalize_Scalars applies to the current unit

   type Operating_Mode_Type is (Check_Syntax, Check_Semantics, Generate_Code);
   Operating_Mode : Operating_Mode_Type := Generate_Code;
   --  GNAT, GNATF
   --  Indicates the operating mode of the compiler. The default is generate
   --  code, which runs the parser, semantics and backend. Switches can be
   --  used to set syntax checking only mode, or syntax and semantics checking
   --  only mode. Operating_Mode can also be modified as a result of detecting
   --  errors during the compilation process. In particular if any error is
   --  detected then this flag is reset from Generate_Code to Check_Semantics
   --  after generating an error message. For GNATF, Generate_Code is treated
   --  as equivalent to Check_Semantics.

   Output_Filename_Present : Boolean := False;
   --  GNATBIND
   --  Set to True when the output C filename is given with option -o

   Queuing_Policy : Character := ' ';
   --  GNAT1, GNATF, GNATBIND
   --  Set to ' ' for the default case (no queuing policy specified). Reset to
   --  Reset to first character (upper case) of locking policy name if a valid
   --  Queuing_Policy pragma is encountered. In the binder, this is set to a
   --  non-blank value if any unit specifies a policy.

   Quiet_Output : Boolean := False;
   --  GNATMAKE
   --  Set to True if the list of compilation commands should not be output.

   RM_Column_Check : Boolean := False;
   --  GNAT, GNATF
   --  Flag set to cause column alignment to be taken into account in
   --  determining legality of various constructs, using the layout rules
   --  specified in the RM.

   Smart_Compilations : Boolean := False;
   --  GNATMAKE
   --  Set to True if smart recompilations requested.

   Software_Overflow_Checking : Boolean;
   --  GNAT
   --  Set to True by Osint.Initialize if the target requires the software
   --  approach to integer arithmetic overflow checking (i.e. the use of
   --  double length arithmetic followed by a range check). Set to False
   --  if the target implements hardware overflow checking. Used only by
   --  GNAT1, not used by GNATF.

   Strict_Math : aliased Boolean := False;
   --  GNAT, GNATF
   --  This switch is set True if the current unit is to be compiled in
   --  strict math mode. The effect is to cause certain library file name
   --  substitutions to implement strict math semantics. See the routine
   --  Adjust_File_Name_For_Configuration, and also the configuration
   --  in the body of Opt.
   --
   --  Note: currently this switch is always False. Eventually it will be
   --  settable by a switch and a configuration pragma.

   Style_Check : Boolean := False;
   --  GNAT, GNATF
   --  Set True to perform style checks. Activates checks carried out
   --  in package Style (see body of this package for details of checks)

   System_Extend_Pragma_Arg : Node_Id := Empty;
   --  Set non-empty if and only if a correct Extend_System pragma was present
   --  in which case it points to the argument of the pragma, and the name can
   --  be located as Chars (Expression (System_Extend_Pragma_Arg)).

   Subunits_Missing : Boolean := False;
   --  This flag is set true if missing subunits are detected with code
   --  generation active. This causes code generation to be skipped.

   Suppress_Options : Suppress_Record;
   --  GNAT, GNATF
   --  Flags set True to suppress corresponding check, i.e. add an implicit
   --  pragma Suppress at the outer level of each unit compiled. Note that
   --  these suppress actions can be overridden by the use of the Unsuppress
   --  pragma. This variable is initialized by Osint.Initialize.

   Table_Factor : Int := 1;
   --  Factor by which all initial table sizes set in Alloc are multiplied.
   --  Used in Table to calculate initial table sizes (the initial table
   --  size is the value in Alloc, used as the Table_Initial parameter
   --  value, multiplied by the factor given here. The default value is
   --  used if no -gnatT switch appears.

   Task_Dispatching_Policy : Character := ' ';
   --  GNAT1, GNATF, GNATBIND
   --  Set to ' ' for the default case (no task dispatching policy specified).
   --  Reset to first character (upper case) of task dispatching policy name
   --  if a valid Task_Dispatching_Policy pragma is encountered. In the binder,
   --  this is set to a non-blank value if any unit specifies a policy.

   Tasking_Used : Boolean := False;
   --  Set True if any tasking construct is encountered. Used to activate the
   --  output of the Q line in ali files.

   Time_Slice_Set : Boolean := False;
   --  Set True if a pragma Time_Slice is processed in the main unit.

   Time_Slice_Value : Nat;
   --  Time slice value. Valid only if Time_Slice_Set is True, i.e. if a
   --  Time_Slice pragma has been processed. Set to the time slice value
   --  in microseconds. Negative values are stored as zero, and the value
   --  is not larger than 1_000_000_000 (1000 seconds). Values larger than
   --  this are reset to this maximum.

   Tree_Output : Boolean := False;
   --  GNAT, GNATF
   --  Set True to generate output tree file

   Try_Semantics : Boolean := False;
   --  GNAT, GNATF
   --  Flag set to force attempt at semantic analysis, even if parser errors
   --  occur. This will probably cause blowups at this stage in the game. On
   --  the other hand, most such blowups will be caught cleanly and simply
   --  say compilation abandoned.

   Upper_Half_Encoding : Boolean := False;
   --  GNAT, GNATF
   --  Normally set False, indicating that upper half ASCII characters are
   --  used in the normal way to represent themselves. If the wide character
   --  encoding method uses the upper bit for this encoding, then this flag
   --  is set True, and upper half characters in the source indicate the
   --  start of a wide character sequence.

   Verbose_Mode : Boolean := False;
   --  GNAT, GNATF, GNATBIND
   --  Set to True to get verbose mode (full error message text and location
   --  information sent to standard output, also header, copyright and summary)

   type Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   Warning_Mode : Warning_Mode_Type := Normal;
   --  GNAT, GNATF, GNATBIND
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, warning messages
   --  are generated and are treated as errors.

   Wide_Character_Encoding_Method : WC_Encoding_Method := WCEM_None;
   --  GNAT, GNATF
   --  Method used for encoding wide characters in the source program. See
   --  description of type in unit System.WCh_Con for a list of the methods
   --  that are currently supported. The default mode (WCEM_None) prohibits
   --  wide character encodings from appearing in the program completely.

   Xref_Analyze : Boolean := False;
   --  GNAT, GNATF
   --  This flag is used to indicate to semantic analyzer that the current
   --  compilation is done for GNATF. So the expander mustn't be called.

   Xref_Flag_1 : Boolean := False;
   --  GNATF
   --  Set to generate warning messages for unused with clauses.

   Xref_Flag_2 : Boolean := False;
   --  GNATF
   --  Set to generate warning messages for unused entities (including
   --  unused with clauses).

   Xref_Flag_3 : Boolean := False;
   --  GNATF
   --  Set to generate cross-reference file listing all references in the
   --  compiled files (also generates warning messages described above).

   Xref_Flag_4 : Boolean := False;
   --  GNATF
   --  Set to include in the reference list all informations about entities
   --  declared in bodies if the corresponding spec declares inlined
   --  subprograms or generics. Includes effects of Xref_Flag_1,2,3).

   Xref_Flag_5 : Boolean := False;
   --  GNATF
   --  Set to include all information in cross-reference listing.
   --  (includes effects of Xref_Flag_1,2,3 described above).

   Xref_Flag_6 : Boolean := False;
   --  GNATF
   --  Same thing as Xef_Flag_5 except that a global xref file is generated

   Xref_Flag_9 : Boolean := False;
   --  GNAT, GNATF
   --  Set to generate a cross-reference listing of Ada 95 features used. This
   --  listing is sorted by category and output to the standard output file.

   Xref_Flag_B : Boolean := False;
   --  GNATF
   --  If set, cross-reference file includes information on required interfaces
   --  for library unit bodies.

   Xref_Flag_S : Boolean := False;
   --  GNATF
   --  If set, cross-reference file includes information on required interfaces
   --  for library package specs.

   -----------------------
   -- Tree I/O Routines --
   -----------------------

   procedure Tree_Read;
   --  Reads switch settings from current tree file using Tree_Read

   procedure Tree_Write;
   --  Writes out switch settings to current tree file using Tree_Write

end Opt;
