------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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
--  which programs (GNAT, GNATBIND, GNATMAKE).

with Hostparm;       use Hostparm;
with Types;          use Types;
with System.WCh_Con; use System.WCh_Con;

package Opt is

   Ada_Bind_File : Boolean := True;
   --  GNATBIND
   --  Set True if binder file to be generated in Ada rather than C

   Ada_83_Switch : Boolean := False;
   --  GNAT
   --  This is the value of the command line switch for Ada 83 mode. At the
   --  start of compiling a unit, Ada_95 and Ada_83 are set from this value
   --  but then they can be subsequently modified by pragmas Ada_83, Ada_95.

   Ada_95 : Boolean := True;
   --  GNAT
   --  Set True if operating in Ada 95 mode
   --  Set False if operating in Ada 83 mode

   Ada_83 : Boolean := False;
   --  GNAT
   --  Set True if operating in Ada 83 mode
   --  Set False if operating in Ada 95 mode

   All_Errors_Mode : Boolean := False;
   --  GNAT
   --  Flag set to force display of multiple errors on a single line and
   --  also repeated error messages for references to undefined identifiers
   --  and certain other repeated error messages.

   All_Sources : Boolean := False;
   --  GNATBIND
   --  Set to True to require all source files to be present. This flag is
   --  directly modified by gnatmake to affect the shared binder routines.

   Alternate_Main_Name : String_Ptr;
   --  Set to non null when Bind_Alternate_Main_Name is True

   Assertions_Enabled : Boolean := False;
   --  GNAT
   --  Enable assertions made using pragma Assert.

   Back_Annotate_Rep_Info : Boolean := False;
   --  GNAT
   --  If set True (by use of -gnatB), enables back annotation of
   --  representation information by gigi, even in -gnatc mode.

   Bind_Main_Program : Boolean := True;
   --  GNATBIND
   --  Set to False if not binding main Ada program.

   Bind_Alternate_Main_Name : Boolean := False;
   --  GNATBIND
   --  Set to True if main should be called Alternate_Main_Name.all

   Brief_Output : Boolean := False;
   --  GNAT, GNATBIND
   --  Force brief error messages to standard error, even if verbose mode is
   --  set (so that main error messages go to standard output).

   Check_Object_Consistency : Boolean := False;
   --  GNATBIND, GNATMAKE
   --  Set to True to check whether every object file is consistent with
   --  with its corresponding ada library information (ali) file. An object
   --  file is inconsistent with the corresponding ali file if the object
   --  file does not exist or if it has an older time stamp than the ali file.

   Check_Only : Boolean := False;
   --  GNATBIND
   --  Set to True to do checks only, no output of binder file.

   Check_Readonly_Files : Boolean := False;
   --  GNATMAKE
   --  Set to True to check readonly files during the make process.

   Check_Source_Files : Boolean := True;
   --  GNATBIND
   --  Set to True to enable consistency checking for any source files that
   --  are present (i.e. date must match the date in the library info file).
   --  Set to False for object file consistency check only. This flag is
   --  directly modified by gnatmake, to affect the shared binder routines.

   Check_Unreferenced : Boolean := False;
   --  GNAT
   --  Set to True to enable checking for unreferenced variables

   Check_Withs : Boolean := False;
   --  GNAT
   --  Set to True to enable checking for unused withs, and also the case
   --  of withing a package and using none of the entities in the package.

   Compile_Only : Boolean := False;
   --  GNATMAKE
   --  Set to True to skip bind and link step.

   Config_File : Boolean := True;
   --  GNAT
   --  Set to False to inhibit reading and processing of gnat.adc file

   subtype Debug_Level_Value is Nat range 0 .. 3;
   Debugger_Level : Debug_Level_Value := 0;
   --  GNATBIND
   --  The value given to the -g parameter.
   --  The default value for -g with no value is 2
   --  This is usually ignored by GNATBIND, except in the VMS version
   --  where it is passed as an argument to __gnat_initialize to trigger
   --  the activation of the remote debugging interface.

   Debug_Generated_Code : Boolean := False;
   --  GNAT
   --  Set True (-gnatD switch) to debug generated expanded code instead
   --  of the original source code. Causes debugging information to be
   --  written with respect to the generated code file that is written.

   type Distribution_Stub_Mode_Type is
   --  GNAT
     (No_Stubs,
      --  Normal mode, no generation/compilation of distribution stubs

      Generate_Receiver_Stub_Body,
      --  The unit being compiled is the RCI body, and the compiler will
      --  generate the body for the receiver stubs and compile it.

      Generate_Caller_Stub_Body);
      --  The unit being compiled is the RCI spec, and the compiler will
      --  generate the body for the caller stubs and compile it.

   Distribution_Stub_Mode : Distribution_Stub_Mode_Type := No_Stubs;
   --  GNAT
   --  This enumeration variable indicates the five states of distribution
   --  annex stub generation/compilation.

   Do_Not_Execute : Boolean := False;
   --  GNATMAKE
   --  Set to True if no actual compilations should be undertaken.

   Dynamic_Elaboration_Checks : Boolean := False;
   --  GNAT
   --  Set True (-gnatE switch) for dynamic inter-unit elaboration checks

   Elab_Dependency_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output complete list of elaboration constraints

   Elab_Order_Output : Boolean := False;
   --  GNATBIND
   --  Set to True to output chosen elaboration order

   Elab_Warnings : Boolean := False;
   --  GNAT
   --  Set to True to generate full elaboration warnings (-gnatwl)

   Exception_Tracebacks : Boolean := False;
   --  GNATBIND
   --  Set to True to store tracebacks in exception occurrences (-E)

   Extensions_Allowed : Boolean := False;
   --  GNAT

   Force_Compilations : Boolean := False;
   --  GNATMAKE
   --  Set to force recompilations even when the objects are up-to-date.

   Full_List : Boolean := False;
   --  GNAT
   --  Set True to generate full source listing with embedded errors

   Float_Format : Character := ' ';
   --  GNAT
   --  A non-blank value indicates that a Float_Format pragma has been
   --  processed, in which case this variable is set to 'I' for IEEE or
   --  to 'V' for VAX. The setting of 'V' is only possible on OpenVMS
   --  versions of GNAT.

   Float_Format_Long : Character := ' ';
   --  GNAT
   --  A non-blank value indicates that a Long_Float pragma has been
   --  processed (this pragma is recognized only in OpenVMS versions
   --  of GNAT), in which case this variable is set to D or G for
   --  D_Float or G_Float.

   Full_Elaboration_Semantics : Boolean := False;
   --  GNATBIND
   --  True if binding with full Ada elaboration semantics (-f switch set)

   Global_Discard_Names : Boolean := False;
   --  GNAT
   --  Set true if a pragma Discard_Names applies to the current unit

   GNAT_Mode : Boolean := False;
   --  GNAT
   --  True if compiling in GNAT system mode (-g switch set)

   HLO_Active : Boolean := False;
   --  GNAT
   --  True if High Level Optimizer is activated

   Identifier_Character_Set : Character;
   --  GNAT
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

   Immediate_Errors : Boolean := False;
   --  GNAT
   --  If set to True, then error messages are output as soon as they are
   --  detected (useful for navigating around compiler error situations)

   Inline_Active : Boolean := False;
   --  GNAT
   --  Set True to activate pragma Inline processing across modules. Default
   --  for now is not to inline across module boundaries.

   Inline_Processing_Required : Boolean := False;
   --  GNAT
   --  Set True if inline processing is required. Inline processing is
   --  required if an active Inline pragma is processed. The flag is set
   --  for a pragma Inline or Inline_Always that is actually active.
   --  This flag is also set True if Inline_All is set.

   Inline_All : Boolean := False;
   --  GNAT
   --  Set True to activate Inline processing across modules for all
   --  subprograms, regardless of whether a pragma Inline appears for
   --  a subprogram or not.

   In_Place_Mode : Boolean := False;
   --  GNATMAKE
   --  Set True to store ALI and object files in place ie in the object
   --  directory if these files already exist or in the source directory
   --  if not.

   Keep_Going : Boolean := False;
   --  GNATMAKE
   --  When True signals gnatmake to ignore compilation errors and keep
   --  processing sources until there is no more work.

   List_Units : Boolean := False;
   --  GNAT
   --  List units in the active lbrary

   List_Dependencies : Boolean := False;
   --  GNATMAKE
   --  When True gnatmake verifies that the objects are up to date and
   --  outputs the list of object dependencies. This list can be used
   --  directly in a Makefile.

   List_Representation_Info : Boolean := False;
   --  GNAT
   --  Set true by -gnatR switch to list representation information

   Locking_Policy : Character := ' ';
   --  GNAT
   --  Set to ' ' for the default case (no locking policy specified). Reset to
   --  first character (upper case) of locking policy name if a valid pragma
   --  Locking_Policy is encountered.

   Look_In_Primary_Dir : Boolean := True;
   --  GNAT, GNATBIND, GNATMAKE
   --  Set to False if a -I- was present on the command line.
   --  When True we are allowed to look in the primary directory to locate
   --  other source or library files.

   Maximum_Errors : Int := 9999;
   --  GNAT, GNATBIND
   --  Maximum number of errors before compilation is terminated

   Maximum_File_Name_Length : Int;
   --  GNAT, GNATBIND
   --  Maximum number of characters allowed in a file name, not counting the
   --  extension, as set by the appropriate switch. If no switch is given,
   --  then this value is initialized by Osint to the appropriate value.

   Maximum_Processes : Positive := 1;
   --  GNATMAKE
   --  Maximum number of processes that should be spawned to carry out
   --  compilations.

   Minimal_Recompilation : Boolean := False;
   --  GNATMAKE
   --  Set to True if minimal recompilation mode requested.

   No_Stdlib : Boolean := False;
   --  GNATMAKE
   --  Set to True if no default library search dirs added to search list.

   No_Stdinc : Boolean := False;
   --  GNATMAKE
   --  Set to True if no default source search dirs added to search list.

   No_Main_Subprogram : Boolean := False;
   --  GNATMAKE, GNATBIND
   --  Set to True if compilation/binding of a program without main
   --  subprogram requested.

   Normalize_Scalars : Boolean := False;
   --  GNAT
   --  Set True if a pragma Normalize_Scalars applies to the current

   No_Run_Time : Boolean := False;
   --  GNAT
   --  Set True if a valid pragma No_Run_Time is processed

   type Operating_Mode_Type is (Check_Syntax, Check_Semantics, Generate_Code);
   Operating_Mode : Operating_Mode_Type := Generate_Code;
   --  GNAT
   --  Indicates the operating mode of the compiler. The default is generate
   --  code, which runs the parser, semantics and backend. Switches can be
   --  used to set syntax checking only mode, or syntax and semantics checking
   --  only mode. Operating_Mode can also be modified as a result of detecting
   --  errors during the compilation process. In particular if any error is
   --  detected then this flag is reset from Generate_Code to Check_Semantics
   --  after generating an error message.

   Output_Filename_Present : Boolean := False;
   --  GNATBIND, GNAT
   --  Set to True when the output C filename is given with option -o
   --  for GNATBIND or when the object filename is given with option
   --  -gnatO for GNAT.

   Output_Object_List : Boolean := False;
   --  GNATBIND
   --  True if output of list of objects is requested (-O switch set)

   Pessimistic_Elab_Order : Boolean := False;
   --  GNATBIND
   --  True if pessimistic elaboration order is to be chosen (-p switch set)

   Polling_Required : Boolean := False;
   --  GNAT
   --  Set to True if polling for asynchronous abort is enabled by using
   --  the -gnatP option for GNAT.

   Print_Generated_Code : Boolean := False;
   --  GNAT
   --  Set to True to enable output of generated code in source form. This
   --  flag is set by the -gnatG switch.

   Propagate_Exceptions : Boolean := False;
   --  GNAT
   --  Indicates if subprogram descriptor exception tables should be
   --  built for imported subprograms. Set True if a Propagate_Exceptions
   --  pragma applies to the extended main unit.

   Queuing_Policy : Character := ' ';
   --  GNAT
   --  Set to ' ' for the default case (no queuing policy specified). Reset to
   --  Reset to first character (upper case) of locking policy name if a valid
   --  Queuing_Policy pragma is encountered.

   Quiet_Output : Boolean := False;
   --  GNATMAKE
   --  Set to True if the list of compilation commands should not be output.

   Shared_Libgnat : Boolean;
   --  GNATBIND
   --  Set to True if a shared libgnat is requested by using the -shared
   --  option for GNATBIND and to False when using the -static option. The
   --  value of this switch is set by Gnatbind.Scan_Bind_Arg.

   Software_Overflow_Checking : Boolean;
   --  GNAT
   --  Set to True by Osint.Initialize if the target requires the software
   --  approach to integer arithmetic overflow checking (i.e. the use of
   --  double length arithmetic followed by a range check). Set to False
   --  if the target implements hardware overflow checking.

   Stack_Checking_Enabled : Boolean;
   --  GNAT
   --  Set to indicate if -fstack-check switch is set for the compilation.
   --  True means that the switch is set, so that stack checking is enabled.
   --  False means that the switch is not set (no stack checking). This
   --  value is obtained from the external imported value flag_stack_check
   --  in the gcc backend using Sem_Util.Set_Stack_Check_Flags. This value
   --  is set at the start of compilation, and may be referenced throughout
   --  the compilation phases.

   Stack_Check_Probes_Val : Boolean;
   --  GNAT (gigi)
   --  This flag is set from the entity Stack_Check_Probes in the *target*
   --  version of System. It indicates whether or not the target system
   --  uses stack probes (True) or the alternative comparison scheme (False).
   --  This value is obtained using Rtsfind by Sem_Util.Set_Stack_Check_Flags.
   --  Note that this value is NOT set until after semantic analysis of the
   --  main unit is complete, so it cannot be referenced during this analysis.

   Strict_Math : aliased Boolean := False;
   --  GNAT
   --  This switch is set True if the current unit is to be compiled in
   --  strict math mode. The effect is to cause certain library file name
   --  substitutions to implement strict math semantics. See the routine
   --  Adjust_File_Name_For_Configuration, and also the configuration
   --  in the body of Opt.
   --
   --  Note: currently this switch is always False. Eventually it will be
   --  settable by a switch and a configuration pragma.

   Style_Check : Boolean := False;
   --  GNAT
   --  Set True to perform style checks. Activates checks carried out
   --  in package Style (see body of this package for details of checks)
   --  This flag is set True by either the -gnatg or -gnaty switches.

   System_Extend_Pragma_Arg : Node_Id := Empty;
   --  GNAT
   --  Set non-empty if and only if a correct Extend_System pragma was present
   --  in which case it points to the argument of the pragma, and the name can
   --  be located as Chars (Expression (System_Extend_Pragma_Arg)).

   Subunits_Missing : Boolean := False;
   --  This flag is set true if missing subunits are detected with code
   --  generation active. This causes code generation to be skipped.

   Suppress_Options : Suppress_Record;
   --  GNAT
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
   --  GNAT
   --  Set to ' ' for the default case (no task dispatching policy specified).
   --  Reset to first character (upper case) of task dispatching policy name
   --  if a valid Task_Dispatching_Policy pragma is encountered.

   Tasking_Used : Boolean := False;
   --  Set True if any tasking construct is encountered. Used to activate the
   --  output of the Q line in ali files.

   Time_Slice_Set : Boolean := False;
   --  Set True if a pragma Time_Slice is processed in the main unit, or
   --  if the T switch is present to set a time slice value.

   Time_Slice_Value : Nat;
   --  Time slice value. Valid only if Time_Slice_Set is True, i.e. if a
   --  Time_Slice pragma has been processed. Set to the time slice value
   --  in microseconds. Negative values are stored as zero, and the value
   --  is not larger than 1_000_000_000 (1000 seconds). Values larger than
   --  this are reset to this maximum.

   Tolerate_Consistency_Errors : Boolean := False;
   --  GNATBIND
   --  Tolerate time stamp and other consistency errors. If this switch is
   --  set true, then inconsistencies result in warnings rather than errors.

   Tree_Output : Boolean := False;
   --  GNAT
   --  Set True to generate output tree file

   Try_Semantics : Boolean := False;
   --  GNAT
   --  Flag set to force attempt at semantic analysis, even if parser errors
   --  occur. This will probably cause blowups at this stage in the game. On
   --  the other hand, most such blowups will be caught cleanly and simply
   --  say compilation abandoned.

   Unique_Error_Tag : Boolean := Tag_Errors;
   --  GNAT
   --  Indicates if error messages are to be prefixed by the string error:
   --  Initialized from Tag_Errors, can be forced on with the -gnatU switch.

   Unreserve_All_Interrupts : Boolean := False;
   --  GNAT, GNATBIND
   --  Normally set False, set True if a valid Unreserve_All_Interrupts
   --  pragma appears anywhere in the main unit for GNAT, or if any ALI
   --  file has the corresponding attribute set in GNATBIND.

   Upper_Half_Encoding : Boolean := False;
   --  GNAT
   --  Normally set False, indicating that upper half ASCII characters are
   --  used in the normal way to represent themselves. If the wide character
   --  encoding method uses the upper bit for this encoding, then this flag
   --  is set True, and upper half characters in the source indicate the
   --  start of a wide character sequence.

   Usage_Requested : Boolean := False;
   --  GNAT, GNATBIND, GNATMAKE
   --  Set to True if h switch encountered requesting usage information

   Use_VADS_Size : Boolean := False;
   --  GNAT
   --  Set to True if a valid pragma Use_VADS_Size is processed

   Verbose_Mode : Boolean := False;
   --  GNAT, GNATBIND
   --  Set to True to get verbose mode (full error message text and location
   --  information sent to standard output, also header, copyright and summary)

   type Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   Warning_Mode : Warning_Mode_Type := Normal;
   --  GNAT, GNATBIND
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, warning messages
   --  are generated and are treated as errors.

   Wide_Character_Encoding_Method : WC_Encoding_Method := WCEM_Brackets;
   --  GNAT
   --  Method used for encoding wide characters in the source program. See
   --  description of type in unit System.WCh_Con for a list of the methods
   --  that are currently supported. Note that brackets notation is always
   --  recognized in source programs regardless of the setting of this
   --  variable. The default setting causes only the brackets notation
   --  to be recognized. If this is the main unit, this setting also
   --  controls the output of the W=? parameter in the ali file, which
   --  is used to provide the default for Wide_Text_IO files.

   Xref_Analyze : Boolean := False;
   --  GNAT
   --  This flag is used to indicate to semantic analyzer that the current
   --  compilation is done for GNATF. So the expander mustn't be called.

   Xref_Active : Boolean := True;
   --  GNAT
   --  Set if cross-referencing is enabled (i.e. xref info in ali files)

   Zero_Cost_Exceptions_Val : Boolean;
   --  GNAT
   --  Shows whether zero cost exception mode is active. This value can
   --  be read only if Zero_Cost_Exceptions_Set is True, indicating that
   --  the value has been set. This flag can be set either by Switch
   --  or by the processing in Sem_Util.Zero_Cost_Exceptions. Note
   --  that gigi may freely reference this swich, since the call to
   --  Sem_Util.Zero_Cost_Exceptions in Frontend guarantees that the
   --  value has been properly set. Note that in the compiler itself,
   --  clients should use the function in Sem_Util, rather than accessing
   --  this value directly.

   Zero_Cost_Exceptions_Set : Boolean := False;
   --  GNAT
   --  If this flag is False, then the value of Zero_Cost_Exceptions_Val
   --  has not been set yet, and cannot be read. If this flag is True,
   --  then Zero_Cost_Exceptions_Val is properly set. This flag can be
   --  set either by Switch or by Sem_Util.Zero_Cost_Exceptions. Note
   --  that when Gigi is called, this switch always has the value True
   --  as a result of the Frontend call to Sem_Util.Zero_Cost_Exceptions.

   -----------------
   -- Subprograms --
   -----------------

   function Check_Ada_95 (File_Name : File_Name_Type) return Boolean;
   --  Set Ada_95 mode for predefined and internal units, otherwise set
   --  mode from switches. Return former Ada_83 mode.

   -----------------------
   -- Tree I/O Routines --
   -----------------------

   procedure Tree_Read;
   --  Reads switch settings from current tree file using Tree_Read

   procedure Tree_Write;
   --  Writes out switch settings to current tree file using Tree_Write

end Opt;
