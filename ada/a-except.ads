------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System.Standard_Library;
with System.Storage_Elements;

package Ada.Exceptions is

   type Exception_Id is private;
   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;
   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name (X : Exception_Occurrence) return String;
   pragma Inline (Exception_Name);
   --  Same as Exception_Name (Exception_Identity (X))

   function Exception_Name (X : Exception_Id) return String;

   procedure Raise_Exception (E : in Exception_Id; Message : in String := "");
   --  Note: it would be really nice to give a pragma No_Return for this
   --  procedure, but it would be wrong, since Raise_Exception does return
   --  if given the null exception. However we do special case the name in
   --  the test in the compiler for issuing a warning for a missing return
   --  after this call. Program_Error seems reasonable enough in such a case.

   function Exception_Message (X : Exception_Occurrence) return String;

   procedure Reraise_Occurrence (X : Exception_Occurrence);

   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;
   pragma Inline (Exception_Identity);

   function Exception_Information (X : Exception_Occurrence) return String;
   --  The format of the exception information is as follows:
   --
   --    exception name (as in Exception_Name)
   --    message (or a null line if no message)
   --    PID=nnnn
   --    16#xxxx_xxxx# or 16#xxxx_xxxx_xxxx_xxxx#
   --    16#xxxx_xxxx# or 16#xxxx_xxxx_xxxx_xxxx#
   --    ...
   --
   --  The lines are separated by an Ascii.CR/Ascii.NL sequence.
   --  The nnnn is the partition Id given as decimal digits.
   --  One 16#...# lines represent traceback program counter locations,
   --  in order with the first one being the exception location. All hex
   --  characters are upper case letters.

   --  Note on ordering: the compiler uses the Save_Occurrence procedure, but
   --  not the function from Rtsfind, so it is important that the procedure
   --  come first, since Rtsfind finds the first matching entity.

   procedure Save_Occurrence
     (Target :    out Exception_Occurrence;
      Source : in     Exception_Occurrence);

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return Exception_Occurrence_Access;

private
   package SSL renames System.Standard_Library;

   subtype EOA is Exception_Occurrence_Access;

   Exception_Msg_Max_Length : constant := 200;

   ------------------
   -- Exception_Id --
   ------------------

   subtype Code_Loc is System.Storage_Elements.Integer_Address;
   --  Code location used in building exception tables and for call
   --  addresses when propagating an exception (also traceback table)
   --  Values of this type are created using the N_Code_Range node,
   --  or extracted from machine states using Get_Code_Loc.

   Null_Loc : constant Code_Loc := 0;
   --  Null code location, used to flag outer level frame

   type Exception_Id is new SSL.Exception_Data_Ptr;

   pragma Warnings (Off); -- for bootstrap path, ignore Stream_Convert???

   function EId_To_String (X : Exception_Id) return String;
   function String_To_EId (S : String) return Exception_Id;
   pragma Stream_Convert (Exception_Id, String_To_EId, EId_To_String);
   --  Functions for implementing Exception_Id stream attributes

   Null_Id : constant Exception_Id := null;

   --------------------------
   -- Exception_Occurrence --
   --------------------------

   Max_Tracebacks : constant := 20;
   --  Maximum number of trace backs stored in exception occurrence

   type Tracebacks_Array is array (1 .. Max_Tracebacks) of Code_Loc;
   --  Traceback array stored in exception occurrence

   type Exception_Occurrence is record
      Id : Exception_Id;
      --  Exception_Identity for this exception occurrence

      Msg_Length : Natural := 0;
      --  Length of message (zero = no message)

      Msg : String (1 .. Exception_Msg_Max_Length);
      --  Characters of message

      Pid : Natural;
      --  Partition_Id for partition raising exception

      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      --  Number of traceback entries stored

      Tracebacks : Tracebacks_Array;
      --  Stored tracebacks (in Tracebacks (1 .. Num_Tracebacks)
   end record;

   function EO_To_String (X : Exception_Occurrence) return String;
   function String_To_EO (S : String) return Exception_Occurrence;
   pragma Stream_Convert (Exception_Occurrence, String_To_EO, EO_To_String);
   --  Functions for implementing Exception_Occurrence stream attributes

   Null_Occurrence : constant Exception_Occurrence := (
     Id             => Null_Id,
     Msg_Length     => 0,
     Msg            => (others => ' '),
     Pid            => 0,
     Num_Tracebacks => 0,
     Tracebacks     => (others => Null_Loc));

   -------------------------
   -- Private Subprograms --
   -------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String;
   --  Like Exception_Name, but returns the simple non-qualified name of
   --  the exception. This is used to implement the Exception_Name function
   --  in Current_Exceptions (the DEC compatible unit). It is called from
   --  the compiler generated code (using Rtsfind, which does not respect
   --  the private barrier, so we can place this function in the private
   --  part where the compiler can find it, but the spec is unchanged.)

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence);
   --  Exactly like Reraise_Occurrence, except that abort is not deferred
   --  before the call. This is used in generated code when it is known
   --  that abort is already deferred.

   ------------------------------------------------------
   -- Data Structures for Zero-Cost Exception Handling --
   ------------------------------------------------------

   --  The following section defines data structures used for zero cost
   --  exception handling if System.Parameters.Zero_Cost_Exceptions is
   --  set true (i.e. zero cost exceptions are implemented on this target).

   --  The approach is to build tables that describe the PC ranges that
   --  are covered by various exception frames. When an exception occurs,
   --  these tables are searched to determine the address of the applicable
   --  handler for the current exception.

   subtype Handler_Loc is System.Storage_Elements.Integer_Address;
   --  Code location representing entry address of a handler. Values of
   --  this type are created using the N_Handler_Loc node, and then
   --  passed to the Enter_Handler procedure to enter a handler.

   type PC_Range is record
      Lo, Hi : Code_Loc;
   end record;
   --  Record used to represent a range of addresses

   -------------
   -- Handler --
   -------------

   --  A Handler is built for each exception handler in a unit

   Others_Dummy_Exception : aliased SSL.Exception_Data;
   Others_Id : constant Exception_Id := Others_Dummy_Exception'Access;
   --  Dummy exception used to signal others exception

   All_Others_Dummy_Exception : aliased SSL.Exception_Data;
   All_Others_Id : constant Exception_Id := All_Others_Dummy_Exception'Access;
   --  Dummy exception used to signal all others exception (including
   --  exceptions not normally handled by others, e.g. Abort_Signal)

   type Handler is record
      Id : Exception_Id;
      --  Id of exception being handled, or one of the above special values

      Handler : Code_Loc;
      --  Address of label at start of handler
   end record;

   type Handler_Ptr is access all Handler;

   type Handler_Array is array (Natural range <>) of Handler_Ptr;
   type Handler_Array_Ptr is access all Handler_Array;

   -------------------
   -- Handler_Table --
   -------------------

   --  A Handler_Table is built for each set of exception handlers
   --  that applies to the same range of PC addresses, i.e. that are
   --  part of the same exception frame.

   type Handler_Table is record
      PC : PC_Range;
      --  Range of PC values of code covered by handlers

      Handlers : Handler_Array_Ptr;
      --  Pointer to list of handlers for this range, the order is not
      --  important, since the language ensures mutual exclusion. The
      --  one exception is that an others handler must be last.
   end record;

   type Handler_Table_Array is array (Natural range <>) of Handler_Table;
   type Handler_Table_Array_Ptr is access all Handler_Table_Array;

   ---------------------
   -- Proc_Descriptor --
   ---------------------

   --  A Proc_Descriptor is built for each subprogram that has contains
   --  at least one exception handler, and also for all other subprograms
   --  if the target makes use of Proc_Info entries.

   subtype Proc_Info_Type is System.Storage_Elements.Integer_Address;
   --  This type is used to represent a pointer to information created by the
   --  backend for use in popping machine states (see separate section on
   --  Machine_State operations). The information might for example include
   --  a record of which save over call registers are saved, and where they
   --  are saved, or the return point location. Values of this type are
   --  created by elaborating the special node N_Proc_Info. If a given target
   --  does not need to use Proc_Info values, then this elaboration may simply
   --  return the special value Null_Proc_Info.

   Null_Proc_Info : constant Proc_Info_Type := 0;
   --  Null information, used to indicate that no procedure information
   --  is available.

   Library_Info : constant Proc_Info_Type := 1;
   --  This is a special value used for handlers that apply to the begin
   --  block of a package body. In this case, the enclosing procedure is
   --  the elaboration procedure for the unit. This procedure is built
   --  by Gigi and we do not know enough to easily describe it, but we
   --  do not need to describe it, since it is at the outer level, and
   --  there is no need to do any more Pop_Frame operations since we
   --  know there are no handlers further out.

   type Proc_Descriptor is record
      PC : PC_Range;
      --  Starting PC value for subprogram code

      Handlers : Handler_Table_Array_Ptr;
      --  Pointer to array of handlers for this subprogram. This array
      --  is sorted inside out, i.e. entries for inner handlers appear
      --  before entries for outer handlers. This ensures that a serial
      --  search finds the innermost applicable handler

      Proc_Info : Proc_Info_Type;
      --  This is a pointer to a target dependent data item that provides
      --  sufficient information for unwinding the stack frame of this
      --  procedure. This pointer is null if Proc_Info pointers are not
      --  used in this target.
   end record;

   type Proc_Descriptor_Ptr is access all Proc_Descriptor;

   type Proc_Descriptor_Array
     is array (Natural range <>) of Proc_Descriptor;
   type Proc_Descriptor_Array_Ptr is access all Proc_Descriptor_Array;

   ---------------------
   -- Exception_Table --
   ---------------------

   --  Each unit contains a named value of type Proc_Descriptor_Array_Ptr,
   --  with an external name of unit_name___PDAP, where unit_name is the
   --  unit name with periods replaced by double underscores as usual.
   --  This value points to the procedure descriptor array for the unit.

   --  The binder calls the routine Build_Proc_Table with a pointer to
   --  a list of pointers to these values. This list is terminated by a
   --  null pointer. The Register_Exceptions routine builds a sorted vector
   --  of pointers to procedure descriptors from this information.

   type Exception_Table_Ptr is access Proc_Descriptor_Array_Ptr;
   type Exception_Table_Ptrs is array (Natural) of Exception_Table_Ptr;

   procedure Build_Proc_Table (T : Exception_Table_Ptrs);
   --  This is the routine that is called at the start of execution to
   --  build and sort the list of procedure descriptor pointers.

end Ada.Exceptions;
