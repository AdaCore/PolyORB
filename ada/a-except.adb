------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
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

with GNAT.Heap_Sort_A;          use GNAT.Heap_Sort_A;

with System;                    use System;
with System.Exception_Table;    use System.Exception_Table;
with System.Standard_Library;   use System.Standard_Library;
with System.Storage_Elements;   use System.Storage_Elements;
with System.Task_Specific_Data; use System.Task_Specific_Data;
with System.Tasking_Soft_Links;

package body Ada.Exceptions is

   --  ??? pragmas No_Return commented out for VMS. Fix later.

   package TSL renames System.Tasking_Soft_Links;

   Proc_Descriptors : Proc_Descriptor_Array_Ptr;
   --  This location is initialized by Register_Exceptions to point to a
   --  list of pointers to procedure descriptors, sorted into ascending
   --  order of PC addresses.

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Note: the exported subprograms in this package body are called directly
   --  from C clients using the given external name, even though they are not
   --  technically visible in the Ada sense.

   Address_Image_Length : constant :=
                            13 + 10 * Boolean'Pos (Standard'Address_Size > 32);
   --  Length of string returned by Address_Image function

   function Address_Image (A : System.Address) return String;
   --  Returns at string of the form 16#xxxx_xxxx# for 32-bit addresses
   --  or 16#xxxx_xxxx_xxxx_xxxx# for 64-bit addresses. Hex characters
   --  are in upper case.

   procedure Raise_Current_Excep (E : Exception_Id);
--   pragma No_Return (Raise_Current_Excep);
   pragma Export (C, Raise_Current_Excep, "__gnat_raise_nodefer_with_msg");
   --  This is the lowest level raise routine. It raises the exception
   --  referenced by Current_Excep.all in the TSD, without deferring
   --  abort (the caller must ensure that abort is deferred on entry).
   --  The parameter E is ignored.
   --
   --  This external name for Raise_Current_Excep is historical, and probably
   --  should be changed but for now we keep it, because gdb knows about it.
   --  The parameter is also present for historical compatibility. ???

   procedure Raise_With_Msg (E : Exception_Id);
--   pragma No_Return (Raise_With_Msg);
   pragma Export (C, Raise_With_Msg, "__gnat_raise_with_msg");
   --  Raises an exception with with given exception id value. A message
   --  is associated with the raise, and has already been stored in the
   --  exception occurrence referenced by the Current_Excep in the TSD.
   --  Abort is deferred before the raise call.

   procedure Reraise;
--   pragma No_Return (Reraise);
   --  Reraises the exception referenced by the Current_Excep field of
   --  the TSD (all fields of this exception occurrence are set). Abort
   --  is deferred before the reraise operation.

   procedure Raise_Constraint_Error;
--   pragma No_Return (Raise_Constraint_Error);
   pragma Export (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error with no message

   procedure Raise_Program_Error;
--   pragma No_Return (Raise_Program_Error);
   pragma Export (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise prgoram error with no message

   procedure Raise_Storage_Error;
--   pragma No_Return (Raise_Storage_Error);
   pragma Export (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error with no message

   procedure Set_Exception_C_Msg
     (Id  : Exception_Id;
      Msg : SSL.Big_String_Ptr);
   --  This routine is called to setup the exception referenced by the
   --  Current_Excep field in the TSD to contain the indicated Id value
   --  and message. Msg is a null terminated string.

   procedure Unhandled_Exception;
--   pragma No_Return (Unhandled_Exception);
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  This procedure is called to terminate execution following an unhandled
   --  exception. The exception information, including traceback if available
   --  has been output. If possible (and certainly if zero cost exception
   --  handling is active), the stack is still intact when this procedure
   --  is called, so this is an appropriate place to breakpoint to get
   --  controlled on an unhandled exception. The exception that is unhandled
   --  is referenced by Current_Excep.all in the TSD.

   ------------------------------
   -- Machine_State Operations --
   ------------------------------

   --  The exception handling approach using the above tables isolates
   --  the target dependent aspects using an abstract data type interface
   --  to the type Machine_State, which is represented as a System.Address
   --  value (presumably implemented as a pointer to an appropriate record
   --  structure).

   function Machine_State_Length return Storage_Offset;
   pragma Import (C, Machine_State_Length, "__gnat_machine_state_length");
   --  Function to determine the length of the Storage_Array needed to hold
   --  a machine state. The machine state will always be maximally aligned.
   --  The value returned is a constant that will be used to allocate space
   --  for a machine state value.

   type Machine_State is new Storage_Array (1 .. Machine_State_Length);
   type Machine_State_Ptr is access all Machine_State;

   --  The initial value of type Machine_State is created by the low level
   --  routine that actually raises an exception using the special builtin
   --  _builtin_machine_state. This value will typically encode the value
   --  of the program counter, and relevant registers. The following
   --  operations are defined on Machine_State values:

   function Get_Code_Loc (M : Machine_State_Ptr) return Code_Loc;
   pragma Import (C, Get_Code_Loc, "__gnat_get_code_loc");
   --  This function extracts the program counter value from a machine
   --  state, which the caller uses for searching the exception tables,
   --  and also for recording entries in the traceback table. The call
   --  returns a value of Null_Loc if the machine state represents the
   --  outer level, or some other frame for which no information can be
   --  provided.

   procedure Pop_Frame (M : Machine_State_Ptr; Info : Proc_Info_Type);
   pragma Import (C, Pop_Frame, "__gnat_pop_frame");
   --  This procedure pops the machine state M so that it represents the
   --  call point, as though the current subprogram had returned. It
   --  changes only the value referenced by M, and does not affect
   --  the current stack environment.
   --
   --  The Info parameter represents information generated by the backend
   --  (see description of Proc_Info node in sinfo.ads). This information
   --  is stored as static data during compilation. The caller then passes
   --  this information to Pop_Frame, which will use it to determine what
   --  must be changed in the machine state (e.g. which save-over-call
   --  registers must be restored, and from where on the stack frame they
   --  must be restored).
   --
   --  A value of Null_Proc_Info_Type for Info means either that the
   --  backend provided no information for current frame, or that the
   --  current frame is an other language frame for which no information
   --  exists. In the latter case, Pop_Frame should attempt the pop if it
   --  it can. If it is helpless, then it should set the code location to
   --  Null_Loc to indicate that the exception is to be considered to be
   --  unhandled. In any case Pop_Frame sets the code location to Null_Loc
   --  when it pops past the outer stack level.
   --
   --  If a Pop_Frame operation results in a machine state for which the
   --  Code_Loc value is Null_Loc, then the exception is considered to be
   --  unhandled, and execution is terminated.

   procedure Enter_Handler (M : Machine_State_Ptr; Handler : Handler_Loc);
--   pragma No_Return (Enter_Handler);
   pragma Import (C, Enter_Handler, "__gnat_enter_handler");
   --  When Propagate_Handler locates an applicable exception handler, it
   --  calls Enter_Handler, passing it two parameters. The first is the
   --  machine state that corresponds to what is required for entry to
   --  the handler, as computed by repeated Pop_Frame calls to reach the
   --  handler to be entered. The second is the code location for the
   --  handler itself which is the address of the label at the start of
   --  the handler code.
   --
   --  Note: The machine state M is likely stored on the part of the
   --  stack that will be popped by the call, so care must be taken
   --  not to pop the stack until the Machine_State is entirely read.
   --  The value passed as Handler was obtained from elaboration of
   --  an N_Handler_Loc node by the backend.

   procedure Propagate_Exception (M : Machine_State_Ptr);
--   pragma No_Return (Propagate_Exception);
   --  This procedure propagates the exception represented by the occurrence
   --  referenced by Current_Excep in the TSD for the current task. M is
   --  the initial machine state, representing the site of the exception
   --  raise operation. Propagate_Exception searches the exception tables
   --  for an applicable handler, calling Pop_Frame as needed. If and when
   --  it locates an applicable handler Propagate_Exception makes a call
   --  to Enter_Handler to actually enter the handler. If the search is
   --  unable to locate an applicable handler, execution is terminated by
   --  calling Unhandled_Exception. Note that in the latter case, since
   --  no call was made to Enter_Handler, the stack is unchanged when the
   --  unhandled exception is signalled.

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (A : Address) return String is
      S : String (1 .. Address_Image_Length);
      P : Natural;
      N : Integer_Address;
      U : Natural;

      H : constant array (Integer range 0 .. 15) of Character :=
                                                         "0123456789ABCDEF";

   begin
      S (S'Last) := '#';
      P := S'Last - 1;
      N := To_Integer (A);
      U := 0;

      while P > 3 loop
         if U = 4 then
            S (P) := '_';
            P := P - 1;
            U := 1;
         else
            U := U + 1;
         end if;

         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
      end loop;

      S (1 .. 3) := "16#";
      return S;
   end Address_Image;

   ----------------------
   -- Build_Proc_Table --
   ----------------------

   procedure Build_Proc_Table (T : Exception_Table_Ptrs) is
      Ndes : Natural;

   begin
      --  First count number of descriptors

      Ndes := 0;
      for J in Natural loop
         exit when T (J) = null;
         Ndes := Ndes + T (J).all'Length;
      end loop;

      --  Now allocate the table and copy in the individual pointers

      Proc_Descriptors := new  Proc_Descriptor_Array (0 .. Ndes);
      Ndes := 0;

      for J in Natural loop
         exit when T (J) = null;

         for K in T (J).all'Range loop
            Ndes := Ndes + 1;
            Proc_Descriptors (Ndes) := T (J).all (K);
         end loop;
      end loop;

      --  Now we need to sort the table into ascending PC order

      declare
         function Lt (Op1, Op2 : Natural) return Boolean;
         --  Used in call to sort, compares two elements

         procedure Move (From : Natural; To : Natural);
         --  Used in call to sort, moves one element

         function Lt (Op1, Op2 : Natural) return Boolean is
         begin
            return
              Proc_Descriptors (Op1).PC.Lo < Proc_Descriptors (Op2).PC.Lo;
         end Lt;

         procedure Move (From : Natural; To : Natural) is
         begin
            Proc_Descriptors (To) := Proc_Descriptors (From);
         end Move;

      begin
         Sort (Ndes, Move'Unrestricted_Access, Lt'Unrestricted_Access);
      end;
   end Build_Proc_Table;

   -------------------
   -- EId_To_String --
   -------------------

   function EId_To_String (X : Exception_Id) return String is
   begin
      if X = Null_Id then
         return "";
      else
         return Exception_Name (X);
      end if;
   end EId_To_String;

   ------------------
   -- EO_To_String --
   ------------------

   --  We use the null string to represent the null occurrence, otherwise
   --  we output the Exeption_Information string for the occurrence.

   function EO_To_String (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         return "";
      else
         return Exception_Information (X);
      end if;
   end EO_To_String;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X    : Exception_Occurrence)
      return Exception_Id
   is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      else
         return X.Id;
      end if;
   end Exception_Identity;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information (X : Exception_Occurrence) return String is
      Msg  : constant String  := Exception_Message (X);
      Name : constant String  := Exception_Name (X);
      Len  : constant Natural := Name'Length;
      Ptr  : Natural := 0;

      --  Allocate string of maximum possible size

      Info : String (1 .. Len +
                          Msg'Length +
                          30 +
                          X.Num_Tracebacks * (Address_Image_Length + 2));

      procedure Add_Info_Nat (N : Natural);
      --  Little internal routine to add CR.

      procedure Add_Info_NL;
      --  Little internal routine to add CR/LF to information

      procedure Add_Info_String (S : String);
      --  Little internal routine to add given string to information

      procedure Add_Info_Nat (N : Natural) is
      begin
         if N > 9 then
            Add_Info_Nat (N / 10);
         end if;

         Ptr := Ptr + 1;
         Info (Ptr) := Character'Val (Character'Pos ('0') + N mod 10);
      end Add_Info_Nat;

      procedure Add_Info_NL is
      begin
         Ptr := Ptr + 1;
         Info (Ptr) := Ascii.CR;
         Ptr := Ptr + 1;
         Info (Ptr) := Ascii.LF;
      end Add_Info_NL;

      procedure Add_Info_String (S : String) is
      begin
         Info (Ptr + 1 .. Ptr + S'Length) := S;
         Ptr := Ptr + S'Length;
      end Add_Info_String;

   --  Start of processing for Exception_Information

   begin
      Add_Info_String (Name);
      Add_Info_NL;

      if Msg'Length /= 0 then
         Add_Info_String (Msg);
      end if;

      Add_Info_NL;

      if X.Pid /= 0 then
         Add_Info_String ("PID=");
         Add_Info_Nat (X.Pid);
         Add_Info_NL;
      end if;

      for J in 1 .. X.Num_Tracebacks loop
         Add_Info_String (Address_Image (To_Address (X.Tracebacks (J))));
         Add_Info_NL;
      end loop;

      return Info (1 .. Ptr);
   end Exception_Information;

   -----------------------
   -- Exception_Message --
   -----------------------

   function Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Msg (1 .. X.Msg_Length);
   end Exception_Message;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : Exception_Id) return String is
   begin
      if X = null then
         raise Constraint_Error;
      end if;

      return X.Full_Name.all (1 .. X.Name_Length - 1);
   end Exception_Name;

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   ---------------------------
   -- Exception_Name_Simple --
   ---------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String is
      Name : constant String := Exception_Name (X);
      P    : Natural;

   begin
      P := Name'Length;
      while P > 1 loop
         exit when Name (P - 1) = '.';
         P := P - 1;
      end loop;

      return Name (P .. Name'Length);
   end Exception_Name_Simple;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception (M : Machine_State_Ptr) is
      Excep    : constant EOA := Get_Current_Excep;
      Loc      : Code_Loc;
      Lo, Hi   : Natural;
      Pdesc    : Natural;
      HTables  : Handler_Table_Array_Ptr;
      Handlers : Handler_Array_Ptr;
      HId      : Exception_Id;
      Info     : Proc_Info_Type;

   begin
      --  Loop through stack frames as exception propagates

      loop
         <<Continue>>
         Loc := Get_Code_Loc (M);
         exit when Loc = Null_Loc;

         if Excep.Num_Tracebacks /= Max_Tracebacks then
            Excep.Num_Tracebacks := Excep.Num_Tracebacks + 1;
            Excep.Tracebacks (Excep.Num_Tracebacks) := Loc;
         end if;

         --  Do binary search on procedure table

         Lo := 1;
         Hi := Proc_Descriptors'Last;

         --  Binary search loop

         loop
            --  If not found, then pop frame if we can and continue

            if Lo > Hi then
               Pop_Frame (M, Null_Loc);
               goto Continue;

            else
               Pdesc := (Lo + Hi) / 2;

               if Loc < Proc_Descriptors (Pdesc).PC.Lo then
                  Hi := Pdesc - 1;

               elsif Loc > Proc_Descriptors (Pdesc).PC.Hi then
                  Lo := Pdesc + 1;

               else
                  exit;
               end if;
            end if;
         end loop;

         --  Here with Proc_Descriptors (Pdesc) referencing the procedure
         --  descriptor that applies to this PC value. Now do a serial
         --  search to see if any handler is applicable to this PC value,
         --  and to the exception that we are propagating

         HTables := Proc_Descriptors (Pdesc).Handlers;

         for J in HTables'Range loop
            if Loc in HTables (J).PC.Lo .. HTables (J).PC.Hi then
               Handlers := HTables (J).Handlers;

               --  PC range is applicable, see if handler is for this exception

               for K in Handlers'Range loop
                  HId := Handlers (K).Id;

                  if HId = All_Others_Id
                    or else Excep.Id = HId
                    or else (HId = Others_Id
                               and not Excep.Id.Not_Handled_By_Others)
                  then
                     Enter_Handler (M, Handlers (K).Handler);
                  end if;
               end loop;
            end if;
         end loop;

         --  Here if no handler found in this frame

         Info := Proc_Descriptors (Pdesc).Proc_Info;
         exit when Info = Library_Info;
         Pop_Frame (M, Info);
      end loop;

      --  Fall through if unhandled exception

      Unhandled_Exception;

   end Propagate_Exception;

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error is
   begin
      Raise_No_Msg (Constraint_Error_Def'Access);
   end Raise_Constraint_Error;

   -------------------------
   -- Raise_Current_Excep --
   -------------------------

   procedure Raise_Current_Excep (E : Exception_Id) is
      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address;

      procedure builtin_longjmp (buffer : Address; Flag : Integer);
      pragma No_Return (builtin_longjmp);
      pragma Import (C, builtin_longjmp, "_gnat_builtin_longjmp");

   begin
      --  If the jump buffer pointer is non-null, it means that a jump
      --  buffer was allocated (obviously that happens only in the case
      --  of zero cost exceptions not implemented, or if a jump buffer
      --  was manually set up by C code).

      if Jumpbuf_Ptr /= Null_Address then
         builtin_longjmp (Jumpbuf_Ptr, 1);

      --  If we have no jump buffer, then either zero cost exception
      --  handling is in place, or we have no handlers anyway. In
      --  either case we have an unhandled exception. If zero cost
      --  exception handling is in place, propagate the exception

      elsif Proc_Descriptors /= null then
         declare
            M : aliased Machine_State;

            --  procedure builtin_machine_state (M : Machine_State_Ptr);
            --  pragma Import (C, builtin_machine_state,
            --                    "_builtin_machine_state");

         begin
            --   builtin_machine_state (M'Unrestricted_Access);
            Propagate_Exception (M'Unrestricted_Access);
         end;

      --  Otherwise, we know the exception is unhandled by the absence
      --  of an allocated jump buffer. Note that this means that we also
      --  have no finalizations to do other than at the outer level.

      else
         Unhandled_Exception;
      end if;
   end Raise_Current_Excep;

   --------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : in Exception_Id;
      Message : in String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);

      Excep : constant EOA := Get_Current_Excep;

   begin
      if E /= null then
         Excep.Msg_Length := Len;
         Excep.Msg (1 .. Len) := Message (1 .. Len);
         Raise_With_Msg (E);
      end if;
   end Raise_Exception;

   ------------------
   -- Raise_No_Msg --
   ------------------

   procedure Raise_No_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep;

   begin
      Excep.Msg_Length := 0;
      Raise_With_Msg (E);
   end Raise_No_Msg;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error is
   begin
      Raise_No_Msg (Program_Error_Def'Access);
   end Raise_Program_Error;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error is
   begin
      Raise_No_Msg (Storage_Error_Def'Access);
   end Raise_Storage_Error;

   ----------------------
   -- Raise_With_C_Msg --
   ----------------------

   procedure Raise_With_C_Msg
     (E : Exception_Id;
      M : SSL.Big_String_Ptr)
   is
   begin
      Set_Exception_C_Msg (E, M);
      Raise_With_Msg (E);
   end Raise_With_C_Msg;

   --------------------
   -- Raise_With_Msg --
   --------------------

   procedure Raise_With_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep;

   begin
      Excep.Id := E;
      Excep.Num_Tracebacks := 0;
      Excep.Pid := Local_Partition_ID;
      TSL.Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Msg;

   -------------
   -- Reraise --
   -------------

   procedure Reraise is
      Excep : constant EOA := Get_Current_Excep;

   begin
      TSL.Abort_Defer.all;
      Raise_Current_Excep (Excep.Id);
   end Reraise;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= null then
         TSL.Abort_Defer.all;
         Save_Occurrence (Get_Current_Excep.all, X);
         Raise_Current_Excep (X.Id);
      end if;
   end Reraise_Occurrence;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence) is
   begin
      Save_Occurrence (Get_Current_Excep.all, X);
      Raise_Current_Excep (X.Id);
   end Reraise_Occurrence_No_Defer;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : in  Exception_Occurrence)
   is
   begin
      Target.Id             := Source.Id;
      Target.Msg_Length     := Source.Msg_Length;
      Target.Num_Tracebacks := Source.Num_Tracebacks;
      Target.Pid            := Source.Pid;

      Target.Msg (1 .. Target.Msg_Length) :=
        Source.Msg (1 .. Target.Msg_Length);

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return   EOA
   is
      Target : EOA := new Exception_Occurrence;

   begin
      Save_Occurrence (Target.all, Source);
      return Target;
   end Save_Occurrence;

   -------------------------
   -- Set_Exception_C_Msg --
   -------------------------

   procedure Set_Exception_C_Msg
     (Id  : Exception_Id;
      Msg : Big_String_Ptr)
   is
      Excep : constant EOA := Get_Current_Excep;

   begin
      Excep.Id             := Id;
      Excep.Num_Tracebacks := 0;
      Excep.Pid            := Local_Partition_ID;
      Excep.Msg_Length     := 0;

      while Msg (Excep.Msg_Length + 1) /= Ascii.NUL
        and then Excep.Msg_Length < Exception_Msg_Max_Length
      loop
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := Msg (Excep.Msg_Length);
      end loop;
   end Set_Exception_C_Msg;

   -------------------
   -- String_To_EId --
   -------------------

   function String_To_EId (S : String) return Exception_Id is
   begin
      if S = "" then
         return Null_Id;
      else
         return Exception_Id (Internal_Exception (S));
      end if;
   end String_To_EId;

   X : Exception_Occurrence;
   --  This should be inside String_To_EO, it is outside to temporarily
   --  get around a bootstrap problem.

   ------------------
   -- String_To_EO --
   ------------------

   function String_To_EO (S : String) return Exception_Occurrence is
      From : Natural;
      To   : Integer;

      procedure Bad_EO;
      --  Signal bad exception occurrence string

      procedure Next_String;
      --  On entry, To points to last character of previous line of the
      --  message, terminated by CR/LF. On return, From .. To are set to
      --  specify the next string, or From<To if there are no more lines.

      procedure Bad_EO is
      begin
         Raise_Exception
           (Program_Error'Identity,
            "bad exception occurrence in stream input");
      end Bad_EO;

      procedure Next_String is
      begin
         From := To + 3;

         if From < S'Last then
            To := From + 1;

            while To < S'Last - 2 loop
               if To >= S'Last then
                  Bad_EO;
               elsif S (To + 1) = Ascii.CR then
                  exit;
               else
                  To := To + 1;
               end if;
            end loop;
         end if;
      end Next_String;

   --  Start of processing for String_To_EO

   begin
      if S = "" then
         return Null_Occurrence;

      else
         To := S'First - 3;
         Next_String;
         X.Id := Exception_Id (Internal_Exception (S (From .. To)));
         Next_String;
         X.Msg_Length := To - From + 1;
         X.Msg (1 .. X.Msg_Length) := S (From .. To);
         Next_String;

         X.Pid := 0;
         From := From + 4; -- skip past PID=

         while From <= To loop
            X.Pid := X.Pid * 10 +
                       (Character'Pos (S (From)) - Character'Pos ('0'));
            From := From + 1;
         end loop;

         X.Num_Tracebacks := 0;
         loop
            declare
               C : Code_Loc;
               N : Code_Loc;

            begin
               Next_String;
               exit when From > To;
               From := From + 3; -- skip past 16#

               C := 0;
               while From < To loop
                  if S (From) /= '_' then
                     N := Character'Pos (S (From)) - Character'Pos ('0');

                     if N > 9 then
                        N := N - 7; -- adjust A-F to 10-15
                     end if;

                     C := C * 10 + N;
                  end if;

                  From := From + 1;
               end loop;

               if X.Num_Tracebacks = Max_Tracebacks then
                  Bad_EO;
               end if;

               X.Num_Tracebacks := X.Num_Tracebacks + 1;
               X.Tracebacks (X.Num_Tracebacks) := C;
            end;
         end loop;

         return X;
      end if;
   end String_To_EO;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
      Excep : constant EOA    := Get_Current_Excep;
      Msg   : constant String := Exception_Message (Excep.all);
      Info  : constant String := Exception_Information (Excep.all);
      Nline : constant String := String'(1 => Ascii.LF);

      type FILEs is new System.Address;
      type int is new Integer;

      function stderr return FILEs;
      pragma Import (C, stderr, "c_constant_stderr");

      stderr_FILE : FILEs := stderr;

      procedure To_Stderr (S : String);
      --  Little routine to output string to stderr

      procedure To_Stderr (S : String) is
         function fputc (C : int; stream : FILEs) return int;
         pragma Import (C, fputc);
         dummy : int;

      begin
         for J in 1 .. S'Length loop
            if S (J) /= Ascii.CR then
               dummy := fputc (Character'Pos (S (J)), stderr_FILE);
            end if;
         end loop;
      end To_Stderr;

   --  Start of processing for Unhandled_Exception

   begin
      --  First call adafinal

      declare
         type Proc_Ptr is access procedure;
         adafinal_Ptr : Proc_Ptr;
         pragma Import (C, adafinal_Ptr, "__gl_adafinal_ptr");

         adafinal_Called : Boolean := False;
         --  Used to prevent recursive call to adafinal in the event that
         --  adafinal processing itself raises an unhandled exception.

      begin
         if not adafinal_Called then
            adafinal_Ptr.all;
            adafinal_Called := True;
         end if;
      end;

      --  Print the unhandled exception

      To_Stderr (Nline);
      To_Stderr ("raised ");
      To_Stderr (Exception_Name (Excep.all));

      if Msg'Length /= 0 then
         To_Stderr (" : ");
         To_Stderr (Msg);
      end if;

      To_Stderr (Nline);

      --  Perform system dependent shutdown code

      declare
         procedure Unhandled_Terminate;
--         pragma No_Return (Unhandled_Terminate);
         pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

      begin
         Unhandled_Terminate;
      end;

   end Unhandled_Exception;

end Ada.Exceptions;
