------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . S T O R A G E S . D S M            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements a distributed shared memory storage
--  support for shared passive packages. The algorithm used for this
--  implementation is based on the variant of the dynamic distributed
--  manager algorithm with dynamic distributed copy set (i.e K .Li and
--  P. Hudak, Memory Coherence in Shared Virtual Memory Systems, ACM
--  Transactions on Computer Systems, nov 1989, vol. 7, num. 4,
--  p. 321-359). Note that the algorithm as described is incomplete,
--  especially concerning all the invalidation phase.

with Ada.Unchecked_Deallocation;
with Ada.Streams;              use Ada.Streams;

with GNAT.Strings;             use GNAT.Strings;

with System.Garlic.Exceptions; use System.Garlic.Exceptions;
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Options;    use System.Garlic.Options;
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Streams;    use System.Garlic.Streams;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Units;      use System.Garlic.Units;

with System.Garlic.Debug;      use System.Garlic.Debug;
pragma Elaborate (System.Garlic.Debug);

package body System.Garlic.Storages.Dsm is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GASTDS", "(s-gastds): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   DSM_Storage_Name : constant String := "dsm";

   procedure Free is
      new Ada.Unchecked_Deallocation
        (Copy_Set_Type, Copy_Set_Access);

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Routine to handle message with opcode DSM_Service. Registered
   --  in Garlic.Heart.

   function Image (R : Request_Message) return String;
   function Image (D : DSM_Data_Access) return String;
   function Image (S : Stream_Element_Access) return String;
   function Image (S : Copy_Set_Access) return String;
   function Image (S : Copy_Set_Type) return String;
   --  Routines to get output when debugging.

   procedure Merge
     (List : in     Copy_Set_Type;
      Into : in out Copy_Set_Access);
   --  Add in Into all the missing partition ids from List.

   --  Some requests cannot be immediatly processed especially when a
   --  variable is locked. Pending requests are stored in a fixed size
   --  table. The size of this table is set arbitrary and may need to
   --  be precisely computed.

   type Pending_Request_Record is record
      Name      : String_Access;
      Request   : Request_Message;
      Partition : Partition_ID;
   end record;

   First_Request : constant Natural := 1;
   Last_Request  : Natural := 0;
   Max_Requests  : constant Natural := 256;
   Requests : array (First_Request .. Max_Requests) of Pending_Request_Record;

   Requests_Watcher : Watcher_Access;

   procedure Append
     (Name      : String_Access;
      Request   : Request_Message;
      Partition : Partition_ID);
   --  Append request to the request table. Suspend in case there is no
   --  space left in Requests.

   procedure Write
     (Stream    : access Params_Stream_Type;
      Name      : String;
      Request   : Request_Message;
      Partition : Partition_ID);
   --  Write the request message in Stream.

   task type Request_Processor_Type;
   type Request_Processor_Access is access Request_Processor_Type;
   Request_Processor : Request_Processor_Access;

   ------------
   -- Append --
   ------------

   procedure Append
     (Name      : String_Access;
      Request   : Request_Message;
      Partition : Partition_ID)
   is
      Version  : Version_Id;
   begin
      pragma Assert (Partition /= Self_PID);
      loop
         Enter_Critical_Section;
         if Last_Request < Max_Requests then
            Last_Request := Last_Request + 1;
            Requests (Last_Request) := (Name, Request, Partition);
            Update (Requests_Watcher);
            Leave_Critical_Section;
            exit;
         end if;

         --  There is no space left in Requests. Wait for an update.

         Lookup (Requests_Watcher, Version);
         Leave_Critical_Section;
         Differ (Requests_Watcher, Version);
      end loop;
   end Append;

   ----------------------
   -- Complete_Request --
   ----------------------

   procedure Complete_Request
     (Var_Data : access DSM_Data_Type) is
   begin
      Enter_Critical_Section;
      Var_Data.Depth := Var_Data.Depth - 1;

      if Var_Data.Is_A_PO and then Var_Data.Depth /= 0 then
         pragma Debug (D ("ignore complete request on " & Var_Data.Name.all));
         Leave_Critical_Section;
         return;
      end if;

      pragma Debug (D ("complete request on " & Var_Data.Name.all));
      --  Resume local tasks waiting for the variable lock.

      Var_Data.Locked := False;
      Update (Var_Data.Watcher);

      --  Resume the request processor as requests concerning this
      --  variable can now be processed.

      Update (Requests_Watcher);
      Leave_Critical_Section;
   end Complete_Request;

   --------------------
   -- Create_Package --
   --------------------

   procedure Create_Package
     (Storage  : in  out DSM_Data_Type;
      Pkg_Name : in      String;
      Pkg_Data : out     Shared_Data_Access)
   is
      pragma Unreferenced (Storage);

      Error  : aliased Error_Type;
      Active : Boolean;
      Pkg    : DSM_Data_Access;

   begin
      Pkg      := new DSM_Data_Type;
      Pkg.Name := new String'(Pkg_Name);

      --  Set Owner and Status to initialize variables from this package.

      Get_Partition (Get_Unit_Id (Pkg_Name), Pkg.Owner, Error);
      if Found (Error) then
         Raise_Communication_Error (Content (Error'Access));
      end if;

      Get_Is_Active_Partition (Pkg.Owner, Active, Error);
      if Found (Error) then
         Raise_Communication_Error (Content (Error'Access));

      --  As this support runs a DSM algorithm, a variable can be
      --  configured on a passive partition.

      elsif not Active then
         Raise_Communication_Error
           ("no dsm storage support allowed on a passive partition");
      end if;

      --  If the variable is configured on this partition, it owns the
      --  the variable in write mode.

      if Pkg.Owner = Self_PID then
         Pkg.Status := Write;
      else
         Pkg.Status := None;
      end if;

      pragma Debug
        (D ("create package " & Pkg.Name.all &
            " with initial status " & Pkg.Status'Img));

      Pkg_Data := Shared_Data_Access (Pkg);
   end Create_Package;

   --------------------
   -- Create_Storage --
   --------------------

   procedure Create_Storage
     (Master   : in out DSM_Data_Type;
      Location : in     String;
      Storage  : out    Shared_Data_Access)
   is
      pragma Unreferenced (Master);

      Result   : DSM_Data_Access;

   begin
      Result      := new DSM_Data_Type;
      Result.Name := new String'(Location);

      pragma Debug
        (D ("create storage with initial location """ &
            Result.Name.all & """"));

      Storage     := Shared_Data_Access (Result);
   end Create_Storage;

   ---------------------
   -- Create_Variable --
   ---------------------

   procedure Create_Variable
     (Pkg_Data : in out DSM_Data_Type;
      Var_Name : in     String;
      Var_Data : out    Shared_Data_Access)
   is
      Var : DSM_Data_Access;

   begin
      Var         := new DSM_Data_Type;
      Var.Name    := new String'(Var_Name);
      Var.Owner   := Pkg_Data.Owner;
      Var.Status  := Pkg_Data.Status;
      Var.Locked  := False;
      Var.Is_A_PO := False;
      Var.Depth   := 0;
      Var.Version := No_Version;
      Var.Stream  := null;
      Var.Offset  := 0;
      Create (Var.Watcher);

      pragma Debug
        (D ("create variable """ & Var.Name.all & """"));

      Var_Data   := Shared_Data_Access (Var);
   end Create_Variable;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Opcode);
      pragma Unreferenced (Reply);
      pragma Unreferenced (Error);

      Name     : String_Access   := new String'(String'Input (Query));
      Request  : Request_Message := Request_Message'Input (Query);
   begin

      --  Always delegate to the request processor. The task handling
      --  a request (executing this procedure) cannot send messages
      --  because Send may block when the receiving partition
      --  transport protocol is still unknown. In this case, Send
      --  sends an information requests and waits for the transport
      --  protocol to be defined. Moreover, these information may come
      --  from the partition sending the current request. In this
      --  case, Send blocks forever waiting for information that
      --  cannot be received by this same task.

      Append (Name, Request, Partition);
   end Handle_Request;

   -----------
   -- Image --
   -----------

   function Image (D : DSM_Data_Access) return String is
   begin
      return "(" &
        D.Name.all & ", " &
        D.Version'Img & ", " &
        D.Status'Img & ", " &
        D.Locked'Img & "," &
        D.Depth'Img & "," &
        D.Owner'Img & ", " &
        Image (D.Copies) & ", " &
        Image (D.Stream) & ")";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (S : Copy_Set_Access) return String is
   begin
      if S = null then
         return "()";
      else
         return "(" & Image (S.all) & ")";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (S : Copy_Set_Type) return String is
   begin
      if S'Length = 1 then
         return S (S'First)'Img;
      else
         return S (S'First)'Img & ", " & Image (S (S'First + 1 .. S'Last));
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (S : Stream_Element_Access) return String
   is
      N : Natural := 0;
   begin
      if S /= null then
         N := S'Length;
      end if;
      return "<" & N'Img & " bytes >";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (R : Request_Message) return String is
   begin
      case R.Kind is
         when Write_Rqst | Read_Rqst =>
            return "(" & R.Kind'Img & "," & R.Reply_To'Img & ")";

         when Write_Data =>
            return "(" & R.Kind'Img &
              ","  & R.Version'Img &
              ", " & Image (R.Copies) &
              ", " & Image (R.Stream) & ")";

         when Read_Data =>
            return "(" & R.Kind'Img &
              ","  & R.Version'Img &
              ", " & Image (R.Stream) & ")";

         when Invalidate_Rqst =>
            return "(" & R.Kind'Img &
              "," & R.Version'Img &
              "," & R.Owner'Img & ")";
      end case;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Root : DSM_Data_Access := new DSM_Data_Type;

   begin
      if Is_Pure_Client then
         Raise_Communication_Error
           ("a pure client cannot run dsm algorithm");
      elsif Termination = Local_Termination then
         Raise_Communication_Error
           ("a partition cannot locally terminate " &
            "while running dsm algorithm");
      end if;

      pragma Debug (D ("register request handler"));
      Register_Handler (DSM_Service, Handle_Request'Access);
      pragma Debug (D ("register storage support"));
      Register_Storage (DSM_Storage_Name, Shared_Data_Access (Root));

      if Request_Processor = null then
         Create (Requests_Watcher);
         Request_Processor := new Request_Processor_Type;
      end if;
   end Initialize;

   ----------------------
   -- Initiate_Request --
   ----------------------

   procedure Initiate_Request
     (Var_Data : access DSM_Data_Type;
      Request  : in Request_Type;
      Success  : out Boolean)
   is
      Version : Version_Id;
      Owner   : Partition_ID;
      Stream  : aliased Params_Stream_Type (0);
      Error   : aliased Error_Type;
      Mode    : Request_Type := Request;

   begin
      Success := True;

      --  We handle a Lock request as a Write request. This variable
      --  is indicated as a protected object and further Read and
      --  Write requests are ignored. We count the number of ignored
      --  Initiate_Request in order to ignore the equivalent number of
      --  Complete_Request.

      Enter_Critical_Section;
      if Request = Lock then
         Mode := Write;
         Var_Data.Is_A_PO := True;
         Var_Data.Depth   := Var_Data.Depth + 1;

      elsif Var_Data.Is_A_PO then
         pragma Debug
           (D ("ignore initiate " & Request'Img &
               " on " & Image (DSM_Data_Access (Var_Data))));

         if Request = Read and then Var_Data.Stream = null then
            Success := False;

         else
            Var_Data.Depth := Var_Data.Depth + 1;
         end if;
         Var_Data.Offset := 0;
         Leave_Critical_Section;
         return;
      end if;

      --  Wait to get the lock.

      loop
         exit when not Var_Data.Locked;
         Lookup (Var_Data.Watcher, Version);
         Leave_Critical_Section;
         Differ (Var_Data.Watcher, Version);
         Enter_Critical_Section;
      end loop;
      Var_Data.Locked := True;

      pragma Debug
        (D ("initiate " & Request'Img &
            " on " & Image (DSM_Data_Access (Var_Data))));

      if Mode = Write then

         --  When the variable status is not Write, send a request to
         --  the probable owner in order to get it in Write mode.

         if Var_Data.Status /= Write then
            Lookup (Var_Data.Watcher, Version);
            Owner := Var_Data.Owner;
            Write
              (Stream'Access,
               Var_Data.Name.all,
               (Write_Rqst, Self_PID),
               Owner);
            Leave_Critical_Section;

            --  Release critical section with a copy of Owner.

            Send (Owner, DSM_Service, Stream'Access, Error);

            --  Wait for variable status to be set to Write.

            Differ (Var_Data.Watcher, Version);
            pragma Assert (Var_Data.Status = Write);

         else
            Leave_Critical_Section;
         end if;

      elsif Mode = Read then

         --  When the variable status is None, send a request to the
         --  probable owner in order to get variable in Read mode.

         if Var_Data.Status = None then

            --  Loop until we definitively get the variable in read
            --  mode. Several passes may be needed because of
            --  successive invalidations.

            loop
               Owner := Var_Data.Owner;
               Lookup (Var_Data.Watcher, Version);
               Write (Stream'Access,
                      Var_Data.Name.all,
                      (Read_Rqst, Self_PID),
                      Owner);
               Leave_Critical_Section;

               --  Release critical section with a copy of Owner.

               Send (Owner, DSM_Service, Stream'Access, Error);

               --  Wait for variable status to be set to Read

               Differ (Var_Data.Watcher, Version);

               Enter_Critical_Section;
               exit when Var_Data.Status = Read;
            end loop;
         end if;

         --  If we have no copy of this variable although its status
         --  is Read or Write, use the value in memory and release the
         --  variable mutex.

         if Var_Data.Stream = null then
            Success         := False;
            Var_Data.Locked := False;
         end if;
         Leave_Critical_Section;
      end if;

      Var_Data.Offset := 0;
   end Initiate_Request;

   -----------
   -- Input --
   -----------

   function Input
     (S : access Ada.Streams.Root_Stream_Type'Class)
     return Request_Message
   is
      Request : Request_Message (Request_Kind'Input (S));

   begin
      case Request.Kind is
         when Write_Rqst | Read_Rqst =>
            Partition_ID'Read (S, Request.Reply_To);

         when others =>
            Version_Id'Read (S, Request.Version);

            case Request.Kind is
               when Invalidate_Rqst =>
                  Partition_ID'Read (S, Request.Owner);

               when Write_Data | Read_Data =>
                  Stream_Element_Access'Read (S, Request.Stream);

                  case Request.Kind is
                     when Write_Data =>
                        Copy_Set_Access'Read (S, Request.Copies);

                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
      return Request;
   end Input;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (List : in Copy_Set_Type;
      Into : in out Copy_Set_Access)
   is
      Set : Copy_Set_Type (1 .. List'Length);
      Len : Natural := 0;
      Elt : Boolean;

   begin
      if Into = null then
         Into := new Copy_Set_Type'(List);

      else
         --  Find the partition from List missing in Into and store
         --  them in Set.

         for I in List'Range loop
            Elt := False;
            for J in Into'Range loop
               if Into (J) = List (I) then
                  Elt := True;
                  exit;
               end if;
            end loop;
            if not Elt then
               Len := Len + 1;
               Set (Len) := List (I);
            end if;
         end loop;

         --  If there are really missing partitions, then rebuild Into
         --  to include these partitions.

         if Len /= 0 then
            declare
               Union : Copy_Set_Type := Into.all & Set (1 .. Len);
            begin
               Free (Into);
               Into := new Copy_Set_Type'(Union);
            end;
         end if;
      end if;
   end Merge;

   ------------
   -- Output --
   ------------

   procedure Output
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Request_Message) is
   begin
      Request_Kind'Write (S, X.Kind);
      case X.Kind is
         when Write_Rqst | Read_Rqst =>
            Partition_ID'Write (S, X.Reply_To);

         when others =>
            Version_Id'Write (S, X.Version);

            case X.Kind is
               when Invalidate_Rqst =>
                  Partition_ID'Write (S, X.Owner);

               when Write_Data | Read_Data =>
                  Stream_Element_Access'Write (S, X.Stream);

                  case X.Kind is
                     when Write_Data =>
                        Copy_Set_Access'Write (S, X.Copies);

                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
   end Output;

   ----------
   -- Read --
   ----------

   procedure Read
     (Data : in out DSM_Data_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      Len : constant Stream_Element_Count := Item'Length;

   begin
      Item := Data.Stream (Data.Offset + 1 .. Data.Offset + Len);
      Last := Item'Last;
      Data.Offset := Data.Offset + Len;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Copy_Set_Access)
   is
      Len : Natural := Natural'Input (S);
      Set : Copy_Set_Access;

   begin
      if Len /= 0 then
         Set := new Copy_Set_Type (1 .. Len);
         for I in Set'Range loop
            Partition_ID'Read (S, Set (I));
         end loop;
      end if;
      X := Set;
   end Read;

   ----------------------------
   -- Request_Processor_Type --
   ----------------------------

   task body Request_Processor_Type is
      Error     : Error_Type;
      Var_Data  : DSM_Data_Access;
      Name      : String_Access;
      Request   : Request_Message;
      Partition : Partition_ID;
      Copies    : Copy_Set_Access;
      Owner     : Partition_ID;
      Shutdown  : Boolean := False;
      Version   : Version_Id;

   begin
      loop
         loop
            Enter_Critical_Section;

            --  Set Partition to Null_Partition_ID to detect that no
            --  request has been selected. This can occur when there
            --  are only read and write requests with locked variables.

            Partition := Null_Partition_ID;
            for R in First_Request .. Last_Request loop
               Name    := Requests (R).Name;
               Request := Requests (R).Request;

               --  Name = null is a convention to shutdown

               if Name = null then
                  Leave_Critical_Section;
                  Shutdown := True;
                  exit;
               end if;

               Var_Data := DSM_Data_Access (Lookup_Variable (Name.all));

               --  Can we handle this request ?

               if not Var_Data.Locked
                 or else Request.Kind in Write_Data .. Read_Data
                 or else (Request.Kind = Invalidate_Rqst
                          and then Var_Data.Status = Read)
               then
                  Partition := Requests (R).Partition;
                  Requests (R .. Last_Request - 1)
                    := Requests (R + 1 .. Last_Request);
                  Last_Request := Last_Request - 1;
                  exit;
               end if;
            end loop;

            exit when Shutdown or else Partition /= Null_Partition_ID;

            Lookup (Requests_Watcher, Version);
            Leave_Critical_Section;

            --  Wait for an update in the request table. Note that
            --  when a variable changes its status, the request table
            --  version is also updated. It is not very efficient.

            Add_Non_Terminating_Task;
            Differ (Requests_Watcher, Version);
            Sub_Non_Terminating_Task;
         end loop;

         exit when Shutdown;

         pragma Debug
           (D ("process " & Image (Request) &
               " on "   & Image (Var_Data) &
               " from" & Partition'Img));

         if Request.Kind = Write_Data then

            --  We want to process the whole request. A subtle
            --  situation arises when we already have a read copy of
            --  the variable. If we store the request stream in the
            --  variable stream, we must not deallocate it when we
            --  invalidate the read copy. To detect this situation, we
            --  declare our ownership on the variable right now.

            if Var_Data.Stream /= null then
               Free (Var_Data.Stream);
            end if;
            Var_Data.Stream := Request.Stream;
            Var_Data.Owner  := Self_PID;

            --  Check whether we have to invalidate our own copy and
            --  whether it has to be done during the current
            --  invalidation phase or once we receive an invalidation
            --  request from the partition which provided us with the
            --  read copy. When we got the current read copy from the
            --  partition which provided us with the write copy, we do
            --  the invalidation immediatly (we are in its copy
            --  set). Otherwise, we send the invalidation request to
            --  the partitions form the copy set sent by the previous
            --  owner of the variable and wait for the invalidation
            --  request from the partition which provided us with the
            --  read copy.

            Copies := Request.Copies;
            if Copies /= null then
               for P in Copies'Range loop
                  if Copies (P) = Self_PID then

                     --  We are already in the copy set of the
                     --  previous variable owner. We can safely merge
                     --  the two copy sets (the one from previous
                     --  owner and the local one).

                     Var_Data.Status := None;
                     if Var_Data.Copies /= null then
                        Merge (Var_Data.Copies.all, Copies);
                        Free (Var_Data.Copies);
                     end if;
                     exit;
                  end if;
               end loop;
            end if;

            --  We are not waiting for an invalidation request when
            --  the status is different from Read. Set status to Write
            --  and resume tasks waiting for variable.

            if Var_Data.Status = None then
               Var_Data.Status  := Write;
               Var_Data.Version := Request.Version + 1;
               Update (Var_Data.Watcher);
            end if;
            Leave_Critical_Section;

            --  Invalidate read copies outside the critical section in
            --  order to avoid deadlocks as Send is potentially blocking.

            if Copies /= null then
               for P in Copies'Range loop
                  if Copies (P) /= Self_PID then
                     declare
                        S : aliased Params_Stream_Type (0);
                     begin
                        Write (S'Access,
                               Name.all,
                               (Invalidate_Rqst, Request.Version, Self_PID),
                               Copies (P));
                        Send (Copies (P), DSM_Service, S'Access, Error);
                     end;
                  end if;
               end loop;
               Free (Copies);
            end if;

         elsif Request.Kind = Read_Data then
            pragma Assert (Var_Data.Status = None);

            if Var_Data.Stream /= null then
               Free (Var_Data.Stream);
            end if;
            Var_Data.Stream  := Request.Stream;
            Var_Data.Version := Request.Version;
            Var_Data.Owner   := Partition;
            Var_Data.Status  := Read;
            Leave_Critical_Section;

            Update (Var_Data.Watcher);

         elsif Request.Kind = Invalidate_Rqst then
            pragma Assert (Var_Data.Status = Read);
            pragma Assert (Var_Data.Version = Request.Version);

            --  If status is read and owner is ourself, then we have
            --  acquired the variable in write mode but we were
            --  waiting for an invalidation request to definitively
            --  set its status to Write as the invalidation phase is
            --  now completed.

            if Var_Data.Owner = Self_PID then
               Var_Data.Status  := Write;
               Var_Data.Version := Request.Version + 1;
               Update (Var_Data.Watcher);

            else
               --  Free our variable value as it is now obsolete. This
               --  is a regular situation of copy invalidation.

               if Var_Data.Stream /= null then
                  Free (Var_Data.Stream);
               end if;
               Var_Data.Status := None;
            end if;
            Copies := Var_Data.Copies;

            Var_Data.Copies := null;
            Var_Data.Owner  := Request.Owner;

            Leave_Critical_Section;

            --  Invalidate read copies outside the critical section in
            --  order to avoid deadlocks as Send is potentially blocking.

            if Copies /= null then
               for P in Copies'Range loop
                  declare
                     S : aliased Params_Stream_Type (0);
                  begin
                     Write (S'Access,
                            Name.all,
                            (Invalidate_Rqst, Request.Version, Request.Owner),
                            Copies (P));
                     Send (Copies (P), DSM_Service, S'Access, Error);
                  end;
               end loop;
               Free (Copies);
            end if;

         elsif Request.Kind = Read_Rqst then
            pragma Assert (not Var_Data.Locked);

            --  When we have a variable copy, send it to the
            --  requesting partition and add it to the copy set.

            if Var_Data.Status in Write .. Read then
               Merge ((1 => Request.Reply_To), Var_Data.Copies);
               declare
                  S : aliased Params_Stream_Type (0);
               begin
                  Write (S'Access,
                         Name.all,
                         (Read_Data, Var_Data.Version, Var_Data.Stream),
                         Request.Reply_To);
                  Leave_Critical_Section;
                  Send (Request.Reply_To, DSM_Service, S'Access, Error);
               end;

            --  When we do not have a copy, forward the request to the
            --  probable owner.

            else
               Owner          := Var_Data.Owner;
               Var_Data.Owner := Request.Reply_To;
               declare
                  S : aliased Params_Stream_Type (0);
               begin
                  Write (S'Access, Name.all, Request, Owner);
                  Leave_Critical_Section;
                  Send (Owner, DSM_Service, S'Access, Error);
               end;
            end if;

         --  The request is a write request and we own the
         --  variable. Send it to the requesting partition with our
         --  copy set. Free variable value and copy set. Keep track of
         --  the new owner (requesting partition).

         elsif Var_Data.Owner = Self_PID then
            pragma Assert (not Var_Data.Locked);
            Var_Data.Owner  := Request.Reply_To;
            Var_Data.Status := None;
            declare
               S : aliased Params_Stream_Type (0);
            begin
               Write (S'Access, Name.all,
                      (Write_Data, Var_Data.Version,
                       Var_Data.Stream, Var_Data.Copies),
                      Request.Reply_To);
               if Var_Data.Copies /= null then
                  Free (Var_Data.Copies);
               end if;
               if Var_Data.Stream /= null then
                  Free (Var_Data.Stream);
               end if;
               Leave_Critical_Section;
               Send (Request.Reply_To, DSM_Service, S'Access, Error);
            end;

         --  The request is a write request and we do not own the
         --  variable. Send the request to the probable owner and keep
         --  track of the new owner (requesting partition).

         else
            pragma Assert (not Var_Data.Locked);
            Owner          := Var_Data.Owner;
            Var_Data.Owner := Request.Reply_To;
            declare
               S : aliased Params_Stream_Type (0);
            begin
               Write (S'Access, Name.all, Request, Owner);
               Leave_Critical_Section;
               Send (Owner, DSM_Service, S'Access, Error);
            end;
         end if;

         Free (Name);
      end loop;

   exception when others =>
      pragma Debug (D ("request processor failed"));
      null;
   end Request_Processor_Type;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Storage : DSM_Data_Type) is
      pragma Unreferenced (Storage);
   begin
      pragma Debug
        (D ("shutdown with" & Last_Request'Img & " pending requests"));

      --  A shutdown is signaled with a variable with a null name.

      Append (null,
              Request_Message'(Kind => Invalidate_Rqst,
                               Version => No_Version,
                               Owner => Null_Partition_ID),
              Null_Partition_ID);
   end Shutdown;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : access Params_Stream_Type;
      Name      : String;
      Request   : Request_Message;
      Partition : Partition_ID)
   is
   begin
      pragma Debug
        (D ("send " & Image (Request) &
            " on "  & Name &
            " to"   & Partition'Img));

      String'Output (Stream, Name);
      Request_Message'Output (Stream, Request);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Data : in out DSM_Data_Type;
      Item : in Ada.Streams.Stream_Element_Array)
   is
      Str : Stream_Element_Access;
      Len : constant Stream_Element_Offset := Item'Length;

   begin
      if Data.Stream = null then
         Data.Stream := new Stream_Element_Array'(Item);
         Data.Offset := Data.Stream'Last;

      elsif Data.Stream'Last = Data.Offset then
         Str := new Stream_Element_Array'(Data.Stream.all & Item);
         Free (Data.Stream);
         Data.Stream := Str;
         Data.Offset := Data.Stream'Last;

      else
         Data.Stream (Data.Offset + 1 .. Data.Offset + Len) := Item;
         Data.Offset := Data.Offset + Len;
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : in Copy_Set_Access) is
   begin
      if X = null then
         Natural'Write (S, 0);
      else
         Natural'Write (S, X'Length);
         for I in X'Range loop
            Partition_ID'Write (S, X (I));
         end loop;
      end if;
   end Write;

end System.Garlic.Storages.Dsm;
