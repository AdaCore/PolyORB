------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--         S Y S T E M . G A R L I C . S T O R A G E S . D S M              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;
with Ada.Streams;              use Ada.Streams;

with System.Garlic.Exceptions; use System.Garlic.Exceptions;
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Streams;    use System.Garlic.Streams;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Units;      use System.Garlic.Units;
with System.Garlic.Utils;      use System.Garlic.Utils;

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

   function Image (R : Request_Record) return String;
   function Image (S : Stream_Element_Access) return String;
   function Image (S : Copy_Set_Access) return String;
   function Image (S : Copy_Set_Type) return String;

   procedure Invalidate
     (Var_Name : in     String_Access;
      Copy_Set : in out Copy_Set_Access;
      Owner    : in     Partition_ID);
   --  Send a cancel request to all the partitions present in Copy_Set
   --  and then deallocate Copy_Set.

   procedure Merge
     (List : in     Copy_Set_Type;
      Into : in out Copy_Set_Access);
   --  Add in Into all the missing partition ids from List.

   procedure Send_Request
     (Partition : in  Partition_ID;
      Var_Name  : in  String_Access;
      Request   : in  Request_Record;
      Error     : in out Error_Type);
   --  Format a request to be processed by Handle_Request on the
   --  remote side. Send it to Partition.

   ----------------------
   -- Complete_Request --
   ----------------------

   procedure Complete_Request
     (Var_Data : in out DSM_Data_Type) is
   begin
      pragma Debug (D ("complete request"));
      Leave (Var_Data.Mutex);
   end Complete_Request;

   --------------------
   -- Create_Package --
   --------------------

   procedure Create_Package
     (Storage  : in  out DSM_Data_Type;
      Pkg_Name : in      String;
      Pkg_Data : out     Shared_Data_Access)
   is
      Error : aliased Error_Type;
      Pkg   : DSM_Data_Access;

   begin
      Pkg      := new DSM_Data_Type;
      Pkg.Name := new String'(Pkg_Name);

      --  Set Owner and Status to initialize variables from this package.

      Get_Partition (Get_Unit_Id (Pkg_Name), Pkg.Owner, Error);
      if Found (Error) then
         Raise_Communication_Error (Content (Error'Access));
      end if;

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
      Var        := new DSM_Data_Type;
      Var.Name   := new String'(Var_Name);
      Var.Owner  := Pkg_Data.Owner;
      Var.Status := Pkg_Data.Status;
      Create (Var.Watcher);
      Create (Var.Mutex);
      Var.Stream := null;
      Var.Offset := 0;

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
      Var_Data  : DSM_Data_Access;
      Var_Name  : constant String := String'Input (Query);
      Request   : Request_Record  := Request_Record'Input (Query);

   begin
      pragma Debug
        (D ("Handle request " & Image (Request) &
            " for variable " & Var_Name));

      Var_Data := DSM_Data_Access (Lookup_Variable (Var_Name));
      if Request.Kind in Write_Data .. Read_Data then
         if Var_Data.Stream /= null then
            Free (Var_Data.Stream);
         end if;
         Var_Data.Stream := Request.Stream;
         Var_Data.Offset := 0;

         if Request.Kind = Write_Data then
            if Request.Copies /= null then
               Merge (Request.Copies.all, Var_Data.Copies);
               Free  (Request.Copies);
            end if;
            Var_Data.Owner := Self_PID;

         else
            Var_Data.Owner := Partition;
         end if;
         Update (Var_Data.Watcher);

      else
         Enter (Var_Data.Mutex);
         if Request.Kind = Cancel_Rqst then
            if Var_Data.Status /= None then
               Invalidate
                 (Var_Name'Unrestricted_Access,
                  Var_Data.Copies,
                  Request.Owner);
               Var_Data.Status := None;
               Var_Data.Owner  := Request.Owner;
               if Var_Data.Stream /= null then
                  Free (Var_Data.Stream);
               end if;
            end if;

         elsif Request.Kind = Read_Rqst then
            if Var_Data.Status /= None then
               Merge ((1 => Request.Reply_To), Var_Data.Copies);
               Var_Data.Status := Read;
               Send_Request
                 (Request.Reply_To,
                  Var_Data.Name,
                  (Read_Data, Var_Data.Stream, null),
                  Error);

            else
               Send_Request
                 (Var_Data.Owner,
                  Var_Data.Name,
                  Request,
                  Error);
               Var_Data.Owner := Request.Reply_To;
            end if;

         else
            if Var_Data.Owner = Self_PID then
               Var_Data.Status := None;
               Send_Request
                 (Request.Reply_To,
                  Var_Data.Name,
                  (Write_Data, Var_Data.Stream, Var_Data.Copies),
                  Error);
               Free (Var_Data.Copies);
               if Var_Data.Stream /= null then
                  Free (Var_Data.Stream);
               end if;

            else
               Send_Request
                 (Var_Data.Owner,
                  Var_Data.Name,
                  Request,
                  Error);
               Var_Data.Owner := Request.Reply_To;
            end if;
         end if;
         Leave (Var_Data.Mutex);
      end if;
   end Handle_Request;

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

   function Image (R : Request_Record) return String is
   begin
      case R.Kind is
         when Write_Rqst | Read_Rqst =>
            return "(" & R.Kind'Img & "," & R.Reply_To'Img & ")";

         when Write_Data | Read_Data =>
            return "(" & R.Kind'Img &
              ", " & Image (R.Stream) &
              ", " & Image (R.Copies) & ")";
         when Cancel_Rqst =>
            return "(" & R.Kind'Img & "," & R.Owner'Img & ")";
      end case;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Root : DSM_Data_Access := new DSM_Data_Type;

   begin
      pragma Debug (D ("register request handler"));
      Register_Handler (DSM_Service, Handle_Request'Access);
      pragma Debug (D ("register storage support"));
      Register_Storage (DSM_Storage_Name, Shared_Data_Access (Root));
   end Initialize;

   ----------------------
   -- Initiate_Request --
   ----------------------

   procedure Initiate_Request
     (Var_Data : in out DSM_Data_Type;
      Request  : in Request_Type;
      Success  : out Boolean)
   is
      Version : Version_Id;
      Error   : aliased Error_Type;

   begin
      Enter (Var_Data.Mutex);
      pragma Debug (D ("initiate request with mode " & Request'Img));
      case Request is
         when Write =>
            if Var_Data.Status /= Write then
               Lookup (Var_Data.Watcher, Version);
               pragma Debug
                 (D ("ask partition" & Var_Data.Owner'Img &
                     " for write copy of variable " & Var_Data.Name.all));
               Send_Request
                 (Var_Data.Owner,
                  Var_Data.Name,
                  (Write_Rqst, Self_PID),
                  Error);
               Differ (Var_Data.Watcher, Version);
               pragma Debug
                 (D ("invalidate copies of variable " & Var_Data.Name.all &
                     " in partitions " & Image (Var_Data.Copies)));
               Invalidate (Var_Data.Name, Var_Data.Copies, Self_PID);
               Var_Data.Status := Write;
            end if;

         when Read =>
            if Var_Data.Status = None then
               Lookup (Var_Data.Watcher, Version);
               pragma Debug
                 (D ("ask partition" & Var_Data.Owner'Img &
                     " for read copy of variable " & Var_Data.Name.all));
               Send_Request
                 (Var_Data.Owner,
                  Var_Data.Name,
                  (Read_Rqst, Self_PID),
                  Error);
               Differ (Var_Data.Watcher, Version);
               Var_Data.Status := Read;
            end if;

         when others =>
            null;
      end case;
      Var_Data.Offset := 0;
      Success := (Var_Data.Stream /= null);
      if Var_Data.Stream = null then
         Leave (Var_Data.Mutex);
      end if;
   end Initiate_Request;

   -----------
   -- Input --
   -----------

   function Input
     (S : access Ada.Streams.Root_Stream_Type'Class)
     return Request_Record
   is
      Request : Request_Record (Request_Kind'Input (S));

   begin
      case Request.Kind is
         when Write_Data .. Read_Data =>
            Stream_Element_Access'Read (S, Request.Stream);
            if Request.Kind = Write_Data then
               Copy_Set_Access'Read (S, Request.Copies);
            end if;

         when Write_Rqst .. Read_Rqst =>
            Partition_ID'Read (S, Request.Reply_To);

         when Cancel_Rqst =>
            Partition_ID'Read (S, Request.Owner);
      end case;
      return Request;
   end Input;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (Var_Name : in     String_Access;
      Copy_Set : in out Copy_Set_Access;
      Owner    : in     Partition_ID)
   is
      Error : aliased Error_Type;

   begin
      if Copy_Set /= null then
         for I in Copy_Set'Range loop
            Send_Request
              (Copy_Set (I),
               Var_Name,
               (Cancel_Rqst, Owner),
               Error);
         end loop;
         Free (Copy_Set);
      end if;
   end Invalidate;

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
      X : in Request_Record) is
   begin
      Request_Kind'Write (S, X.Kind);
      case X.Kind is
         when Write_Data .. Read_Data =>
            Stream_Element_Access'Write (S, X.Stream);
            if X.Kind = Write_Data then
               Copy_Set_Access'Write (S, X.Copies);
            end if;

         when Write_Rqst .. Read_Rqst =>
            Partition_ID'Write (S, X.Reply_To);

         when Cancel_Rqst =>
            Partition_ID'Write (S, X.Owner);
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

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Partition : in  Partition_ID;
      Var_Name  : in  String_Access;
      Request   : in  Request_Record;
      Error     : in out Error_Type)
   is
      Stream : aliased Params_Stream_Type (0);

   begin
      if Partition = Self_PID then
         return;
      end if;
      String'Output         (Stream'Access, Var_Name.all);
      Request_Record'Output (Stream'Access, Request);
      pragma Debug (D ("send request " & Image (Request)));
      Send (Partition, DSM_Service, Stream'Access, Error);
   end Send_Request;

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
         Data.Offset  := Data.Stream'Last;

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
