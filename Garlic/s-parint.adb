------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--          S Y S T E M . P A R T I T I O N _ I N T E R F A C E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.24                             --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNAT.Htable;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);
with System.RPC; use System.RPC;
pragma Elaborate_All (System.RPC);

package body System.Partition_Interface is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("INTERFACE", "(s-parint): ");
   procedure D
     (Level   : in Debug_Levels;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type String_Access is access String;

   procedure Free is
      new Ada.Unchecked_Deallocation (Unit_Name, Unit_Name_Access);

   type Unit_Info is record
      Name     : Unit_Name_Access;
      Id       : Partition_ID;
      Version  : String_Access;
      Receiver : RPC_Receiver;
      Known    : Boolean := False;
      Queried  : Boolean := False;
      Empty    : Boolean := False;
   end record;
   type Unit_Info_Access is access Unit_Info;
   --  Information on a unit.

   No_Unit_Info : constant Unit_Info_Access := null;

   type Hash_Index is range 1 .. 101;
   function Hash_Function (F : Unit_Name_Access) return Hash_Index;
   function Equal (F1, F2 : Unit_Name_Access) return Boolean;
   package Unit_Htable is
      new GNAT.Htable.Simple_Htable (Header_Num => Hash_Index,
                                     Element    => Unit_Info_Access,
                                     No_Element => No_Unit_Info,
                                     Key        => Unit_Name_Access,
                                     Hash       => Hash_Function,
                                     Equal      => Equal);
   use Unit_Htable;
   --  This Htable is used to store and retrieve quickly unit information.

   protected type Unit_Map_Type is
      entry Lock;
      procedure Unlock;
      entry Get (Name : in Unit_Name; Info : out Unit_Info_Access);
      function Get_Immediate (Name : Unit_Name) return Unit_Info_Access;
   private
      entry Get_Waiting (Name : in Unit_Name; Info : out Unit_Info_Access);
      Locked      : Boolean := False;
      In_Progress : Boolean := False;
   end Unit_Map_Type;
   --  This unit map stores and retrieves information about a unit. The first
   --  time a unit is queried, if it's not registered, it sets Known to
   --  False and Queried to True. This doesn't occur if we are on the
   --  main partition (this is blocking until the information is known).
   --  Requeuing on Get_Waiting may seem inefficient (in fact it is :-) but
   --  this is yet the most simple solution because Unit_Name is not
   --  a small-enough constrained type.

   type Unit_Map_Access is access Unit_Map_Type;
   procedure Free is new
     Ada.Unchecked_Deallocation (Unit_Map_Type, Unit_Map_Access);

   Unit_Map : Unit_Map_Access := new Unit_Map_Type;

   function Get_Unit_Info
     (Name        : Unit_Name;
      Elaboration : Elaboration_Access := null)
      return Unit_Info;
   --  Get unit info (and retrieve it if needed).

   type Name_Opcode is (Get_Unit_Info, Set_Unit_Info);
   --  Opcode to discuss.

   Public_Receiver_Is_Installed : Boolean := False;
   protected Public_Receiver_Installed is
      procedure Check;
   end Public_Receiver_Installed;
   --  See comment in System.RPC (body).

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Opcode;
      Params    : access Params_Stream_Type);
   --  Receive data.

   procedure Partition_RPC_Receiver
     (Params : access Params_Stream_Type;
      Result : access Params_Stream_Type);
   --  Global RPC receiver.

   Partition_RPC_Receiver_Installed : Boolean := False;
   --  No need to protect this because it will be called only during
   --  packages elaboration, that is sequentially.

   procedure Send_Unit_Info
     (Partition : in Partition_ID; Info : in Unit_Info);
   --  Send a Set_Unit_Info to this partition.

   procedure Read_Unit_Info
     (Params : access Params_Stream_Type; Info : access Unit_Info);
   --  Read unit information from this stream.

   task Shutdown_Waiter;
   --  This task waits for Shutdown_Keeper from being unblocked.

   procedure Shutdown;
   --  Called on shutdown.

   ----------------------
   -- Elaboration_Type --
   ----------------------

   protected body Elaboration_Type is

      ---------------------------
      -- Complete_Invalidation --
      ---------------------------

      procedure Complete_Invalidation is
      begin
         raise Program_Error; --  XXXXX Not implemented
      end Complete_Invalidation;

      ------------------
      -- Get_RCI_Data --
      ------------------

      entry Get_RCI_Data
        (Receiver  : out System.RPC.RPC_Receiver;
         Partition : out System.RPC.Partition_ID;
         Done      : out Boolean)
      when True is
      begin
         if RCI_Unit = null then
            Done      := False;

            --  Those two initializations are needed to not raise
            --  Constraint_Error.

            Receiver  := null;
            Partition := Partition_ID'First;

         else
            Done      := True;
            Receiver  := Package_Receiver;
            Partition := Active_Partition;
         end if;
      end Get_RCI_Data;

      ------------------
      -- Get_RCI_Unit --
      ------------------

      function Get_RCI_Unit return Unit_Name_Access is
      begin
         return RCI_Unit;
      end Get_RCI_Unit;

      ---------------------------
      -- Initiate_Invalidation --
      ---------------------------

      procedure Initiate_Invalidation is
      begin
         raise Program_Error; --  XXXXX Not implemented
      end Initiate_Invalidation;

      ------------------
      -- Set_RCI_Data --
      ------------------

      procedure Set_RCI_Data
        (RCI_Name  : Unit_Name_Access;
         Receiver  : System.RPC.RPC_Receiver;
         Partition : System.RPC.Partition_ID)
      is
      begin
         RCI_Unit         := RCI_Name;
         Package_Receiver := Receiver;
         Active_Partition := Partition;
      end Set_RCI_Data;

   end Elaboration_Type;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Unit_Name_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID (RCI_Unit : Unit_Name)
     return Partition_ID is
      Partition : Partition_ID;
   begin
      Partition := Get_Unit_Info (RCI_Unit).Id;
      return Partition;
   end Get_Active_Partition_ID;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (RCI_Unit    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
     return Partition_ID is
      Receiver  : RPC_Receiver;
      Partition : Partition_ID;
      Done      : Boolean;
   begin
      Elaboration.Get_RCI_Data (Receiver, Partition, Done);
      if Done then
         return Partition;
      else
         return Get_Unit_Info (RCI_Unit.all, Elaboration) .Id;
      end if;
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version (RCI_Unit : Unit_Name) return String is
   begin
      return Get_Unit_Info (RCI_Unit).Version.all;
   end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID return Partition_ID is
   begin
      return Get_My_Partition_ID;
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_RCI_package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver (RCI_Unit : in Unit_Name)
     return RPC_Receiver is
   begin
      return Get_Unit_Info (RCI_Unit).Receiver;
   end Get_RCI_Package_Receiver;

   ------------------------------
   -- Get_RCI_package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (RCI_Unit    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
     return RPC_Receiver is
      Receiver  : RPC_Receiver;
      Partition : Partition_ID;
      Done      : Boolean;
   begin
      Elaboration.Get_RCI_Data (Receiver, Partition, Done);
      if Done then
         return Receiver;
      else
         return Get_Unit_Info (RCI_Unit.all, Elaboration) .Receiver;
      end if;
   end Get_RCI_Package_Receiver;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info
     (Name        : Unit_Name;
      Elaboration : Elaboration_Access := null)
     return Unit_Info is
      Result_P : Unit_Info_Access;
      Low_Name : constant Unit_Name := To_Lower (Name);
   begin

      D (D_RNS, "I am being asked information about package " & Low_Name);

      if not Partition_RPC_Receiver_Installed then
         Establish_RPC_Receiver (Get_Local_Partition_ID,
                                 Partition_RPC_Receiver'Access);
         Partition_RPC_Receiver_Installed := True;
      end if;

      Unit_Map.Get (Low_Name, Result_P);
      if Result_P.Queried then

         --  There is some work for us... Let's query some info.

         declare
            Params : aliased Params_Stream_Type (0);
         begin
            D (D_RNS, "Querying info for package " & Low_Name);
            Name_Opcode'Write (Params'Access, Get_Unit_Info);
            Unit_Name'Output (Params'Access, Low_Name);
            Send (Get_Boot_Server, Name_Service, Params'Access);
         end;

         --  Waiting again.

         Unit_Map.Get (Low_Name, Result_P);
      end if;

      D (D_RNS, "Info for package " & Low_Name & " is available");
      if Elaboration /= null then
         Elaboration.Set_RCI_Data (Result_P.Name,
                                   Result_P.Receiver,
                                   Result_P.Id);
      end if;
      return Result_P.all;
   end Get_Unit_Info;

   -------------------
   -- Hash_Function --
   -------------------

   function Hash_Function (F : Unit_Name_Access) return Hash_Index is
      function Hash_Unit_Name is
         new GNAT.Htable.Hash (Hash_Index);
   begin
      return Hash_Unit_Name (F.all);
   end Hash_Function;

   -------------------------------
   -- Invalidate_Receiving_Stub --
   -------------------------------

   procedure Invalidate_Receiving_Stub
     (RCI_Unit  : in Unit_Name_Access;
      Partition : in RPC.Partition_ID)
   is
   begin
      raise Program_Error; --  XXXXX Not implemented
   end Invalidate_Receiving_Stub;

   ----------------------------
   -- Partition_RPC_Receiver --
   ----------------------------

   procedure Partition_RPC_Receiver
     (Params : access Params_Stream_Type;
      Result : access Params_Stream_Type)
   is
      Receiver : RPC_Receiver;
   begin
      RPC_Receiver'Read (Params, Receiver);
      Receiver (Params, Result);
   end Partition_RPC_Receiver;

   -------------------------------
   -- Public_Receiver_Installed --
   -------------------------------

   protected body Public_Receiver_Installed is

      -----------
      -- Check --
      -----------

      procedure Check is
      begin
         Receive (Name_Service, Public_RPC_Receiver'Access);
         Public_Receiver_Is_Installed := True;
      end Check;

   end Public_Receiver_Installed;

   -------------------------
   -- Public_RPC_Receiver --
   -------------------------

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Opcode;
      Params    : access Params_Stream_Type)
   is
      Code : Name_Opcode;
   begin

      D (D_Debug,
         "Got something from partition" & Partition_ID'Image (Partition));

      Name_Opcode'Read (Params, Code);
      case Code is

         when Get_Unit_Info =>
            declare
               Name   : Unit_Name_Access;
               Info   : Unit_Info;
            begin
               Name := new Unit_Name'(Unit_Name'Input (Params));
               Info := Get_Unit_Info (Name.all);
               D (D_RNS,
                  "Answering unit info request for package " & Name.all &
                  " from partition" & Partition_ID'Image (Partition));
               Send_Unit_Info (Partition, Info);
               Free (Name);
            end;

         when Set_Unit_Info =>
            declare
               Name : Unit_Name_Access;
               Info : Unit_Info_Access;
            begin
               Name := new Unit_Name'(Unit_Name'Input (Params));
               D (D_RNS,
                  "Got unit info for package " & Name.all);
               Unit_Map.Lock;
               Info := Unit_Map.Get_Immediate (Name.all);
               Read_Unit_Info (Params, Info);
               Info.Known := True;
               Info.Queried := False;
               Unit_Map.Unlock;
               D (D_RNS, "Registered unit info for package " & Name.all);
               Free (Name);
            end;

      end case;
   end Public_RPC_Receiver;

   --------------------
   -- Read_Unit_Info --
   --------------------

   procedure Read_Unit_Info
     (Params : access Params_Stream_Type;
      Info   : access Unit_Info)
   is
   begin
      Partition_ID'Read (Params, Info.Id);
      Info.Version := new String'(String'Input (Params));
      RPC_Receiver'Read (Params, Info.Receiver);
   end Read_Unit_Info;

   ---------------------------
   -- Register_Calling_Stub --
   ---------------------------

   procedure Register_Calling_Stub
     (Partition   : in Partition_ID;
      Elaboration : in Elaboration_Access)
   is
   begin
      raise Program_Error; --  XXXXX Not implemented
   end Register_Calling_Stub;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (RCI_Unit : in Unit_Name;
      Receiver : in RPC_Receiver;
      Version  : in String := "")
   is
      Info : Unit_Info_Access;
   begin
      if not Partition_RPC_Receiver_Installed then
         Establish_RPC_Receiver (Get_Local_Partition_ID,
                                 Partition_RPC_Receiver'Access);
         Partition_RPC_Receiver_Installed := True;
      end if;
      Unit_Map.Lock;
      Info := Unit_Map.Get_Immediate (To_Lower (RCI_Unit));
      Info.Id       := Get_My_Partition_ID;
      Info.Receiver := Receiver;
      Info.Version := new String'(Version);
      Info.Known := True;
      Info.Queried := False;
      Unit_Map.Unlock;
      if not Is_Boot_Partition then
         Send_Unit_Info (Get_Boot_Server, Info.all);
      end if;
   end Register_Receiving_Stub;

   --------------------
   -- Send_Unit_Info --
   --------------------

   procedure Send_Unit_Info
     (Partition : in Partition_ID; Info : in Unit_Info)
   is
      Params : aliased Params_Stream_Type (0);
   begin
      Name_Opcode'Write (Params'Access, Set_Unit_Info);
      Unit_Name'Output (Params'Access, Info.Name.all);
      Partition_ID'Write (Params'Access, Info.Id);
      String'Output (Params'Access, Info.Version.all);
      RPC_Receiver'Write (Params'Access, Info.Receiver);
      Send (Partition, Name_Service, Params'Access);
   end Send_Unit_Info;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Free (Unit_Map);
   end Shutdown;

   ---------------------
   -- Shutdown_Waiter --
   ---------------------

   task body Shutdown_Waiter is
   begin
      System.Garlic.Termination.Add_Non_Terminating_Task;
      Shutdown_Keeper.Wait;
      D (D_Debug, "Shutdown_Waiter exiting because of Shutdown_Keeper");
      Shutdown;
      System.Garlic.Termination.Sub_Non_Terminating_Task;
   end Shutdown_Waiter;

   -------------------
   -- Unit_Map_Type --
   -------------------

   protected body Unit_Map_Type is

      ---------
      -- Get --
      ---------

      entry Get (Name : in Unit_Name; Info : out Unit_Info_Access)
      when not In_Progress is
         Result : Unit_Info_Access;
      begin
         Result := Get_Immediate (Name);
         if not Result.Known then
            if Result.Queried or Is_Boot_Partition then
               requeue Get_Waiting with abort;
            end if;
            Result.Queried := True;
         end if;
         Info := Result;
      end Get;

      -------------------
      -- Get_Immediate --
      -------------------

      function Get_Immediate (Name : Unit_Name) return Unit_Info_Access is
         Name_P : Unit_Name_Access := new Unit_Name'(Name);
         Result : Unit_Info_Access := Get (Name_P);
      begin
         if Result = No_Unit_Info then
            Result := new Unit_Info;
            Result.Name := Name_P;
            Set (Name_P, Result);
         else
            Free (Name_P);
         end if;
         return Result;
      end Get_Immediate;

      -----------------
      -- Get_Waiting --
      -----------------

      entry Get_Waiting (Name : in Unit_Name; Info : out Unit_Info_Access)
      when In_Progress is
      begin
         if Get_Waiting'Count = 0 then
            In_Progress := False;
         end if;
         requeue Get with abort;
      end Get_Waiting;

      ----------
      -- Lock --
      ----------

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Locked := False;
         if Get_Waiting'Count > 0 then
            In_Progress := True;
         end if;
      end Unlock;

   end Unit_Map_Type;

begin
   Public_Receiver_Installed.Check;
end System.Partition_Interface;
