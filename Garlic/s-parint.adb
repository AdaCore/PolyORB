------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Utils; use System.Garlic.Utils;
with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
with System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);
with System.RPC; use System.RPC;
pragma Elaborate_All (System.RPC);

with GNAT.IO; use GNAT.IO;

package body System.Partition_Interface is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("INTERFACE", "(s-parint): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type String_Access is access String;

   procedure Free is
      new Ada.Unchecked_Deallocation (Unit_Name, Unit_Name_Access);

   protected type Cache_Type is

      procedure Get_RCI_Data
        (Receiver  : out RPC_Receiver;
         Partition : out Partition_ID;
         Done      : out Boolean);
      --  Return Receiver and Partition if stored in the cache.

      procedure Set_RCI_Data
        (Receiver  : in RPC_Receiver;
         Partition : in Partition_ID);
      --  Store Receiver and Partition in the cache.

      procedure Unset_RCI_Data;
      --  Invalidate the cache.

      procedure Set_RCI_Name (Name : in Unit_Name);
      --  Set the unit name that it protects.

      function  Get_RCI_Name return Unit_Name_Access;
      --  Return the unit name that it protects.

   private

      Cache_Consistent : Boolean := False;
      Active_Partition : Partition_ID;
      Package_Receiver : RPC_Receiver;
      RCI_Package_Name : Unit_Name_Access;

   end Cache_Type;
   type Cache_Access is access Cache_Type;

   type Unit_Status is (Unknown, Queried, Known);
   type Requests_Type is array (Partition_ID) of Boolean;

   type Unit_Info;
   type Unit_Info_Access is access Unit_Info;
   type Unit_Info is record
      Name      : Unit_Name_Access;
      Partition : Partition_ID;
      Version   : String_Access;
      Receiver  : RPC_Receiver;
      Cache     : Cache_Access;
      Status    : Unit_Status := Unknown;
      Pending   : Boolean := False;
      Requests  : Requests_Type := (others => False);
      Next      : Unit_Info_Access;
   end record;
   --  A hash table would be more efficient but do we really need it ?

   Unit_Info_Root : Unit_Info_Access := null;

   Units_Keeper : Semaphore_Access := new Semaphore_Type;

   type Name_Opcode is (Get_Unit_Info, Set_Unit_Info, Unset_Unit_Info);
   --  Opcode of requests exchanged between the boot server cache and
   --  the slave caches.

   procedure Public_Message_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Global message receiver.

   procedure Partition_RPC_Receiver
     (Params : access Params_Stream_Type;
      Result : access Params_Stream_Type);
   --  Global RPC receiver.

   Partition_RPC_Receiver_Installed : Boolean := False;
   --  No need to protect this because it will be called only during
   --  packages elaboration, that is sequentially.

   function Get_Unit_Info
     (Name  : Unit_Name_Access;
      Cache : Cache_Access)
      return Unit_Info;
   --  Get unit info (and retrieve it if needed).

   function Get_Unit_Info
     (Name  : Unit_Name)
      return Unit_Info;
   --  Get unit info (and retrieve it if needed).

   procedure Send_Unit_Info
     (Partition : in Partition_ID;
      Info      : in Unit_Info_Access;
      Opcode    : in Name_Opcode := Set_Unit_Info);
   --  Send a Set_Unit_Info to this partition.

   task Shutdown_Waiter;
   --  This task waits for Shutdown_Keeper from being unblocked.

   procedure Shutdown;
   --  Called on shutdown.

   Upper_To_Lower : constant := Character'Pos ('a') - Character'Pos ('A');

   function To_Lower (Item : String) return String;
   --  Guess what.

   function Get_Active_Partition_ID
     (Cache : Cache_Access)
      return Partition_ID;
   --  Similar to previous Get_Active_Partition_ID,
   --  but uses a protected type.

   function Get_RCI_Package_Receiver
     (Cache : Cache_Access)
      return RPC_Receiver;
   --  Similar to previous Get_RCI_Package_Receiver,
   --  but uses a protected type.

   function Find (Name : Unit_Name_Access) return Unit_Info_Access;
   --  Find unit info in Htable, otherwise allocate a unit info node.

   function Allocate (Name : Unit_Name) return Cache_Access;
   --  Allocate a new cache and set its package name.

   ----------------
   -- Cache_Type --
   ----------------

   protected body Cache_Type is

      --------------------
      -- Unset_RCI_Data --
      --------------------

      procedure Unset_RCI_Data is
      begin
         Cache_Consistent := False;
      end Unset_RCI_Data;

      ------------------
      -- Get_RCI_Data --
      ------------------

      procedure Get_RCI_Data
        (Receiver  : out RPC_Receiver;
         Partition : out Partition_ID;
         Done      : out Boolean) is
      begin
         if not Cache_Consistent then
            Done      := False;
            Receiver  := null;
            Partition := Partition_ID'First;
         else
            Done      := True;
            Receiver  := Package_Receiver;
            Partition := Active_Partition;
         end if;
      end Get_RCI_Data;

      ------------------
      -- Set_RCI_Data --
      ------------------

      procedure Set_RCI_Data
        (Receiver  : in RPC_Receiver;
         Partition : in Partition_ID) is
      begin
         Cache_Consistent := True;
         Package_Receiver := Receiver;
         Active_Partition := Partition;
      end Set_RCI_Data;

      ------------------
      -- Get_RCI_Name --
      ------------------

      function Get_RCI_Name return Unit_Name_Access is
      begin
         return RCI_Package_Name;
      end Get_RCI_Name;

      ------------------
      -- Set_RCI_Name --
      ------------------

      procedure Set_RCI_Name (Name : in Unit_Name) is
      begin
         RCI_Package_Name := new Unit_Name'(Name);
      end Set_RCI_Name;

   end Cache_Type;

   --------------
   -- Allocate --
   --------------

   function Allocate (Name : in Unit_Name) return Cache_Access is
      Cache : Cache_Access := new Cache_Type;
   begin
      Cache.Set_RCI_Name (To_Lower (Name));
      return Cache;
   end Allocate;

   ----------
   -- Find --
   ----------

   function Find (Name : Unit_Name_Access) return Unit_Info_Access is
      Unit  : Unit_Info_Access := Unit_Info_Root;
   begin
      while Unit /= null and then Unit.Name.all /= Name.all loop
         Unit := Unit.Next;
      end loop;
      if Unit = null then
         Unit := new Unit_Info;
         Unit.Name := new Unit_Name'(Name.all);
         Unit.Next      := Unit_Info_Root;
         Unit_Info_Root := Unit;
      end if;
      return Unit;
   end Find;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         if Item (I) in 'A' .. 'Z' then
            Result (I) :=
               Character'Val (Character'Pos (Item (I)) + Upper_To_Lower);
         else
            Result (I) := Item (I);
         end if;
      end loop;
      return Result;
   end To_Lower;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (Cache : Cache_Access)
     return Partition_ID is
      Receiver  : RPC_Receiver;
      Partition : Partition_ID;
      Done      : Boolean;
   begin
      Cache.Get_RCI_Data (Receiver, Partition, Done);
      if Done then
         return Partition;
      else
         return Get_Unit_Info (Cache.Get_RCI_Name, Cache).Partition;
      end if;
   end Get_Active_Partition_ID;

   ------------------------------
   -- Get_RCI_package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Cache : Cache_Access)
     return RPC_Receiver is
      Receiver  : RPC_Receiver;
      Partition : Partition_ID;
      Done      : Boolean;
   begin
      Cache.Get_RCI_Data (Receiver, Partition, Done);
      if Done then
         return Receiver;
      else
         return Get_Unit_Info (Cache.Get_RCI_Name, Cache) .Receiver;
      end if;
   end Get_RCI_Package_Receiver;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info
     (Name  : Unit_Name)
      return Unit_Info is
      Unit : Unit_Name_Access;
      Info : Unit_Info;
   begin
      Unit := new Unit_Name'(Name);
      Info := Get_Unit_Info (Unit, null);
      Free (Unit);
      return Info;
   end Get_Unit_Info;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info
     (Name  : Unit_Name_Access;
      Cache : Cache_Access)
     return Unit_Info is
      Unit   : Unit_Info_Access;
      Status : Unit_Status;
   begin

      if not Partition_RPC_Receiver_Installed then
         Establish_RPC_Receiver
           (Get_Local_Partition_ID,
            Partition_RPC_Receiver'Access);
         Partition_RPC_Receiver_Installed := True;
      end if;

      --  Loop while info are not available.
      loop

         pragma Debug (D (D_RNS, "Get info from " & Name.all));
         begin

            --  We don't want to be aborted since we are going to
            --  lock some resources.
            pragma Abort_Defer;

            Units_Keeper.Lock;
            Unit   := Find (Name);
            Status := Unit.Status;

            if Unit.Status = Unknown then

               --  If not on boot server, send a request.

               if not Is_Boot_Partition then
                  declare
                     Params : aliased Params_Stream_Type (0);
                  begin
                     pragma Debug (D (D_RNS, "Query RNS for " & Name.all));
                     Name_Opcode'Write (Params'Access, Get_Unit_Info);
                     Unit_Name'Output (Params'Access, Name.all);
                     Send (Get_Boot_Server, Name_Service, Params'Access);
                     Unit.Status := Queried;
                  end;
               end if;

               --  Depending on who is calling this procedure, the cache has
               --  been allocated or not. If this is the case, update it.

               if Cache /= null then
                  Unit.Cache  := Cache;
               end if;

            --  When info are available, update Unit.Cache if needed,
            --  unlock the resource and exit the loop.

            elsif Status = Known then
               pragma Debug (D (D_RNS, "Query locally for " & Name.all));
               if Unit.Cache = null then
                  Unit.Cache  := Cache;
               end if;
               Units_Keeper.Unlock (Unmodified);
               exit;
            end if;

            --  If the status is queried or unknown, then unlock the
            --  the resource and suspend execution until a unit has
            --  been updated.

            Units_Keeper.Unlock (Wait_Until_Modified);
            pragma Debug (D (D_RNS, "Resume request for " & Name.all));

         end;

      end loop;

      if Unit.Cache /= null then
         pragma Debug (D (D_RNS, "Update cache for " & Name.all));
         Unit.Cache.Set_RCI_Data (Unit.Receiver, Unit.Partition);
      end if;
      return Unit.all;

   end Get_Unit_Info;

   ----------------------------
   -- Partition_RPC_Receiver --
   ----------------------------

   procedure Partition_RPC_Receiver
     (Params : access Params_Stream_Type;
      Result : access Params_Stream_Type) is
      Receiver : RPC_Receiver;
   begin
      RPC_Receiver'Read (Params, Receiver);
      Receiver (Params, Result);
   end Partition_RPC_Receiver;

   -----------------------------
   -- Public_Message_Receiver --
   -----------------------------

   procedure Public_Message_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type) is
      Code : Name_Opcode;
      Info : Unit_Info_Access;
      Name : Unit_Name_Access;
   begin

      pragma Debug
         (D (D_Debug, "Got something from partition" & Partition'Img));

      Name_Opcode'Read (Params, Code);
      if not Code'Valid then
         pragma Debug (D (D_Debug, "Invalid name code received"));
         raise Constraint_Error;
      end if;
      Name := new Unit_Name'(Unit_Name'Input (Params));

      pragma Debug (D (D_Debug, "Request " & Code'Img & " for " & Name.all));

      --  Dispatch accoring to opcode.

      case Code is

         --  This code is executed on the boot server.

         when Get_Unit_Info =>

            Units_Keeper.Lock;

            pragma Debug (D (D_RNS, "Get request on " & Name.all));
            Info := Find (Name);

            if Info.Status = Known then
               pragma Debug (D (D_RNS, "Send info on " & Name.all));
               Send_Unit_Info (Partition, Info);

            else
               pragma Debug (D (D_RNS, "Queue request on " & Name.all));
               Info.Requests (Partition) := True;
               Info.Pending := True;
            end if;

            Units_Keeper.Unlock (Unmodified);

         when Set_Unit_Info =>

            Units_Keeper.Lock;

            pragma Debug (D (D_RNS, "Set request on " & Name.all));

            Info := Find (Name);
            Partition_ID'Read (Params, Info.Partition);

            if not Info.Partition'Valid then
               pragma Debug (D (D_Debug, "Invalid partition ID received"));
               raise Constraint_Error;
            end if;

            RPC_Receiver'Read (Params, Info.Receiver);
            Info.Version := new String'(String'Input (Params));
            Info.Status := Known;

            --  Requests are pending only on the boot server.

            if Info.Pending then
               pragma Debug
                 (D (D_RNS, "Answer to pending requests on " & Name.all));
               for P in RPC.Partition_ID loop
                  if Info.Requests (P) then
                     Send_Unit_Info (P, Info);
                     Info.Requests (P) := False;
                  end if;
               end loop;
               Info.Pending := False;
            end if;

            Units_Keeper.Unlock (Modified);



         when Unset_Unit_Info =>

            --  When a unit is no longer available because the partition
            --  has crashed, remove it from the cache.

            declare
               Partition : Partition_ID;
               Receiver  : RPC_Receiver;
               Version   : String_Access;
            begin

               Units_Keeper.Lock;
               pragma Debug (D (D_RNS, "Unset info on " & Name.all));
               Info := Find (Name);
               Partition_ID'Read (Params, Partition);

               if Info.Partition = Partition then
                  Info.Status := Unknown;
                  Units_Keeper.Unlock (Modified);

               else
                  pragma Debug (D (D_RNS, "Invalid unset for " & Name.all));
                  Units_Keeper.Unlock (Unmodified);

               end if;

            end;

      end case;
      Free (Name);

   end Public_Message_Receiver;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      Cache : Cache_Access := Allocate (Name);

      -----------------------------
      -- Get_Active_Partition_ID --
      -----------------------------

      function Get_Active_Partition_ID return Partition_ID is
      begin
         return Get_Active_Partition_ID (Cache);
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return RPC_Receiver is
      begin
         return Get_RCI_Package_Receiver (Cache);
      end Get_RCI_Package_Receiver;

   end RCI_Info;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC_Receiver;
      Version  : in String := "") is

      --  This procedure should not be aborted at this stage.
      --  Resources are locked and data should be kept consistent
      --  between tables.

      Info : Unit_Info_Access;
      Unit : Unit_Name_Access;

   begin

      pragma Abort_Defer;

      if not Partition_RPC_Receiver_Installed then
         Establish_RPC_Receiver (Get_Local_Partition_ID,
                                 Partition_RPC_Receiver'Access);
         Partition_RPC_Receiver_Installed := True;
      end if;

      Unit           := new Unit_Name'(To_Lower (Name));
      Units_Keeper.Lock;

      pragma Debug (D (D_RNS, "Register package " & Unit.all));

      Info           := Find (Unit);
      Info.Partition := Get_My_Partition_ID;
      Info.Receiver  := Receiver;
      Info.Version   := new String'(Version);
      Info.Status    := Known;

      --  Query the boot server.
      if not Is_Boot_Partition then
         pragma Debug (D (D_RNS, "Send RNS info on " & Unit.all));
         Send_Unit_Info (Get_Boot_Server, Info);

      --  This code is executed only by the boot server.
      elsif Info.Pending then
         pragma Debug (D (D_RNS, "Answer pending requests on " & Unit.all));
         for P in Partition_ID loop
            if Info.Requests (P) then
               Send_Unit_Info (P, Info);
               Info.Requests (P) := False;
            end if;
         end loop;
         Info.Pending := False;
      end if;

      --  We signal the modification to unblock tasks waiting for
      --  a modification (Wait_Until_Modified).
      Units_Keeper.Unlock (Modified);
      Free (Unit);

   end Register_Receiving_Stub;

   -------------------------------
   -- Invalidate_Receiving_Stub --
   -------------------------------

   procedure Invalidate_Receiving_Stub
     (Name     : in Unit_Name) is

      --  Name is not always in lower case. This procedure should not be
      --  aborted at this stage.

      Info : Unit_Info_Access;
      Unit : Unit_Name_Access;

   begin

      pragma Abort_Defer;
      Unit := new Unit_Name'(To_Lower (Name));
      Units_Keeper.Lock;
      pragma Debug (D (D_RNS, "Invalidate info on " & Unit.all));
      Info := Find (Unit);
      Info.Status := Unknown;
      if not Is_Boot_Partition then
         pragma Debug (D (D_RNS, "Send RNS unset request for " & Unit.all));
         Send_Unit_Info (Get_Boot_Server, Info, Unset_Unit_Info);
      end if;
      pragma Debug (D (D_RNS, "Unset cached data for " & Unit.all));
      Info.Cache.Unset_RCI_Data;
      Units_Keeper.Unlock (Modified);
      Free (Unit);

   end Invalidate_Receiving_Stub;

   --------------------
   -- Send_Unit_Info --
   --------------------

   procedure Send_Unit_Info
     (Partition : in Partition_ID;
      Info      : in Unit_Info_Access;
      Opcode    : in Name_Opcode := Set_Unit_Info) is
      Params : aliased Params_Stream_Type (0);
   begin
      pragma Debug
        (D (D_RNS,
            "Send " & Opcode'Img &
            " on " & Info.Name.all &
            " to " & Partition'Img));
      Name_Opcode'Write (Params'Access, Opcode);
      Unit_Name'Output (Params'Access, Info.Name.all);
      Partition_ID'Write (Params'Access, Info.Partition);
      if Opcode = Set_Unit_Info then
         RPC_Receiver'Write (Params'Access, Info.Receiver);
         String'Output (Params'Access, Info.Version.all);
      end if;
      Send (Partition, Name_Service, Params'Access);
   end Send_Unit_Info;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Free (Units_Keeper);
   end Shutdown;

   ---------------------
   -- Shutdown_Waiter --
   ---------------------

   task body Shutdown_Waiter is
   begin
      System.Garlic.Termination.Add_Non_Terminating_Task;
      Shutdown_Keeper.Wait;
      pragma Debug
        (D (D_Debug, "Shutdown_Waiter exiting because of Shutdown_Keeper"));
      Shutdown;
      System.Garlic.Termination.Sub_Non_Terminating_Task;
   end Shutdown_Waiter;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return Partition_ID is
   begin
      pragma Debug (D (D_RNS, "Normal Get_Active_Partition_ID on " & Name));
      return Get_Unit_Info (To_Lower (Name)).Partition;
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version
     (Name : Unit_Name)
      return String is
   begin
      pragma Debug (D (D_RNS, "Normal Get_Active_Version on " & Name));
      return Get_Unit_Info (To_Lower (Name)).Version.all;
   end Get_Active_Version;

   ------------------------------
   -- Get_RCI_package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return RPC_Receiver is
   begin
      pragma Debug (D (D_RNS, "Normal Get_RCI_Package_Receiver on " & Name));
      return Get_Unit_Info (To_Lower (Name)).Receiver;
   end Get_RCI_Package_Receiver;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
      return Partition_ID is
   begin
      raise Program_Error; --  XXXXX Not implemented
      return Get_Unit_Info (To_Lower (Name)).Partition;
   end Get_Passive_Partition_ID;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID
     return Partition_ID is
   begin
      return Get_My_Partition_ID;
   end Get_Local_Partition_ID;

begin
   Receive (Name_Service, Public_Message_Receiver'Access);
end System.Partition_Interface;








