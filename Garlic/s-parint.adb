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

with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Utils; use System.Garlic.Utils;
with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
with System.Garlic.Units; use System.Garlic.Units;
pragma Elaborate_All (System.Garlic.Units);
with System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);
with System.RPC; use System.RPC;
pragma Elaborate_All (System.RPC);

package body System.Partition_Interface is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("INTERFACE", "(s-parint): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   procedure Process
     (N       : in Unit_Id;
      Request : in Request_Type;
      Unit    : in out Unit_Type;
      Status  : out Status_Type);

   procedure Public_Message_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Global message receiver.

   procedure Partition_RPC_Receiver
     (Params : access Params_Stream_Type;
      Result : access Params_Stream_Type);
   --  Global RPC receiver.

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id);

   Local_Partition  : constant Partition_ID := Get_My_Partition_ID;
   Get_Unit_Request : constant Request_Type
     := (Get_Unit, Local_Partition, null, null, null);

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return Partition_ID is
      N : String := Name;
      U : Unit_Id;

   begin
      To_Lower (N);
      U := Units.Get (N);
      pragma Debug (D (D_Debug, "Request Get_Active_Partition_ID"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return Units.Get (U).Partition;
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version
     (Name : Unit_Name)
      return String is
      N : String := Name;
      U : Unit_Id;

   begin
      To_Lower (N);
      U := Units.Get (N);
      pragma Debug (D (D_Debug, "Request Get_Active_Version"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return Units.Get (U).Version.all;
   end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID
     return Partition_ID is
   begin
      return Local_Partition;
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
      return Partition_ID is
   begin
      return Null_Partition_ID;
   end Get_Passive_Partition_ID;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return RPC_Receiver is
      N : String := Name;
      U : Unit_Id;

   begin
      To_Lower (N);
      U := Units.Get (N);
      pragma Debug (D (D_Debug, "Request Get_Package_Receiver"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return Units.Get (U).Receiver;
   end Get_RCI_Package_Receiver;

   -------------------------------
   -- Invalidate_Receiving_Stub --
   -------------------------------

   procedure Invalidate_Receiving_Stub
     (Name : in Unit_Name) is
   begin
      null;
   end Invalidate_Receiving_Stub;

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

   -------------
   -- Process --
   -------------

   procedure Process
     (N       : in Unit_Id;
      Request : in Request_Type;
      Unit    : in out Unit_Type;
      Status  : out Status_Type) is
      Server : Partition_ID := Get_Boot_Server;

   begin
      case Request.Command is
         when Get_Unit =>

            --  The client cache is different from null when the request
            --  comes from the RCI_Info package. This cache reference
            --  has to be saved for later use.

            if Request.Cache /= null then
               Unit.Cache := Request.Cache;
            end if;

            if Unit.Status = Known then
               pragma Debug (D (D_Debug, Units.Get (N) & " is known"));

               Status := Unmodified;

               --  When the request does not come from a local partition
               --  then send a "set" request to this partition. Note that
               --  the cache reference is useless.

               if Request.Partition /= Local_Partition then
                  declare
                     R : Request_Type
                       := (Set_Unit,
                           Unit.Partition,
                           Unit.Receiver,
                           Unit.Version,
                           null);
                  begin
                     Send (Request.Partition, R, N);
                  end;
               end if;

            else
               pragma Debug (D (D_Debug, Units.Get (N) & " is unknown"));

               Status := Postponed;

               if not Is_Boot_Partition then

                  Send (Server, Request, N);
                  --  Ask the boot server for this info.

               elsif Request.Partition /= Server then
                  pragma Debug
                    (D (D_Debug,
                        "Queuing request from" & Request.Partition'Img &
                        " on " & Units.Get (N)));

                  --  Queue this request in order to answer it when info is
                  --  available.

                  Unit.Pending := True;
                  Unit.Requests (Request.Partition) := True;
               end if;
            end if;

         when Set_Unit =>
            Status         := Modified;

            Unit.Status    := Known;
            Unit.Receiver  := Request.Receiver;
            Unit.Version   := Request.Version;
            Unit.Partition := Request.Partition;

            if not Is_Boot_Partition then
               pragma Debug
                 (D (D_Debug, Units.Get (N) & " registered on boot server"));

               Send (Server, Request, N);
               --  Send this info to boot server once it is saved locally.

            elsif Unit.Pending then
               pragma Debug
                 (D (D_Debug, "Answer pending requests on " & Units.Get (N)));

               --  Dequeue pending requests in the boot server.

               for P in Unit.Requests'Range loop
                  if Unit.Requests (P) then
                     Send (P, Request, N);
                     Unit.Requests (P) := False;
                  end if;
               end loop;
               Unit.Pending := False;
            end if;

      end case;
   end Process;

   -----------------------------
   -- Public_Message_Receiver --
   -----------------------------

   procedure Public_Message_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type) is
      R : Request_Type;
      U : Unit_Id;
      N : Unit_Name := Unit_Name'(Unit_Name'Input (Params));

   begin
      U := Units.Get (N);

      Request_Id'Read (Params, R.Command);
      if R.Command = Set_Unit then
         Partition_ID'Read (Params, R.Partition);
         RPC_Receiver'Read (Params, R.Receiver);
         R.Version := new String'(String'Input (Params));
      else
         R.Partition := Partition;
      end if;

      pragma Debug
        (D (D_RNS,
            "Recv "   & R.Command'Img &
            " on "    & N &
            " from "  & Partition'Img));

      Units.Apply (U, R, Process'Access);
   end Public_Message_Receiver;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC_Receiver;
      Version  : in String := "") is
      Uname   : String := Name;
      Request : Request_Type
        := (Set_Unit, Local_Partition, Receiver, new String'(Version), null);

   begin
      pragma Debug (D (D_Debug, "Request Register_Receiving_Stub"));

      To_Lower (Uname);
      Units.Apply (Units.Get (Uname), Request, Process'Access);
   end Register_Receiving_Stub;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      Cache   : Cache_Access := new Cache_Type;
      Name    : String       := RCI_Name;
      Uname   : Unit_Id;

      Request : Request_Type := (Get_Unit, Local_Partition, null, null, Cache);

      -----------------------------
      -- Get_Active_Partition_ID --
      -----------------------------

      function Get_Active_Partition_ID return Partition_ID is
         Unit : Unit_Type;
         Done : Boolean;

      begin
         Cache.Get_RCI_Data (Unit.Receiver, Unit.Partition, Done);
         if not Done then
            pragma Debug (D (D_Debug, "RCI_Info Get_Active_Partition_ID"));

            Units.Apply (Uname, Request, Process'Access);
            Unit := Units.Get (Uname);
            Cache.Set_RCI_Data (Unit.Receiver, Unit.Partition);
         end if;
         return Unit.Partition;
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return RPC_Receiver is
         Unit : Unit_Type;
         Done : Boolean;

      begin
         Cache.Get_RCI_Data (Unit.Receiver, Unit.Partition, Done);
         if not Done then
            pragma Debug (D (D_Debug, "RCI_Info Get_Active_Partition_ID"));

            Units.Apply (Uname, Request, Process'Access);
            Cache.Set_RCI_Data (Unit.Receiver, Unit.Partition);
         end if;
         return Unit.Receiver;
      end Get_RCI_Package_Receiver;

   begin
      To_Lower (Name);
      Uname := Units.Get (Name);
   end RCI_Info;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id) is
      Params : aliased Params_Stream_Type (0);
      Name   : String := Units.Get (Unit);

   begin
      pragma Debug
        (D (D_RNS,
            "Send " & Request.Command'Img &
            " on "  & Name &
            " to "  & Partition'Img));

      String'Output (Params'Access, Name);
      Request_Id'Write (Params'Access, Request.Command);
      if Request.Command = Set_Unit then
         Partition_ID'Write (Params'Access, Request.Partition);
         RPC_Receiver'Write (Params'Access, Request.Receiver);
         String'Output      (Params'Access, Request.Version.all);
      end if;

      Send (Partition, Name_Service, Params'Access);
   end Send;

begin
   Receive (Name_Service, Public_Message_Receiver'Access);
   Establish_RPC_Receiver (Local_Partition, Partition_RPC_Receiver'Access);
end System.Partition_Interface;
