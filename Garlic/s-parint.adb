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
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with GNAT.HTable;           use GNAT.HTable;
with GNAT.Table;

with System.Garlic.Debug;   use System.Garlic.Debug;

with System.Garlic.Exceptions;   use System.Garlic.Exceptions;
pragma Elaborate (System.Garlic.Exceptions);

with System.Garlic.Heart;   use System.Garlic.Heart;
pragma Elaborate (System.Garlic.Heart);

with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Remote;  use System.Garlic.Remote;

pragma Warnings (Off);
with System.Garlic.Startup;
pragma Elaborate (System.Garlic.Startup);
pragma Warnings (On);

with System.Garlic.Streams; use System.Garlic.Streams;
pragma Elaborate (System.Garlic.Streams);

with System.Garlic.Types;   use System.Garlic.Types;
pragma Elaborate (System.Garlic.Types);

with System.Garlic.Units;   use System.Garlic.Units;
pragma Elaborate (System.Garlic.Units);

with System.Garlic.Utils;   use System.Garlic.Utils;
pragma Elaborate (System.Garlic.Utils);

with System.RPC;

package body System.Partition_Interface is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("INTERFACE", "(s-parint): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   function Convert is
     new Ada.Unchecked_Conversion
     (RPC_Receiver, RPC.RPC_Receiver);

   function Convert is
     new Ada.Unchecked_Conversion
     (RPC.RPC_Receiver, RPC_Receiver);

   procedure Process
     (N       : in Unit_Id;
      Request : in Request_Type;
      Unit    : in out Unit_Type;
      Status  : out Status_Type);

   procedure Public_Message_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Global message receiver

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id);

   Local_Partition  : constant Partition_ID := Get_My_Partition_ID;
   Get_Unit_Request : constant Request_Type :=
     (Get_Unit, Local_Partition, null, null, null);

   type Hash_Index is range 0 .. 100;
   function Hash (K : RACW_Stub_Type_Access) return Hash_Index;

   function Compare_Content (Left, Right : RACW_Stub_Type_Access)
     return Boolean;

   package Objects_HTable is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => RACW_Stub_Type_Access,
                         No_Element => null,
                         Key        => RACW_Stub_Type_Access,
                         Hash       => Hash,
                         Equal      => Compare_Content);

   procedure Free is
      new Ada.Unchecked_Deallocation (RACW_Stub_Type, RACW_Stub_Type_Access);

   function Hash (K : Address) return Hash_Index;

   package Address_HTable is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Natural,
                         No_Element => 0,
                         Key        => Address,
                         Hash       => Hash,
                         Equal      => "=");

   package Address_Table is
      new GNAT.Table (Table_Component_Type => Address,
                      Table_Index_Type     => Natural,
                      Table_Low_Bound      => 1,
                      Table_Initial        => 16,
                      Table_Increment      => 100);

   ---------------------
   -- Compare_Content --
   ---------------------

   function Compare_Content (Left, Right : RACW_Stub_Type_Access)
     return Boolean
   is
   begin
      return Left /= null and then Right /= null and then Left.all = Right.all;
   end Compare_Content;


   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID is
      N : String := Name;
      U : Unit_Id;

   begin
      To_Lower (N);
      U := Units.Get_Index (N);
      pragma Debug (D (D_Debug, "Request Get_Active_Partition_ID"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return RPC.Partition_ID (Units.Get_Component (U).Partition);
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
      U := Units.Get_Index (N);
      pragma Debug (D (D_Debug, "Request Get_Active_Version"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return Units.Get_Component (U).Version.all;
   end Get_Active_Version;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Handle : Natural) return Address is
      Result : Address;
   begin
      if Handle = 0 then
         return Null_Address;
      end if;
      Enter (Global_Mutex);
      Result := Address_Table.Table (Handle);
      Leave (Global_Mutex);
      return Result;
   end Get_Address;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID
     return RPC.Partition_ID is
   begin
      return RPC.Partition_ID (Local_Partition);
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID is
   begin
      return RPC.Partition_ID (Null_Partition_ID);
   end Get_Passive_Partition_ID;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return RPC.RPC_Receiver is
      N : String := Name;
      U : Unit_Id;

   begin
      To_Lower (N);
      U := Units.Get_Index (N);
      pragma Debug (D (D_Debug, "Request Get_Package_Receiver"));

      Units.Apply (U, Get_Unit_Request, Process'Access);
      return Convert (Units.Get_Component (U).Receiver);
   end Get_RCI_Package_Receiver;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access) is
      Answer : constant RACW_Stub_Type_Access := Objects_HTable.Get (Handler);
   begin
      if Answer = null then
         Objects_HTable.Set (Handler, Handler);
      else
         Free (Handler);
         Handler := Answer;
      end if;
   end Get_Unique_Remote_Pointer;

   ----------
   -- Hash --
   ----------

   function Hash (K : RACW_Stub_Type_Access) return Hash_Index is
   begin
      return Hash_Index (Natural (K.Addr) mod Positive (Hash_Index'Last + 1));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (K : Address) return Hash_Index is
   begin
      return Hash_Index (Integer (K) mod Integer (Hash_Index'Last));
   end Hash;

   -------------------------------
   -- Invalidate_Receiving_Stub --
   -------------------------------

   procedure Invalidate_Receiving_Stub
     (Name : in Unit_Name) is
   begin
      null;
   end Invalidate_Receiving_Stub;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Rsh_Command  : in String;
      Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String) is
   begin
      if not Nolaunch then
         if Name_Is_Host then
            Full_Launch (Rsh_Command, General_Name, Command_Line);
         else
            Full_Launch (Rsh_Command, Get_Host (General_Name), Command_Line);
         end if;
      end if;
   end Launch;

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
               pragma Debug (D (D_Debug, Units.Get_Name (N) & " is known"));

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

            elsif Unit.Status = Unknown then
               pragma Debug (D (D_Debug, Units.Get_Name (N) & " is unknown"));

               Status := Postponed;

               if not Is_Boot_Partition then

                  --  Ask the boot server for this info

                  Send (Server, Request, N);

               elsif Request.Partition /= Server then
                  pragma Debug
                    (D (D_Debug,
                        "Queuing request from" & Request.Partition'Img &
                        " on " & Units.Get_Name (N)));

                  --  Queue this request in order to answer it when info is
                  --  available. Note that this is a remote request which
                  --  should not be postponed.

                  Unit.Pending := True;
                  Unit.Requests (Request.Partition) := True;

                  Status := Modified;
               end if;

            else
               Status := Postponed;
            end if;

         when Set_Unit =>
            Status         := Modified;

            Unit.Status    := Known;
            Unit.Receiver  := Request.Receiver;
            Unit.Version   := Request.Version;
            Unit.Partition := Request.Partition;

            if not Is_Boot_Partition then

               if Unit.Partition = Local_Partition then
                  pragma Debug
                    (D (D_Debug, Units.Get_Name (N) &
                        " registered on boot server"));

                  Send (Server, Request, N);
                  --  Send this info to boot server once it is saved locally.

               end if;

            elsif Unit.Pending then
               pragma Debug
                 (D (D_Debug,
                     "Answer pending requests on " &
                     Units.Get_Name (N)));

               --  Dequeue pending requests in the boot server

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
      U := Units.Get_Index (N);

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

   ------------------------------------
   -- Raise_Program_Error_For_E_4_18 --
   ------------------------------------

   procedure Raise_Program_Error_For_E_4_18 is
   begin
      Ada.Exceptions.Raise_Exception (Program_Error'Identity,
        "Illegal usage of remote access to class-wide type. See RM E.4(18)");
   end Raise_Program_Error_For_E_4_18;

   ----------------------
   -- Register_Address --
   ----------------------

   function Register_Address (Addr : Address) return Natural is
      Index  : Natural;
   begin
      if Addr = Null_Address then
         return 0;
      end if;
      Enter (Global_Mutex);
      Index := Address_HTable.Get (Addr);
      if Index = 0 then
         Index := Address_Table.Allocate;
         Address_HTable.Set (Addr, Index);
         Address_Table.Table (Index) := Addr;
      end if;
      Leave (Global_Mutex);
      return Index;
   end Register_Address;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "") is
      Uname   : String := Name;
      Request : Request_Type
        := (Set_Unit, Local_Partition,
            Convert (Receiver),
            new String'(Version), null);

   begin
      pragma Debug (D (D_Debug, "Request Register_Receiving_Stub"));

      To_Lower (Uname);
      Units.Apply (Units.Get_Index (Uname), Request, Process'Access);
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

      function Get_Active_Partition_ID return RPC.Partition_ID is
         Unit : Unit_Type;
         Done : Boolean;

      begin
         Cache.Get_RCI_Data (Unit.Receiver, Unit.Partition, Done);
         if not Done then
            pragma Debug (D (D_Debug, "RCI_Info Get_Active_Partition_ID"));

            Units.Apply (Uname, Request, Process'Access);
            Unit := Units.Get_Component (Uname);
            Cache.Set_RCI_Data (Unit.Receiver, Unit.Partition);
         end if;
         return RPC.Partition_ID (Unit.Partition);
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return RPC.RPC_Receiver is
         Unit : Unit_Type;
         Done : Boolean;

      begin
         Cache.Get_RCI_Data (Unit.Receiver, Unit.Partition, Done);
         if not Done then
            pragma Debug (D (D_Debug, "RCI_Info Get_Active_Partition_ID"));

            Units.Apply (Uname, Request, Process'Access);
            Cache.Set_RCI_Data (Unit.Receiver, Unit.Partition);
         end if;
         return Convert (Unit.Receiver);
      end Get_RCI_Package_Receiver;

   begin
      To_Lower (Name);
      Uname := Units.Get_Index (Name);
   end RCI_Info;

   ---------
   -- Run --
   ---------

   procedure Run (Main : in Main_Subprogram_Type) is
      What    : Ada.Exceptions.Exception_Id;
      Message : String_Access;

   begin
      select
         Fatal_Error.Occurred (What, Message);
         Soft_Shutdown;
         Ada.Exceptions.Raise_Exception (What, Message.all);
      then abort
         Main.all;
      end select;
      Free (Message);
   end Run;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id) is
      Params : aliased Params_Stream_Type (0);
      Name   : String := Units.Get_Name (Unit);

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

   -----------
   -- Check --
   -----------

   procedure Check (Name : in Unit_Name; Version : in String) is
   begin
      if Different (Version, Get_Active_Version (Name)) then
         Soft_Shutdown;
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Versions differ for RCI unit """ &
            Name & """");
      end if;
   end Check;

begin
   Receive (Name_Service, Public_Message_Receiver'Access);
end System.Partition_Interface;
