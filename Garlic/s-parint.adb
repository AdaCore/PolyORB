------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with GNAT.HTable;              use GNAT.HTable;
with GNAT.Table;
with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Heart;      use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Options;    use System.Garlic.Options;
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Remote;     use System.Garlic.Remote;
with System.Garlic.Soft_Links;
with System.Garlic.Startup;
pragma Warnings (Off, System.Garlic.Startup);
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Units;      use System.Garlic.Units;
with System.Garlic.Utils;      use System.Garlic.Utils;
with System.RPC;

package body System.Partition_Interface is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_PARINT", "(s-parint): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package Units renames System.Garlic.Units.Units;

   Passive_Prefix : constant String := "SP__";
   --  Prefix to use for Shared_Passive packages

   function Convert is
     new Ada.Unchecked_Conversion
     (RPC_Receiver, RPC.RPC_Receiver);

   function Convert is
     new Ada.Unchecked_Conversion
     (RPC.RPC_Receiver, RPC_Receiver);

   function Convert is
      new Ada.Unchecked_Conversion
     (RPC.RPC_Receiver, System.Address);

   procedure Complete_Termination (Termination : Termination_Type);
   --  Select the correct soft link

   function Different (V1, V2 : String) return Boolean;
   --  Compare two version ids. If one of these version ids is a string
   --  of blank characters then they will be considered as identical.

   function Get_Unit_Version (Name : String; RCI : Boolean) return String;
   --  When RCI, get active version. Otherwise, get passive version.

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

   --  This is a list of caller units whose Version_ID needs to check.

   type Caller_Node;
   type Caller_List is access Caller_Node;
   type Caller_Node is
      record
         Name    : String_Access;
         Version : String_Access;
         RCI     : Boolean;
         Next    : Caller_List;
      end record;
   Callers : Caller_List;

   procedure Free is new Ada.Unchecked_Deallocation (Caller_Node, Caller_List);

   procedure Raise_Communication_Error (S : String);

   -----------
   -- Check --
   -----------

   procedure Check
     (Name    : in Unit_Name;
      Version : in String;
      RCI     : in Boolean := True)
   is
      Caller : Caller_List := new Caller_Node;
   begin
      Caller.Name    := new String'(Name);
      Caller.Version := new String'(Version);
      Caller.RCI     := RCI;
      Caller.Next    := Callers;
      Callers        := Caller;
   end Check;

   ---------------------
   -- Compare_Content --
   ---------------------

   function Compare_Content (Left, Right : RACW_Stub_Type_Access)
     return Boolean
   is
   begin
      return Left /= null and then Right /= null and then Left.all = Right.all;
   end Compare_Content;

   --------------------------
   -- Complete_Termination --
   --------------------------

   procedure Complete_Termination (Termination : Termination_Type) is
   begin
      case Termination is
         when Local_Termination  =>
            System.Garlic.Soft_Links.Local_Termination;
         when Global_Termination =>
            System.Garlic.Soft_Links.Global_Termination;
         when others => null;
      end case;
   end Complete_Termination;

   ---------------
   -- Different --
   ---------------

   function Different (V1, V2 : String) return Boolean is

      function Not_Null_Version (V : in String) return Boolean;
      --  Return true when V is not a string of blank characters

      ----------------------
      -- Not_Null_Version --
      ----------------------

      function Not_Null_Version (V : in String) return Boolean is
         Null_String : constant String (V'Range) := (others => ' ');
      begin
         return V /= Null_String;
      end Not_Null_Version;

   begin
      return     Not_Null_Version (V1)
        and then Not_Null_Version (V2)
        and then V1 /= V2;
   end Different;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID
   is
      N : String := Name;
      U : Unit_Id;
      I : Unit_Info;
      E : Error_Type;
   begin
      pragma Debug (D (D_Debug, "Request Get_Active_Partition_ID"));

      To_Lower (N);
      U := Units.Get_Index (N);

      Get_Unit_Info (U, I, E);
      if Found (E) then
         Raise_Communication_Error (E.all);
      end if;

      return RPC.Partition_ID (I.Partition);
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version
     (Name : Unit_Name)
      return String
   is
      N : String := Name;
      U : Unit_Id;
      I : Unit_Info;
      E : Error_Type;
   begin
      pragma Debug (D (D_Debug, "Request Get_Active_Version"));

      To_Lower (N);
      U := Units.Get_Index (N);

      Get_Unit_Info (U, I, E);
      if Found (E) then
         Raise_Communication_Error (E.all);
      end if;

      return I.Version.all;
   end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_ID --
   ----------------------------

   function Get_Local_Partition_ID
     return RPC.Partition_ID is
   begin
      return RPC.Partition_ID (Self_PID);
   end Get_Local_Partition_ID;

   ------------------------
   -- Get_Partition_Name --
   ------------------------

   function Get_Partition_Name
     (Partition : Integer)
     return String is
      Name  : String_Access;
      Error : Error_Type;
   begin
      System.Garlic.Partitions.Get_Name
        (System.Garlic.Types.Partition_ID (Partition), Name, Error);
      if Found (Error) then
         Raise_Communication_Error (Error.all);
      end if;
      return Name.all;
   end Get_Partition_Name;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID is
   begin
      return Get_Active_Partition_ID (Passive_Prefix & Name);
   end Get_Passive_Partition_ID;

   -------------------------
   -- Get_Passive_Version --
   -------------------------

   function Get_Passive_Version (Name : Unit_Name) return String is
   begin
      return Get_Active_Version (Passive_Prefix & Name);
   end Get_Passive_Version;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return Interfaces.Unsigned_64
   is
      N : String := Name;
      U : Unit_Id;
      I : Unit_Info;
      E : Error_Type;
   begin
      pragma Debug (D (D_Debug, "Request Get_Package_Receiver"));

      To_Lower (N);
      U := Units.Get_Index (N);

      Get_Unit_Info (U, I, E);
      if Found (E) then
         Raise_Communication_Error (E.all);
      end if;

      return I.Receiver;
   end Get_RCI_Package_Receiver;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
   is
      Answer : constant RACW_Stub_Type_Access := Objects_HTable.Get (Handler);
   begin
      if Answer = null then
         Objects_HTable.Set (Handler, Handler);
      else
         Free (Handler);
         Handler := Answer;
      end if;
   end Get_Unique_Remote_Pointer;

   ----------------------
   -- Get_Unit_Version --
   ----------------------

   function Get_Unit_Version (Name : String; RCI : Boolean) return String is
   begin
      if RCI then
         return Get_Active_Version (Name);
      else
         return Get_Passive_Version (Name);
      end if;
   end Get_Unit_Version;

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

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Rsh_Command  : in String;
      Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String)
   is
   begin
      if not Nolaunch then
         if Name_Is_Host then
            Full_Launch (Rsh_Command, General_Name, Command_Line);
         else
            Full_Launch (Rsh_Command, Get_Host (General_Name), Command_Line);
         end if;
      end if;
   end Launch;

   ------------------------------------
   -- Raise_Program_Error_For_E_4_18 --
   ------------------------------------

   procedure Raise_Program_Error_For_E_4_18 is
   begin
      Ada.Exceptions.Raise_Exception (Program_Error'Identity,
        "Illegal usage of remote access to class-wide type. See RM E.4(18)");
   end Raise_Program_Error_For_E_4_18;

   -------------------------------
   -- Raise_Communication_Error --
   -------------------------------

   procedure Raise_Communication_Error (S : String) is
   begin
      Ada.Exceptions.Raise_Exception (RPC.Communication_Error'Identity, S);
   end Raise_Communication_Error;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : in Unit_Name;
      Version : in String := "")
   is
   begin
      Register_Receiving_Stub (Passive_Prefix & Name, null, Version);
   end Register_Passive_Package;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "")
   is
      N : String := Name;
   begin
      pragma Debug (D (D_Debug, "Request Register_Receiving_Stub"));

      To_Lower (N);
      Register_Unit
        (N, Interfaces.Unsigned_64 (System.Address'(Convert (Receiver))),
         new String'(Version));
   end Register_Receiving_Stub;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      Name : String := RCI_Name;
      Unit : Unit_Id;

      -----------------------------
      -- Get_Active_Partition_ID --
      -----------------------------

      function Get_Active_Partition_ID return RPC.Partition_ID
      is
         Info  : Unit_Info;
         Error : Error_Type;
      begin
         Get_Unit_Info (Unit, Info, Error);
         if Found (Error) then
            Raise_Communication_Error (Error.all);
         end if;

         if Info.Status = Invalid then
            Raise_Communication_Error
              ("Partition" & Info.Partition'Img & " is unreachable");
         end if;
         return RPC.Partition_ID (Info.Partition);
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return Interfaces.Unsigned_64
      is
         Info  : Unit_Info;
         Error : Error_Type;
      begin
         Get_Unit_Info (Unit, Info, Error);
         if Found (Error) then
            Raise_Communication_Error (Error.all);
         end if;

         if Info.Status = Invalid then
            Raise_Communication_Error
              ("Partition" & Info.Partition'Img & " is unreachable");
         end if;
         return Info.Receiver;
      end Get_RCI_Package_Receiver;

   begin
      To_Lower (Name);
      Unit := Units.Get_Index (Name);
   end RCI_Info;

   ---------
   -- Run --
   ---------

   procedure Run
     (Main : in Main_Subprogram_Type := null)
   is
      Caller  : Caller_List := Callers;
      Dummy   : Caller_List;
      Error   : Error_Type := No_Error;
   begin
      pragma Debug (D (D_Debug, "Complete elaboration"));
      System.Garlic.Heart.Complete_Elaboration;

      Register_Units_On_Boot_Server (Error);
      if Found (Error) then
         Raise_Exception (RPC.Communication_Error'Identity, Error.all);
      end if;

      pragma Debug (D (D_Debug, "Establish RPC Receiver"));
      RPC.Establish_RPC_Receiver (RPC.Partition_ID (Self_PID), null);

      while Caller /= null loop
         pragma Debug (D (D_Debug, "Check " & Caller.Name.all &
                          " version consistency"));
         if Different (Caller.Version.all,
                       Get_Unit_Version (Caller.Name.all, Caller.RCI)) then

            pragma Debug (D (D_Debug, "Versions differ for unit """ &
                             Caller.Name.all & """"));

            --  If not boot partition, then terminate without waiting for
            --  boot partition request.

            if not Is_Boot_Server then
               Set_Termination (Local_Termination);
            end if;

            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity,
               "Versions differ for unit """ &
               Caller.Name.all & """");

         end if;
         Free (Caller.Version);
         Free (Caller.Name);
         Dummy  := Caller;
         Caller := Caller.Next;
         Free (Dummy);
      end loop;

      pragma Debug (D (D_Debug, "Execute main suprogram"));
      if Main /= null then
         Main.all;
      end if;

      pragma Debug (D (D_Debug, "Complete termination"));
      Complete_Termination (System.Garlic.Options.Termination);

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Run: " & Exception_Information (E)));
         pragma Debug (D (D_Debug,
                          "Complete termination after handling exception"));
         Complete_Termination (System.Garlic.Options.Termination);
         raise;
   end Run;

end System.Partition_Interface;
