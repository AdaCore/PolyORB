------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Exceptions;
with Interfaces;
with System.RPC;

package System.Partition_Interface is

   pragma Elaborate_Body;

   type Subprogram_Id is new Natural;
   --  This type is used exclusively by stubs

   subtype Unit_Name is String;
   --  Name of Ada units

   type Main_Subprogram_Type is access procedure;

   type RACW_Stub_Type is tagged record
      Origin       : RPC.Partition_ID;
      Receiver     : Interfaces.Unsigned_64;
      Addr         : Interfaces.Unsigned_64;
      Asynchronous : Boolean;
   end record;
   type RACW_Stub_Type_Access is access RACW_Stub_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating
   --  exp_dist.adb.

   procedure Check
     (Name    : in Unit_Name;
      Version : in String;
      RCI     : in Boolean := True);
   --  Use by the main subprogram to check that a remote receiver
   --  unit has has the same version than the caller's one.

   procedure Register_Passive_Partition
     (Partition : out RPC.Partition_ID;
      Name      : in String;
      Location  : in String);
   --  Register a passive partition as it cannot register itself. To
   --  avoid conflicts due to multiple registrations, the partition
   --  name is used as a key. If the partition is already declared,
   --  then ignored request. If not, create a factory for this
   --  partition using its location.

   procedure Register_Passive_Package_On_Passive_Partition
     (Partition : in RPC.Partition_ID;
      Name      : in String;
      Version   : in String := "");
   --  Register a passive package configured on a passive
   --  partition. As a passive partition has no elaboration code, each
   --  partition using this package performs the registration itself.

   procedure Elaborate_Passive_Partition
     (Partition : in RPC.Partition_ID);
   --  Register all the units of this partition on the boot server and
   --  check that these units are uniquely defined.

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return System.RPC.Partition_ID;
   --  Similar in some respects to RCI_Info.Get_Active_Partition_ID
   --  XXXXX: Rename it in Get_RCI_Partition_ID

   function Get_Active_Version
      (Name : Unit_Name)
       return String;
   --  Similar in some respects to Get_Active_Partition_ID
   --  XXXXX: Rename it in Get_RCI_Version

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  Return the Partition_ID of the current partition

   function Get_Partition_Name
     (Partition : Integer)
     return String;
   --  Return the name of the partition

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
     return System.RPC.Partition_ID;
   --  Return the Partition_ID of the given shared passive partition
   --  XXXXX: Rename it in Get_SP_Partition_ID

   function Get_Passive_Version (Name : Unit_Name) return String;
   --  Return the version corresponding to a shared passive unit
   --  XXXXX: Rename it in Get_SP_Version

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return Interfaces.Unsigned_64;
   --  Similar in some respects to RCI_Info.Get_RCI_Package_Receiver

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in System.RPC.RPC_Receiver;
      Version  : in String := "");
   --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.

   procedure Register_Passive_Package
     (Name    : in Unit_Name;
      Version : in String := "");
   --  Register a shared passive package during its elaboration.

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access);
   --  Get a unique pointer on a remote object

   procedure Raise_Program_Error_For_E_4_18;
   pragma No_Return (Raise_Program_Error_For_E_4_18);
   --  Raise Program_Error with an error message explaining why it has been
   --  raised. The rule in E.4 (18) is tricky and misleading for most users
   --  of the distributed systems annex.

   procedure Raise_Program_Error_Unknown_Tag
     (E : in Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Raise_Program_Error_Unknown_Tag);
   --  Raise Program_Error with the same message as E one

   generic
      RCI_Name : String;
   package RCI_Info is
      function Get_RCI_Package_Receiver return Interfaces.Unsigned_64;
      function Get_Active_Partition_ID return RPC.Partition_ID;
      --  XXXXX: Rename it in Get_RCI_Partition_ID
   end RCI_Info;
   --  RCI package information caching

   procedure Run (Main : in Main_Subprogram_Type := null);
   --  Run the main subprogram

end System.Partition_Interface;
