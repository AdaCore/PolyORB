------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for PolyORB.
--  For each entity, we document whether it is shared with the
--  default GNAT implementation, or with the GLADE implementation.

--  $Id$

with Ada.Exceptions;
--  with Interfaces;
with System.RPC;

with System.PolyORB_Interface;
package System.Partition_Interface is

   pragma Elaborate_Body;

   type DSA_Implementation_Name is (No_DSA, GLADE_DSA, PolyORB_DSA);
   DSA_Implementation : constant DSA_Implementation_Name := PolyORB_DSA;

--    type Subprogram_Id is new Natural;
--    --  This type is used exclusively by stubs

   subtype Unit_Name is String;
   --  Name of Ada units
   --  SHARED: GNAT,GLADE,PolyORB

--    type Main_Subprogram_Type is access procedure;

   subtype RACW_Stub_Type is System.PolyORB_Interface.RACW_Stub_Type;
   --  SHARED: none
--    type RACW_Stub_Type is tagged record
--       Origin       : RPC.Partition_ID;
--       Receiver     : Interfaces.Unsigned_64;
--       Addr         : Interfaces.Unsigned_64;
--       Asynchronous : Boolean;
--    end record;
   subtype RACW_Stub_Type_Access is
     System.PolyORB_Interface.RACW_Stub_Type_Access;
   --  SHARED: none
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating
   --  exp_dist.adb.

   subtype RAS_Proxy_Type is System.PolyORB_Interface.RAS_Proxy_Type;
   subtype RAS_Proxy_Type_Access is
     System.PolyORB_Interface.RAS_Proxy_Type_Access;
   --  SHARED: none

--    procedure Check
--      (Name    : in Unit_Name;
--       Version : in String;
--       RCI     : in Boolean := True);
--    --  Use by the main subprogram to check that a remote receiver
--    --  unit has has the same version than the caller's one.

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID;
   --  SHARED: GNAT,GLADE,PolyORB
   --  IMPL-SHARED: none
   --  Similar in some respects to RCI_Info.Get_Active_Partition_ID

--    function Get_Active_Version
--       (Name : Unit_Name)
--        return String;
--    --  Similar in some respects to Get_Active_Partition_ID

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  SHARED: GNAT,GLADE,PolyORB
   --  IMPL-SHARED: none
   --  Return the Partition_ID of the current partition

--    function Get_Passive_Partition_ID
--      (Name : Unit_Name)
--      return RPC.Partition_ID;
--    --  Return the Partition_ID of the given shared passive partition

--    function Get_Passive_Version (Name : Unit_Name) return String;
--    --  Return the version corresponding to a shared passive unit

--    function Get_RCI_Package_Receiver
--      (Name : Unit_Name)
--       return Interfaces.Unsigned_64;
--    --  Similar in some respects to RCI_Info.Get_RCI_Package_Receiver

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access);
   --  SHARED: GNAT,GLADE,PolyORB
   --  IMPL-SHARED: none
   --  Get a unique pointer on a remote object

   procedure Raise_Program_Error_Unknown_Tag
     (E : in Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Raise_Program_Error_Unknown_Tag);
   --  SHARED: GNAT,GLADE,PolyORB
   --  IMPL-SHARED: GNAT,GLADE,PolyORB
   --  Raise Program_Error with the same message as E one

--    procedure Register_Receiving_Stub
--      (Name     : in Unit_Name;
--       Receiver : in RPC.RPC_Receiver;
--       Version  : in String := "");
--    --  Register the fact that the Name receiving stub is now elaborated.
--    --  Register the access value to the package RPC_Receiver procedure.

--    procedure Register_Passive_Package
--      (Name    : in Unit_Name;
--       Version : in String := "");
--    --  Register a passive package

--    generic
--       RCI_Name : String;
--    package RCI_Info is
--       function Get_RCI_Package_Receiver return Interfaces.Unsigned_64;
--       function Get_Active_Partition_ID return RPC.Partition_ID;
--    end RCI_Info;
--    --  RCI package information caching

--    procedure Run (Main : in Main_Subprogram_Type := null);
--    --  Run the main subprogram

end System.Partition_Interface;
