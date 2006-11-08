------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

with Interfaces;
with System.Garlic.Exceptions;
with System.Garlic.Types;

package System.Garlic.Units is

   pragma Elaborate_Body;

   --  This package maintains a catalogue of all locally registered
   --  RCI units, as well as a cache for information about remote
   --  units registered with the name server.

   type Subprogram_Id is new Natural;
   --  Each subprogram in an RCI unit is assigned a subprogram
   --  identifier, which is used to denote the subprogram in the
   --  context of remote calls.

   First_RCI_Subprogram_Id : constant := 2;
   --  The first two subprogram IDs have special meaning:
   --    0 denotes a remote call performed through a dereference
   --      of a remote access-to-subprogram type;
   --    1 is a call to an inteernally-generated lookup service
   --      allowing a remote customer to retrieve the address of
   --      the proxy object associated with a given subprogram ID.
   --      This address is used to build values for remote access-to-
   --      subprogram types.
   --  Thus, user subprograms are numbered starting at 2.
   --  This constant must be kept consistent with its counterpart
   --  in Exp_Dist.

   --  RCI receiving stubs contain a table of descriptors for
   --  all user subprograms exported by the unit.

   type RCI_Subp_Info is record
      Addr  : System.Address;
      --  Local address of the proxy object
   end record;
   type RCI_Subp_Info_Access is access all RCI_Subp_Info;
   type RCI_Subp_Info_Array is array (Integer range <>) of
     aliased RCI_Subp_Info;

   --  The subprograms below allow various properties of a unit
   --  to be queried. If an error occurs during their execution,
   --  an error condition is returned in the Error out parameter.

   function Get_Unit_Id (Name : String) return Types. Unit_Id;
   --  Retrieve the unit ID assigned to the named unit.

   procedure Get_Partition
     (Unit      : Types.Unit_Id;
      Partition : out Types.Partition_ID;
      Error     : in out Exceptions.Error_Type);
   --  Retrieve the partition ID of the partition on which Unit
   --  is instantiated.

   procedure Get_Receiver
     (Unit     : Types.Unit_Id;
      Receiver : out Interfaces.Unsigned_64;
      Error    : in out Exceptions.Error_Type);
   --  Retrieve the RPC receiver address for Unit.

   procedure Get_Version
     (Unit    : Types.Unit_Id;
      Version : out Types.Version_Type;
      Error   : in out Exceptions.Error_Type);
   --  Retrieve the version for Unit.

   function Get_Subprogram_Info
     (Unit    : Types.Unit_Id;
      Subp_Id : Subprogram_Id)
      return RCI_Subp_Info_Access;
   --  Retrieve the subprogram descriptor for the given subprogram
   --  in the named (local) unit, or null if not found.

   procedure Initialize;

   procedure Invalidate_Partition_Units
     (Partition : Types.Partition_ID);
   --  Invalidate all the units configured on this partition. The exact
   --  invalidation will depend on the reconnection mode of this
   --  partition. When reconnection mode is Reject_On_Restart or
   --  Fail_Until_Restart, the status of these units will be set to
   --  Invalid. Otherwise, it will be set to Undefined.

   procedure Register_Unit
     (Partition     : Types.Partition_ID;
      Name          : String;
      Receiver      : Interfaces.Unsigned_64;
      Version       : Types.Version_Type;
      Subp_Info     : System.Address;
      Subp_Info_Len : Integer);
   --  Register locally this unit. The remote registration is
   --  postponed and will be performed by Register_Units_On_Boot_Server.

   procedure Register_Units_On_Boot_Server
     (Partition : Types.Partition_ID;
      Error    : in out Exceptions.Error_Type);
   --  Register all the units previously declared by partition. Then,
   --  get back info on these units to check that these units are
   --  valid.

   procedure Shutdown;
   --  Resume tasks waiting for an update of units info table.

end System.Garlic.Units;
