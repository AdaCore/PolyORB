------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 S p e c                                  --
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

with Interfaces;
with System.Garlic.Table;
with System.Garlic.Types;
with System.Garlic.Utils;

package System.Garlic.Units is

   pragma Elaborate_Body;

   --  This package needs comments ???

   type Unit_Id is new Natural;
   Null_Unit_Id  : constant Unit_Id := 0;
   First_Unit_Id : constant Unit_Id := 2_000_000;

   type Request_List is array (Types.Partition_ID) of Boolean;
   type Request_Id is (Get_Unit, Set_Unit, Invalidate);

   type Unit_Status is (Unknown, Queried, Known, Invalid);

   type Cache_Type is limited private;

   procedure Get_RCI_Data
     (Cache     : in Cache_Type;
      Receiver  : out Interfaces.Unsigned_64;
      Partition : out Types.Partition_ID;
      Status    : out Unit_Status);
   --  Get Receiver and Partition in a unit cache. Status can be either
   --  Invalid (partition is down) or Known.

   function Partition_RCI_List
     (Partition : in Types.Partition_ID)
      return Unit_Id;
   --  Build an unique unit name from a partition id. This name
   --  corresponds to a fake unit. This unit is the root of a rci unit
   --  list. Each unit of this list is configured on this partition.

   procedure Set_RCI_Data
     (Cache     : out Cache_Type;
      Receiver  : in Interfaces.Unsigned_64;
      Partition : in Types.Partition_ID);
   --  Set Receiver and Partition in a unit cache. Status becomes Known.

   procedure Update
     (Cache     : out Cache_Type;
      Partition : in  Types.Partition_ID;
      Status    : in  Unit_Status);
   --  Update cache status

   type Cache_Access is access Cache_Type;

   type Unit_Type is
      record
         Next_Unit : Unit_Id;
         Partition : Types.Partition_ID;
         Receiver  : Interfaces.Unsigned_64;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
         Status    : Unit_Status;
         Pending   : Boolean;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Type
     := (Next_Unit => Null_Unit_Id,
         Partition => System.Garlic.Types.Null_Partition_ID,
         Receiver  => 0,
         Version   => null,
         Cache     => null,
         Status    => Unknown,
         Pending   => False,
         Requests  => (others => False));

   --  Next_Unit   : units on the same partition are linked together
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id
   --  Cache       : reference ot the caller cache
   --  Status      : unit info status
   --  Pending     : true when requests are pending
   --  Requests    : request(p) true for a pending request from partition p

   type Request_Type is
      record
         Command   : Request_Id;
         Partition : Types.Partition_ID;
         Receiver  : Interfaces.Unsigned_64;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
      end record;

   Null_Request : constant Request_Type :=
     (Command   => Get_Unit,
      Partition => System.Garlic.Types.Null_Partition_ID,
      Receiver  => 0,
      Version   => null,
      Cache     => null);

   --  Command     : operation to perform
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id
   --  Cache       : reference ot the caller cache

   package Table is new System.Garlic.Table.Complex
     (Index_Type     => Unit_Id,
      Null_Index     => Null_Unit_Id,
      First_Index    => First_Unit_Id,
      Initial_Size   => 20,
      Increment_Size => 20,
      Component_Type => Unit_Type,
      Null_Component => Null_Unit,
      Parameter_Type => Request_Type);

   procedure Process
     (N       : in Unit_Id;
      Request : in Request_Type;
      Unit    : in out Unit_Type;
      Status  : out Utils.Status_Type);
   --  Execute request Request on unit Unit (N) in a critical section.
   --  When Status is Unmodified, Unit is kept unmodified. When Modified,
   --  Unit is saved in Table. When Postponed, Unit is saved in Table
   --  and the task is suspended until another task returns with a Modified
   --  Status. This behaviour comes from the implementation of Table
   --  Mutex (see Garlic.Utils.Mutex_Type).

   procedure Send
     (Partition : in Types.Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id);
   --  Send a request on a unit to a partition

private

   type Cache_Type is record
      RCI_Unit_Status  : Unit_Status := Unknown;
      Active_Partition : Types.Partition_ID;
      Package_Receiver : Interfaces.Unsigned_64;
   end record;

end System.Garlic.Units;
