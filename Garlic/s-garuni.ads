------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
   Unit_Id_Increment : constant := 10;

   type Request_List is array (Types.Partition_ID) of Boolean;
   Null_List : constant Request_List := (others => False);

   type Request_Kind is (Copy_Units_Table,
                         Define_New_Units,
                         Invalidate_Units,
                         Pull_Units_Table,
                         Push_Units_Table);

   type Unit_Status is (Queried, Undefined, Declared, Defined, Invalid);
   --  The order is very important. At the beginning, a unit is undefined
   --  on the caller side. It will send a request to get info on this
   --  request and the new status will be queried (just to avoid multiple
   --  requests). On the receiver side, it is declared. The receiver
   --  registers the unit to the boot server. The boot server will try to
   --  get a first agreement from other boot mirrors. If the unit info has
   --  not been modified after a first pass, then the unit becomes defined
   --  and this new info is sent once again to other boot mirrors. When the
   --  unit becomes Defined, the boot mirrors are allowed to answer to
   --  pending request from other partitions. When a partition dies, all
   --  its units are invalidated. If the reconnection mode of the partition
   --  is Blocked_Until_Restart, then the unit status is set to
   --  Undefined. Otherwise, it is set to Invalidated.

   type Unit_Info is
      record
         Next_Unit : Unit_Id;
         Partition : Types.Partition_ID;
         Receiver  : Interfaces.Unsigned_64;
         Version   : Types.Version_Type;
         Status    : Unit_Status;
         Pending   : Boolean;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Info :=
     (Next_Unit => Null_Unit_Id,
      Partition => Types.Null_PID,
      Receiver  => 0,
      Version   => Types.Null_Version,
      Status    => Undefined,
      Pending   => False,
      Requests  => Null_List);

   --  Next_Unit   : units on the same partition are linked together
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id
   --  Status      : unit info status
   --  Pending     : true when requests are pending
   --  Requests    : request(p) true for a pending request from partition p

   type Request_Type (Kind : Request_Kind := Pull_Units_Table) is
      record
         case Kind is
            when Copy_Units_Table |
                 Define_New_Units |
                 Pull_Units_Table |
                 Push_Units_Table =>
               null;

            when Invalidate_Units =>
               Partition : Types.Partition_ID;

         end case;
      end record;

   package Units is new System.Garlic.Table.Complex
     (Index_Type     => Unit_Id,
      Null_Index     => Null_Unit_Id,
      First_Index    => First_Unit_Id,
      Initial_Size   => Unit_Id_Increment,
      Increment_Size => Unit_Id_Increment,
      Component_Type => Unit_Info,
      Null_Component => Null_Unit);

   procedure Dump_Unit_Table;

   procedure Get_Unit_Info
     (Unit  : in Unit_Id;
      Info  : out Unit_Info;
      Error : in out Utils.Error_Type);
   --  Return unit info on these unit. If status is unknown, then ask a
   --  potential boot server or mirror for a copy of unit info.

   procedure Initialize;

   procedure Invalidate_Partition_Units
     (Partition : in Types.Partition_ID);
   --  Invalidate all the units configured on this partition. The exact
   --  invalidation will depend on the reconnection mode of this
   --  partition. When reconnection mode is Reject_On_Restart or
   --  Failed_Until_Restart, the status of these units will be set to
   --  Invalid. Otherwise, it will be set to Undefined.

   procedure Register_Unit
     (Name     : in String;
      Receiver : in Interfaces.Unsigned_64;
      Version  : in Utils.String_Access);
   --  Register locally this unit. The remote registration is postponed and
   --  will be performed with procedure above.

   procedure Register_Units_On_Boot_Server
     (Error : in out Utils.Error_Type);
   --  Register all the units previously locally registered. Then, get back
   --  info on these units to check that these units are valid.

end System.Garlic.Units;
