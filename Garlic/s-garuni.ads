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
   type Request_Kind is (Get_Unit_Info, Set_Unit_Info, Invalidate_Info);

   type Unit_Status is (Unknown, Queried, Known, Invalid);

   type Unit_Info is
      record
         Next_Unit : Unit_Id;
         Partition : Types.Partition_ID;
         Receiver  : Interfaces.Unsigned_64;
         Version   : Utils.String_Access;
         Status    : Unit_Status;
         Pending   : Boolean;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Info :=
     (Next_Unit => Null_Unit_Id,
      Partition => Types.Null_PID,
      Receiver  => 0,
      Version   => null,
      Status    => Unknown,
      Pending   => False,
      Requests  => (others => False));

   --  Next_Unit   : units on the same partition are linked together
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id
   --  Status      : unit info status
   --  Pending     : true when requests are pending
   --  Requests    : request(p) true for a pending request from partition p

   type Request_Type (Kind : Request_Kind := Get_Unit_Info) is
      record
         case Kind is
            when Get_Unit_Info =>
               null;

            when Set_Unit_Info =>
               Partition : Types.Partition_ID;
               Receiver  : Interfaces.Unsigned_64;
               Version   : Utils.String_Access;

            when Invalidate_Info =>
               Wrong_PID : Types.Partition_ID;

         end case;
      end record;

   --  Kind        : operation to perform
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id

   package Units is new System.Garlic.Table.Complex
     (Index_Type     => Unit_Id,
      Null_Index     => Null_Unit_Id,
      First_Index    => First_Unit_Id,
      Initial_Size   => 20,
      Increment_Size => 20,
      Component_Type => Unit_Info,
      Null_Component => Null_Unit);

   function Get_Unit_Info
     (Unit : Unit_Id)
      return Unit_Info;

   procedure Initialize;

   procedure Invalidate_Partition
     (Partition : in Types.Partition_ID);

   procedure Set_Unit_Info
     (Unit     : in Unit_Id;
      Receiver : in Interfaces.Unsigned_64;
      Version  : in Utils.String_Access);

end System.Garlic.Units;
