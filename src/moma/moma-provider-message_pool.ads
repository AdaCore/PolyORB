------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           M O M A . P R O V I D E R . M E S S A G E _ P O O L            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Actual implementation of the Message_Pool object. It is derived
--  from PolyORB's Minimal_Servant. This package contains Message_Pool
--  skeleton and implementation subroutines.

--  $Id$

with MOMA.Types;
with MOMA.Provider.Warehouse;

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.Obj_Adapters.Simple;

package MOMA.Provider.Message_Pool is

   type Object is new PolyORB.Minimal_Servant.Servant with private;

   type Object_Acc is access Object;

   procedure Initialize (Self : access Object;
                        Info : MOMA.Types.Message_Pool);
   --  Initialize the object.

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access);
   --  Message_Pool servant skeleton.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Interface description for SOA object adapter.

private

   type Object is new PolyORB.Minimal_Servant.Servant with record
     Pool : MOMA.Types.Message_Pool;
         --  Pool information.

     W : MOMA.Provider.Warehouse.Warehouse;
         --  XXX up to now, we use one and only one Warehouse, per
         --  message_pool, more warehouses would require message analysis,
         --  => to be done later, after proper message definition.

     Message_Id : Natural := 0;
         --  XXX Dummy counter for message_id, to be trashed ...

     Last_Read_Id : Natural := 0;
         --  XXX Dummy counter for message_id, to be trashed ...

   end record;

end MOMA.Provider.Message_Pool;
