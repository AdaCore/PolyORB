------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.MOMA_P.PROVIDER.MESSAGE_POOL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Actual implementation of the Message_Pool object. It is derived
--  from PolyORB's Minimal_Servant. This package contains Message_Pool
--  skeleton and implementation subroutines.

with MOMA.Types;

with PolyORB.MOMA_P.Provider.Warehouse;

with PolyORB.Minimal_Servant;
with PolyORB.References;
with PolyORB.Requests;

package PolyORB.MOMA_P.Provider.Message_Pool is

   type Object is new PolyORB.Minimal_Servant.Servant with private;

   type Object_Acc is access Object;

   procedure Initialize
     (Self : access Object;
      Info :        MOMA.Types.Message_Pool);
   --  Initialize the object.

   procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access);
   --  Message_Pool servant skeleton.

private

   use MOMA.Types;
   use PolyORB.References;

   type Object is new PolyORB.Minimal_Servant.Servant with record
     Pool : MOMA.Types.Message_Pool;
         --  Pool information.

     W : PolyORB.MOMA_P.Provider.Warehouse.Warehouse;
         --  XXX up to now, we use one and only one Warehouse, per
         --  message_pool, more warehouses would require message analysis,
         --  => to be done later, after proper message definition.

     Message_Id : Natural := 0;
         --  XXX Dummy counter for message_id, to be trashed ...

     Last_Read_Id : Natural := 0;
         --  XXX Dummy counter for message_id, to be trashed ...

     Message_Handler : PolyORB.References.Ref := PolyORB.References.Nil_Ref;
         --  Reference of the Message_Handler to which Notify or Handle
         --  Requests must be sent.

     Behavior : MOMA.Types.Call_Back_Behavior := None;
         --  Specifies if a Notify or Handle request must be sent on reception
         --  of a message, or none.

   end record;

end PolyORB.MOMA_P.Provider.Message_Pool;
