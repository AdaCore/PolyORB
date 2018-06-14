------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.MOMA_P.PROVIDER.MESSAGE_POOL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

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

   overriding procedure Invoke
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
