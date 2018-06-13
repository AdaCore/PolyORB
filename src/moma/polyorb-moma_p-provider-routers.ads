------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . M O M A _ P . P R O V I D E R . R O U T E R S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  A servant used for routing topic messages.

with MOMA.Destinations;
with MOMA.Types;

with PolyORB.MOMA_P.Provider.Topic_Datas;

with PolyORB.Minimal_Servant;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Tasking.Rw_Locks;

package PolyORB.MOMA_P.Provider.Routers is

   use MOMA.Types;
   use PolyORB.References;

   type Router is new PolyORB.Minimal_Servant.Servant with private;
   --  Id       : the Id of the router.
   --  Routers  : the list of routers the router will exchange messages with.
   --  Self_Ref : a reference to the router, that it can give to other routers.
   --  Topics   : the list of all topics, with their subscribers.

   type Router_Acc is access Router;

   type Routers_List is private;
   --  A protected list of routers.

   function Create_Destination
     (Self : Router)
     return MOMA.Destinations.Destination;
   --  Return a destination object whose reference is the router and whose
   --  name is the router Id.

   procedure Initialize
     (Self       : access Router;
      Router_Ref :        PolyORB.References.Ref);
   --  Initialize a Router.
   --  Router_Ref is a reference to another router on the network (it can be
   --  Nil_Ref) the router will register with.

   overriding procedure Invoke
     (Self : access Router;
      Req  :        PolyORB.Requests.Request_Access);
   --  Router servant skeleton.

   --  Accessors to internal data.

   function Get_Id
     (Self : Router)
     return MOMA.Types.String;

   procedure Set_Id
     (Self  : in out Router;
      Id    :        MOMA.Types.String);

   function Get_Self_Ref
     (Self : Router)
     return PolyORB.References.Ref;

   procedure Set_Self_Ref
     (Self  : in out Router;
      Ref   :        PolyORB.References.Ref);

private

   use PolyORB.MOMA_P.Provider.Topic_Datas;

   type Routers_List is record
      List           : Destination_List.List;
      L_Initialized  : Boolean := False;
      L_Lock         : PolyORB.Tasking.Rw_Locks.Rw_Lock_Access;
   end record;

   type Router is new PolyORB.Minimal_Servant.Servant with record
      Id       : MOMA.Types.String;
      Routers  : Routers_List;
      Self_Ref : PolyORB.References.Ref;
      Topics   : PolyORB.MOMA_P.Provider.Topic_Datas.Topic_Data;
   end record;

   pragma Inline (Get_Id);
   pragma Inline (Set_Id);
   pragma Inline (Get_Self_Ref);
   pragma Inline (Set_Self_Ref);

end PolyORB.MOMA_P.Provider.Routers;
