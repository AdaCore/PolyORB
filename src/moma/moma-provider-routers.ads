------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                M O M A . P R O V I D E R . R O U T E R S                 --
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

--  A servant used for routing topic messages.

--  $Id$

with MOMA.Destinations;
with MOMA.Provider.Topic_Datas;
with MOMA.Types;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Minimal_Servant;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Tasking.Rw_Locks;
with PolyORB.Utils.Chained_Lists;

package MOMA.Provider.Routers is

   use MOMA.Types;
   use PolyORB.References;

   package Destination_List is
      new PolyORB.Utils.Chained_Lists (MOMA.Destinations.Destination);
   --  A chained list of destinations.

   type Router is new PolyORB.Minimal_Servant.Servant with private;
   --  Id       : the Id of the router.
   --  Routers  : the list of routers the router will exchange messages with.
   --  Self_Ref : a reference to the router, that it can give to other routers.
   --  Topics   : the list of all topics, with their subscribers.

   type Router_Acc is access Router;

   type Routers_List is private;
   --  A protected list of routers.

   function Create_Destination (Self : Router)
      return MOMA.Destinations.Destination;
   --  Return a destination object whose reference is the router and whose
   --  name is the router Id.

   procedure Initialize (Self       : access Router;
                         Router_Ref : PolyORB.References.Ref);
   --  Initialize a Router.
   --  Router_Ref is a reference to another router on the network (it can be
   --  Nil_Ref) the router will register with.

   procedure Invoke
     (Self : access Router;
      Req  : PolyORB.Requests.Request_Access);
   --  Router servant skeleton.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Interface description for SOA object adapter.

   --  Accessors to internal data.
   function Get_Id (Self : Router) return MOMA.Types.String;
   procedure Set_Id (Self  : in out Router;
                     Id    : MOMA.Types.String);
   function Get_Self_Ref (Self : Router) return PolyORB.References.Ref;
   procedure Set_Self_Ref (Self  : in out Router;
                           Ref   : PolyORB.References.Ref);

private

   type Routers_List is record
      List           : Destination_List.List;
      L_Initialized  : Boolean := False;
      L_Lock         : PolyORB.Tasking.Rw_Locks.Rw_Lock_Access;
   end record;

   type Router is new PolyORB.Minimal_Servant.Servant with record
      Id       : MOMA.Types.String;
      Routers  : Routers_List;
      Self_Ref : PolyORB.References.Ref;
      Topics   : MOMA.Provider.Topic_Datas.Topic_Data;
   end record;

   procedure Add_Router (Self    : in out Router;
                         Router  : MOMA.Destinations.Destination);
   --  Add a Router to the list of known routers.

   procedure Publish (Self             : access Router;
                      Message          : PolyORB.Any.Any;
                      From_Router_Id   : MOMA.Types.String :=
                                            To_MOMA_String (""));
   --  Publish a Message on the topic given by the Message destination.
   --  From_Router_Id is the Id of the router the message is coming from, if
   --  it's received from a router and not from a client.

   procedure Register (Self         : access Router;
                       Router_Ref   : PolyORB.References.Ref);
   --  Register a router with another one : this means they will exchange
   --  messages one with each other.

   procedure Route (Self      : access Router;
                    Message   : PolyORB.Any.Any;
                    To_Router : MOMA.Destinations.Destination);
   --  Route a Message to another router.

   procedure Store (Pool      : Ref;
                    Message   : PolyORB.Any.Any);
   --  Store a Message in a Pool.
   --  XXX Code from Moma.Provider.Message_Producer is duplicated.

   procedure Subscribe (Self     : access Router;
                        Topic    : MOMA.Destinations.Destination;
                        Pool     : MOMA.Destinations.Destination);
   --  Subscribe a Pool to a Topic.
   --  Topic's kind must be set to "Topic".
   --  Pool's kind must be set to "Pool".

   function Get_Parameter_Profile (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

   function Get_Result_Profile (Method : String)
     return PolyORB.Any.Any;
   --  Result part of the interface description.

   --  Private accessors to internal data.

   function Get_Routers (Self : Router) return Destination_List.List;
   --  Return a copy of the list Self.Routers.List.

end MOMA.Provider.Routers;
