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

with MOMA.Provider.Topic_Datas;
with MOMA.Types;

with PolyORB.Any;
with PolyORB.Minimal_Servant;
with PolyORB.References;
with PolyORB.Requests;

package MOMA.Provider.Routers is

   use PolyORB.References;

   type Router is new PolyORB.Minimal_Servant.Servant with private;
   --  Topics : the list of all topics, with their subscribers.

   procedure Invoke
     (Self : access Router;
      Req  : PolyORB.Requests.Request_Access);
   --  Router servant skeleton.

private

   type Router is new PolyORB.Minimal_Servant.Servant with record
      Topics   : MOMA.Provider.Topic_Datas.Topic_Data;
   end record;

   procedure Publish (Self       : access Router;
                      Message    : PolyORB.Any.Any;
                      Topic_Id   : MOMA.Types.String);
   --  Publish a Message on the topic designed by Topic_Id.

   procedure Store (Pool      : Ref;
                    Message   : PolyORB.Any.Any);
   --  Store a Message in a Pool.
   --  XXX Code from Moma.Provider.Message_Producer is duplicated.

end MOMA.Provider.Routers;
