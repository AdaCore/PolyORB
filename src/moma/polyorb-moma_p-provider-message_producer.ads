------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.MOMA_P.PROVIDER.MESSAGE_PRODUCER                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Actual implementation of the Message_Producer object. It is
--  derived from PolyORB's Minimal_Servant. This package contains
--  Message_Producer skeleton and implementation subroutines. By
--  construction, its implementation subroutines contain parts of a
--  stub for the Message_Pool object.

with PolyORB.Minimal_Servant;
with PolyORB.References;
with PolyORB.Requests;

package PolyORB.MOMA_P.Provider.Message_Producer is

   type Object is new PolyORB.Minimal_Servant.Servant with private;
   --  Remote_Ref : Reference to the remote object to which send messages.

   type Object_Acc is access Object;

   procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access);
   --  Message_Producer servant skeleton.

   --  Accessors to Object internals.

   function Get_Remote_Ref
     (Self : Object)
     return PolyORB.References.Ref;

   procedure Set_Remote_Ref
     (Self : in out Object;
      Ref  :        PolyORB.References.Ref);

private

   type Object is new PolyORB.Minimal_Servant.Servant with record
      Remote_Ref : PolyORB.References.Ref;
   end record;

end PolyORB.MOMA_P.Provider.Message_Producer;
