------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
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

--  $Id$

with PolyORB.Components;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Requests;

package PolyORB.Minimal_Servant is

   pragma Elaborate_Body;

   type Servant is abstract new PolyORB.Smart_Pointers.Entity
     with private;

   type Servant_Acc is access all Servant;

   function Handle_Message
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class;

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access;

   procedure Invoke
     (Self    : access Servant;
      Request : in     PolyORB.Requests.Request_Access)
      is abstract;

private

   type Implementation (As_Servant : access Servant'Class)
   is new PolyORB.Servants.Servant with null record;
   --  The MOMA personality is based on the Portable Object Adapter.

   function "=" (X, Y : Implementation) return Boolean;
   --  XXX Why does the compiler require the presence of this operator?
   --  As a descendant of Component, Implementation is a limited type!

   function Handle_Message
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Servant is abstract new PolyORB.Smart_Pointers.Entity with
   record
      Neutral_View : aliased Implementation (Servant'Access);
      --  The PolyORB (personality-neutral) view of this servant.
   end record;

end PolyORB.Minimal_Servant;
