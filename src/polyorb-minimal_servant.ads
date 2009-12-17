------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

--  A Minimal_Servant is servant independant from any application
--  personalities. It allows the creation of servants on top of PolyORB's
--  neutral core layer.
--
--  Hence, these servants can be made available to all applications
--  personalities, without the need of a specific one, allowing easy
--  deployment of common services.
--
--  However, it is a 'minimal' servant : it is incomplete and you will have
--  to write Invoke function corresponding to your servant.
--  This allows you to precisely control the servants created.

with PolyORB.Components;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Smart_Pointers.Controlled_Entities;
with PolyORB.Requests;

package PolyORB.Minimal_Servant is

   pragma Elaborate_Body;

   type Servant is abstract new Smart_Pointers.Controlled_Entities.Entity
     with private;

   type Servant_Acc is access all Servant;

   function Execute_Servant
     (Self : not null access Servant;
      Msg  : Components.Message'Class) return Components.Message'Class;

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access;

   procedure Invoke
     (Self    : access Servant;
      Request : PolyORB.Requests.Request_Access)
      is abstract;

private

   type Implementation (As_Servant : access Servant'Class) is
     new Servants.Servant with null record;

   function Execute_Servant
     (Self : not null access Implementation;
      Msg  : Components.Message'Class) return Components.Message'Class;

   type Servant is abstract new Smart_Pointers.Controlled_Entities.Entity
   with record
      Neutral_View : aliased Implementation (Servant'Access);
      --  The PolyORB (personality-neutral) view of this servant.
      --  This instance of the multiple views idiom allows the
      --  implementation of a multiple inheritance relationship:
      --  here, Servant inherits from both Entity (a reference-counted
      --  thing) and Servant (a Component that implements the
      --  Objects interface).
   end record;

end PolyORB.Minimal_Servant;
