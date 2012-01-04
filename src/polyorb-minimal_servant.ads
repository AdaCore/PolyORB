------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . M I N I M A L _ S E R V A N T               --
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

pragma Ada_2005;

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

with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Smart_Pointers.Controlled_Entities;
with PolyORB.Requests;

package PolyORB.Minimal_Servant is

   pragma Elaborate_Body;

   Discard_Request : exception;

   type Servant is abstract new Smart_Pointers.Controlled_Entities.Entity
     with private;

   type Servant_Acc is access all Servant;

   function Execute_Servant
     (Self : not null access Servant;
      Req  : Requests.Request_Access) return Boolean;

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access;

   procedure Invoke
     (Self    : access Servant;
      Request : PolyORB.Requests.Request_Access) is abstract;
   --  Run Request. If Discard_Request is raised, no further processing is
   --  done (no reply sent to caller).

private

   type Implementation (As_Servant : access Servant'Class) is
     new Servants.Servant with null record;

   overriding function Execute_Servant
     (Self : not null access Implementation;
      Req  : Requests.Request_Access) return Boolean;

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
