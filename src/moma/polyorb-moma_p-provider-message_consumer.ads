------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.MOMA_P.PROVIDER.MESSAGE_CONSUMER                  --
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

--  Actual implementation of the Message_Consumer object. It is
--  derived from PolyORB's Minimal_Servant. This package contains
--  Message_Consumer skeleton and implementation subroutines. By
--  construction, its implementation subroutines contain parts of a
--  stub for the Message_Pool object.

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.References;

package PolyORB.MOMA_P.Provider.Message_Consumer is

   type Object is new PolyORB.Minimal_Servant.Servant with private;
   --  Remote_Ref : Reference to the pool from which receive messages.

   type Object_Acc is access Object;

   overriding procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access);
   --  Message_Consumer servant skeleton.

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

end PolyORB.MOMA_P.Provider.Message_Consumer;
