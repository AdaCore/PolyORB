------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            M O M A . C O N N E C T I O N _ F A C T O R I E S             --
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

--  A Connection_Factory contains all information to create a connection to
--  the MOMA provider.

--  XXX need to clarify the notion of provider.

with MOMA.Types;

package MOMA.Connection_Factories is

   type Connection_Factory is private;

   procedure Create
     (Self     : out Connection_Factory;
      Remote   :     MOMA.Types.Ref);
   --  Create a new connection factory, with the provider Remote.

   --  Accessors to the Connection_Factory internals.

   procedure Set_Ref
     (Self    : in out Connection_Factory;
      Remote  :        MOMA.Types.Ref);

   function Get_Ref
     (Self    : Connection_Factory)
     return MOMA.Types.Ref;

private

   type Connection_Factory is record
      Remote : MOMA.Types.Ref;
      --  The access point to the MOMA domain.
      --  XXX : this is a concept to clarify.
   end record;

end MOMA.Connection_Factories;
