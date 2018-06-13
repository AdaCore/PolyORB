------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  M O M A . M E S S A G E S . M M A P S                   --
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

--  MMap message type.
--
--  A MMap message contains a sequence of named value, similar to a bag
--  container.

with MOMA.Types;

package MOMA.Messages.MMaps is

   type MMap is new Message with private;

   function Create_Map_Message return Messages.MMaps.MMap;
   --  Create a MMap message.

   overriding function Image (Self : MMap) return String;
   --  Image function for MMap type.

   --  Accessors to MMap payload.

   function Get_Map (Self : MMap) return MOMA.Types.Map;

   procedure Set_Map (Self : in out MMap; Value : MOMA.Types.Map);

private

   type MMap is new Message with null record;

end MOMA.Messages.MMaps;
