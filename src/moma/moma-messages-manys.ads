------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  M O M A . M E S S A G E S . M A N Y S                   --
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

pragma Ada_2012;

--  MAny message type.
--
--  A MAny message's payload is a PolyORB Any. It's an 'all purpose' message
--  type designed to create ad hoc messages, from various data types such as
--  arrays, records, user defined types ..

with MOMA.Types;

package MOMA.Messages.MAnys is

   type MAny is new Message with private;

   function Create_Any_Message return Messages.MAnys.MAny;
   --  Create a MAny message.

   overriding function Image (Self : MAny) return String;
   --  Image function for MAny type.

   --  Accessors to MAny payload.

   function Get_Any (Self : MAny) return MOMA.Types.Any;

   procedure Set_Any (Self : in out MAny; Value : MOMA.Types.Any);

private

   type MAny is new Message with null record;

end MOMA.Messages.MAnys;
