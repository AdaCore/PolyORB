------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  M O M A . M E S S A G E S . M A N Y S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  MAny message type.
--
--  A MAny message's payload is a PolyORB Any. It's an 'all purpose' message
--  type designed to create ad hoc messages, from various data types such as
--  arrays, records, user defined types ..

--  $Id$

with MOMA.Types;

package MOMA.Messages.MAnys is

   type MAny is new Message with private;

   function Create_Any_Message return Messages.MAnys.MAny;
   --  Create a MAny message.

   function Image (Self : MAny) return String;
   --  Image function for MAny type.

   --  Accessors to MAny payload.

   function Get_Any (Self : MAny) return MOMA.Types.Any;

   procedure Set_Any (Self : in out MAny; Value : MOMA.Types.Any);

private

   type MAny is new Message with null record;

end MOMA.Messages.MAnys;
