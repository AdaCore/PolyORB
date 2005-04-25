------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E S . M S T R E A M S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  XXX should we keep it ?

package MOMA.Messages.MStreams is

   ---------------------
   --  MStream Object --
   ---------------------
   type MStream is new Message with null record;

   -------------------
   --  Read_Boolean --
   -------------------
   function Read_Boolean return Boolean;

   ----------------
   --  Read_Char --
   ----------------
   function Read_Char return Character;

   -----------------
   --  Read_Float --
   -----------------
   function Read_Float return Float;

   -------------------
   --  Read_Integer --
   -------------------
   function Read_Integer return Integer;

   ------------------
   --  Read_String --
   ------------------
   function Read_String return String;

   ------------
   --  Reset --
   ------------
   procedure Reset;

   ------------------------
   --  Set_Boolean_Value --
   ------------------------
   procedure Set_Boolean (Value : Boolean);

   ---------------
   --  Set_Char --
   ---------------
   procedure Set_Char (Value : Character);

   ----------------
   --  Set_Float --
   ----------------
   procedure Set_Float (Value : Float);

   ------------------
   --  Set_Integer --
   ------------------
   procedure Set_Integer (Value : Integer);

   -----------------
   --  Set_String --
   -----------------
   procedure Set_String (Value : String);

end MOMA.Messages.MStreams;
