------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  M O M A . M E S S A G E S . M M A P S                   --
--                                                                          --
--                                 B o d y                                  --
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

with MOMA.Types;
with PolyORB.Any;

package body MOMA.Messages.MMaps is

   use MOMA.Types;

   --------------
   -- Get_Map --
   --------------

   function Get_Map (Self : MMap)
                     return MOMA.Types.Map is
   begin
      return MOMA.Types.From_Any (Get_Payload (Self));
   end Get_Map;

   --------------
   -- Set_Map --
   --------------

   procedure Set_Map (Self : in out MMap;
                      Value : MOMA.Types.Map) is
   begin
      Set_Payload (Self, MOMA.Types.To_Any (Value));
   end Set_Map;

   -------------------------
   -- Create_Map_Message --
   -------------------------

   function Create_Map_Message
            return MMap
   is
      Result : MMap;
   begin
      Set_Type (Result, Map_M);
      Set_Default_Message_Header (Result);

      return Result;
   end Create_Map_Message;

   -----------
   -- Image --
   -----------

   function Image (Self : MMap) return String
   is
      use PolyORB.Any;
   begin
      return Image (Get_Payload (Self));
   end Image;

end MOMA.Messages.MMaps;

