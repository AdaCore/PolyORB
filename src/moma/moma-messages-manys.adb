------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  M O M A . M E S S A G E S . M A N Y S                   --
--                                                                          --
--                                 B o d y                                  --
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

package body MOMA.Messages.MAnys is

   --------------
   -- Get_Any --
   --------------

   function Get_Any (Self : MAny) return MOMA.Types.Any is
   begin
      return Get_Payload (Self);
   end Get_Any;

   --------------
   -- Set_Any --
   --------------

   procedure Set_Any (Self  : in out MAny; Value : MOMA.Types.Any) is

   begin
      Set_Payload (Self, Value);
   end Set_Any;

   -------------------------
   -- Create_Any_Message --
   -------------------------

   function Create_Any_Message return MAny
   is
      Result : MAny;

   begin
      Set_Type (Result, MOMA.Types.Any_M);
      Set_Default_Message_Header (Result);

      return Result;
   end Create_Any_Message;

   -----------
   -- Image --
   -----------

   function Image (Self : MAny) return String is
   begin
      return Image (Get_Any (Self));
   end Image;

end MOMA.Messages.MAnys;
