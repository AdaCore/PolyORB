------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . M E S S A G E S . M T E X T S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Types;

package body MOMA.Messages.MTexts is

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Self : MText) return MOMA.Types.String is
   begin
      return MOMA.Types.String (PolyORB.Types.String'(PolyORB.Any.From_Any
                                                      (Get_Payload (Self))));
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Self : in out MText; Value : MOMA.Types.String) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.String (Value)));
   end Set_Text;

   -------------------------
   -- Create_Text_Message --
   -------------------------

   function Create_Text_Message return MText
   is
      Result : MText;
   begin
      Set_Type (Result, MOMA.Types.Text_M);
      Set_Default_Message_Header (Result);

      return Result;
   end Create_Text_Message;

   -----------
   -- Image --
   -----------

   function Image (Self : MText) return String is
   begin
      return MOMA.Types.To_Standard_String (Get_Text (Self));
   end Image;

end MOMA.Messages.MTexts;
