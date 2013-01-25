------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . M E S S A G E S . M T E X T S                  --
--                                                                          --
--                                 B o d y                                  --
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

   overriding function Image (Self : MText) return String is
   begin
      return MOMA.Types.To_Standard_String (Get_Text (Self));
   end Image;

end MOMA.Messages.MTexts;
