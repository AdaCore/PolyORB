------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . M E S S A G E S . M E X E C U T E S               --
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

pragma Ada_2012;

with MOMA.Types;

package body MOMA.Messages.MExecutes is

   -------------------
   -- Get_Parameter --
   -------------------

   function Get_Parameter (Self : MExecute) return MOMA.Types.Map is
   begin
      return MOMA.Types.From_Any (Get_Payload (Self));
   end Get_Parameter;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter (Self : in out MExecute; Value : MOMA.Types.Map) is
   begin
      Set_Payload (Self, MOMA.Types.To_Any (Value));
   end Set_Parameter;

   ----------------------------
   -- Create_Execute_Message --
   ----------------------------

   function Create_Execute_Message return MExecute
   is
      Result : MExecute;

   begin
      Set_Type (Result, MOMA.Types.Execute_M);
      Set_Default_Message_Header (Result);

      return Result;
   end Create_Execute_Message;

   -----------
   -- Image --
   -----------

   overriding function Image (Self : MExecute) return String is
   begin
      return Image (Get_Payload (Self));
   end Image;

end MOMA.Messages.MExecutes;
