------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              M O M A . M E S S A G E S . M E X E C U T E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with MOMA.Types;

package body MOMA.Messages.MExecutes is

   -------------------
   -- Get_Parameter --
   -------------------

   function Get_Parameter (Self : MExecute)
                          return MOMA.Types.Map is
   begin
      return MOMA.Types.From_Any (Get_Payload (Self));
   end Get_Parameter;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter (Self : in out MExecute;
                            Value : MOMA.Types.Map) is
   begin
      Set_Payload (Self, MOMA.Types.To_Any (Value));
   end Set_Parameter;

   ----------------------------
   -- Create_Execute_Message --
   ----------------------------

   function Create_Execute_Message
     return MExecute
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

   function Image (Self : MExecute)
                  return String is
   begin
      return Image (Get_Payload (Self));
   end Image;

end MOMA.Messages.MExecutes;
