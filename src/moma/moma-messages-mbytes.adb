------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . M E S S A G E S . M B Y T E S                  --
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
with PolyORB.Types;

package body MOMA.Messages.MBytes is

   use MOMA.Types;

   -------------------------
   -- Create_Byte_Message --
   -------------------------

   function Create_Byte_Message
            return MByte
   is
      Result : MByte;
   begin
      Set_Type (Result, Byte_M);
      --  XXX should initialize other fields as well ???
      return Result;
   end Create_Byte_Message;

   -----------
   -- Image --
   -----------

   function Image (Self : MByte) return String is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Image (Self);
      pragma Warnings (On);
   end Image;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Self : MByte)
                         return MOMA.Types.Boolean is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Boolean;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Self : in out MByte;
                          Value : MOMA.Types.Boolean) is
   begin
      Set_Payload (Self,
                   PolyORB.Any.To_Any (PolyORB.Types.Boolean (Value)));
   end Set_Boolean;

   --------------
   -- Get_Byte --
   --------------

   function Get_Byte (Self : MByte)
                      return MOMA.Types.Byte is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Byte;

   --------------
   -- Set_Byte --
   --------------

   procedure Set_Byte (Self : in out MByte;
                       Value : MOMA.Types.Byte) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Octet (Value)));
   end Set_Byte;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Self : MByte)
                      return MOMA.Types.Char is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Char;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (Self : in out MByte;
                       Value : MOMA.Types.Char) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Char (Value)));
   end Set_Char;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double (Self : MByte)
                        return MOMA.Types.Double is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Double;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double (Self : in out MByte;
                         Value : MOMA.Types.Double) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Double (Value)));
   end Set_Double;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Self : MByte)
                       return MOMA.Types.Float is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Float;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float (Self : in out MByte;
                        Value : MOMA.Types.Float) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Float (Value)));
   end Set_Float;

   --------------
   -- Get_Long --
   --------------

   function Get_Long (Self : MByte)
                      return MOMA.Types.Long is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Long;

   --------------
   -- Set_Long --
   --------------

   procedure Set_Long (Self : in out MByte;
                       Value : MOMA.Types.Long) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Long (Value)));
   end Set_Long;

   ---------------
   -- Get_Short --
   ---------------

   function Get_Short (Self : MByte)
                       return MOMA.Types.Short is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Short;

   ---------------
   -- Set_Short --
   ---------------

   procedure Set_Short (Self : in out MByte;
                        Value : MOMA.Types.Short) is
   begin
      Set_Payload (Self, PolyORB.Any.To_Any (PolyORB.Types.Short (Value)));
   end Set_Short;

   -----------------------
   -- Get_Unsigned_Long --
   -----------------------

   function Get_Unsigned_Long (Self : MByte)
                               return MOMA.Types.Unsigned_Long is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Unsigned_Long;

   -----------------------
   -- Set_Unsigned_Long --
   -----------------------

   procedure Set_Unsigned_Long (Self : in out MByte;
                                Value : MOMA.Types.Unsigned_Long) is
   begin
      Set_Payload (Self,
                   PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Long (Value)));
   end Set_Unsigned_Long;

   ------------------------
   -- Get_Unsigned_Short --
   ------------------------

   function Get_Unsigned_Short (Self : MByte)
                                return MOMA.Types.Unsigned_Short is
   begin
      return PolyORB.Any.From_Any (Get_Payload (Self));
   end Get_Unsigned_Short;

   ------------------------
   -- Set_Unsigned_Short --
   ------------------------

   procedure Set_Unsigned_Short (Self : in out MByte;
                                 Value : MOMA.Types.Unsigned_Short) is
   begin
      Set_Payload (Self,
                   PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (Value)));
   end Set_Unsigned_Short;

end MOMA.Messages.MBytes;
