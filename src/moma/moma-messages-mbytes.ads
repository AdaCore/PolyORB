------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . M E S S A G E S . M B Y T E S                  --
--                                                                          --
--                                 S p e c                                  --
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

--  MByte message type.
--
--  A MByte message's payload is a basic type among: Boolean, Byte, Char,
--  Double, Float, Long, Short, Unsigned_Short, Unsigned_Long.

--  $Id$

with MOMA.Types;

package MOMA.Messages.MBytes is

   type MByte is new Message with private;

   function Create_Byte_Message return MByte;
   --  Create a MByte message.

   function Image (Self : MByte)
                   return String;
   --  Image function for MByte type.

   --  Accessors to MByte payload.

   function Get_Boolean (Self : MByte)
                         return MOMA.Types.Boolean;

   procedure Set_Boolean (Self : in out MByte;
                          Value : MOMA.Types.Boolean);

   function Get_Byte (Self : MByte)
                      return MOMA.Types.Byte;

   procedure Set_Byte (Self : in out MByte;
                       Value : MOMA.Types.Byte);

   function Get_Char (Self : MByte)
                      return MOMA.Types.Char;

   procedure Set_Char (Self : in out MByte;
                       Value : MOMA.Types.Char);

   function Get_Double (Self : MByte)
                        return MOMA.Types.Double;

   procedure Set_Double (Self : in out MByte;
                         Value : MOMA.Types.Double);

   function Get_Float (Self : MByte)
                       return MOMA.Types.Float;

   procedure Set_Float (Self : in out MByte;
                        Value : MOMA.Types.Float);

   function Get_Long (Self : MByte)
                      return MOMA.Types.Long;

   procedure Set_Long (Self : in out MByte;
                       Value : MOMA.Types.Long);

   function Get_Short (Self : MByte)
                       return MOMA.Types.Short;

   procedure Set_Short (Self : in out MByte;
                        Value : MOMA.Types.Short);

   function Get_Unsigned_Short (Self : MByte)
                                return MOMA.Types.Unsigned_Short;

   procedure Set_Unsigned_Short (Self : in out MByte;
                                 Value : MOMA.Types.Unsigned_Short);

   function Get_Unsigned_Long (Self : MByte)
                               return MOMA.Types.Unsigned_Long;

   procedure Set_Unsigned_Long (Self : in out MByte;
                                Value : MOMA.Types.Unsigned_Long);

private
   type MByte is new Message with null record;

end MOMA.Messages.MBytes;
