------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           M O M A . T Y P E S                            --
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

--  MOMA Types definitions.

--  $Id$

with PolyORB.Types;

package MOMA.Types is

   --  Generic types.
   --
   --  Whenever possible, we reuse definitions from the CORBA specifications

   subtype Boolean is Standard.Boolean;
   subtype String  is PolyORB.Types.String;

   --  String conversion fonctions.

   function To_Standard_String (V : PolyORB.Types.String)
                                return Standard.String
     renames PolyORB.Types.To_Standard_String;

   function To_MOMA_String (V : Standard.String)
                            return PolyORB.Types.String
     renames PolyORB.Types.To_PolyORB_String;

   --  MOMA specific types

   type Meta_Data        is new    Integer;
   type Acknowledge_Type is new    Integer;
   type Property_Type    is new    Integer;
   type String_Ptr       is access String;
   type Priority         is new    Integer range 1 .. 10;
   type Record_Type      is new    Integer;
   type Array_Type       is new    Integer;

   type Message_Type is (Byte_M, Text_M);
   --  XXX to be completed

end MOMA.Types;

