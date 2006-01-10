------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   D Y N A M I C A N Y . D Y N E N U M                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA;

with DynamicAny.DynAny;

package DynamicAny.DynEnum is

   type Local_Ref is new DynamicAny.DynAny.Local_Ref with null record;

   function Get_As_String (Self : Local_Ref) return CORBA.String;

   procedure Set_As_String
     (Self  : Local_Ref;
      Value : CORBA.String);

   function Get_As_ULong (Self : Local_Ref) return CORBA.Unsigned_Long;

   procedure Set_As_ULong
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Long);

   --  Repository Ids

   Repository_Id               : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynEnum:1.0";

   Get_As_String_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynEnum/get_as_string:1.0";

   Get_As_ULong_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynEnum/get_as_ulong:1.0";

   Set_As_String_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynEnum/set_as_string:1.0";

   Set_As_ULong_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynEnum/set_as_ulong:1.0";

end DynamicAny.DynEnum;
