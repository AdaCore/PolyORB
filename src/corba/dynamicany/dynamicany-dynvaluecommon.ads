------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            D Y N A M I C A N Y . D Y N V A L U E C O M M O N             --
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

package DynamicAny.DynValueCommon is

   type Local_Ref is new DynamicAny.DynAny.Local_Ref with null record;

   function Is_Null (Self : Local_Ref) return CORBA.Boolean;

   procedure Set_To_Null (Self : Local_Ref);

   procedure Set_To_Value (Self : Local_Ref);

   --  Repository Ids

   Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValueCommon:1.0";

   Is_Null_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValueCommon/is_null:1.0";

   Set_To_Null_Repository_Id  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValueCommon/set_to_null:1.0";

   Set_To_Value_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValueCommon/set_to_value:1.0";

end DynamicAny.DynValueCommon;
