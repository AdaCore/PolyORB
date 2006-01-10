------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  D Y N A M I C A N Y . D Y N V A L U E                   --
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

with DynamicAny.DynValueCommon;

package DynamicAny.DynValue is

   type Local_Ref is new DynamicAny.DynValueCommon.Local_Ref with null record;

   function Current_Member_Name (Self : Local_Ref) return FieldName;

   function Current_Member_Kind (Self : Local_Ref) return CORBA.TCKind;

   function Get_Members (Self : Local_Ref) return NameValuePairSeq;

   procedure Set_Members
     (Self  : Local_Ref;
      Value : NameValuePairSeq);

   function Get_Members_As_Dyn_Any
     (Self : Local_Ref)
      return NameDynAnyPairSeq;

   procedure Set_Members_As_Dyn_Any
     (Self  : Local_Ref;
      Value : NameDynAnyPairSeq);

   --  Repository Ids

   Repository_Id                        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue:1.0";

   Current_Member_Kind_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/current_member_kind:1.0";

   Current_Member_Name_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/current_member_name:1.0";

   Get_Members_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/get_members:1.0";

   Get_Members_As_Dyn_Any_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/get_members_as_dyn_any:1.0";

   Set_Members_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/set_members:1.0";

   Set_Members_As_Dyn_Any_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynValue/set_members_as_dyn_any:1.0";

end DynamicAny.DynValue;
