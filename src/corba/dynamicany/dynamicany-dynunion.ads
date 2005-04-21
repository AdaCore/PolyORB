------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  D Y N A M I C A N Y . D Y N U N I O N                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;

with DynamicAny.DynAny;

package DynamicAny.DynUnion is

   type Local_Ref is new DynamicAny.DynAny.Local_Ref with null record;

   function Get_Discriminator (Self : in Local_Ref) return DynAny.Local_Ref;

   procedure Set_Discriminator
     (Self : in Local_Ref;
      D    : in DynAny.Local_Ref);

   procedure Set_To_Default_Member (Self : in Local_Ref);

   procedure Set_To_No_Active_Member (Self : in Local_Ref);

   function Has_No_Active_Member (Self : in Local_Ref) return CORBA.Boolean;

   function Discriminator_Kind (Self : in Local_Ref) return CORBA.TCKind;

   function Member (Self : in Local_Ref) return DynAny.Local_Ref;

   function Member_Name (Self : in Local_Ref) return FieldName;

   function Member_Kind (Self : in Local_Ref) return CORBA.TCKind;

   --  Repository Ids

   Repository_Id                         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion:1.0";

   Discriminator_Kind_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/discriminator_kind:1.0";

   Get_Discriminator_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/get_discriminator:1.0";

   Has_No_Active_Member_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/has_no_active_member:1.0";

   Member_Repository_Id                  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/member:1.0";

   Member_Kind_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/member_kind:1.0";

   Member_Name_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/member_name:1.0";

   Set_Discriminator_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/set_discriminator:1.0";

   Set_To_Default_Member_Repository_Id   : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/set_to_default_member:1.0";

   Set_To_No_Active_Member_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynUnion/set_to_no_active_member:1.0";

end DynamicAny.DynUnion;
