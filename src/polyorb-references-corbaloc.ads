------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E F E R E N C E S . C O R B A L O C           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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

with PolyORB.Binding_Data;
with PolyORB.Types;

package PolyORB.References.Corbaloc is

   function Profile_To_String
     (P : Binding_Data.Profile_Access) return Types.String;

   function String_To_Profile
     (Obj_Addr : Types.String) return Binding_Data.Profile_Access;
   --  Return null if failed

   subtype Corbaloc_Type is PolyORB.References.Ref;

   type String_Array is array (Integer range <>) of Types.String;

   -------------------------------------
   -- Object reference <-> Corbaloc --
   -------------------------------------

   function Object_To_String_With_Best_Profile
     (Corbaloc : Corbaloc_Type)
     return Types.String;
   --  Return the corbaloc string for the best profile

   function Object_To_String
     (Corbaloc : Corbaloc_Type)
     return Types.String
     renames Object_To_String_With_Best_Profile;

   function Object_To_String
     (Corbaloc : Corbaloc_Type;
      Profile  : PolyORB.Binding_Data.Profile_Tag)
     return Types.String;
   --  Return the corbaloc string for the asked profile

   function Object_To_Strings (Corbaloc : Corbaloc_Type) return String_Array;
   --  Return an array of strings containing one corbaloc for each
   --  profile of this ref that supports them.

   ---------------------
   -- Profile Factory --
   ---------------------

   type Profile_To_String_Body_Type is access function
     (Profile : Binding_Data.Profile_Access) return String;

   type String_To_Profile_Body_Type is access function
     (Str : String) return Binding_Data.Profile_Access;

   procedure Register
     (Tag                    : PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : String;
      Profile_To_String_Body : Profile_To_String_Body_Type;
      String_To_Profile_Body : String_To_Profile_Body_Type);
   --  Register a corbaloc <-> profile mapping

private

   Corbaloc_Prefix : constant String := "corbaloc:";
   function String_To_Object (Str : String) return Corbaloc_Type;

end PolyORB.References.Corbaloc;
