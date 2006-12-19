------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E F E R E N C E S . C O R B A L O C           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data;

package PolyORB.References.Corbaloc is

   subtype Corbaloc_Type is PolyORB.References.Ref;

   -----------------------------------
   -- Object reference <-> Corbaloc --
   -----------------------------------

   function Object_To_String_With_Best_Profile
     (Corbaloc : Corbaloc_Type)
     return String;
   --  Return the corbaloc string for the best profile

   function Object_To_String
     (Corbaloc : Corbaloc_Type)
     return String
     renames Object_To_String_With_Best_Profile;

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

end PolyORB.References.Corbaloc;
