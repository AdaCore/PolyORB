------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . U R I                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with PolyORB.Binding_Data;
with PolyORB.Types;

package PolyORB.References.URI is

   function Profile_To_String
     (P : Binding_Data.Profile_Access)
     return String;

   function String_To_Profile
     (Str : String)
     return Binding_Data.Profile_Access;
   --  Returns null if it failed

   subtype URI_Type is PolyORB.References.Ref;

   type String_Array is array (Integer range <>)
     of PolyORB.Utils.Strings.String_Ptr;

   procedure Free (SA : in out String_Array);
   --  Free a String_Array

   ------------------------------
   -- Object reference <-> URI --
   ------------------------------

   function Object_To_String_With_Best_Profile
     (URI : URI_Type)
     return String;
   --  Returns the URI string for the best profile

   function Object_To_String
     (URI : URI_Type)
     return String
     renames Object_To_String_With_Best_Profile;

   function Object_To_String
     (URI     : URI_Type;
      Profile : PolyORB.Binding_Data.Profile_Tag)
     return String;
   --  Returns the URI string for the requested profile

   function Object_To_Strings (URI : URI_Type) return String_Array;
   --  Returns an array of strings containing one URI for each
   --  profile of this ref that supports them.

   function String_To_Object (Str : String) return URI_Type;

   ---------------------
   -- Profile Factory --
   ---------------------

   type Profile_To_String_Body_Type is access function
     (Profile : Binding_Data.Profile_Access)
     return Types.String;

   type String_To_Profile_Body_Type is access function
     (Str : Types.String)
     return Binding_Data.Profile_Access;

   procedure Register
     (Tag                    : in PolyORB.Binding_Data.Profile_Tag;
      Proto_Ident            : in Types.String;
      Profile_To_String_Body : in Profile_To_String_Body_Type;
      String_To_Profile_Body : in String_To_Profile_Body_Type);

end PolyORB.References.URI;
