------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . U R I                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Binding_Data;

package PolyORB.References.URI is

   subtype URI_Type is PolyORB.References.Ref;

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

   function String_To_Object (Str : String) return URI_Type;

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

end PolyORB.References.URI;
