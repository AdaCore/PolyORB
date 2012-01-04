------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . I O R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Representation of object references as typed
--  Interoperable Object References.

--  An IOR aggregates the identification of an interface (i.e. a type
--  identifier) and a set of profiles designating an object that supports
--  this interface. An IOR can be converted to a stringified
--  representation by marshalling it according to CDR, and converting
--  the resulting stream element array into a string of hexadecimal digits.

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Binding_Data;

package PolyORB.References.IOR is

   use PolyORB.Buffers;

   procedure Marshall_Profile
     (Buffer  : access Buffer_Type;
      P       :        Binding_Data.Profile_Access;
      Success :    out Boolean);

   function Unmarshall_Profile
     (Buffer : access Buffer_Type)
     return Binding_Data.Profile_Access;
   --  Return null if failed

   procedure Marshall_IOR
     (Buffer : access Buffer_Type;
      Value  : PolyORB.References.Ref);

   function  Unmarshall_IOR
     (Buffer : access Buffer_Type)
     return  PolyORB.References.Ref;

   --------------------------------------
   -- Object reference <-> opaque data --
   --------------------------------------

   function Object_To_Opaque (IOR : PolyORB.References.Ref)
     return Ada.Streams.Stream_Element_Array;

   function Opaque_To_Object
     (Opaque : access Ada.Streams.Stream_Element_Array)
     return PolyORB.References.Ref;

   ------------------------------------------
   -- Object reference <-> stringified IOR --
   ------------------------------------------

   function Object_To_String (IOR : PolyORB.References.Ref) return String;

   ---------------------
   -- Profile Factory --
   ---------------------

   type Marshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile :        Binding_Data.Profile_Access);

   type Unmarshall_Profile_Body_Type is access function
     (Buffer  : access Buffers.Buffer_Type)
     return Binding_Data.Profile_Access;

   procedure Register
     (Profile                 : Binding_Data.Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type);

private

   function String_To_Object (Str : String) return PolyORB.References.Ref;

end PolyORB.References.IOR;
