------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . R E F E R E N C E S . I O R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  Representation of object references as typed
--  Interoperable Object References.

--  An IOR aggregates the identification of an interface (i.e. a type
--  identifier) and a set of profiles designating an object that supports
--  this interface. An IOR can be converted to a stringified
--  representation by marshalling it according to CDR, and converting
--  the resulting stream element array into a string of hexadecimal digits.

--  $Id$

with Ada.Streams;

with PolyORB.Buffers;      use PolyORB.Buffers;
with PolyORB.Types;

with PolyORB.Sequences.Unbounded;

package PolyORB.References.IOR is

   type Marshall_Profile_Body_Type is access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Binding_Data.Profile_Access);

   type Unmarshall_Profile_Body_Type is access function
     (Buffer  : access Buffers.Buffer_Type)
     return Binding_Data.Profile_Access;

   type Profile_Record is record
      Tag                     : Binding_Data.Profile_Tag;
      Marshall_Profile_Body   : Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : Unmarshall_Profile_Body_Type;
   end record;

   package Profile_Record_Seq is
      new PolyORB.Sequences.Unbounded (Profile_Record);

   --  An object reference (whose supported interface is not
   --  reflected by its Ada type) and the associated type information
   --  (within the PolyORB typing model).

   subtype IOR_Type is PolyORB.References.Ref;

   procedure Marshall_IOR
     (Buffer : access Buffer_Type;
      Value  : in IOR_Type);

   function  Unmarshall_IOR
     (Buffer : access Buffer_Type)
   return  IOR_Type;

   --------------------------------------
   -- Object reference <-> opaque data --
   --------------------------------------

   function Object_To_Opaque (IOR : IOR_Type)
     return Ada.Streams.Stream_Element_Array;

   function Opaque_To_Object
     (Opaque : access Ada.Streams.Stream_Element_Array)
     return IOR_Type;

   ------------------------------------------
   -- Object reference <-> stringified IOR --
   ------------------------------------------

   function Object_To_String (IOR : IOR_Type)
      return Types.String;

   function  String_To_Object (Str : Types.String)
     return IOR_Type;

   procedure Register
     (Profile                 : in Binding_Data.Profile_Tag;
      Marshall_Profile_Body   : in Marshall_Profile_Body_Type;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);

private

   Callbacks : Profile_Record_Seq.Sequence;

end PolyORB.References.IOR;
