------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . A S N 1                          --
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

with Ada.Streams;

package PolyORB.ASN1 is

   ASN1_Error : exception;

   --  ASN.1 OBJECT IDENTIFIER

   type Object_Identifier is private;
   Null_Object_Identifier : constant Object_Identifier;

   function Is_Equivalent
     (OID1 : Object_Identifier;
      OID2 : Object_Identifier)
      return Boolean;

   function Duplicate (Item : Object_Identifier) return Object_Identifier;

   procedure Free (Item : in out Object_Identifier);

   procedure Destroy (Item : in out Object_Identifier)
     renames Free;

   function Encode
     (Item : Object_Identifier)
      return Ada.Streams.Stream_Element_Array;

   function Decode
     (Item : Ada.Streams.Stream_Element_Array)
      return Object_Identifier;

   function To_String (Item : Object_Identifier) return String;

   function To_Object_Identifier (Item : String) return Object_Identifier;

private

   type Object_Identifier_Record is null record;
   pragma Convention (C, Object_Identifier_Record);

   type Object_Identifier is access all Object_Identifier_Record;
   Null_Object_Identifier : constant Object_Identifier := null;

end PolyORB.ASN1;
