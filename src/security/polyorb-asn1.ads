------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . A S N 1                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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
