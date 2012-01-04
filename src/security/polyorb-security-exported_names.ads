------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E C U R I T Y . E X P O R T E D _ N A M E S       --
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

--  This package provide data type for representation of a
--  GSS Mechanism-Independent Exported Name object as defined in
--  [IETF RFC 2743], as well as encodeing/decoding subprograms

with Ada.Streams;

with PolyORB.ASN1;
with PolyORB.Errors;

package PolyORB.Security.Exported_Names is

   type Exported_Name_Type is abstract tagged private;

   type Exported_Name_Access is access all Exported_Name_Type'Class;

   function Is_Equivalent
     (Left  : access Exported_Name_Type;
      Right : access Exported_Name_Type'Class)
      return Boolean
      is abstract;

   function Get_Mechanism_OID
     (Item : access Exported_Name_Type)
      return PolyORB.ASN1.Object_Identifier;

   function Get_Printable_Name
     (Item : access Exported_Name_Type)
      return String
      is abstract;

   function Duplicate
     (Item : access Exported_Name_Type)
      return Exported_Name_Access
      is abstract;
   --  Return copy of Exported Name

   procedure Release_Contents (Item : access Exported_Name_Type);

   procedure Destroy (Item : in out Exported_Name_Access);
   --  Release contents and destroy Exported Name

   function Encode_Name_BLOB
     (Item : access Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array
      is abstract;
   --  Encode Name part of Exported Name. This is an internal subprogram.

   procedure Decode_Name_BLOB
     (Item  : access Exported_Name_Type;
      BLOB  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Decode Name part of Exported Name. This is an internal subprogram.

   function Encode
     (Item : access Exported_Name_Type'Class)
      return Ada.Streams.Stream_Element_Array;
   --  Encode an Exported Name

   procedure Decode
     (Item  :        Ada.Streams.Stream_Element_Array;
      Name  :    out Exported_Name_Access;
      Error : in out PolyORB.Errors.Error_Container);
   --  Decode an Exported Name

private

   type Exported_Name_Type is abstract tagged record
      Mechanism_OID : PolyORB.ASN1.Object_Identifier;
   end record;

   --  Registry for known External Name types

   type Empty_Exported_Name_Factory is
     access function return Exported_Name_Access;
   --  Return empty Exported Name of corresponding derived type

   procedure Register
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Factory       : Empty_Exported_Name_Factory);

end PolyORB.Security.Exported_Names;
