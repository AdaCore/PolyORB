------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.EXPORTED_NAMES.UNKNOWN                  --
--                                                                          --
--                                 B o d y                                  --
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

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body PolyORB.Security.Exported_Names.Unknown is

   ----------------------
   -- Decode_Name_BLOB --
   ----------------------

   overriding procedure Decode_Name_BLOB
     (Item  : access Unknown_Exported_Name_Type;
      BLOB  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Error);

   begin
      Item.Name_BLOB := new Ada.Streams.Stream_Element_Array'(BLOB);
   end Decode_Name_BLOB;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (Item : access Unknown_Exported_Name_Type)
      return Exported_Name_Access
   is
   begin
      return
        new Unknown_Exported_Name_Type'
        (Mechanism_OID => PolyORB.ASN1.Duplicate (Item.Mechanism_OID),
         Name_BLOB     =>
           new Ada.Streams.Stream_Element_Array'(Item.Name_BLOB.all));
   end Duplicate;

   ----------------------
   -- Encode_Name_BLOB --
   ----------------------

   overriding function Encode_Name_BLOB
     (Item : access Unknown_Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array
   is
   begin
      return Item.Name_BLOB.all;
   end Encode_Name_BLOB;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   overriding function Get_Printable_Name
     (Item : access Unknown_Exported_Name_Type)
      return String
   is
      pragma Unreferenced (Item);

   begin
      return "[UNKNOWN]";
   end Get_Printable_Name;

   -------------------
   -- Is_Equivalent --
   -------------------

   overriding function Is_Equivalent
     (Left  : access Unknown_Exported_Name_Type;
      Right : access Exported_Name_Type'Class)
      return Boolean
   is
      use type Ada.Streams.Stream_Element_Array;
      use type PolyORB.ASN1.Object_Identifier;

   begin
      if Right.all not in Unknown_Exported_Name_Type then
         return False;
      end if;

      return
        Left.Mechanism_OID = Right.Mechanism_OID
        and then Left.Name_BLOB.all
          = Unknown_Exported_Name_Type (Right.all).Name_BLOB.all;
   end Is_Equivalent;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (Item : access Unknown_Exported_Name_Type)
   is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Ada.Streams.Stream_Element_Array,
         PolyORB.Security.Types.Stream_Element_Array_Access);

   begin
      Free (Item.Name_BLOB);
      Release_Contents (Exported_Name_Type (Item.all)'Access);
   end Release_Contents;

end PolyORB.Security.Exported_Names.Unknown;
