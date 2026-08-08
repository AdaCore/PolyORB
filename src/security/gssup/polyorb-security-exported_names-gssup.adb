------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.EXPORTED_NAMES.GSSUP                   --
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

with Ada.Unchecked_Conversion;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Security.Types;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Exported_Names.GSSUP is

   use Ada.Streams;

   function Create_Empty_Exported_Name return Exported_Name_Access;

   procedure Initialize;

   --------------------------------
   -- Create_Empty_Exported_Name --
   --------------------------------

   function Create_Empty_Exported_Name return Exported_Name_Access is
   begin
      return new GSSUP_Exported_Name_Type;
   end Create_Empty_Exported_Name;

   --------------------------------
   -- Create_GSSUP_Exported_Name --
   --------------------------------

   function Create_GSSUP_Exported_Name
     (Scoped_Name : String)
      return Exported_Name_Access
   is
   begin
      return
        new GSSUP_Exported_Name_Type'
        (Mechanism_OID =>
           PolyORB.ASN1.To_Object_Identifier
           (PolyORB.Security.Types.GSSUPMechOID),
         Scoped_Name   => new String'(Scoped_Name));
   end Create_GSSUP_Exported_Name;

   ----------------------
   -- Decode_Name_BLOB --
   ----------------------

   overriding procedure Decode_Name_BLOB
     (Item  : access GSSUP_Exported_Name_Type;
      BLOB  :        Ada.Streams.Stream_Element_Array;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      function To_Character is
        new Ada.Unchecked_Conversion (Stream_Element, Character);

   begin
      if BLOB'Length = 0 then
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));

         return;
      end if;

      Item.Scoped_Name :=
        new String (Integer (BLOB'First) .. Integer (BLOB'Last));

      for J in Item.Scoped_Name'Range loop
         Item.Scoped_Name (J) :=
           To_Character (BLOB (Stream_Element_Offset (J)));
      end loop;
   end Decode_Name_BLOB;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (Item : access GSSUP_Exported_Name_Type)
      return Exported_Name_Access
   is
   begin
      return new GSSUP_Exported_Name_Type'
        (Mechanism_OID => PolyORB.ASN1.Duplicate (Item.Mechanism_OID),
         Scoped_Name   => new String'(Item.Scoped_Name.all));
   end Duplicate;

   ----------------------
   -- Encode_Name_BLOB --
   ----------------------

   overriding function Encode_Name_BLOB
     (Item : access GSSUP_Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array
   is

      function To_Stream_Element is
        new Ada.Unchecked_Conversion (Character, Stream_Element);

      Result : Stream_Element_Array
       (Stream_Element_Offset (Item.Scoped_Name'First)
         .. Stream_Element_Offset (Item.Scoped_Name'Last));

   begin
      for J in Item.Scoped_Name'Range loop
         Result (Stream_Element_Offset (J)) :=
           To_Stream_Element (Item.Scoped_Name (J));
      end loop;

      return Result;
   end Encode_Name_BLOB;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   overriding function Get_Printable_Name
     (Item : access GSSUP_Exported_Name_Type)
      return String
   is
   begin
      return "[GSSUP]" & Item.Scoped_Name.all;
   end Get_Printable_Name;

--   --------------
--   -- Get_Name --
--   --------------
--
--   function Get_Name
--     (Item : access GSSUP_Exported_Name_Type)
--      return String
--   is
--   begin
--      return Item.Name.all;
--   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (PolyORB.ASN1.To_Object_Identifier
         (PolyORB.Security.Types.GSSUPMechOID),
         Create_Empty_Exported_Name'Access);
   end Initialize;

   -------------------
   -- Is_Equivalent --
   -------------------

   overriding function Is_Equivalent
     (Left  : access GSSUP_Exported_Name_Type;
      Right : access Exported_Name_Type'Class)
      return Boolean
   is
   begin
      return
        Right.all in GSSUP_Exported_Name_Type
        and then Left.Scoped_Name.all
          = GSSUP_Exported_Name_Type (Right.all).Scoped_Name.all;
   end Is_Equivalent;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (Item : access GSSUP_Exported_Name_Type)
   is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => String,


         Name   => String_Access);

   begin
      Free (Item.Scoped_Name);
      Release_Contents (Exported_Name_Type (Item.all)'Access);
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.exported_names.gssup",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Exported_Names.GSSUP;
