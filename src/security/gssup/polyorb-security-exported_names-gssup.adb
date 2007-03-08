------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.EXPORTED_NAMES.GSSUP                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

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

   procedure Decode_Name_BLOB
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

   function Duplicate
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

   function Encode_Name_BLOB
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

   function Get_Printable_Name
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

   function Is_Equivalent
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

   procedure Release_Contents (Item : access GSSUP_Exported_Name_Type) is

      procedure Free is
        new Ada.Unchecked_Deallocation (String, String_Access);

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
