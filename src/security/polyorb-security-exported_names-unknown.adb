------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SECURITY.EXPORTED_NAMES.UNKNOWN                  --
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

with Ada.Unchecked_Deallocation;

package body PolyORB.Security.Exported_Names.Unknown is

   ----------------------
   -- Decode_Name_BLOB --
   ----------------------

   procedure Decode_Name_BLOB
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

   function Duplicate
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

   function Encode_Name_BLOB
     (Item : access Unknown_Exported_Name_Type)
      return Ada.Streams.Stream_Element_Array
   is
   begin
      return Item.Name_BLOB.all;
   end Encode_Name_BLOB;

   ------------------------
   -- Get_Printable_Name --
   ------------------------

   function Get_Printable_Name
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

   function Is_Equivalent
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

   procedure Release_Contents (Item : access Unknown_Exported_Name_Type) is

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Ada.Streams.Stream_Element_Array,
         PolyORB.Security.Types.Stream_Element_Array_Access);

   begin
      Free (Item.Name_BLOB);
      Release_Contents (Exported_Name_Type (Item.all)'Access);
   end Release_Contents;

end PolyORB.Security.Exported_Names.Unknown;
