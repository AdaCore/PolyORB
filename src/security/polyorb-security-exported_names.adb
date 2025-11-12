------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E C U R I T Y . E X P O R T E D _ N A M E S       --
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

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Security.Exported_Names.Unknown;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Security.Exported_Names is

   use Ada.Streams;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.security.exported_names");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Registry_Item is record
      Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Factory       : Empty_Exported_Name_Factory;
   end record;

   package Registry_Item_Lists is
     new PolyORB.Utils.Chained_Lists (Registry_Item);

   Registry : Registry_Item_Lists.List;

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Item  :        Ada.Streams.Stream_Element_Array;
      Name  :    out Exported_Name_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;
      use type PolyORB.ASN1.Object_Identifier;

      OID_Length  : Stream_Element_Offset;
      BLOB_Length : Stream_Element_Offset;
      OID         : PolyORB.ASN1.Object_Identifier;

   begin
      --  Check minimum data length and token identifier

      if Item'Length < 2 + 2 + 4  --  token identifier, OID length, BLOB length
        or else Item (Item'First) /= 16#04#
        or else Item (Item'First + 1) /= 16#01#
      then
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));

         return;
      end if;

      --  Calculate OID length

      OID_Length :=
        Stream_Element_Offset (Item (Item'First + 2)) * 2**8
        + Stream_Element_Offset (Item (Item'First + 3));

      --  Check is data amount enought to contents BLOB length

      if 2 + 2 + OID_Length + 4 - 1 > Item'Length then
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));

         return;
      end if;

      --  Calculate BLOB length

      BLOB_Length :=
        Stream_Element_Offset (Item (Item'First + OID_Length + 4)) * 2**24
        + Stream_Element_Offset (Item (Item'First + OID_Length + 5)) * 2**16
        + Stream_Element_Offset (Item (Item'First + OID_Length + 6)) * 2**8
        + Stream_Element_Offset (Item (Item'First + OID_Length + 7));

      --  Check is total amount of data same with encoded value

      if 2 + 2 + OID_Length + 4 + BLOB_Length /= Item'Length then
         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(Minor => 0, Completed => Completed_No));

         return;
      end if;

      --  Decode mechanism OID

      begin
         OID :=
           PolyORB.ASN1.Decode
           (Item (Item'First + 4 .. Item'First + OID_Length + 3));

      exception
         when PolyORB.ASN1.ASN1_Error =>
            Throw
              (Error,
               Marshal_E,
               System_Exception_Members'
               (Minor => 0, Completed => Completed_No));

            return;
      end;

      --  Construct empty Exported Name. If Exported Name is unknown, then
      --  create unknown Exported Name for represent value.

      declare
         use Registry_Item_Lists;

         Iter : Iterator := First (Registry);

      begin
         while not Last (Iter) loop
            if PolyORB.ASN1.Is_Equivalent
              (Value (Iter).Mechanism_OID, OID)
            then
               Name := Value (Iter).Factory.all;

               exit;
            end if;

            Next (Iter);
         end loop;
      end;

      if Name = null then
         pragma Debug
           (C, O ("(Decode) Unknown exported name mechanism: "
            & PolyORB.ASN1.To_String (OID)));

         Name := new Unknown.Unknown_Exported_Name_Type;
      end if;

      --  Setup internal mechanism OID

      Name.Mechanism_OID := OID;

      --  Decode name BLOB

      Decode_Name_BLOB
        (Name,
         Item (Item'First + OID_Length + 8 .. Item'Last),
         Error);

      if Found (Error) then
         Destroy (Name);

         return;
      end if;
   end Decode;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Exported_Name_Access) is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Exported_Name_Type'Class,


         Name   => Exported_Name_Access);

   begin
      if Item /= null then
         Release_Contents (Item);
         Free (Item);
      end if;
   end Destroy;

   ------------
   -- Encode --
   ------------

   function Encode
     (Item : access Exported_Name_Type'Class)
      return Ada.Streams.Stream_Element_Array
   is
      OID    : constant Stream_Element_Array
        := PolyORB.ASN1.Encode (Item.Mechanism_OID);
      Name   : constant Stream_Element_Array := Encode_Name_BLOB (Item);
      Result :
        Stream_Element_Array (1 .. 2 + 2 + OID'Length + 4 + Name'Length);

   begin
      --  Token Identifier

      Result (1 .. 2) := 16#04# & 16#01#;

      --  Length of Mechanism OID

      Result (3 .. 4) :=
        Stream_Element ((OID'Length / 2**8) mod 2**8)
        & Stream_Element (OID'Length mod 2**8);

      --  Mechanism OID

      Result (5 .. OID'Length + 4) := OID;

      --  Length of Name

      Result (OID'Length + 5 .. OID'Length + 8) :=
        Stream_Element ((Name'Length / 2**24) mod 2**8)
        & Stream_Element ((Name'Length / 2**16) mod 2**8)
        & Stream_Element ((Name'Length / 2**8) mod 2**8)
        & Stream_Element (Name'Length mod 2**8);

      --  Name

      Result (OID'Length + 9 .. Result'Last) := Name;

      return Result;
   end Encode;

   -----------------------
   -- Get_Mechanism_OID --
   -----------------------

   function Get_Mechanism_OID
     (Item : access Exported_Name_Type)
      return PolyORB.ASN1.Object_Identifier
   is
   begin
      return Item.Mechanism_OID;
   end Get_Mechanism_OID;

   --------------
   -- Register --
   --------------

   procedure Register
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Factory       : Empty_Exported_Name_Factory)
   is
   begin
      pragma Debug
        (C, O ("Register exported name mechanism: "
         & PolyORB.ASN1.To_String (Mechanism_OID)));

      Registry_Item_Lists.Append (Registry, (Mechanism_OID, Factory));
   end Register;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (Item : access Exported_Name_Type) is
   begin
      PolyORB.ASN1.Destroy (Item.Mechanism_OID);
   end Release_Contents;

end PolyORB.Security.Exported_Names;
