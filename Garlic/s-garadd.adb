------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . A D D R E S S E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with GNAT.HTable;              use GNAT.HTable;
with GNAT.Table;
with System.Storage_Elements;  use System.Storage_Elements;

package body System.Garlic.Addresses is

   Have_Same_Size : constant Boolean := Address'Size = Handle'Size;

   function Convert is
      new Ada.Unchecked_Conversion (Address, Handle);
   function Convert is
      new Ada.Unchecked_Conversion (Handle, Address);
   pragma Warnings (Off, Convert);

   function Access_To_Integer_Address is
      new Ada.Unchecked_Conversion (Address, Integer_Address);

   package Handle_Table is new GNAT.Table
     (Table_Component_Type => Address,
      Table_Index_Type     => Handle,
      Table_Low_Bound      => 1,
      Table_Initial        => 16,
      Table_Increment      => 100);

   type Header_Num is new Interfaces.Integer_16;
   function Hash (F : Address) return Header_Num;
   No_Element : constant Handle := -1;

   package Handle_HTable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Handle,
      No_Element => No_Element,
      Key        => Address,
      Hash       => Hash,
      Equal      => "=");
   use Handle_HTable;

   -------------------
   -- Create_Handle --
   -------------------

   function Create_Handle (A : Address) return Handle is
   begin
      if Have_Same_Size then
         return Convert (A);
      elsif A = Null_Address then
         return 0;
      else
         declare
            H : Handle := Get (A);
         begin
            if H = No_Element then
               H := Handle_Table.Allocate;
               Handle_Table.Table (H) := A;
               Set (A, H);
            end if;
            return H;
         end;
      end if;
   end Create_Handle;

   ----------
   -- Hash --
   ----------

   function Hash (F : Address) return Header_Num is
      I_A : constant Integer_Address := Access_To_Integer_Address (F);
   begin
      return Header_Num ((I_A / 2**16) mod (I_A mod 2 ** 16));
   end Hash;

   ----------------------
   -- Retrieve_Address --
   ----------------------

   function Retrieve_Address (H : Handle) return Address is
   begin
      if Have_Same_Size then
         return Convert (H);
      elsif H = 0 then
         return Null_Address;
      else
         pragma Assert (H > 0 and H < Handle_Table.Last);
         return Handle_Table.Table (H);
      end if;
   end Retrieve_Address;

end System.Garlic.Addresses;
