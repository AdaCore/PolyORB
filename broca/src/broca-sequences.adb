------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      B R O C A . S E Q U E N C E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Marshalling; use Broca.Marshalling;
with Ada.Unchecked_Conversion;

package body Broca.Sequences is

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence) is
   begin
      Compute_New_Size
        (Buffer,
         UL_Size,
         O_Size,
         Octet_Sequences.Length (Value));
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence)
   is
      package OS renames Octet_Sequences;
      Octets : constant OS.Element_Array := OS.To_Element_Array (Value);
      subtype Sub_Element_Array is OS.Element_Array (Octets'Range);
      subtype Sub_Buffer_Type is Buffer_Type (0 .. Octets'Length - 1);
      function Element_Array_To_Buffer_Type is
        new Ada.Unchecked_Conversion (Sub_Element_Array, Sub_Buffer_Type);
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Octets'Length));
      Write (Buffer, Element_Array_To_Buffer_Type (Octets));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Octet_Sequences.Sequence)
   is
      package S renames Octet_Sequences;
      Length : Buffer_Index_Type;
   begin
      Unmarshall (Buffer, CORBA.Unsigned_Long (Length));
      declare
         subtype Sub_Element_Array is S.Element_Array (1 .. Natural (Length));
         subtype Sub_Buffer_Type is Buffer_Type (0 .. Length - 1);
         function Buffer_Type_To_Element_Array is
           new Ada.Unchecked_Conversion (Sub_Buffer_Type, Sub_Element_Array);
         Bytes : Sub_Buffer_Type;
      begin
         Read (Buffer, Bytes);
         Result := S.To_Sequence (Buffer_Type_To_Element_Array (Bytes));
      end;
   end Unmarshall;

end Broca.Sequences;
