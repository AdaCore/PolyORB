------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      B R O C A . S E Q U E N C E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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

with Broca.CDR; use Broca.CDR;

package body Broca.Sequences is

   package OS renames Octet_Sequences;

   function To_CORBA_Octet_Array
     (Data : Octet_Array)
     return CORBA_Octet_Array
   is
      Result : CORBA_Octet_Array (1 .. Data'Length);
   begin
      for I in Data'Range loop
         Result (Integer (I - Data'First + 1))
           := CORBA.Octet (Data (I));
      end loop;

      return Result;
   end To_CORBA_Octet_Array;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Octet_Sequences.Sequence)
   is
      Octets : constant OS.Element_Array
        := OS.To_Element_Array (Data);
      --  subtype Sub_Element_Array is OS.Element_Array (Octets'Range);
      --  subtype Sub_Buffer_Type is Buffer_Type (0 .. Octets'Length - 1);
      --  function Element_Array_To_Buffer_Type is
      --    new Ada.Unchecked_Conversion (Sub_Element_Array, Sub_Buffer_Type);
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Octets'Length));
      for I in Octets'Range loop
         Marshall (Buffer, Octets (I));
      end loop;
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Octet_Sequences.Sequence)
   is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Octet_Sequences.Sequence
   is
      Length : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);

      subtype Sub_Element_Array is
        OS.Element_Array (1 .. Integer (Length));
      Octets : Sub_Element_Array;
   begin
      for I in Octets'Range loop
         Octets (I) := Unmarshall (Buffer);
      end loop;
      return OS.To_Sequence (Octets);
   end Unmarshall;

end Broca.Sequences;
