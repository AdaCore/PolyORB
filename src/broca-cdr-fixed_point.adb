------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . C D R                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with Broca.Exceptions;
with Broca.Opaque; use Broca.Opaque;
with Broca.Debug;
with Interfaces;

package body Broca.CDR.Fixed_Point is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.cdr.fixed_point");
   procedure O is new Broca.Debug.Output (Flag);

   subtype BO_Octet is Broca.Opaque.Octet;

   use Interfaces;

   Fixed_Positive_Zero : constant BO_Octet
     := 16#C#;
   Fixed_Negative : constant BO_Octet
     := 16#D#;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access F) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in F)
   is
      N_Digits : Integer
        := 0;

      Val : F := Data;
   begin

      loop
         N_Digits := N_Digits + 1;
         Val := Val * 0.1;
         exit when Val = 0.0;
      end loop;

      declare
         Octets : Octet_Array
           (0 .. Index_Type ((N_Digits + 2) / 2 - 1))
           := (others => 0);
         --  The size of the representation is
         --  at least 1, plus 1 nibble for the sign.

         Offset : Integer;
         Bias : F;
      begin
         if N_Digits mod 2 /= 0 then
            Offset := 0;
         else
            Offset := 1;
         end if;

         Val := Data;
         Bias := F (10.0 ** (N_Digits - F'Scale + 1));

         for I in Offset .. Offset + N_Digits loop
            declare
               Digit : constant BO_Octet
                 := BO_Octet (Val / Bias);
            begin
               if I mod 2 = 0 then
                  Octets (Index_Type (I / 2)) := Digit * 16;
               else
                  Octets (Index_Type (I / 2))
                    := Octets (Index_Type (I / 2)) + Digit;
               end if;
               Val := Val - F (Digit) * Bias;
               Bias := 0.1 * Bias;
            end;
         end loop;
         if Data >= 0.0 then
            Octets (Octets'Last) :=
              Octets (Octets'Last) + Fixed_Positive_Zero;
         else
            Octets (Octets'Last) :=
              Octets (Octets'Last) + Fixed_Negative;
         end if;
         Align_Marshall_Copy (Buffer, Octets, 1);
      end;
   end Marshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return F
   is
      use CORBA;

      O : CORBA.Octet;
      Result : F := 0.0;
   begin
      loop
         O := Unmarshall (Buffer);
         if O / 16 > 9
           or else
           (O mod 16 > 9
            and then O mod 16 /= CORBA.Octet (Fixed_Positive_Zero)
           and then O mod 16 /= CORBA.Octet (Fixed_Negative))
         then
            Broca.Exceptions.Raise_Marshal;
         end if;

         Result := Result * 10 + F (O / 16) * F'Delta;
         if O mod 16 < 10 then
            Result := Result * 10 + F (O mod 16) * F'Delta;
         else
            if O mod 16 = CORBA.Octet (Fixed_Negative) then
               Result := -Result;
            end if;
            exit;
         end if;
      end loop;

      return Result;
   end Unmarshall;

end Broca.CDR.Fixed_Point;
