------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                             T E S T _ C D R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

with Ada.Text_IO; use Ada.Text_IO;
with Broca.CDR;   use Broca.CDR;
with CORBA;       use CORBA;

procedure Test_CDR is

   Stream, Inner1, Inner2 : aliased Buffer_Type;

   procedure Dump (S : access Buffer_Type);

   ----------
   -- Dump --
   ----------

   procedure Dump (S : access Buffer_Type) is

      function Hex (O : Octet) return String;

      ---------
      -- Hex --
      ---------

      function Hex (O : Octet) return String is
         Mapping : constant String := "0123456789ABCDEF";
      begin
         return Mapping ((Natural (O) / 16) + 1) &
           Mapping ((Natural (O) mod 16) + 1);
      end Hex;

      Content : constant Octet_Array := Get_Content (S);
      Count   : Natural              := 0;

   begin
      Put ("Byte ordering: ");
      if Content (Content'First) = 0 then
         Put_Line ("big endian");
      elsif Content (Content'First) = 1 then
         Put_Line ("little endian");
      else
         Put_Line ("invalid");
      end if;
      for I in Content'Range loop
         Put (Hex (Content (I)) & " ");
         Count := Count + 1;
         if Count = 25 then
            New_Line;
            Count := 0;
         end if;
      end loop;
      New_Line;
   end Dump;

begin
   Initialize (Inner1'Access, Big_Endian);
   Initialize (Inner2'Access, Little_Endian);
   Marshall (Inner1'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Marshall (Inner2'Access, CORBA.Unsigned_Long'(16#1234ABCD#));

   Marshall (Stream'Access, True);
   Marshall (Stream'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Marshall (Stream'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Marshall (Stream'Access, Inner1);
   Marshall (Stream'Access, Inner2);
   Dump (Stream'Access);
end Test_CDR;
