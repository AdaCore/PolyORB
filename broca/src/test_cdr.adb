------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                             T E S T _ C D R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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
with Broca.Buffers; use Broca.Buffers;
with Broca.CDR;   use Broca.CDR;
with CORBA;       use CORBA;

procedure Test_CDR is

   Stream, Inner1 : aliased Buffer_Type;
   Inner2 : aliased Buffer_Type (Endianness => Big_Endian);

begin
   --  Initialize (Inner1'Access, Big_Endian);
   --  Initialize (Inner2'Access, Little_Endian);

   Start_Encapsulation (Inner1'Access);
   Start_Encapsulation (Inner2'Access);

   Marshall (Inner1'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Marshall (Inner2'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   --  GIOP.Start_Message (Stream);
   --   <=> Marshall (message_header)

   Marshall (Stream'Access, True);
   Marshall (Stream'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   Marshall (Stream'Access, CORBA.Unsigned_Long'(16#1234ABCD#));
   --  Marshall (Stream'Access, Encaps (Inner1));
   --  Marshall (Stream'Access, Encaps (Inner2));

   declare
      E1 : aliased Encapsulation := Encapsulate (Inner1'Access);
      E2 : aliased Encapsulation := Encapsulate (Inner2'Access);
   begin
      Put_Line ("Inner1 :");
      Show (Inner1);
      Put_Line ("Inner2 :");
      Show (Inner2);
      Marshall (Stream'Access, E1);
      Marshall (Stream'Access, E2);
   end;
   Put_Line ("Stream:");
   Show (Stream);
end Test_CDR;
