------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . C O R B A _ P . C O D E C _ U T I L S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

with CORBA;

package body PolyORB.CORBA_P.Codec_Utils is

   use Ada.Streams;

   ----------------------
   -- To_Encapsulation --
   ----------------------

   function To_Encapsulation
     (Item : in IOP.IDL_Sequence_Octet.Sequence)
      return PolyORB.Representations.CDR.Common.Encapsulation
   is
      Result : PolyORB.Representations.CDR.Common.Encapsulation
        (1 .. Stream_Element_Offset (IOP.IDL_Sequence_Octet.Length (Item)));

   begin
      for J in Result'Range loop
         Result (J) :=
           Stream_Element
            (IOP.IDL_Sequence_Octet.Element_Of (Item, Integer (J)));
      end loop;

      return Result;
   end To_Encapsulation;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Item : in PolyORB.Representations.CDR.Common.Encapsulation)
      return IOP.IDL_Sequence_Octet.Sequence
   is
      Result : IOP.IDL_Sequence_Octet.Sequence;

   begin
      for J in Item'Range loop
         IOP.IDL_Sequence_Octet.Append (Result, CORBA.Octet (Item (J)));
      end loop;

      return Result;
   end To_Sequence;

end PolyORB.CORBA_P.Codec_Utils;
