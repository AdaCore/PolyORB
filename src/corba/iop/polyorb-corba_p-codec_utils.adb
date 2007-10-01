------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . C O R B A _ P . C O D E C _ U T I L S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with Ada.Streams;

package body PolyORB.CORBA_P.Codec_Utils is

   use Ada.Streams;

   ----------------------
   -- To_Encapsulation --
   ----------------------

   function To_Encapsulation
     (Item : CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence)
      return PolyORB.Representations.CDR.Common.Encapsulation
   is
      Result : PolyORB.Representations.CDR.Common.Encapsulation
        (1 .. Stream_Element_Offset
               (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Length (Item)));

   begin
      for J in Result'Range loop
         Result (J) :=
           Stream_Element
            (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Get_Element
              (Item, Integer (J)));
      end loop;

      return Result;
   end To_Encapsulation;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Item : PolyORB.Representations.CDR.Common.Encapsulation)
      return CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence
   is
      Result : CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence;

   begin
      for J in Item'Range loop
         CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Append
          (Result, CORBA.Octet (Item (J)));
      end loop;

      return Result;
   end To_Sequence;

end PolyORB.CORBA_P.Codec_Utils;
