------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      B R O C A . S E Q U E N C E S                       --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with CORBA.Sequences.Unbounded;
with Broca.Opaque; use Broca.Opaque;
with Broca.Buffers; use Broca.Buffers;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package Broca.Sequences is

   package Octet_Sequences is new CORBA.Sequences.Unbounded (CORBA.Octet);

   subtype Octet_Sequence is Octet_Sequences.Sequence;
   subtype CORBA_Octet_Array is Octet_Sequences.Element_Array;

   Null_Sequence : Octet_Sequence renames Octet_Sequences.Null_Sequence;

   function To_CORBA_Octet_Array
     (Data : Octet_Array)
     return CORBA_Octet_Array;
   --  Return an array of CORBA.Octet (suitable for
   --  creation of an Octet_Sequence) from an Octet_Array.

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Octet_Sequences.Sequence);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Octet_Sequences.Sequence);

   function Unmarshall (Buffer : access Buffer_Type)
     return Octet_Sequences.Sequence;

end Broca.Sequences;
