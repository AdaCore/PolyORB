------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                      B R O C A . S E Q U E N C E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
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

with CORBA;
with CORBA.Sequences.Unbounded;
with Broca.Buffers; use Broca.Buffers;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package Broca.Sequences is

   package Octet_Sequences is new CORBA.Sequences.Unbounded (CORBA.Octet);

   subtype Octet_Sequence is Octet_Sequences.Sequence;

   Null_Sequence : Octet_Sequence renames Octet_Sequences.Null_Sequence;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Octet_Sequences.Sequence);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Octet_Sequences.Sequence);

end Broca.Sequences;
