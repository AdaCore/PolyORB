------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . B U F F E R S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

with Ada.Unchecked_Deallocation;

package Broca.Buffers is

   --  Buffer to/from network before unmarshalling or after marshalling.
   --  Must start at 0, according to CORBA V2.2 13.3

   type Element is mod 2 ** 8;
   type Buffer_Index_Type is new Natural;
   type Buffer_Type is array (Buffer_Index_Type range <>) of Element;
   type Buffer_Ptr is access Buffer_Type;

   --  A buffer with the current position, as needed by unmarshall and
   --  endianness.
   type Buffer_Descriptor is
      record
         Buffer : Buffer_Ptr := null;
         Pos : Buffer_Index_Type := 0;
         Little_Endian : Boolean := False;
      end record;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => Buffer_Ptr, Object => Buffer_Type);
end Broca.Buffers;
