------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . M A R S H A L L I N G                     --
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
with Broca.Buffers; use Broca.Buffers;

package Broca.Marshalling is

   O_Size  : constant Buffer_Index_Type := 1;
   C_Size  : constant Buffer_Index_Type := 1;
   B_Size  : constant Buffer_Index_Type := 1;
   S_Size  : constant Buffer_Index_Type := 2;
   US_Size : constant Buffer_Index_Type := 2;
   L_Size  : constant Buffer_Index_Type := 4;
   UL_Size : constant Buffer_Index_Type := 4;
   F_Size  : constant Buffer_Index_Type := 4;
   D_Size  : constant Buffer_Index_Type := 8;

   --  Compute_Size, Marshall and Unmarshall predefined types.

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Short);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in String);

   procedure Compute_New_Size
     (Buffer        : in out Buffer_Descriptor;
      Length_Size   : in Buffer_Index_Type;
      Element_Size  : in Buffer_Index_Type;
      Array_Length  : in Natural);

   --  Marshall
   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Short);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in String);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String);

   --  Unmarshall
   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Octet);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Char);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Boolean);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Short);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Short);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Long);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Long);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.String);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Float);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Double);

   procedure Skip_String (Buffer : in out Buffer_Descriptor);
   --  Unmarshall String and ignore result.

end Broca.Marshalling;
