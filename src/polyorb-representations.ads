------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R E P R E S E N T A T I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Data representation methods

--  $Id$

with Ada.Streams;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Opaque;

package PolyORB.Representations is

   pragma Elaborate_Body;

   type Representation is abstract tagged limited private;
   type Representation_Access is access all Representation;
   --  A Representation is a method for transforming an
   --  arbitrary piece of data (in the form of an 'Any'
   --  object) into a sequence of Stream_Elements, and
   --  back.

   procedure Marshall_From_Any
     (R      : Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any)
     is abstract;

   procedure Unmarshall_To_Any
     (R      : Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any)
     is abstract;

private

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Opaque;

   type Representation is abstract tagged limited null record;

   ---------------------------------------------------
   -- Utility subprograms for children of this unit --
   ---------------------------------------------------

   function Rev
     (Octets : Stream_Element_Array)
     return Stream_Element_Array;
   --  Reverse the order of an array of octets.

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in big-endian
   --  byte order.

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in big-endian byte order.

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in the
   --  host's byte order.

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in the host's byte order.

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : in Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy
   --  of Octets into Buffer, as is.

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy
   --  of Size octets from Buffer's data, as is.

end PolyORB.Representations;
