------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . R E P R E S E N T A T I O N S . T E S T          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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

--  A dummy data representation method, just for show.

--  $Id$

with PolyORB.Buffers; use PolyORB.Buffers;

package PolyORB.Representations.Test is

   pragma Elaborate_Body;

   type Rep_Test is new Representation with private;
   type Rep_Test_Access is access all Rep_Test;

   --  A real representation function should implement the
   --  following two subprograms.

   procedure Marshall_From_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any);

   procedure Unmarshall_To_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any);

   --  The following methods are specific to Rep_Test and are
   --  here only to facilitate testing of other parts of the ORB.

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character);
   --  Marshall one character.

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character;
   --  Unmarshall one character.

   procedure Marshall_String
     (R : access Rep_Test;
      B : access Buffer_Type;
      S : String);
   --  Marshall a string.

   function Unmarshall_String
     (R : access Rep_Test;
      B : access Buffer_Type)
     return String;
   --  Unmarshall a string terminated by a CR/LF sequence.

private

   type Rep_Test is new Representation with null record;

end PolyORB.Representations.Test;
