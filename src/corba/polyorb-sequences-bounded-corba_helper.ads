------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SEQUENCES.BOUNDED.CORBA_HELPER                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  Any conversion subprograms for bounded sequences

with CORBA;

with PolyORB.Any;
with PolyORB.Sequences.Bounded.Helper;
pragma Elaborate_All (PolyORB.Sequences.Bounded.Helper);

generic
   with function Element_From_Any (Item : CORBA.Any) return Element;
   with function Element_To_Any   (Item : Element) return CORBA.Any;
   with function Element_Wrap
     (X : access Element) return PolyORB.Any.Content'Class;

package PolyORB.Sequences.Bounded.CORBA_Helper is

   function From_Any (Item : CORBA.Any) return Sequence;
   function To_Any   (Item : Sequence) return CORBA.Any;
   function Wrap (X : access Sequence) return PolyORB.Any.Content'Class;

   procedure Initialize
     (Element_TC, Sequence_TC : CORBA.TypeCode.Object);

private

   function Element_From_Any_Wrapper (Item : PolyORB.Any.Any) return Element;
   function Element_To_Any_Wrapper (Item : Element) return PolyORB.Any.Any;
   --  Helpers operating on PolyORB Any's, constructed from the formal helpers
   --  operating on CORBA Any's.

   package Neutral_Helper is new PolyORB.Sequences.Bounded.Helper
     (Element_From_Any => Element_From_Any_Wrapper,
      Element_To_Any   => Element_To_Any_Wrapper,
      Element_Wrap     => Element_Wrap);

end PolyORB.Sequences.Bounded.CORBA_Helper;
