------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ S L O T S    --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with PolyORB.Annotations;
with PolyORB.Sequences.Unbounded;
with PortableInterceptor;

package PolyORB.CORBA_P.Interceptors_Slots is

   type Slots_Note is new PolyORB.Annotations.Note with private;

   function Invalid_Slots_Note return Slots_Note;

   function Allocate_Slot_Id return PortableInterceptor.SlotId;

   function Get_Slot
     (Note : Slots_Note;
      Id   : PortableInterceptor.SlotId)
      return CORBA.Any;

   procedure Set_Slot
     (Note : in out Slots_Note;
      Id   : PortableInterceptor.SlotId;
      Data : CORBA.Any);

   procedure Allocate_Slots
     (Note : in out Slots_Note);

   function Is_Allocated (Note : Slots_Note) return Boolean;
   --  Return True if slot table is allocated.

   ORB_Initializer_Done : Boolean := False;

private

   package Any_Sequences is new PolyORB.Sequences.Unbounded (CORBA.Any);

   type Slots_Note is new PolyORB.Annotations.Note with record
      Allocated : Boolean := False;
      Slots     : Any_Sequences.Sequence;
   end record;

end PolyORB.CORBA_P.Interceptors_Slots;
