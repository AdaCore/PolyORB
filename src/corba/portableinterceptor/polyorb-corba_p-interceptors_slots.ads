------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ S L O T S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
