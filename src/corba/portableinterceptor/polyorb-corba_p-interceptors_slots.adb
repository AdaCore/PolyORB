------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ S L O T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Exceptions;

package body PolyORB.CORBA_P.Interceptors_Slots is

   use Any_Sequences;
   use PortableInterceptor;

   Last_Allocated_Slot_Id : PortableInterceptor.SlotId := 0;

   procedure Raise_InvalidSlot;

   ----------------------
   -- Allocate_Slot_Id --
   ----------------------

   function Allocate_Slot_Id
      return SlotId
   is
   begin
      Last_Allocated_Slot_Id := Last_Allocated_Slot_Id + 1;

      return Last_Allocated_Slot_Id;
   end Allocate_Slot_Id;

   --------------------
   -- Allocate_Slots --
   --------------------

   procedure Allocate_Slots
     (Note : in out Slots_Note)
   is
      Empty : CORBA.Any := CORBA.Get_Empty_Any (CORBA.TC_Null);
   begin
      Note.Slots := Null_Sequence;

      for J in 1 .. Last_Allocated_Slot_Id loop
         Append (Note.Slots, Empty);
      end loop;

      Note.Allocated := True;
   end Allocate_Slots;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (Note : in Slots_Note;
      Id   : in SlotId)
      return CORBA.Any
   is
   begin
      pragma Assert (Note.Allocated);

      if Id not in 1 .. SlotId (Length (Note.Slots)) then
         Raise_InvalidSlot;
      end if;

      return Get_Element (Note.Slots, Integer (Id));
   end Get_Slot;

   ------------------------
   -- Invalid_Slots_Note --
   ------------------------

   function Invalid_Slots_Note return Slots_Note is
      Aux : constant Slots_Note
        := (PolyORB.Annotations.Note with False, Any_Sequences.Null_Sequence);
   begin
      return Aux;
   end Invalid_Slots_Note;

   ------------------
   -- Is_Allocated --
   ------------------

   function Is_Allocated (Note : in Slots_Note) return Boolean is
   begin
      return Note.Allocated;
   end Is_Allocated;

   -----------------------
   -- Raise_InvalidSlot --
   -----------------------

   procedure Raise_InvalidSlot is
      Members : InvalidSlot_Members;
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidSlot'Identity,
         Members);
   end Raise_InvalidSlot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Note : in out Slots_Note;
      Id   : in     SlotId;
      Data : in     CORBA.Any)
   is
   begin
      pragma Assert (Note.Allocated);

      if Id not in 1 .. SlotId (Length (Note.Slots)) then
         Raise_InvalidSlot;
      end if;

      Replace_Element (Note.Slots, Integer (Id), Data);
   end Set_Slot;

end PolyORB.CORBA_P.Interceptors_Slots;
