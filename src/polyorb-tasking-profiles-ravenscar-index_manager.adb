------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--             . R A V E N S C A R . I N D E X _ M A N A G E R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of a thread safe index manager.

package body PolyORB.Tasking.Profiles.Ravenscar.Index_Manager is

   type Flag_Array is array (Index_Type) of Boolean;

   type Index_Type_Array is array (Index_Type) of Index_Type;

   protected Index_Manager is
      --  This protected object manage the pool of Index,
      --  and the "allocations" of ID.
      --  this manager uses a stack of fixed size
      --  "release" push an ID in this stack,
      --  and "get" pop  an ID.
      --  this stack is implemented by an array, with an offset
      --  pointing at the next ID available.

      procedure Get (Id : out Index_Type);
      --  Get a free Index_Type.

      procedure Release (Id : in Index_Type);
      --  Release Id.

      procedure Init;
      --  Initialize the Index_Manager.

   private
      Initialized    : Boolean := False;
      Free_Stack     : Index_Type_Array;
      Offset         : Index_Type;
      Number_Of_Used : Integer;
      Used           : Flag_Array;
   end Index_Manager;

   ---------
   -- Get --
   ---------

   procedure Get (Id : out Index_Type) is
   begin
      Index_Manager.Get (Id);
   end Get;

   -------------------
   -- Index_Manager --
   -------------------

   protected body  Index_Manager is

      -----------------------
      -- Index_Manager.Get --
      -----------------------

      procedure Get (Id : out Index_Type) is
      begin
         pragma Assert (Initialized);
         if Number_Of_Used > Index_Type'Last then
            raise No_More_Identifier;
         end if;
         Id := Free_Stack (Offset);
         Offset := Modular (Integer (Offset) - 1);
         Number_Of_Used := Number_Of_Used + 1;
         Used (Id) := True;
      end Get;

      ------------------------
      -- Index_Manager.Init --
      ------------------------

      procedure Init is
      begin
         pragma Assert (not Initialized);
         for J in Free_Stack'Range loop
            Free_Stack (J) := J;
            Used (J) := False;
         end loop;
         Number_Of_Used := 0;
         Offset := Free_Stack'Last;
         Initialized := True;
      end Init;

      ---------------------------
      -- Index_Manager.Release --
      ---------------------------

      procedure Release (Id : in Index_Type) is
      begin
         pragma Assert (Initialized);
         if not Used (Id) then
            raise Identifier_Already_Released;
         end if;
         Offset := Modular (Integer (Offset) + 1);
         Free_Stack (Offset) := Id;
         Number_Of_Used := Number_Of_Used - 1;
         Used (Id) := False;
      end Release;

   end Index_Manager;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Index_Manager.Init;
   end Initialize;

   -------------
   -- Modular --
   -------------

   function Modular (I : Integer) return Index_Type is
   begin
      return Index_Type (I mod Number_Of_Indices);
   end Modular;

   -------------
   -- Release --
   -------------

   procedure Release (Id : in Index_Type) is
   begin
      Index_Manager.Release (Id);
   end Release;

end PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;
