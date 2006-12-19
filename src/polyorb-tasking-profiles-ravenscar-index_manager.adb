------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.TASKING.PROFILES.RAVENSCAR.INDEX_MANAGER              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Implementation of a thread safe index manager.

with PolyORB.Log;

package body PolyORB.Tasking.Profiles.Ravenscar.Index_Manager is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.ravenscar.index_manager");

   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
      --  Get a free Index_Type

      procedure Release (Id : Index_Type);
      --  Release Id

      procedure Init (Error_On_Initialize : Boolean := True);
      --  Initialize the Index_Manager

   private
      Initialized    : Boolean := False;
      Free_Stack     : Index_Type_Array;
      Offset         : Index_Type;
      Number_Of_Used : Integer;
      Used           : Flag_Array;
   end Index_Manager;

   function Modular (I : Integer) return Index_Type;
   pragma Inline (Modular);
   --  Convert an Integer to an Index_Type, returning
   --  I mod Number_Of_Indices

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
            raise Tasking_Error;
         end if;

         Id := Free_Stack (Offset);
         Offset := Modular (Integer (Offset) - 1);
         Number_Of_Used := Number_Of_Used + 1;
         pragma Debug (O ("Get " & Integer'Image (Id)));
         Used (Id) := True;
      end Get;

      ------------------------
      -- Index_Manager.Init --
      ------------------------

      procedure Init (Error_On_Initialize : Boolean := True) is
      begin
         pragma Assert (not (Initialized and then Error_On_Initialize));
         if not Initialized then
            for J in Free_Stack'Range loop
               Free_Stack (J) := J;
               Used (J) := False;
            end loop;
            Number_Of_Used := 0;
            Offset := Free_Stack'Last;
            Initialized := True;
         end if;
      end Init;

      ---------------------------
      -- Index_Manager.Release --
      ---------------------------

      procedure Release (Id : Index_Type) is
      begin
         pragma Debug (O ("Release" & Integer'Image (Id)));
         pragma Assert (Initialized);
         if not Used (Id) then
            raise Program_Error;
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

   procedure Initialize  (Error_On_Initialize : Boolean := True) is
   begin
      Index_Manager.Init (Error_On_Initialize);
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

   procedure Release (Id : Index_Type) is
   begin
      Index_Manager.Release (Id);
   end Release;

end PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;
