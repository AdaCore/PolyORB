------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . A B O R T A B L E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

with Ada.Tags.Generic_Dispatching_Constructor;

package body PolyORB.Tasking.Abortables is

   Abortable_Tag : Ada.Tags.Tag := Abortable'Tag;
   Initialized   : Boolean      := False;

   ---------------
   -- Abort_Run --
   ---------------

   procedure Abort_Run (AR : access Abortable) is
   begin
      --  By default abortion is not supported and this opeartion has no effect
      null;
   end Abort_Run;

   ------------
   -- Create --
   ------------

   function Create (R : not null access PTT.Runnable'Class) return Abortable is
   begin
      return Abortable'(R => PTT.Runnable_Access (R));
   end Create;

   --------------------
   -- Make_Abortable --
   --------------------

   function Make_Abortable
     (R : access PTT.Runnable'Class) return Abortable'Class
   is
      function Create is
        new Ada.Tags.Generic_Dispatching_Constructor
              (T           => Abortable,
               Parameters  => PTT.Runnable'Class,
               Constructor => Create);
   begin
      return Create (Abortable_Tag, R);
   end Make_Abortable;

   ----------------------------
   -- Register_Abortable_Tag --
   ----------------------------

   procedure Register_Abortable_Tag (T : Ada.Tags.Tag) is
   begin
      pragma Assert (not Initialized);

      if not Initialized then
         Abortable_Tag := T;
         Initialized := True;
      end if;
   end Register_Abortable_Tag;

   ---------
   -- Run --
   ---------

   procedure Run (AR : access Abortable) is
   begin
      PTT.Run (AR.R);
   end Run;

end PolyORB.Tasking.Abortables;
