------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.TASKING.PROFILES.FULL_TASKING.CONDITION_VARIABLES         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Implementation of POSIX-like condition variables with full Ada
--  tasking. More comments can be found at polyorb-tasking-condition_variables.

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables is

   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Full_Tasking_Condition_Type is
     new PTCV.Condition_Type with private;

   type Full_Tasking_Condition_Access is
     access all Full_Tasking_Condition_Type'Class;

   overriding procedure Wait
     (Cond : access Full_Tasking_Condition_Type;
      M    : access PTM.Mutex_Type'Class);

   overriding procedure Signal (Cond : access Full_Tasking_Condition_Type);

   overriding procedure Broadcast (Cond : access Full_Tasking_Condition_Type);

   type Full_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with private;

   type Full_Tasking_Condition_Factory_Access is
     access all Full_Tasking_Condition_Factory_Type'Class;

   The_Condition_Factory : constant Full_Tasking_Condition_Factory_Access;

   overriding function Create
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access;

   overriding procedure Destroy
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Cond : in out PTCV.Condition_Access);

private

   type Condition_PO;
   type Condition_PO_Access is access Condition_PO;

   type Full_Tasking_Condition_Type is new PTCV.Condition_Type with record
      The_PO : Condition_PO_Access;
   end record;

   type Full_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with null record;

   The_Condition_Factory : constant Full_Tasking_Condition_Factory_Access
     := new Full_Tasking_Condition_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
