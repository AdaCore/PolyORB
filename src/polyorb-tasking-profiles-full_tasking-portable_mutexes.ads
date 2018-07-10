------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.FULL_TASKING.PORTABLE_MUTEXES           --
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

--  Implementation of mutexes under the Full_Tasking profile.
--  This is a variant that uses only standard Ada constructs. It is not
--  used by default.

with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.Full_Tasking.Portable_Mutexes is

   package PTM renames PolyORB.Tasking.Mutexes;

   type Full_Tasking_Mutex_Type is
     new PTM.Mutex_Type with private;

   type Full_Tasking_Mutex_Access is
     access all Full_Tasking_Mutex_Type'Class;

   overriding procedure Enter (M : access Full_Tasking_Mutex_Type);
   pragma Inline (Enter);

   overriding procedure Leave (M : access Full_Tasking_Mutex_Type);
   pragma Inline (Leave);

   type Full_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with private;

   type Full_Tasking_Mutex_Factory_Access is
     access all Full_Tasking_Mutex_Factory_Type'Class;

   The_Mutex_Factory : constant Full_Tasking_Mutex_Factory_Access;

   overriding function Create
     (MF   : access Full_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access;

   overriding procedure Destroy
     (MF : access Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access);

private

   type Mutex_PO;
   type Mutex_PO_Access is access Mutex_PO;

   type Full_Tasking_Mutex_Type is new PTM.Mutex_Type with record
      The_PO : Mutex_PO_Access;
   end record;

   type Full_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with null record;

   The_Mutex_Factory : constant Full_Tasking_Mutex_Factory_Access
     := new Full_Tasking_Mutex_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Portable_Mutexes;
