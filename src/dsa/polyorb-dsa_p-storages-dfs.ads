------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . D S A _ P . S T O R A G E S . D F S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

pragma Warnings (Off);
--  System.Global_Locks is an internal GNAT unit
with System.Global_Locks;
pragma Warnings (On);

with PolyORB.Tasking.Mutexes;
with PolyORB.Any;

package PolyORB.DSA_P.Storages.DFS is

   use PolyORB.Tasking.Mutexes;

   package SIO renames Ada.Streams.Stream_IO;

   package SGL renames System.Global_Locks;

   package OS renames GNAT.OS_Lib;

   ----------------------
   -- DFS_Manager_Type --
   ----------------------

   --  Manage coherence of a shared passive variable.

   type DFS_Manager_Type is new Shared_Data_Manager_Type with private;
   type DFS_Manager_Access is access all DFS_Manager_Type'Class;

   --  DFS_Manager_Type type primitives

   overriding
   procedure Read
     (Self : access DFS_Manager_Type;
      Var  : SDT.Any_Container_Ptr);

   overriding
   procedure Write
     (Self : access DFS_Manager_Type;
      Var  : SDT.Any_Container_Ptr);

   overriding
   procedure Lock   (Self : access DFS_Manager_Type);

   overriding
   procedure Unlock (Self : access DFS_Manager_Type);

   overriding
   function Create
     (Manager_Factory : access DFS_Manager_Type;
      Full_Name       : String) return Shared_Data_Manager_RACW;

   procedure Register_Passive_Package
     (Pkg_Name : String;
      Is_Owner : Boolean;
      Location : String);
   --  Register a DFS manager factory for package Pkg_name

private

   type DFS_Manager_Type is new Shared_Data_Manager_Type with record
      Data  : PolyORB.Any.Any;
      Name  : OS.String_Access;
      File  : SIO.File_Type;
      Lock  : SGL.Lock_Type;
      Mutex : Mutex_Access;
      Count : Natural;
      Dir   : OS.String_Access;
      Prev  : DFS_Manager_Access;
      Next  : DFS_Manager_Access;
      Self  : DFS_Manager_Access;
   end record;

end PolyORB.DSA_P.Storages.DFS;
