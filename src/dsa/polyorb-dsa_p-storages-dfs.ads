------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . S T O R A G E S . D F S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008, Free Software Foundation, Inc.               --
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

pragma Ada_2005;

with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

with System.Global_Locks;

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
