------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . D S A _ P . S T O R A G E S               --
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

with System.DSA_Types;

package PolyORB.DSA_P.Storages is

   pragma Remote_Types;

   package SDT renames System.DSA_Types;

   -------------------------
   -- Shared_Data_Manager --
   -------------------------

   type Shared_Data_Manager_Type is abstract tagged limited private;
   type Shared_Data_Manager_RACW is
     access all Shared_Data_Manager_Type'Class;
   pragma Asynchronous (Shared_Data_Manager_RACW);

   --  Primitives of Shared_Data_Manager_Type

   procedure Read
     (Self : access Shared_Data_Manager_Type;
      Var  : in out SDT.Any_Container_Ptr) is abstract;
   --  Shared passive variable access routine. Each reference to the
   --  shared variable, V, is preceded by a call to the corresponding
   --  Read procedure, which either leaves the initial value unchanged
   --  if the storage does not exist, or reads the current value from
   --  the shared storage.

   procedure Write
     (Self : access Shared_Data_Manager_Type;
      Var  : in out SDT.Any_Container_Ptr) is abstract;
   --  Shared passive variable assignement routine. Each assignment to
   --  the shared variable, V, is followed by a call to the corresponding
   --  Write procedure, which writes the new value to the shared storage.

   procedure Lock (Self : access Shared_Data_Manager_Type) is abstract;
   --  Used for shared protected ojects, it ensures that others partitions
   --  can't obtain any access to the shared variable while Unlock procedure
   --  haven't been called by local partition.

   procedure Unlock (Self : access Shared_Data_Manager_Type) is abstract;
   --  Used for shared protected ojects, it finalizes the use of the shared
   --  variable by the local partiton.

   function Create
     (Manager_Factory : access Shared_Data_Manager_Type;
      Full_Name       : String)
      return Shared_Data_Manager_RACW
      is abstract;
   --  Create a new manager type according to given factory type.

   --  General services

   procedure Lookup_Variable
     (Var_Name    : String;
      Var_Data    : out Shared_Data_Manager_RACW);
   --  Find a shared data manager Var_Data in hash table, identified by its
   --  storage name Var_Name.

   procedure Lookup_Package
     (Pkg_Name : String;
      Pkg_Data : out Shared_Data_Manager_RACW);
   --  Find a package factory in hash table, identified by its name Pkg_Name.

   procedure Register_Factory
     (Factory_Name : String;
      Factory_Data : Shared_Data_Manager_RACW);
   --  Register a factory corresponding to chosen storage location.

private

   type Shared_Data_Manager_Type is abstract tagged limited null record;

end PolyORB.DSA_P.Storages;
