------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                    S Y S T E M . G A R L I C . D F S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

with System.Global_Locks;
with System.Garlic.Storages;
pragma Elaborate (System.Garlic.Storages);

package System.Garlic.Dfs is

   package SIO renames Ada.Streams.Stream_IO;

   package SGS renames System.Garlic.Storages;
   package SGL renames System.Global_Locks;

   package OS renames GNAT.OS_Lib;

   type DFS_Data_Type is new SGS.Shared_Data_Type with private;

   --  Management subprograms

   procedure Create_Storage
     (Master   : in out DFS_Data_Type;
      Location : in  String;
      Storage  : out SGS.Shared_Data_Access);

   procedure Create_Package
     (Storage  : in  DFS_Data_Type;
      Pkg_Name : in  String;
      Pkg_Data : out SGS.Shared_Data_Access);

   procedure Create_Variable
     (Pkg_Data : in  DFS_Data_Type;
      Var_Name : in  String;
      Var_Data : out SGS.Shared_Data_Access);

   procedure Initialize;

   procedure Enter_Variable (Var_Data : in out DFS_Data_Type);
   procedure Leave_Variable (Var_Data : in out DFS_Data_Type);

   procedure Set_Access_Mode
     (Var_Data : in out DFS_Data_Type;
      Var_Mode : in  SGS.Access_Mode;
      Failure  : out Boolean);

   procedure Read
     (Data : in out DFS_Data_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Data : in out DFS_Data_Type;
      Item : in Ada.Streams.Stream_Element_Array);

private

   type DFS_Data_Access is access DFS_Data_Type;

   type DFS_Data_Type is
     new SGS.Shared_Data_Type with
      record
         Data_Name  : OS.String_Access;
         Data_File  : SIO.File_Type;
         Lock_Count : Natural;
         Lock       : SGL.Lock_Type;
         Previous   : DFS_Data_Access;
         Next       : DFS_Data_Access;
         Self       : DFS_Data_Access;
      end record;

end System.Garlic.Dfs;
