------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements services for Project-aware tools, related
--  to the environment (gnat.adc, ADA_INCLUDE_PATH, ADA_OBJECTS_PATH)

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Prj.Env is

   procedure Initialize;
   --  Put Standard_Naming_Data into the Namings table.
   --  Called by Prj.Initialize.

   procedure Print_Sources;
   --  Output the list of sources, after Project files have been scanned.

   procedure Create_Gnat_Adc (Project : Project_Id);
   --  If there needs to have SFN pragmas, either for non standard
   --  naming schemes or for individual units,
   --  create a new gnat.adc file in the current directory,
   --  while saving a possibly existing gnat.adc under another name.

   procedure Restore_Gnat_Adc;
   --  Delete the dynamically created gnat.adc (created by
   --  Create_Gnat_Adc), and restore the one that existed before, if any.

   function Ada_Include_Path (Project : Project_Id) return String_Access;
   --  Get the ADA_INCLUDE_PATH of a Project file.
   --  For the first call, compute it and chache it.

   function Ada_Objects_Path (Project : Project_Id) return String_Access;
   --  Get the ADA_OBJECTS_PATH of a Project file.
   --  For the first call, compute it and chache it.

   function Path_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return    String;
   --  Returns the Path of a library unit.

   function File_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return    String;
   --  Returns the file name of a library unit, in canonical case.
   --  Name may or may not have an extension (corresponding to
   --  the naming scheme of the project).
   --  If there is no body with this name, but there is a spec,
   --  the name of the spec is returned.
   --  If neither a body or a spec can be found, return an empty string.

   procedure Get_Reference
     (Source_File_Name : String;
      Project          : out Project_Id;
      Path             : out Name_Id);
   --  Returns the project of a source.

   generic
      with procedure Action (Path : String);
   procedure For_All_Source_Dirs (Project : Project_Id);
   --  Iterate through all the source directories of a project,
   --  including those of imported or modified projects.

   generic
      with procedure Action (Path : String);
   procedure For_All_Object_Dirs (Project : Project_Id);
   --  Iterate through all the object directories of a project,
   --  including those of imported or modified projects.

end Prj.Env;
