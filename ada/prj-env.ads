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
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

--  The following implements services for Project-aware tools, related
--  to the environment (gnat.adc, ADA_INCLUDE_PATH, ADA_OBJECTS_PATH)

package Prj.Env is

   procedure Set_Verbosity (To : Verbosity);
   --  Set the verbosity when using the services of this package.

   procedure Print_Sources;
   --  Output the list of sources, after Project files have been scanned.

   procedure Create_Gnat_Adc (Ref : Reference);
   --  Create a gnat.adc file in the current directory,
   --  while saving a possibly existing gnat.adc under another name.

   procedure Restore_Gnat_Adc;
   --  Delete the dynamically created gnat.adc (created by
   --  Create_Gnat_Adc), and restore the one that existed before, if any.

   function Ada_Include_Path (Ref : Reference) return String;
   --  Compute (and cache) the ADA_INCLUDE_PATH of a Project file.

   function Ada_Objects_Path (Ref : Reference) return String;
   --  Compute (and cache) the ADA_OBJECTS_PATH of a Project file.

   function Path_Name_Of_Library_Unit_Body
     (Name : String;
      Ref  : Reference)
      return String;
   --  Returns the Path of la library unit.

   function File_Name_Of_Library_Unit_Body
     (Name : String;
      Ref  : Reference)
      return String;
   --  Returns the file name of a library unit.

   procedure Get_Reference
     (Source_File_Name : String;
      Ref              : in out Reference;
      Path             : in out String_Access);
   --  Returns the project of a source.

   generic
      with procedure Action (Path : String);
   procedure For_All_Source_Dirs (Ref : Reference);
   --  Iterate through all the source directories of a project,
   --  including those of imported projects.

   generic
      with procedure Action (Path : String);
   procedure For_All_Object_Dirs (Ref : Reference);
   --  Iterate through all the object directories of a project,
   --  including those of imported projects.

end Prj.Env;
