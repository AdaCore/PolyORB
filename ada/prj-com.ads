------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . C O M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  The following package declares data types for GNAT project.
--  These data types are used in the bodies of the Prj hierarchy.

package Prj.Com is

   --  At one point, this package was private.
   --  It cannot be private, because it is used outside of
   --  the Prj hierarchy.

   type Spec_Or_Body is
     (Specification, Body_Part);

   type File_Name_Data is record
      Name : String_Access;
      Path : String_Access;
      Needs_Pragma : Boolean := False;
   end record;
   --  File and Path name of a spec or body.

   type File_Names_Data is array (Spec_Or_Body) of File_Name_Data;

   type Unit_Data;
   type Unit_List is access Unit_Data;
   type Unit_Data is record
      Name : String_Access;
      File_Names : File_Names_Data;
      Ref : Reference;
      Next : Unit_List;
   end record;
   --  File and Path names of a unit, with a reference to its
   --  GNAT Project File. All units are linked together in a
   --  list.

   First_Unit : Unit_List;
   Last_Unit : Unit_List;
   --  The list of all units of all GNAT Project Files.

   type Project_Data;
   type Project_Ref is access Project_Data;
   type Project_Data is record
      Project : Reference;
      Next : Project_Ref;
   end record;
   --  Used to build a list of all GNAT Project Files.

   First_Project : Project_Ref;
   Last_Project : Project_Ref;
   --  The list of all GNAT Project_Files.

end Prj.Com;
