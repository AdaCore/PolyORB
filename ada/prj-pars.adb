------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R S                             --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Errout;                     use Errout;
with Output;                     use Output;
with Prj.Com;                    use Prj.Com;
with Prj.Nmsc;
with Prj.Part;
with Prj.Proc;
with Prj.Tree;                   use Prj.Tree;
with Types;                      use Types;

package body Prj.Pars is

   procedure Check (Project : in out Project_Id);
   --  Set all projects to not checked, then call Recursive_Check for
   --  the main project Project.
   --  Project is set to No_Project if errors occurred.

   procedure Recursive_Check (Project : Project_Id);
   --  If Project is marked as not checked, mark it as checked,
   --  call Check_Naming_Scheme for the project, then call itself
   --  for a possible modified project and all the imported projects
   --  of Project.

   -----------
   -- Check --
   -----------

   procedure Check (Project : in out Project_Id) is
   begin
      --  Make sure that all projects are marked as not checked.

      for Index in 1 .. Projects.Last loop
         Projects.Table (Index).Checked := False;
      end loop;

      Recursive_Check (Project);

      if Errout.Errors_Detected > 0 then
         Project := No_Project;
      end if;

   end Check;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project           : out Project_Id;
      Project_File_Name : String)
   is
      Project_Tree      : Project_Node_Id := Empty_Node;
      The_Project       : Project_Id      := No_Project;

   begin

      --  Parse the main project file into a tree

      Prj.Part.Parse
        (Project           => Project_Tree,
         Project_File_Name => Project_File_Name);

      --  If there were no error, process the tree

      if Project_Tree /= Empty_Node then

         Errout.Initialize;

         Prj.Proc.Process
           (Project           => The_Project,
            From_Project_Node => Project_Tree);

         --  If there were no error, check the projects

         if The_Project /= No_Project then
            Check (The_Project);
         end if;

      end if;

      begin
         Errout.Finalize;
      exception
         --  Any exception can be ignored
         when others =>
            null;
      end;

      Project := The_Project;

   exception
      when X : others =>

         --  Internal error
         Write_Line (Exception_Information (X));
         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised, while processing project file");
         Project := No_Project;
   end Parse;

   procedure Recursive_Check (Project : Project_Id) is
      Data : Project_Data;
      Imported_Project_List : Project_List := Empty_Project_List;
   begin
      --  Do nothing if Project is No_Project, or Project has already
      --  been marked as checked.

      if Project /= No_Project
        and then not Projects.Table (Project).Checked
      then
         --  Mark Project as checked

         Projects.Table (Project).Checked := True;

         Prj.Nmsc.Check_Naming_Scheme (Project);
         Data := Projects.Table (Project);

         --  Call itself for a possible modified project.
         --  (if there is no modified project, then nothing happens).

         Recursive_Check (Data.Modifies);

         --  Call itself for all imported projects

         Imported_Project_List := Data.Imported_Projects;
         while Imported_Project_List /= Empty_Project_List loop
            Recursive_Check
              (Project_Lists.Table (Imported_Project_List).Project);
            Imported_Project_List :=
              Project_Lists.Table (Imported_Project_List).Next;
         end loop;

      end if;

   end Recursive_Check;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (To : in Verbosity) is
   begin
      Current_Verbosity := To;
   end Set_Verbosity;

end Prj.Pars;
