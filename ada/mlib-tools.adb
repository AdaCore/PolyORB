------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           M L I B . T O O L S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 1999, Ada Core Technologies, Inc.             --
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

--  This package provides an easy way of calling various tools such as gcc,
--  gnatbind, ar, etc...

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with MLib.Files;
with MLib.Target;
with Namet; use Namet;
with Osint; use Osint;

package body MLib.Tools is

   use Ada;
   use GNAT;
   use Target;

   Gcc_Name      : constant String := "gcc";
   Gcc_Exec      : OS_Lib.String_Access;

   Gnatbind_Name : constant String := "gnatbind";
   Gnatbind_Exec : OS_Lib.String_Access;

   Ar_Name       : constant String := "ar";
   Ar_Exec       : OS_Lib.String_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation (String, OS_Lib.String_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (OS_Lib.Argument_List,
                                     OS_Lib.Argument_List_Access);

   -------------------
   -- Lib_Directory --
   -------------------

   function Lib_Directory return String;
   --  Return the directory containing libgnat

   function Lib_Directory return String is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Libgnat);
      Get_Name_String (Find_File (Name_Enter, Library));

      --  remove libgnat.a

      return Name_Buffer (1 .. Name_Len - Libgnat'Length);
   end Lib_Directory;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Filename : in String) is
      File   : constant String := Filename & ASCII.Nul;
      Sucess : Boolean;
   begin
      OS_Lib.Delete_File (File'Address, Sucess);
   end Delete_File;

   ---------
   -- Gcc --
   ---------

   procedure Gcc
     (Output_File : in String;
      Objects     : in Argument_List;
      Options     : in Argument_List;
      Base_File   : in String := "";
      Build_Lib   : in Boolean := False)
   is
      Arguments : OS_Lib.Argument_List
        (1 .. 7 + Objects'Length + Options'Length);
      A         : Natural := 0;

      Success   : Boolean;
      C_Opt     : OS_Lib.String_Access := new String' ("-c");
      Out_Opt   : OS_Lib.String_Access := new String' ("-o");
      Out_V     : OS_Lib.String_Access := new String' (Output_File);
      Lib_Dir   : OS_Lib.String_Access := new String' ("-L" & Lib_Directory);
      Lib_Opt   : OS_Lib.String_Access := new String' (Target.Dynamic_Option);
      Bas_Opt   : OS_Lib.String_Access :=
        new String '(Target.Base_Option & Base_File);
      Lang_Opt1 : OS_Lib.String_Access := new String' ("-x");
      Lang_Opt2 : OS_Lib.String_Access := new String' ("ada");

   begin
      A := A + 1;

      if Build_Lib then
         Arguments (A) := Lib_Opt;
         A := A + 1;
         Arguments (A .. A + 1) := (Out_Opt, Out_V);
         A := A + 1;

      else
         Arguments (A) := C_Opt;

         if not Files.Is_Ada (Objects (Objects'First).all) then
            A := A + 1;
            Arguments (A) := Lang_Opt1;
            A := A + 1;
            Arguments (A) := Lang_Opt2;
         end if;
      end if;

      if Build_Lib then
         A := A + 1;
         Arguments (A) := Lib_Dir;
      end if;

      if Base_File /= "" then
         A := A + 1;
         Arguments (A) := Bas_Opt;
      end if;

      if Build_Lib then
         A := A + 1;
         Arguments (A .. A + Options'Length - 1) := Options;
         A := A + Options'Length - 1;
      else
         declare
            Largs : Argument_List (Options'Range);
            L     : Natural := Largs'First - 1;
         begin
            for K in Options'Range loop
               if Options (K).all /= "" and then
                 Options (K) (1 .. 2) /= "-l"
               then
                  L := L + 1;
                  Largs (L) := Options (K);
               end if;
            end loop;

            A := A + 1;
            Arguments (A .. A + L - Largs'First) := Largs (Largs'First .. L);
            A := A + L - Largs'First;
         end;
      end if;

      A := A + 1;
      Arguments (A .. A + Objects'Length - 1) := Objects;
      A := A + Objects'Length - 1;

      if not Quiet then
         Text_IO.Put ("gcc");

         for J in 1 .. A loop
            Text_IO.Put (" " & Arguments (J).all);
         end loop;

         Text_IO.New_Line;
      end if;

      OS_Lib.Spawn (Gcc_Exec.all, Arguments (1 .. A), Success);

      if not Success then

         if Continue_On_Errors then
            Report_On_Error (Gcc_Name & " execution error.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gcc_Name & " execution error.");
         end if;

      end if;

      Free (Lib_Opt);
      Free (Out_Opt);
      Free (Out_V);
      Free (Bas_Opt);
      Free (Lib_Dir);
      Free (Lang_Opt1);
      Free (Lang_Opt2);
   end Gcc;

   --------------
   -- Gnatbind --
   --------------

   procedure Gnatbind (Alis : in Argument_List) is
      Arguments : OS_Lib.Argument_List_Access;
      Success   : Boolean;

      No_Main_Opt : OS_Lib.String_Access := new String'("-n");
      Generate_C  : OS_Lib.String_Access := new String'("-C");

   begin
      Arguments := new OS_Lib.Argument_List (1 .. 2 + Alis'Length);
      Arguments (1) := No_Main_Opt;
      Arguments (2) := Generate_C;
      Arguments (3 .. Arguments'Last) := Alis;

      if not Quiet then
         Text_IO.Put ("gnatbind");

         for I in Arguments'Range loop
            Text_IO.Put (" " & Arguments (I).all);
         end loop;

         Text_IO.New_Line;
      end if;

      OS_Lib.Spawn (Gnatbind_Exec.all, Arguments.all, Success);

      if not Success then

         if Continue_On_Errors then
            Report_On_Error (Gnatbind_Name & " execution error.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gnatbind_Name & " execution error.");
         end if;

      end if;

      Free (No_Main_Opt);
      Free (Arguments);
   end Gnatbind;

   --------
   -- Ar --
   --------

   procedure Ar (Output_File : in String; Objects : in Argument_List) is
      Create_Add_Opt : OS_Lib.String_Access := new String' ("cr");

      Arguments : OS_Lib.Argument_List (1 .. 2 + Objects'Length);
      Success   : Boolean;

   begin
      Arguments (1) := Create_Add_Opt;
      Arguments (2) := new String'(Files.Ext_To (Output_File, Archive_Ext));
      Arguments (3 .. Arguments'Last) := Objects;

      if not Quiet then
         Text_IO.Put (Ar_Name);

         for J in Arguments'Range loop
            Text_IO.Put (" " & Arguments (J).all);
         end loop;

         Text_IO.New_Line;
      end if;

      Delete_File (Output_File);

      OS_Lib.Spawn (Ar_Exec.all, Arguments, Success);
      if not Success then

         if Continue_On_Errors then
            Report_On_Error (Ar_Name & " execution error.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Ar_Name & " execution error.");
         end if;
      end if;

      Free (Create_Add_Opt);
   end Ar;

   ------------
   -- Locate --
   ------------

   procedure Locate is
      use type OS_Lib.String_Access;
   begin
      --  gcc

      Gcc_Exec := OS_Lib.Locate_Exec_On_Path (Gcc_Name);

      if Gcc_Exec = null then

         if Continue_On_Errors then
            Report_On_Error (Gcc_Name & " not found in path.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gcc_Name & " not found in path");
         end if;

      elsif Verbose then
         Text_IO.Put_Line ("found " & Gcc_Exec.all);
      end if;

      --  gnatbind

      Gnatbind_Exec := OS_Lib.Locate_Exec_On_Path (Gnatbind_Name);

      if Gnatbind_Exec = null then

         if Continue_On_Errors then
            Report_On_Error (Gnatbind_Name & " not found in path.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gnatbind_Name & " not found in path");
         end if;

      elsif Verbose then
         Text_IO.Put_Line ("found " & Gnatbind_Exec.all);
      end if;

      --  ar

      Ar_Exec := OS_Lib.Locate_Exec_On_Path (Ar_Name);

      if Ar_Exec = null then

         if Continue_On_Errors then
            Report_On_Error (Ar_Name & " not found in path.");
         else
            Exceptions.Raise_Exception
              (Tools_Error'Identity,
               Ar_Name & " not found in path");
         end if;

      elsif Verbose then
         Text_IO.Put_Line ("found " & Ar_Exec.all);
      end if;
   end Locate;

end MLib.Tools;
