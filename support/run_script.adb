------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           R U N _ S C R I P T                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Wrapper to run shell scripts under Windows.

--  $Id$

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Registry;
with GNAT.Directory_Operations;

procedure Run_Script is

   use Ada.Command_Line;
   use Ada.Text_IO;
   use GNAT.OS_Lib;
   use GNAT.Directory_Operations;

   function All_Arguments return Argument_List;
   --  Return an argument list corresponding to the command line.
   --  All backslashes are changed to slashes.

   function MinGW_Resolve (Filename : String) return String;
   --  Strip name and resolve it, for MinGW

   function Cygwin_Resolve (Filename : String) return String;
   --  Resolve a name relative to Cygwin mount points to the
   --  corresponding native path.

   -------------------
   -- All_Arguments --
   -------------------

   function All_Arguments return Argument_List is
      Result : Argument_List (1 .. Argument_Count);
   begin
      for J in Result'Range loop
         Result (J) := new String'(Argument (J));
         for K in Result (J)'Range loop
            if Result (J) (K) = '\' then
               Result (J) (K) := '/';
            end if;
         end loop;
      end loop;
      return Result;
   end All_Arguments;

   -------------------
   -- MinGW_Resolve --
   -------------------

   function MinGW_Resolve (Filename : String) return String is
      Split : Integer := Filename'Last;
   begin
      while Split > Filename'First and then Filename (Split) /= '/' loop
         Split := Split - 1;
      end loop;

      return Filename (Split + 1 .. Filename'Last);
   end MinGW_Resolve;

   --------------------
   -- Cygwin_Resolve --
   --------------------

   function Cygwin_Resolve (Filename : String) return String is

      use GNAT.Registry;

      Mounts_Keys_Base : constant String
        := "SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\";

      Split : Integer := Filename'Last;
   begin
      loop
         while Split > Filename'First and then Filename (Split) /= '/' loop
            Split := Split - 1;
         end loop;
         if Split > Filename'First then
            Split := Split - 1;
         end if;
         exit when Split < Filename'First;
         begin
            declare
               K : constant HKEY := Open_Key (HKEY_LOCAL_MACHINE,
                 Mounts_Keys_Base & Filename (Filename'First .. Split));
               Native_Path : constant String := Query_Value (K, "native");
            begin
               --  Key found!
               return Native_Path & Filename (Split + 1 .. Filename'Last);
            end;
         exception
            when Registry_Error =>
               null;
         end;
      end loop;
      return Filename;
   end Cygwin_Resolve;

   Self : constant String := Format_Pathname (Command_Name, UNIX);
   Script_Last : Integer;
   Script : Ada.Text_IO.File_Type;

   Line : String (1 .. 256);
   First, Last : Integer;

begin
   if Self'Length >= 4 and then Self (Self'Last - 3 .. Self'Last) = ".exe" then
      Script_Last := Self'Last - 4;
   else
      Script_Last := Self'Last;
   end if;
   Open (Script, In_file, Self (Self'First .. Script_Last));
   Get_Line (Script, Line, Last);
   if Last >= 2 and then Line (1 .. 2) = "#!" then
      First := 3;
      while First < Last
        and then (Line (First) = ' ' or else Line (First) = ASCII.HT)
      loop
         First := First + 1;
      end loop;

      declare
         Interp_Command : constant String := Line (First .. Last);
         Args : constant Argument_List_Access
           := Argument_String_To_List (Interp_Command);

         New_Args : constant Argument_List
           := Args (Args'First + 1 .. Args'Last)
            & new String'(Self (Self'First .. Script_Last))
            & All_Arguments;

         Interp : String renames Args (Args'First).all;
         Interp_Path : String_Access;

      begin
         Interp_Path := Locate_Exec_On_Path (Interp);

         if Interp_Path = null then
            Interp_Path := Locate_Exec_On_Path (MinGW_Resolve (Interp));
         end if;

         if Interp_Path = null then
            Interp_Path := Locate_Exec_On_Path (Cygwin_Resolve (Interp));
         end if;

         if Interp_Path = null then
            Put_Line ("Interp = """ & Interp & """ not found ");
            Put_Line ("Tried:");
            Put_Line ("  normal resolv = " & Interp);
            Put_Line ("  for MinGW     = " & MinGW_Resolve (Interp));
            Put_Line ("  for Cygwin    = " & Cygwin_Resolve (Interp));
            OS_Exit (-1);
         end if;

         OS_Exit (Spawn (Interp_Path.all, New_Args));
      end;
   end if;

end Run_Script;
