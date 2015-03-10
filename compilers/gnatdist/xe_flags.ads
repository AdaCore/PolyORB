------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             X E _ F L A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2015, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

--  This package contains the flags available for GNATDIST as well as
--  those used by GNATDIST and passed to GNATMAKE and GNATLS.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with XE_Utils; use XE_Utils;

package XE_Flags is

   Quiet_Mode           : Boolean := False;
   Verbose_Mode         : Boolean := False;
   Debug_Mode           : Boolean := False;
   Check_Readonly_Files : Boolean := False;
   Keep_Going           : Boolean := False;
   Resolve_Links        : Boolean := False;

   Keep_Tmp_Files       : Boolean := False;
   --  Do not remove temporary files

   User_Provided_S_RPC  : Boolean := False;
   --  User provided his own version of s-rpc.adb, overriding the one from the
   --  PCS.

   Use_GPRBuild         : Boolean := False;
   --  Use GPRBuild instead of gnatmake

   Display_Compilation_Progress : Boolean := False;

   Readonly_Flag       : constant Unbounded_String := +"-a";
   Bind_Only_Flag      : constant Unbounded_String := +"-b";
   Compile_Only_Flag   : constant Unbounded_String := +"-c";
   Object_Dir_Flag     : constant Unbounded_String := +"-D";
   Progress_Flag       : constant Unbounded_String := +"-d";
   Keep_Going_Flag     : constant Unbounded_String := +"-k";
   Link_Only_Flag      : constant Unbounded_String := +"-l";
   Output_Flag         : constant Unbounded_String := +"-o";
   Project_File_Flag   : constant Unbounded_String := +"-P";
   Quiet_Flag          : constant Unbounded_String := +"-q";
   Verbose_Flag        : constant Unbounded_String := +"-v";
   GLADE_List_Flag     : constant Unbounded_String := +"-V";
   External_Units_Flag : constant Unbounded_String := +"-x";
   No_Main_Proc_Flag   : constant Unbounded_String := +"-z";
   Semantic_Only_Flag  : constant Unbounded_String := +"-gnatc";
   Skel_Flag           : constant Unbounded_String := +"-gnatzr";
   Stub_Flag           : constant Unbounded_String := +"-gnatzc";
   Comp_Args_Flag      : constant Unbounded_String := +"-cargs";
   Bind_Args_Flag      : constant Unbounded_String := +"-bargs";
   Link_Args_Flag      : constant Unbounded_String := +"-largs";
   Make_Args_Flag      : constant Unbounded_String := +"-margs";

   Project_File_Name : Unbounded_String;

   Make_Switches, List_Switches, Source_Directories : Argument_Vec;

end XE_Flags;
