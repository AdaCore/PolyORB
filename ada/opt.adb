------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;
with Gnatvsn; use Gnatvsn;
with Fname;   use Fname;
with System;  use System;
with Tree_IO; use Tree_IO;

package body Opt is

   Tree_Version_String : String (Gnat_Version_String'Range);
   --  Used to store the compiler version string read from a tree file to
   --  check if it is the same as stored in the version ctring in Gnatvsn.
   --  Therefore its length is taken directly from the version string in
   --  Gnatvsn. If the length of the version string stored in the three is
   --  different, then versions are for sure different.

   ------------------
   -- Check_Ada_95 --
   ------------------

   function Check_Ada_95 (File_Name : File_Name_Type) return Boolean is
      Save_Ada_83_Mode : constant Boolean := Ada_83;

   begin
      --  If internal unit, set Ada 95 mode.

      if Is_Internal_File_Name
           (Fname => File_Name,
            Renamings_Included => True)
      then
         Ada_83 := False;
         Ada_95 := True;

      --  Otherwise Initialize Ada_83 mode from compiler switches

      else
         Ada_83 := Ada_83_Switch;
         Ada_95 := not Ada_83;
      end if;

      return Save_Ada_83_Mode;

   end Check_Ada_95;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      Tree_Version_String_Len : Nat;

   begin
      Tree_Read_Bool (Brief_Output);
      Tree_Read_Bool (GNAT_Mode);
      Tree_Read_Char (Identifier_Character_Set);
      Tree_Read_Int  (Maximum_File_Name_Length);
      Tree_Read_Data (Suppress_Options'Address,
                      Suppress_Record'Object_Size / Storage_Unit);
      Tree_Read_Bool (Verbose_Mode);
      Tree_Read_Data (Warning_Mode'Address,
                      Warning_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Ada_83_Switch);
      Tree_Read_Bool (All_Errors_Mode);
      Tree_Read_Bool (Assertions_Enabled);
      Tree_Read_Bool (Full_List);

      --  Read and check version string

      Tree_Read_Int (Tree_Version_String_Len);

      if Tree_Version_String_Len = Tree_Version_String'Length then
         Tree_Read_Data
           (Tree_Version_String'Address, Tree_Version_String'Length);
      end if;

      if Tree_Version_String_Len /= Tree_Version_String'Length
        or else Tree_Version_String /= Gnat_Version_String
      then
         Raise_Exception
           (Program_Error'Identity,
            "Inconsistent versions of GNAT and ASIS, contact ACT");
      end if;

      Tree_Read_Data (Distribution_Stub_Mode'Address,
                      Distribution_Stub_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Immediate_Errors);
      Tree_Read_Bool (Inline_Active);
      Tree_Read_Bool (Inline_All);
      Tree_Read_Bool (Inline_Processing_Required);
      Tree_Read_Bool (List_Units);
      Tree_Read_Bool (No_Run_Time);
      Tree_Read_Data (Operating_Mode'Address,
                      Operating_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Software_Overflow_Checking);
      Tree_Read_Bool (Try_Semantics);
      Tree_Read_Data (Wide_Character_Encoding_Method'Address,
                      WC_Encoding_Method'Object_Size / Storage_Unit);
      Tree_Read_Bool (Upper_Half_Encoding);
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Tree_Write_Bool (Brief_Output);
      Tree_Write_Bool (GNAT_Mode);
      Tree_Write_Char (Identifier_Character_Set);
      Tree_Write_Int  (Maximum_File_Name_Length);
      Tree_Write_Data (Suppress_Options'Address,
                       Suppress_Record'Object_Size / Storage_Unit);
      Tree_Write_Bool (Verbose_Mode);
      Tree_Write_Data (Warning_Mode'Address,
                       Warning_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Ada_83_Switch);
      Tree_Write_Bool (All_Errors_Mode);
      Tree_Write_Bool (Assertions_Enabled);
      Tree_Write_Bool (Full_List);
      Tree_Write_Int  (Int (Gnat_Version_String'Length));
      Tree_Write_Data (Gnat_Version_String'Address,
                       Gnat_Version_String'Length);
      Tree_Write_Data (Distribution_Stub_Mode'Address,
                       Distribution_Stub_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Immediate_Errors);
      Tree_Write_Bool (Inline_Active);
      Tree_Write_Bool (Inline_All);
      Tree_Write_Bool (Inline_Processing_Required);
      Tree_Write_Bool (List_Units);
      Tree_Write_Bool (No_Run_Time);
      Tree_Write_Data (Operating_Mode'Address,
                       Operating_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Software_Overflow_Checking);
      Tree_Write_Bool (Try_Semantics);
      Tree_Write_Data (Wide_Character_Encoding_Method'Address,
                       WC_Encoding_Method'Object_Size / Storage_Unit);
      Tree_Write_Bool (Upper_Half_Encoding);
   end Tree_Write;

end Opt;
