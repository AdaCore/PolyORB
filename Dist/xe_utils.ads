------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;
with ALI;
with GNAT.OS_Lib;
with Types;

package XE_Utils is

   Obj_Suffix    : Types.File_Name_Type;
   Exe_Suffix    : Types.File_Name_Type;

   ALI_Suffix    : Types.File_Name_Type;
   ADS_Suffix    : Types.File_Name_Type;
   ADB_Suffix    : Types.File_Name_Type;

   Spec_Suffix   : Types.File_Name_Type;
   Body_Suffix   : Types.File_Name_Type;

   DSA_Dir       : Types.File_Name_Type;
   Caller_Dir    : Types.File_Name_Type;
   Receiver_Dir  : Types.File_Name_Type;

   I_Current_Dir : GNAT.OS_Lib.String_Access;
   I_Caller_Dir  : GNAT.OS_Lib.String_Access;

   L_Current_Dir : GNAT.OS_Lib.String_Access;
   L_Caller_Dir  : GNAT.OS_Lib.String_Access;

   GNATLib_Compile_Flag  : GNAT.OS_Lib.String_Access;

   PWD_Id                : Types.File_Name_Type;

   Build_Stamp_File      : Types.File_Name_Type;
   Elaboration_File      : Types.File_Name_Type;
   Elaboration_Name      : Types.File_Name_Type;
   Partition_Main_File   : Types.File_Name_Type;
   Partition_Main_Name   : Types.File_Name_Type;

   A_GARLIC_Dir          : GNAT.OS_Lib.String_Access;
   I_GARLIC_Dir          : GNAT.OS_Lib.String_Access;
   L_GARLIC_Dir          : GNAT.OS_Lib.String_Access;

   -- Exceptions --

   Fatal_Error         : exception;   --  Operating system error
   Scanning_Error      : exception;   --  Error during scanning
   Parsing_Error       : exception;   --  Error during parsing
   Partitioning_Error  : exception;   --  Error during partitionning
   Usage_Error         : exception;   --  Command line error
   Not_Yet_Implemented : exception;

   function "&" (N1, N2 : Types.File_Name_Type)
                 return Types.File_Name_Type;
   function "&" (N1 : Types.File_Name_Type; N2 : String)
                 return Types.File_Name_Type;

   function Dir
     (D1 : Types.File_Name_Type;
      D2 : Types.File_Name_Type := Types.No_File;
      D3 : Types.File_Name_Type := Types.No_File;
      D4 : Types.File_Name_Type := Types.No_File)
      return Types.File_Name_Type;
   --  Concatenate several names and insert a directory separator between them.

   procedure Change_Dir (To : in Types.File_Name_Type);

   procedure Compile_RCI_Caller
     (Source, Object : in Types.File_Name_Type);
   --  Compile the caller stubs (-gnatzC).

   procedure Compile_RCI_Receiver
     (Source, Object : in Types.File_Name_Type);
   --  Compile the receiver stubs (-gnatzR).

   procedure Copy_With_File_Stamp
     (Source, Target : in Types.File_Name_Type;
      Maybe_Symbolic : in Boolean := False);
   --  Copy source into target and preserves file stamps.

   procedure Create
     (File : in out GNAT.OS_Lib.File_Descriptor;
      Name : in Types.File_Name_Type;
      Exec : in Boolean := False);

   procedure Create_Dir
     (To : in Types.File_Name_Type);

   procedure Delete
     (File : in Types.File_Name_Type);

   procedure Execute
     (Prog : in GNAT.OS_Lib.String_Access;
      Args : in GNAT.OS_Lib.Argument_List);
   --  Execute the command and raise Fatal Error if not successful

   procedure Execute_Bind
     (Lib  : in Types.File_Name_Type;
      Args : in GNAT.OS_Lib.Argument_List);
   --  Execute gnatbind and add gnatdist flags

   procedure Execute_Gcc
     (File   : in Types.File_Name_Type;
      Object : in Types.File_Name_Type;
      Args   : in GNAT.OS_Lib.Argument_List);
   --  Execute gcc and add gnatdist compilation flags

   procedure Execute_Link
     (Lib  : in Types.File_Name_Type;
      Exec : in Types.File_Name_Type;
      Args : in GNAT.OS_Lib.Argument_List);
   --  Execute gnatlink and add gnatdist flags

   function Find_Source (U : Types.Name_Id) return Types.File_Name_Type;
   --  Retrieve main source file of unit U.

   procedure Free is
     new Unchecked_Deallocation (String, GNAT.OS_Lib.String_Access);

   function  GNAT_Style (N : Types.Name_Id) return Types.Name_Id;
   --  Return a string that approx. follows GNAT style.

   procedure Initialize;

   function Is_Directory    (File : Types.File_Name_Type) return Boolean;
   function Is_Regular_File (File : Types.File_Name_Type) return Boolean;
   function Is_Relative_Dir (File : Types.File_Name_Type) return Boolean;

   procedure Message
     (S1 : in String        := "";
      S2 : in Types.Name_Id := Types.No_Name;
      S3 : in String        := "";
      S4 : in Types.Name_Id := Types.No_Name;
      S5 : in String        := "");

   function Stamp (F : Types.File_Name_Type) return String;

   function Str_To_Id (S : String) return Types.Name_Id;
   --  Set into name table and return id.

   function Strlen (Name : in Types.Name_Id) return Natural;

   procedure To_Lower (S : in out String);
   procedure To_Lower (N : in out Types.Name_Id);

   function U_To_N
     (U : in Types.Unit_Name_Type)
      return Types.Name_Id;
   --  Strip %[bs] from U.

   procedure Unlink_File (File : in Types.File_Name_Type);

   procedure Write_Compile_Command (Name : in Types.File_Name_Type);
   --  Generates on standard-out the command needed to compile
   --  a sub-tree from a given package.

   procedure Write_Eol
     (File   : in GNAT.OS_Lib.File_Descriptor;
      Stdout : in Boolean := False);

   procedure Write_File_Stamp
     (File : in Types.File_Name_Type);

   procedure Write_Missing_File
     (File  : in Types.File_Name_Type);

   procedure Write_Name
     (File   : in GNAT.OS_Lib.File_Descriptor;
      Name   : in Types.File_Name_Type;
      Stdout : in Boolean := False);

   procedure Write_Stamp_Comparison
     (Newer, Older   : in Types.File_Name_Type);

   procedure Write_Str
     (File   : in GNAT.OS_Lib.File_Descriptor;
      Line   : in String;
      Stdout : in Boolean := False);

   procedure Write_Unit_Name
     (U : in Types.Unit_Name_Type);

end XE_Utils;
