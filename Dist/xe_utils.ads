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
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

with ALI;
with GNAT.OS_Lib;
with Opt;
with Types;

package XE_Utils is

   subtype Int                 is Types.Int;
   subtype Name_Id             is Types.Name_Id;
   subtype File_Name_Type      is Types.File_Name_Type;
   subtype Unit_Name_Type      is Types.Unit_Name_Type;
   subtype Source_Ptr          is Types.Source_Ptr;
   subtype Source_Buffer_Ptr   is Types.Source_Buffer_Ptr;
   subtype Text_Ptr            is Types.Text_Ptr;
   subtype Text_Buffer_Ptr     is Types.Text_Buffer_Ptr;
   subtype Time_Stamp_Type     is Types.Time_Stamp_Type;

   subtype File_Descriptor     is GNAT.Os_Lib.File_Descriptor;
   subtype String_Access       is GNAT.Os_Lib.String_Access;

   subtype ALI_Id              is ALI.ALI_Id;
   subtype Unit_Id             is ALI.Unit_Id;
   subtype Main_Program_Type   is ALI.Main_Program_Type;
   subtype Unit_Type           is ALI.Unit_Type;

   No_Name    : constant Name_Id           := Types.No_Name;
   No_File    : constant File_Name_Type    := Types.No_File;

   Standout   : constant File_Descriptor   := GNAT.Os_Lib.Standout;

   No_ALI_Id  : constant ALI_Id            := ALI.No_ALI_Id;
   None       : constant Main_Program_Type := ALI.None;
   No_Unit_Id : constant Unit_Id           := ALI.No_Unit_Id;

   Is_Body      : constant Unit_Type       := ALI.Is_Body;
   Is_Spec_Only : constant Unit_Type       := ALI.Is_Spec_Only;

   Check_Internal_Files : Boolean          := Opt.Check_Internal_Files;
   Force_Compilations   : Boolean          := Opt.Force_Compilations;

   package Unit  renames ALI.Unit;
   package ALIs  renames ALI.ALIs;
   package Withs renames ALI.Withs;

   Directory_Separator : constant Character := GNAT.Os_Lib.Directory_Separator;

   First_Source_Ptr : constant Source_Ptr   := Types.First_Source_Ptr;
   EOF              : constant Character    := Types.EOF;

   --  This package is intended to provide all the OS facilities

   Obj_Suffix   : File_Name_Type;
   ALI_Suffix   : File_Name_Type;
   ADS_Suffix   : File_Name_Type;
   ADB_Suffix   : File_Name_Type;

   Spec_Suffix  : File_Name_Type;
   Body_Suffix  : File_Name_Type;

   Com_Sep_Id   : File_Name_Type;
   Dir_Sep_Id   : File_Name_Type;
   Dot_Sep_Id   : File_Name_Type;

   DSA_Dir      : File_Name_Type;
   Caller_Dir   : File_Name_Type;
   Receiver_Dir : File_Name_Type;
   Parent_Dir   : File_Name_Type;
   Original_Dir : File_Name_Type;

   PWD_Id       : File_Name_Type;

   Build_Stamp_File      : File_Name_Type;
   Elaboration_File      : File_Name_Type;
   Elaboration_Name      : File_Name_Type;
   Partition_Main_File   : File_Name_Type;
   Partition_Main_Name   : File_Name_Type;

   Separator : Character renames Directory_Separator;

   -- Exceptions --

   Fatal_Error         : exception;   --  Operating system error
   Scanning_Error      : exception;   --  Error during scanning
   Parsing_Error       : exception;   --  Error during parsing
   Partitioning_Error  : exception;   --  Error during partitionning
   Usage_Error         : exception;   --  Command line error
   Not_Yet_Implemented : exception;

   function "&" (Prefix, Suffix : File_Name_Type) return File_Name_Type;

   function "+" (X, Y : Int) return Int renames Types."+";
   function "+" (X, Y : Text_Ptr) return Text_Ptr renames Types."+";
   function "-" (X, Y : Int) return Int renames Types."-";

   function "-" (X, Y : Text_Ptr) return Text_Ptr renames Types."-";

   function "<=" (X, Y : Int) return Boolean renames Types."<=";

   function "=" (X, Y : Int) return Boolean renames Types."=";
   function "=" (X, Y : Name_Id) return Boolean renames Types."=";

   function "=" (X, Y : Text_Ptr) return Boolean renames Types."=";
   function "=" (X, Y : Text_Buffer_Ptr) return Boolean renames Types."=";
   function "=" (X, Y : Source_Buffer_Ptr) return Boolean renames Types."=";

   function "=" (X, Y : Main_Program_Type) return Boolean renames Types."=";
   function "=" (X, Y : Unit_Type) return Boolean renames Types."=";
   function "=" (X, Y : ALI_Id) return Boolean renames Types."=";

   function ">" (X, Y : Int) return Boolean renames Types.">";
   function ">" (X, Y : Time_Stamp_Type) return Boolean renames Types.">";
   procedure Change_Dir (To : in File_Name_Type);

   procedure Close (FD : File_Descriptor) renames GNAT.Os_Lib.Close;

   procedure Compile_RCI_Caller
     (Source    : in File_Name_Type);
   --  Compile the caller stubs (-gnatzC).

   procedure Compile_RCI_Receiver
     (Source    : in File_Name_Type);
   --  Compile the receiver stubs (-gnatzR).

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False);
   --  Basically, this procedure copies source into target and
   --  preserves file stamps.

   procedure Create
     (File : in out File_Descriptor;
      Name : in File_Name_Type;
      Exec : in Boolean := False);

   procedure Create_Dir (To : in File_Name_Type);

   procedure Delete
     (File : in File_Name_Type);

   procedure Expand_And_Compile_RCI_Caller
     (Source, Target : in File_Name_Type);
   --  Generates the source's caller stubs into target (-gnatzc).

   procedure Expand_And_Compile_RCI_Receiver
     (Source, Target : in File_Name_Type);
   --  Generates the source's receiver stubs into target (-gnatzr).

   procedure Initialize;

   procedure Initialize_ALI renames ALI.Initialize_ALI;

   function Is_Directory    (File : File_Name_Type) return Boolean;

   function Is_Regular_File (File : File_Name_Type) return Boolean;

   function Is_Relative_Dir (File : File_Name_Type) return Boolean;

   function More_Recent (File1, File2 : Name_Id) return Boolean;

   procedure Produce_Partition_Executable
     (Partition     : in Name_Id;
      Executable    : in File_Name_Type);
   --  Generates the partition ada main subprogram (compilation, bind and
   --  link).

   procedure Read_ALI (Id : ALI_Id) renames ALI.Read_ALI;
   function Scan_ALI (F : File_Name_Type; T : Text_Buffer_Ptr)
                      return ALI_Id renames ALI.Scan_ALI;
   function Str_To_Id           (S : String) return Name_Id;
   --  Set into name table and return id.

   function Strlen (Name : in Name_Id) return Natural;

   procedure Unlink_File
     (File : in File_Name_Type);

   procedure Write_Compile_Command (Name : in File_Name_Type);
   --  Generates on standard-out the command needed to compile
   --  a sub-tree from a given package.

   procedure Write_Eol
     (File   : in File_Descriptor;
      Stdout : in Boolean := False);

   procedure Write_File_Stamp
     (File : in File_Name_Type);

   procedure Write_Message
     (Message : in String);

   procedure Write_Missing_File
     (File  : in File_Name_Type);

   procedure Write_Name
     (File   : in File_Descriptor;
      Name   : in File_Name_Type;
      Stdout : in Boolean := False);

   procedure Write_Stamp_Comparison
     (Newer, Older : in File_Name_Type);

   procedure Write_Str
     (File   : in File_Descriptor;
      Line   : in String;
      Stdout : in Boolean := False);

   procedure Write_Unit_Name
     (U : in Unit_Name_Type);

end XE_Utils;
