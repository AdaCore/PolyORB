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
--                 GLADE is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Types;       use Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with XE_Defs;     use XE_Defs;
package XE_Utils is

   --  This package is intended to provide all the OS facilities

   function More_Recent (File1, File2 : Name_Id) return Boolean;

   Obj_Suffix   : Name_Id;
   ALI_Suffix   : Name_Id;
   ADS_Suffix   : Name_Id;
   ADB_Suffix   : Name_Id;

   Spec_Suffix  : Name_Id;
   Body_Suffix  : Name_Id;

   Com_Sep_Id   : Name_Id;
   Dir_Sep_Id   : Name_Id;
   Dot_Sep_Id   : Name_Id;

   DSA_Dir      : Name_Id;
   Caller_Dir   : Name_Id;
   Receiver_Dir : Name_Id;
   Parent_Dir   : Name_Id;
   Original_Dir : Name_Id;

   PWD_Id       : Name_Id;

   Elaboration_Name      : Name_Id;
   Elaboration_Full_Name : Name_Id;

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False);
   --  Basically, this procedure copies source into target and
   --  preserves file stamps.

   procedure Build_RCI_Caller
     (Source, Target : in File_Name_Type);
   --  Generates the source's caller stubs into target (-gnatzc).

   procedure Build_RCI_Receiver
     (Source, Target : in File_Name_Type);
   --  Generates the source's receiver stubs into target (-gnatzr).

   procedure Build_Partition
     (Partition : in Name_Id;
      Exec      : in File_Name_Type);
   --  Generates the partition ada main subprogram (generation
   --  of Ada code, compilation, bind and link). Exec is the name
   --  of the executable.

   procedure Build_Compile_Command (Name : in File_Name_Type);
   --  Generates on standard-out the command needed to compile
   --  a sub-tree from a given package.

   procedure Compile_RCI_Caller
     (Source    : in File_Name_Type);
   --  Compile the caller stubs (-gnatzC).

   procedure Compile_RCI_Receiver
     (Source    : in File_Name_Type);
   --  Compile the receiver stubs (-gnatzR).

   Separator : Character renames Directory_Separator;

   procedure Change_Dir (To : in File_Name_Type);
   procedure Create_Dir (To : in File_Name_Type);

   function Is_Regular_File (File : File_Name_Type) return Boolean;
   function Is_Directory    (File : File_Name_Type) return Boolean;

   procedure Create
     (File : in out File_Descriptor;
      Name : in File_Name_Type;
      Exec : in Boolean := False);

   procedure Delete
     (File : in File_Name_Type);

   procedure Unlink_File
     (File : in File_Name_Type);

   procedure Write_Stamp
     (File : in Name_Id);

   procedure Write_Str
     (File   : in File_Descriptor;
      Line   : in String;
      Stdout : in Boolean := False);

   procedure Write_Name
     (File   : in File_Descriptor;
      Name   : in Name_Id;
      Stdout : in Boolean := False);

   procedure Write_Eol
     (File   : in File_Descriptor;
      Stdout : in Boolean := False);

   procedure Write_Unit_Name
     (N : in Unit_Name_Type);

   function Strlen (Name : in Name_Id) return Natural;

   function "&" (Prefix, Suffix : Name_Id) return Name_Id;

   procedure Initialize;

end XE_Utils;




