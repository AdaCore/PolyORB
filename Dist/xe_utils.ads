------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                            X E _ U T I L S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------
with Types;       use Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with XE_Defs;     use XE_Defs;
package XE_Utils is

   --  This package is intended to provide all the OS facilities

   function Later (File1, File2 : Name_Id) return Boolean;

   function Register (Name : String) return Name_Id;

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
   G_Parent_Dir : Name_Id;

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False);
   procedure Build_RCI_Caller     (Source, Target : File_Name_Type);
   procedure Build_RCI_Receiver   (Source, Target : File_Name_Type);
   procedure Build_Partition      (Partition : Name_Id; Exec : File_Name_Type);
   procedure Compile_RCI_Caller   (Source    : File_Name_Type);
   procedure Compile_RCI_Receiver (Source    : File_Name_Type);

   Separator : Character renames Directory_Separator;

   procedure Change_Dir (To : in File_Name_Type);
   procedure Create_Dir (To : in File_Name_Type);

   function Is_Regular_File (File : File_Name_Type) return Boolean;
   function Is_Directory    (File : File_Name_Type) return Boolean;

   procedure Create
     (File : in out File_Descriptor;
      Name : in File_Name_Type;
      Exec : in Boolean := False);

   procedure Delete (File : File_Name_Type);

   procedure Unlink_File (File : File_Name_Type);

   procedure Write_Stamp
     (File : in Name_Id);

   procedure Write_Str
     (File : in File_Descriptor;
      Line : in String);

   procedure Write_Name
     (File   : in File_Descriptor;
      Name   : in Name_Id);

   procedure Write_Eol
     (File : in File_Descriptor);

   procedure Write_Unit_Name (N : Unit_Name_Type);

   function Strlen (Name : in Name_Id) return Natural;

   function "&" (Prefix, Suffix : Name_Id) return Name_Id;

   procedure Initialize;

end XE_Utils;




