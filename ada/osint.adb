------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                            --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

with Hostparm;
with Namet;       use Namet;
with Output;      use Output;
with Switch;      use Switch;
with Opt;         use Opt;
with Sdefault;    use Sdefault;
with Tree_IO;     use Tree_IO;

with Unchecked_Conversion;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.HTable;

package body Osint is

   -------------------------------------
   -- Use of Name_Find and Name_Enter --
   -------------------------------------

   --  This package creates a number of source, ALI and object file names
   --  that are used to locate the actual file and for the purpose of
   --  message construction. These names need not be accessible by Name_Find,
   --  and can be therefore created by using routine Name_Enter. The files in
   --  question are file names with a prefix directory (ie the files not
   --  in the current directory). File names without a prefix directory are
   --  entered with Name_Find because special values might be attached to
   --  the various Info fields of the corresponding name table entry.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Append_Suffix_To_File_Name
     (Name   : Name_Id;
      Suffix : String)
      return   Name_Id;
   --  Appends Suffix to Name and returns the new name.

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type;
   --  Convert OS format time to GNAT format time stamp

   procedure Create_File_And_Check
     (Fdesc : out File_Descriptor;
      Fmode : Mode);
   --  Create file whose name (NUL terminated) is in Name_Buffer (with the
   --  length in Name_Len), and place the resulting descriptor in Fdesc.
   --  Issue message and exit with fatal error if file cannot be created.
   --  The Fmode parameter is set to either Text or Binary (see description
   --  of GNAT.OS_Lib.Create_File).

   procedure Find_Program_Name;
   --  Put in Buffer_Name the name of the current program being run
   --  excluding the directory path

   procedure Write_With_Check (A  : Address; N  : Integer);
   --  Writes N bytes from buffer starting at address A to file whose FD is
   --  stored in Output_FD, and whose file name is stored as a
   --  File_Name_Type in Output_File_Name. A check is made for disk full,
   --  and if this is detected, the file being written is deleted, and a
   --  fatal error is signalled.

   function More_Files return Boolean;
   --  Implements More_Source_Files and More_Lib_Files.

   function Next_Main_File return File_Name_Type;
   --  Implements Next_Main_Source and Next_Main_Lib_File.

   type File_Type is (Source, Library);

   function Locate_File
     (N    : File_Name_Type;
      T    : File_Type;
      Dir  : Natural;
      Name : String)
      return File_Name_Type;
   --  See if the file N whose name is Name exists in directory Dir. Dir is
   --  an index into the Lib_Search_Directories table if T = Library.
   --  Otherwise if T = Source, Dir is an index into the
   --  Src_Search_Directories table. Returns the File_Name_Type of the
   --  full file name if file found, or No_File if not found.

   function Find_File
     (N :    File_Name_Type;
      T :    File_Type)
      return File_Name_Type;
   --  Finds a source or library file depending on the value of T following
   --  the directory search order rules unless N is the name of the file
   --  just read with Next_Main_File and already contains directiory
   --  information, in which case just look in the
   --  Primary_Directory. Returns File_Name_Type of the full file name if
   --  found, No_File if file not found. Note that for the special case of
   --  gnat.adc, only the compilation environment directory is searched,
   --  i.e. the directory where the ali an object files are written

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of a C string.  Does check for null address
   --  (returns 0).

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer)
      return      String_Access;
   --  Converts a C String to an Ada String.  Are we doing this to avoid
   --  withing Interfaces.C.Strings ???

   ------------------------------
   -- Other Local Declarations --
   ------------------------------

   ALI_Suffix : constant String_Ptr := new String'("ali");
   --  The suffix used for the library files (also known as ALI files).

   EOL : constant Character := Ascii.LF;
   --  End of line character

   Argument_Count : constant Integer := Arg_Count - 1;
   --  Number of arguments (excluding program name)

   File_Names : array (Int range 1 .. Int (Argument_Count)) of String_Ptr;
   --  As arguments are scanned in Initialize, filenames are stored
   --  in this array. The string does not contain a terminating NUL.

   Number_File_Names : Int := 0;
   --  The total number of filenames found on command line and placed in
   --  File_Names.

   Current_File_Name_Index : Int := 0;
   --  The index in File_Names of the last file opened by Next_Main_Source
   --  or Next_Main_Lib_File. The value 0 indicates that no files have been
   --  opened yet.

   Current_Main : File_Name_Type := No_File;
   --  Used to save a simple file name between calls to Next_Main_Source and
   --  Read_Source_File. If the file name argument to Read_Source_File is
   --  No_File, that indicates that the file whose name was returned by the
   --  last call to Next_Main_Source (and stored here) is to be read.

   Current_Full_Source_Name  : File_Name_Type  := No_File;
   Current_Full_Source_Stamp : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Lib_Name     : File_Name_Type  := No_File;
   Current_Full_Lib_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Obj_Name     : File_Name_Type  := No_File;
   Current_Full_Obj_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   --  Respectively full name (with directory info) and time stamp of
   --  the latest source, library and object files opened by Read_Source_File
   --  and Read_Library_Info.

   In_Binder   : Boolean := False;
   In_Compiler : Boolean := False;
   In_Make     : Boolean := False;
   --  Exactly one of these flags is set True to indicate which program
   --  is bound and executing with Osint, which is used by all these programs.

   Output_FD : File_Descriptor;
   --  The file descriptor for the current library info, tree or binder output

   Output_File_Name : File_Name_Type;
   --  File_Name_Type for name of open file whose FD is in Output_FD, the name
   --  stored does not include the trailing NUL character.

   Output_Filename : String_Ptr := null;
   --  The name after the -o option

   ------------------
   -- Search Paths --
   ------------------

   Primary_Directory : constant := 0;
   --  This is index in the tables created below for the first directory to
   --  search in for source or library information files. This is the
   --  directory containing the latest main input file (a source file for
   --  the compiler or a library file for the binder).

   package Src_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => Primary_Directory,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Osint.Src_Search_Directories");
   --  Table of names of directories in which to search for source (Compiler)
   --  files. This table is filled in the order in which the directories are
   --  to be searched, and then used in that order.

   package Lib_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => Primary_Directory,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Osint.Lib_Search_Directories");
   --  Table of names of directories in which to search for library (Binder)
   --  files. This table is filled in the order in which the directories are
   --  to be searched and then used in that order. The reason for having two
   --  distinct tables is that we need them both in gnatmake.

   ---------------------
   -- File Hash Table --
   ---------------------

   --  The file hash table is provided to free the programmer from any
   --  efficiency concern when retrieving full file names or time stamps of
   --  source files. If the programmer calls Source_File_Data (Cache => True)
   --  he is guaranteed that the price to retrieve the full name (ie with
   --  directory info) or time stamp of the file will be payed only once,
   --  the first time the full name is actually searched (or the first time
   --  the time stamp is actually retrieved). This is achieved by employing
   --  a hash table that stores as a key the File_Name_Type of the file and
   --  associates to that File_Name_Type the full file name of the file and its
   --  time stamp.

   File_Cache_Enabled : Boolean := False;
   --  Set to true if you want the enable the file data caching mechanism.

   type File_Hash_Num is range 0 .. 1020;

   function File_Hash (F : File_Name_Type) return File_Hash_Num;
   --  Compute hash index for use by Simple_HTable

   package File_Name_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => File_Hash_Num,
     Element    => File_Name_Type,
     No_Element => No_File,
     Key        => File_Name_Type,
     Hash       => File_Hash,
     Equal      => "=");

   package File_Stamp_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => File_Hash_Num,
     Element    => Time_Stamp_Type,
     No_Element => Empty_Time_Stamp,
     Key        => File_Name_Type,
     Hash       => File_Hash,
     Equal      => "=");

   function Smart_Find_File
     (N    : File_Name_Type;
      T    : File_Type)
      return File_Name_Type;
   --  Exactly like Find_File except that if File_Cache_Enabled is True this
   --  routine looks first in the hash table to see if the full name of the
   --  file is already available.

   function Smart_File_Stamp
     (N    : File_Name_Type;
      T    : File_Type)
      return Time_Stamp_Type;
   --  Takes the same parameter as the routine above (N is a file name
   --  without any prefix directory information) and behaves like File_Stamp
   --  except that if File_Cache_Enabled is True this routine looks first in
   --  the hash table to see if the file stamp of the file is already
   --  available.

   ------------------------
   -- Add_Lib_Search_Dir --
   ------------------------

   procedure Add_Lib_Search_Dir (Dir : String) is
   begin
      if Dir'Length = 0 then
         Fail ("missing library directory name");
      end if;

      Lib_Search_Directories.Increment_Last;
      Lib_Search_Directories.Table (Lib_Search_Directories.Last) :=
        Normalize_Directory_Name (Dir);
   end Add_Lib_Search_Dir;

   ------------------------
   -- Add_Src_Search_Dir --
   ------------------------

   procedure Add_Src_Search_Dir (Dir : String) is
   begin
      if Dir'Length = 0 then
         Fail ("missing source directory name");
      end if;

      Src_Search_Directories.Increment_Last;
      Src_Search_Directories.Table (Src_Search_Directories.Last) :=
        Normalize_Directory_Name (Dir);
   end Add_Src_Search_Dir;

   --------------------------------
   -- Append_Suffix_To_File_Name --
   --------------------------------

   function Append_Suffix_To_File_Name
     (Name   : Name_Id;
      Suffix : String)
      return   Name_Id
   is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;
      return Name_Find;
   end Append_Suffix_To_File_Name;

   -------------------------
   -- Close_Binder_Output --
   -------------------------

   procedure Close_Binder_Output is
   begin
      pragma Assert (In_Binder);
      Close (Output_FD);
   end Close_Binder_Output;

   -----------------------
   -- Close_Stub_Output --
   -----------------------

   procedure Close_Stub_Output is
   begin
      pragma Assert (In_Compiler);
      Close (Output_FD);
      Restore_Output_FD;
   end Close_Stub_Output;

   -------------------------------
   -- Close_Output_Library_Info --
   -------------------------------

   procedure Close_Output_Library_Info is
   begin
      pragma Assert (In_Compiler);
      Close (Output_FD);
   end Close_Output_Library_Info;

   -----------------------
   -- Close_Xref_Output --
   -----------------------

   procedure Close_Xref_Output is
   begin
      pragma Assert (In_Compiler);
      Close (Output_FD);
   end Close_Xref_Output;

   --------------------------
   -- Create_Binder_Output --
   --------------------------

   procedure Create_Binder_Output
     (Output_Filename : String;
      Typ             : Character;
      Bfile           : out Name_Id)
   is
      File_Name : String_Ptr;
      Findex1   : Natural;
      Findex2   : Natural;
      Flength   : Natural;

   begin
      pragma Assert (In_Binder);

      if Output_Filename /= "" then
         Name_Buffer (Output_Filename'Range) := Output_Filename;
         Name_Buffer (Output_Filename'Last + 1) := Ascii.NUL;

         if Typ = 's' then
            Name_Buffer (Output_Filename'Last) := 's';
         end if;

         Name_Len := Output_Filename'Last;

      else
         Name_Buffer (1) := 'b';
         File_Name := File_Names (Current_File_Name_Index);

         Findex1 := File_Name'First;

         --  The ali file might be specified by a full path name. However,
         --  the binder generated file should always be created in the
         --  current directory, so the path might need to be stripped away.
         --  In addition to the default directory_separator allow the '/' to
         --  act as separator since this is allowed in MS-DOS and OS2 ports.

         for J in reverse File_Name'Range loop
            if File_Name (J) = Directory_Separator
              or else File_Name (J) = '/'
            then
               Findex1 := J + 1;
               exit;
            end if;
         end loop;

         Findex2 := File_Name'Last;
         while File_Name (Findex2) /=  '.' loop
            Findex2 := Findex2 - 1;
         end loop;

         Flength := Findex2 - Findex1;

         if Maximum_File_Name_Length > 0 then

            --  Make room for the extra two characters in "b?"

            while Int (Flength) > Maximum_File_Name_Length - 2 loop
               Findex2 := Findex2 - 1;
               Flength := Findex2 - Findex1;
            end loop;
         end if;

         Name_Buffer (3 .. Flength + 2) := File_Name (Findex1 .. Findex2 - 1);
         Name_Buffer (Flength + 3) := '.';

         --  C bind file, name is b_xxx.c

         if Typ = 'c' then
            Name_Buffer (2) := '_';
            Name_Buffer (Flength + 4) := 'c';
            Name_Buffer (Flength + 5) := Ascii.NUL;
            Name_Len := Flength + 4;

         --  Ada bind file, name is b~xxx.adb or b~xxx.ads
         --  (with $ instead of ~ in VMS)

         else
            if Hostparm.OpenVMS then
               Name_Buffer (2) := '$';
            else
               Name_Buffer (2) := '~';
            end if;

            Name_Buffer (Flength + 4) := 'a';
            Name_Buffer (Flength + 5) := 'd';
            Name_Buffer (Flength + 6) := Typ;
            Name_Buffer (Flength + 7) := Ascii.NUL;
            Name_Len := Flength + 6;
         end if;
      end if;

      Bfile := Name_Find;
      Create_File_And_Check (Output_FD, Text);

   end Create_Binder_Output;

   ---------------------------
   -- Create_File_And_Check --
   ---------------------------

   procedure Create_File_And_Check
     (Fdesc : out File_Descriptor;
      Fmode : Mode)
   is
   begin
      Output_File_Name := Name_Enter;
      Fdesc := Create_File (Name_Buffer'Address, Fmode);

      if Fdesc = Invalid_FD then
         Fail ("Cannot create: ", Name_Buffer (1 .. Name_Len));
      end if;
   end Create_File_And_Check;

   --------------------------------
   -- Create_Output_Library_Info --
   --------------------------------

   procedure Create_Output_Library_Info is
      Dot_Index : Natural;

   begin
      pragma Assert (In_Compiler);
      Get_Name_String (Current_Main);

      Dot_Index := 0;
      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Dot_Index := J;
            exit;
         end if;
      end loop;

      --  Should be impossible to not have an extension

      if Dot_Index = 0 then
         pragma Assert (False);
         raise Program_Error;
      end if;

      --  Make sure that the output file name matches the source file name.
      --  To compare them, remove filename directories and extensions.

      if Output_Filename /= null then
         declare
            Name : String  := Name_Buffer (1 .. Dot_Index);
            Len  : Natural := Dot_Index;

         begin
            Name_Buffer (1 .. Output_Filename'Length) := Output_Filename.all;

            for J in reverse Output_Filename'Range loop
               if Name_Buffer (J) = '.' then
                  Dot_Index := J;
                  exit;
               end if;
            end loop;

            if Name /= Name_Buffer (Dot_Index - Len + 1 .. Dot_Index) then
               Fail ("incorrect object file name");
            end if;
         end;
      end if;

      Name_Buffer (Dot_Index + 1 .. Dot_Index + 3) := ALI_Suffix.all;
      Name_Buffer (Dot_Index + 4) := Ascii.NUL;
      Name_Len := Dot_Index + 3;
      Create_File_And_Check (Output_FD, Text);

   end Create_Output_Library_Info;

   -----------------------
   -- Create_Req_Output --
   -----------------------

   procedure Create_Req_Output is
   begin
      pragma Assert (In_Compiler);
      Create_File_And_Check (Output_FD, Text);
   end Create_Req_Output;

   ------------------------
   -- Create_Stub_Output --
   ------------------------

   procedure Create_Stub_Output is
      FD : File_Descriptor;

   begin
      pragma Assert (In_Compiler);
      Create_File_And_Check (FD, Text);
      Set_Output_FD (FD);
   end Create_Stub_Output;

   ------------------------
   -- Create_Xref_Output --
   ------------------------

   procedure Create_Xref_Output (Typ : Xfiltyp) is
      Ptr : Natural;

   begin
      pragma Assert (In_Compiler);

      if Typ /= Xglobal then
         Get_Name_String (Current_Main);
         Ptr := Name_Len;

         while Ptr > 1 loop
            if Name_Buffer (Ptr) = '.' then
               Name_Len := Ptr - 1;
               exit;

            elsif Name_Buffer (Ptr) = ']'
              or else Name_Buffer (Ptr) = '\'
              or else Name_Buffer (Ptr) = '/'
            then
               exit;

            else
               Ptr := Ptr - 1;
            end if;
         end loop;

         if Typ = Xbody then
            Add_Str_To_Name_Buffer (".xrb");
         else
            Add_Str_To_Name_Buffer (".xrs");
         end if;

         Name_Buffer (Name_Len + 1) := Ascii.NUL;


      else -- Typ = Xglobal
         Name_Buffer (1 .. 5) := "X.ref";
         Name_Buffer (6) := Ascii.NUL;
         Name_Len := 5;
      end if;

      Create_File_And_Check (Output_FD, Text);
   end Create_Xref_Output;

   --------------------------------
   -- Current_Library_File_Stamp --
   --------------------------------

   function Current_Library_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Lib_Stamp;
   end Current_Library_File_Stamp;

   -------------------------------
   -- Current_Object_File_Stamp --
   -------------------------------

   function Current_Object_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Obj_Stamp;
   end Current_Object_File_Stamp;

   -------------------------------
   -- Current_Source_File_Stamp --
   -------------------------------

   function Current_Source_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Source_Stamp;
   end Current_Source_File_Stamp;

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name (Name : File_Name_Type) return File_Name_Type is
      Exec_Suffix : String_Access;

   begin
      if Name = No_File then
         return No_File;
      end if;

      Get_Name_String (Name);
      Exec_Suffix := Get_Executable_Suffix;

      for J in Exec_Suffix.all'Range loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Exec_Suffix.all (J);
      end loop;

      return Name_Enter;
   end Executable_Name;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Exit_Code : Exit_Code_Type) is
   begin
      case Exit_Code is
         when E_Success    => OS_Exit (0);
         when E_Warnings   => OS_Exit (0);
         when E_Errors     => OS_Exit (1);
         when E_No_Code    => OS_Exit (1);
         when E_No_Compile => OS_Exit (1);
         when E_Fatal      => OS_Exit (2);
         when E_Abort      => OS_Abort;
      end case;
   end Exit_Program;

   ----------
   -- Fail --
   ----------

   procedure Fail (S1 : String; S2 : String := ""; S3 : String := "") is
   begin
      Osint.Write_Program_Name;
      Write_Str (": ");
      Write_Str (S1);
      Write_Str (S2);
      Write_Str (S3);
      Write_Eol;
      Exit_Program (E_Fatal);
   end Fail;

   ---------------
   -- File_Hash --
   ---------------

   function File_Hash (F : File_Name_Type) return File_Hash_Num is
   begin
      return File_Hash_Num (Int (F) rem File_Hash_Num'Range_Length);
   end File_Hash;

   ----------------
   -- File_Stamp --
   ----------------

   function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type is
   begin
      if Name = No_File then
         return Empty_Time_Stamp;
      end if;

      Get_Name_String (Name);

      if not Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         return Empty_Time_Stamp;
      else
         Name_Buffer (Name_Len + 1) := Ascii.NUL;
         return OS_Time_To_GNAT_Time (File_Time_Stamp (Name_Buffer));
      end if;
   end File_Stamp;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = Directory_Separator
           or else Name_Buffer (J) = '/'
         then
            Name_Len := J;
            return Name_Find;
         end if;
      end loop;

      Name_Len := Hostparm.Normalized_CWD'Length;
      Name_Buffer (1 .. Name_Len) := Hostparm.Normalized_CWD;
      return Name_Find;
   end Get_Directory;

   -----------------
   -- Locate_File --
   -----------------

   function Locate_File
     (N    : File_Name_Type;
      T    : File_Type;
      Dir  : Natural;
      Name : String)
      return File_Name_Type
   is
      Dir_Name : String_Ptr;

   begin
      if T = Library then
         Dir_Name := Lib_Search_Directories.Table (Dir);
      elsif T = Source then
         Dir_Name := Src_Search_Directories.Table (Dir);
      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      declare
         Full_Name : String (1 .. Dir_Name'Length + Name'Length);

      begin
         Full_Name (1 .. Dir_Name'Length) := Dir_Name.all;
         Full_Name (Dir_Name'Length + 1 .. Full_Name'Length) := Name;

         if not Is_Regular_File (Full_Name) then
            return No_File;

         else
            --  If the file is in the current directory then return N itself

            if Dir_Name'Length = 0 then
               return N;
            else
               Name_Len := Full_Name'Length;
               Name_Buffer (1 .. Name_Len) := Full_Name;
               return Name_Enter;
            end if;
         end if;
      end;
   end Locate_File;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (N :    File_Name_Type;
      T :    File_Type)
      return File_Name_Type
   is
   begin
      Get_Name_String (N);

      declare
         File_Name : String renames Name_Buffer (1 .. Name_Len);
         File      : File_Name_Type := No_File;
         Last_Dir  : Natural;

      begin
         --  If we are looking for gnat.adc, look only in the current
         --  directory, i.e. return input argument unchanged.

         if File_Name = "gnat.adc" then
            return N;

         --  If we are trying to find the current main file just look in the
         --  directory where the user said it was.

         elsif Current_Main = N then
            return Locate_File (N, T, Primary_Directory, File_Name);

         --  Otherwise do standard search for source file

         else
            --  First place to look is in the primary directory (i.e. the same
            --  directory as the source) unless this has been disabled with -I-

            if Opt.Look_In_Primary_Dir then
               File := Locate_File (N, T, Primary_Directory, File_Name);

               if File /= No_File then
                  return File;
               end if;
            end if;

            --  Finally look in directories specified with switches -I/-aI/-aO

            if T = Library then
               Last_Dir := Lib_Search_Directories.Last;
            else
               Last_Dir := Src_Search_Directories.Last;
            end if;

            for D in Primary_Directory + 1 .. Last_Dir loop
               File := Locate_File (N, T, D, File_Name);

               if File /= No_File then
                  return File;
               end if;
            end loop;

            return No_File;
         end if;
      end;
   end Find_File;

   ------------------------
   -- Full_Lib_File_Name --
   ------------------------

   function Full_Lib_File_Name (N : File_Name_Type) return File_Name_Type is
   begin
      return Find_File (N, Library);
   end Full_Lib_File_Name;

   ----------------------------
   -- Full_Library_Info_Name --
   ----------------------------

   function Full_Library_Info_Name return File_Name_Type is
   begin
      return Current_Full_Lib_Name;
   end Full_Library_Info_Name;

   ---------------------------
   -- Full_Object_File_Name --
   ---------------------------

   function Full_Object_File_Name return File_Name_Type is
   begin
      return Current_Full_Obj_Name;
   end Full_Object_File_Name;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   function Full_Source_Name return File_Name_Type is
   begin
      return Current_Full_Source_Name;
   end Full_Source_Name;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   function Full_Source_Name (N : File_Name_Type) return File_Name_Type is
   begin
      return Smart_Find_File (N, Source);
   end Full_Source_Name;

   -------------------------
   -- Is_Readonly_Library --
   -------------------------

   function Is_Readonly_Library (File : in File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);

      pragma Assert (Name_Buffer (Name_Len - 3 .. Name_Len) = ".ali");

      return not Is_Writable_File (Name_Buffer (1 .. Name_Len));
   end Is_Readonly_Library;

   -------------------------------
   -- Matching_Full_Source_Name --
   -------------------------------

   function Matching_Full_Source_Name
     (N    : File_Name_Type;
      T    : Time_Stamp_Type)
      return File_Name_Type
   is
   begin
      Get_Name_String (N);

      declare
         File_Name : constant String := Name_Buffer (1 .. Name_Len);
         File      : File_Name_Type := No_File;
         Last_Dir  : Natural;

      begin
         if Opt.Look_In_Primary_Dir then
            File := Locate_File (N, Source, Primary_Directory, File_Name);

            if File /= No_File and then T = File_Stamp (N) then
               return File;
            end if;
         end if;

         Last_Dir := Src_Search_Directories.Last;

         for D in Primary_Directory + 1 .. Last_Dir loop
            File := Locate_File (N, Source, D, File_Name);

            if File /= No_File and then T = File_Stamp (File) then
               return File;
            end if;
         end loop;

         return No_File;
      end;
   end Matching_Full_Source_Name;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (N : File_Name_Type) return File_Name_Type is
      Object_Suffix : String_Access;

   begin
      if N = No_File then
         return No_File;
      end if;

      Get_Name_String (N);
      Name_Len := Name_Len - ALI_Suffix'Length - 1;
      Object_Suffix := Get_Object_Suffix;

      for J in Object_Suffix.all'Range loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Object_Suffix.all (J);
      end loop;

      return Name_Enter;
   end Object_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (P : Program_Type) is
      function Get_Default_Identifier_Character_Set return Character;
      pragma Import (C, Get_Default_Identifier_Character_Set,
                       "Get_Default_Identifier_Character_Set");
      --  Function to determine the default identifier character set,
      --  which is system dependent. See Opt package spec for a list of
      --  the possible character codes and their interpretations.

      function Get_Maximum_File_Name_Length return Int;
      pragma Import (C, Get_Maximum_File_Name_Length,
                    "Get_Maximum_File_Name_Length");
      --  Function to get maximum file name length for system

      procedure Adjust_OS_Resource_Limits;
      pragma Import (C, Adjust_OS_Resource_Limits,
                        "adjust_os_resource_limits");
      --  Procedure to make system specific adjustments to make GNAT
      --  run better.

   --  Start of processing for Initialize

   begin
      Program := P;

      case Program is
         when Binder   => In_Binder   := True;
         when Compiler => In_Compiler := True;
         when Make     => In_Make     := True;
      end case;

      if In_Compiler then
         Adjust_OS_Resource_Limits;
      end if;

      Src_Search_Directories.Init;
      Lib_Search_Directories.Init;

      Identifier_Character_Set := Get_Default_Identifier_Character_Set;
      Maximum_File_Name_Length := Get_Maximum_File_Name_Length;

      --  Following should be removed by having above function return
      --  Integer'Last as indication of no maximum instead of -1 ???

      if Maximum_File_Name_Length = -1 then
         Maximum_File_Name_Length := Int'Last;
      end if;

      --  Start off by setting all suppress options to False, these will
      --  be reset later (turning some on if -gnato is not specified, and
      --  turning all of them on if -gnatp is specified).

      Suppress_Options := (others => False);

      --  Set software overflow check flag. For now all targets require the
      --  use of software overflow checks. Later on, this will have to be
      --  specialized to the backend target. Also, if software overflow
      --  checking mode is set, then the default for suppressing overflow
      --  checks is True, since the software approach is expensive.

      Software_Overflow_Checking := True;
      Suppress_Options.Overflow_Checks := True;

      --  Reserve the first slot in the search paths table.  This is the
      --  directory of the main source file or main library file and is
      --  filled in by each call to Next_Main_Source/Next_Main_Lib_File with
      --  the directory specified for this main source or library file. This
      --  is the directory which is searched first by default. This default
      --  search is inhibited by the option -I- for both source and library
      --  files.

      Src_Search_Directories.Set_Last (Primary_Directory);
      Src_Search_Directories.Table (Primary_Directory) := new String'("");

      Lib_Search_Directories.Set_Last (Primary_Directory);
      Lib_Search_Directories.Table (Primary_Directory) := new String'("");

   end Initialize;

   ------------------------
   -- Scan_Compiler_Args --
   ------------------------

   procedure Scan_Compiler_Args is
      Output_Filename_Seen : Boolean := False;
      --  Set to True after having scanned the file_name for
      --  switch "-o file_name"

      Next_Arg    : Positive;

   begin
      --  Loop through command line arguments, storing them for later access

      Next_Arg := 1;
      Scan_Args : loop

         exit when not In_Compiler;

         if Next_Arg > Argument_Count
           and then Output_Filename_Present
           and then not Output_Filename_Seen
         then
            Fail ("Output filename missing after -o");
         end if;

         exit when Next_Arg > Argument_Count;

         declare
            Next_Argv : String (1 .. Len_Arg (Next_Arg));

         begin
            Fill_Arg (Next_Argv'Address, Next_Arg);

            --  If the previous switch has set the Output_Filename_Present
            --  flag (that is we have seen a -o), then the next argument is
            --  the name of the output file.

            if Output_Filename_Present
              and then not Output_Filename_Seen
            then
               Output_Filename_Seen := True;

               if Next_Argv'Length = 0
                 or else (Next_Argv'Length >= 1
                          and then (Next_Argv (1) = Switch_Character
                                    or else Next_Argv (1) = '-'))
               then
                  Fail ("Output filename missing after -o");

               else
                  pragma Assert (Output_Filename = null);
                  Output_Filename := new String'(Next_Argv);
               end if;

            elsif Next_Argv'Length = 1
              and then (Next_Argv (1) = Switch_Character
                        or else Next_Argv (1) = '-')
            then
               Fail ("switch character cannot be followed by a blank");

            elsif Next_Argv'Length >= 2
              and then (Next_Argv (1) = Switch_Character
                        or else Next_Argv (1) = '-')
            then
               if Next_Argv (2 .. Next_Argv'Last) = "I-" then
                  Opt.Look_In_Primary_Dir := False;

               elsif Next_Argv (2) = 'I' then
                  Add_Src_Search_Dir (Next_Argv (3 .. Next_Argv'Last));
                  Add_Lib_Search_Dir (Next_Argv (3 .. Next_Argv'Last));

               elsif Next_Argv'Length >= 3
                 and then Next_Argv (2 .. 3) = "aI"
               then
                  Add_Src_Search_Dir (Next_Argv (4 .. Next_Argv'Last));

               elsif Next_Argv'Length >= 3
                 and then Next_Argv (2 .. 3) = "aO"
               then
                  Add_Lib_Search_Dir (Next_Argv (4 .. Next_Argv'Last));

               --  All other options are single character and are handled
               --  by Scan_Switches.

               else
                  Scan_Switches (Next_Argv);
               end if;

            --  Not a switch, so must be a filename (if non-empty)

            elsif Next_Argv'Length /= 0 then
               Number_File_Names := Number_File_Names + 1;
               File_Names (Number_File_Names) := new String'(Next_Argv);
            end if;
         end;

         Next_Arg := Next_Arg + 1;
      end loop Scan_Args;

      --  Make sure that the object file has the expected extension.

      if Output_Filename /= null then
         declare
            S1 : String_Access := Get_Object_Suffix;
            S2 : String_Ptr    := Output_Filename;
            L1 : Natural := S1'Length;
            L2 : Natural := S2'Length;

         begin
            if L2 <= L1 or else S2 (L2 - L1 + 1 .. L2) /= S1.all then
               Fail ("incorrect object file extension");
            end if;
         end;
      end if;

   end Scan_Compiler_Args;

   -------------------------------
   -- Get_Next_Dir_In_Path_Init --
   -------------------------------

   Search_Path_Pos : Integer;
   --  Keeps track of current position in search path. Initialized by the
   --  call to Get_Next_Dir_In_Path_Init, updated by Get_Next_Dir_In_Path.

   procedure Get_Next_Dir_In_Path_Init (Search_Path : String_Access) is
   begin
      Search_Path_Pos := Search_Path'First;
   end Get_Next_Dir_In_Path_Init;

   --------------------------
   -- Get_Next_Dir_In_Path --
   --------------------------

   function Get_Next_Dir_In_Path
     (Search_Path : String_Access)
      return        String_Access
   is
      Lower_Bound : Positive := Search_Path_Pos;
      Upper_Bound : Positive;

   begin
      loop
         while Lower_Bound <= Search_Path'Last
           and then Search_Path.all (Lower_Bound) = Path_Separator
         loop
            Lower_Bound := Lower_Bound + 1;
         end loop;

         exit when Lower_Bound > Search_Path'Last;

         Upper_Bound := Lower_Bound;
         while Upper_Bound <= Search_Path'Last
           and then Search_Path.all (Upper_Bound) /= Path_Separator
         loop
            Upper_Bound := Upper_Bound + 1;
         end loop;

         Search_Path_Pos := Upper_Bound;
         return new String'(Search_Path.all (Lower_Bound .. Upper_Bound - 1));
      end loop;

      return null;
   end Get_Next_Dir_In_Path;

   -----------------------------
   -- Add_Default_Search_Dirs --
   -----------------------------

   procedure Add_Default_Search_Dirs is
      Search_Dir  : String_Access;
      Search_Path : String_Access;

      procedure Add_Search_Dir
        (Search_Dir            : String_Access;
         Additional_Source_Dir : Boolean);
      --  Needs documentation ???

      function Get_Libraries_From_Registry return String_Ptr;
      --  On Windows systems, get the list of installed standard libraries
      --  from the registry key:
      --  HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\
      --                             GNAT\Standard Libraries
      --  Return an empty string on other systems

      function Update_Path (Path : String_Ptr) return String_Ptr;
      --  Update the specified path to replace the prefix with
      --  the location where GNAT is installed.   See the file prefix.c
      --  in GCC for more details.

      --------------------
      -- Add_Search_Dir --
      --------------------

      procedure Add_Search_Dir
        (Search_Dir            : String_Access;
         Additional_Source_Dir : Boolean)
      is
      begin
         if Additional_Source_Dir then
            Add_Src_Search_Dir (Search_Dir.all);
         else
            Add_Lib_Search_Dir (Search_Dir.all);
         end if;
      end Add_Search_Dir;

      ---------------------------------
      -- Get_Libraries_From_Registry --
      ---------------------------------

      function Get_Libraries_From_Registry return String_Ptr is
         function C_Get_Libraries_From_Registry return Address;
         pragma Import (C, C_Get_Libraries_From_Registry,
                        "Get_Libraries_From_Registry");
         function Strlen (Str : Address) return Integer;
         pragma Import (C, Strlen, "strlen");
         procedure Strncpy (X : Address; Y : Address; Length : Integer);
         pragma Import (C, Strncpy, "strncpy");
         Result_Ptr : Address;
         Result_Length : Integer;
         Out_String : String_Ptr;

      begin
         Result_Ptr := C_Get_Libraries_From_Registry;
         Result_Length := Strlen (Result_Ptr);

         Out_String := new String (1 .. Result_Length);
         Strncpy (Out_String.all'Address, Result_Ptr, Result_Length);
         return Out_String;
      end Get_Libraries_From_Registry;

      -----------------
      -- Update_Path --
      -----------------

      function Update_Path (Path : String_Ptr) return String_Ptr is

         function C_Update_Path (Path, Component : Address) return Address;
         pragma Import (C, C_Update_Path, "update_path");

         function Strlen (Str : Address) return Integer;
         pragma Import (C, Strlen, "strlen");

         procedure Strncpy (X : Address; Y : Address; Length : Integer);
         pragma Import (C, Strncpy, "strncpy");

         In_Length      : constant Integer := Path'Length;
         In_String      : String (1 .. In_Length + 1);
         Component_Name : aliased String := "GNAT" & ASCII.NUL;
         Result_Ptr     : Address;
         Result_Length  : Integer;
         Out_String     : String_Ptr;

      begin
         In_String (1 .. In_Length) := Path.all;
         In_String (In_Length + 1) := Ascii.NUL;
         Result_Ptr := C_Update_Path (In_String'Address,
                                      Component_Name'Address);
         Result_Length := Strlen (Result_Ptr);

         Out_String := new String (1 .. Result_Length);
         Strncpy (Out_String.all'Address, Result_Ptr, Result_Length);
         return Out_String;
      end Update_Path;

   --  Start of processing for Add_Default_Search_Dirs

   begin
      --  After the locations specified on the command line, the next places
      --  to look for files are the directories specified by the appropriate
      --  environment variable. Get this value, extract the directory names
      --  and store in the tables.

      for Additional_Source_Dir in False .. True loop

         if Additional_Source_Dir then
            Search_Path := To_Canonical_Path_Spec
              (Getenv ("ADA_INCLUDE_PATH").all);
         else
            Search_Path := To_Canonical_Path_Spec
              (Getenv ("ADA_OBJECTS_PATH").all);
         end if;

         Get_Next_Dir_In_Path_Init (Search_Path);
         loop
            Search_Dir := Get_Next_Dir_In_Path (Search_Path);
            exit when Search_Dir = null;
            Add_Search_Dir (Search_Dir, Additional_Source_Dir);
         end loop;
      end loop;

      --  For WIN32 systems, look for any system libraries defined in
      --  the registry.  These are added to both source and object
      --  directories.

      Search_Path := String_Access (Get_Libraries_From_Registry);
      Get_Next_Dir_In_Path_Init (Search_Path);
      loop
         Search_Dir := Get_Next_Dir_In_Path (Search_Path);
         exit when Search_Dir = null;
         Add_Search_Dir (Search_Dir, False);
         Add_Search_Dir (Search_Dir, True);
      end loop;

      --  The last place to look are the defaults

      Search_Path := String_Access (Update_Path (Include_Dir_Default_Name));
      Get_Next_Dir_In_Path_Init (Search_Path);
      loop
         Search_Dir := Get_Next_Dir_In_Path (Search_Path);
         exit when Search_Dir = null;
         Add_Search_Dir (Search_Dir, True);
      end loop;

      Search_Path := String_Access (Update_Path (Object_Dir_Default_Name));
      Get_Next_Dir_In_Path_Init (Search_Path);
      loop
         Search_Dir := Get_Next_Dir_In_Path (Search_Path);
         exit when Search_Dir = null;
         Add_Search_Dir (Search_Dir, False);
      end loop;

   end Add_Default_Search_Dirs;

   -------------------------------
   -- Nb_Dir_In_Src_Search_Path --
   -------------------------------

   function Nb_Dir_In_Src_Search_Path return Natural is
   begin
      if Opt.Look_In_Primary_Dir then
         return Src_Search_Directories.Last -  Primary_Directory + 1;
      else
         return Src_Search_Directories.Last -  Primary_Directory;
      end if;
   end Nb_Dir_In_Src_Search_Path;

   ----------------------------
   -- Dir_In_Src_Search_Path --
   ----------------------------

   function Dir_In_Src_Search_Path (Position : Natural) return String_Ptr is
   begin
      if Opt.Look_In_Primary_Dir then
         return
           Src_Search_Directories.Table (Primary_Directory + Position - 1);
      else
         return Src_Search_Directories.Table (Primary_Directory + Position);
      end if;
   end Dir_In_Src_Search_Path;

   -------------------------------
   -- Nb_Dir_In_Obj_Search_Path --
   -------------------------------

   function Nb_Dir_In_Obj_Search_Path return Natural is
   begin
      if Opt.Look_In_Primary_Dir then
         return Lib_Search_Directories.Last -  Primary_Directory + 1;
      else
         return Lib_Search_Directories.Last -  Primary_Directory;
      end if;
   end Nb_Dir_In_Obj_Search_Path;

   ----------------------------
   -- Dir_In_Obj_Search_Path --
   ----------------------------

   function Dir_In_Obj_Search_Path (Position : Natural) return String_Ptr is
   begin
      if Opt.Look_In_Primary_Dir then
         return
           Lib_Search_Directories.Table (Primary_Directory + Position - 1);
      else
         return Lib_Search_Directories.Table (Primary_Directory + Position);
      end if;
   end Dir_In_Obj_Search_Path;

   -------------------
   -- Lib_File_Name --
   -------------------

   function Lib_File_Name
     (Source_File : File_Name_Type)
      return        File_Name_Type
   is
      Fptr : Natural;
      --  Pointer to location to set extension in place

   begin
      Get_Name_String (Source_File);
      Fptr := Name_Len + 1;

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Fptr := J;
            exit;
         end if;
      end loop;

      Name_Buffer (Fptr) := '.';
      Name_Buffer (Fptr + 1 .. Fptr + ALI_Suffix'Length) := ALI_Suffix.all;
      Name_Buffer (Fptr + ALI_Suffix'Length + 1) := Ascii.NUL;
      Name_Len := Fptr + ALI_Suffix'Length;
      return Name_Find;
   end Lib_File_Name;

   ------------------------
   -- Library_File_Stamp --
   ------------------------

   function Library_File_Stamp (N : File_Name_Type) return Time_Stamp_Type is
   begin
      return File_Stamp (Find_File (N, Library));
   end Library_File_Stamp;

   ----------------
   -- More_Files --
   ----------------

   function More_Files return Boolean is
   begin
      return (Current_File_Name_Index < Number_File_Names);
   end More_Files;

   --------------------
   -- More_Lib_Files --
   --------------------

   function More_Lib_Files return Boolean is
   begin
      pragma Assert (In_Binder);
      return More_Files;
   end More_Lib_Files;

   -----------------------
   -- More_Source_Files --
   -----------------------

   function More_Source_Files return Boolean is
   begin
      pragma Assert (In_Compiler or else In_Make);
      return More_Files;
   end More_Source_Files;

   --------------------
   -- Next_Main_File --
   --------------------

   function Next_Main_File return File_Name_Type is
      File_Name : String_Ptr;
      Dir_Name  : String_Ptr;
      Fptr      : Natural;

   begin
      Current_File_Name_Index := Current_File_Name_Index + 1;

      --  Fatal error if no more files (use More_Files to check)

      pragma Assert (Current_File_Name_Index <= Number_File_Names);

      --  Otherwise return name of the file

      File_Name := File_Names (Current_File_Name_Index);
      Fptr := File_Name'First;

      for J in reverse File_Name'Range loop
         if File_Name (J) = Directory_Separator
           or else File_Name (J) = '/'
         then
            if J = File_Name'Last then
               Fail ("File name missing");
            end if;

            Fptr := J + 1;
            exit;
         end if;
      end loop;

      --  Save name of directory in which main unit resides for use in
      --  locating other units

      Dir_Name := new String'(File_Name (File_Name'First .. Fptr - 1));

      if In_Compiler then
         Src_Search_Directories.Table (Primary_Directory) := Dir_Name;

      elsif In_Make then
         Src_Search_Directories.Table (Primary_Directory) := Dir_Name;

      elsif In_Binder then
         Dir_Name := Normalize_Directory_Name (Dir_Name.all);
         Lib_Search_Directories.Table (Primary_Directory) := Dir_Name;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      Name_Len := File_Name'Last - Fptr + 1;
      Name_Buffer (1 .. Name_Len) := File_Name (Fptr .. File_Name'Last);

      Current_Main := File_Name_Type (Name_Find);

      --  In the gnatmake case, the main file may have not have the
      --  extension. Try ".adb" first then ".ads"

      if In_Make then
         declare
            Orig_Main : File_Name_Type := Current_Main;

         begin
            if Strip_Suffix (Orig_Main) = Orig_Main then
               Current_Main := Append_Suffix_To_File_Name (Orig_Main, ".adb");

               if Full_Source_Name (Current_Main) = No_File then
                  Current_Main :=
                    Append_Suffix_To_File_Name (Orig_Main, ".ads");

                  if Full_Source_Name (Current_Main) = No_File then
                     Current_Main := Orig_Main;
                  end if;
               end if;
            end if;
         end;
      end if;

      return Current_Main;
   end Next_Main_File;

   ------------------------
   -- Next_Main_Lib_File --
   ------------------------

   function Next_Main_Lib_File return File_Name_Type is
   begin
      pragma Assert (In_Binder);
      return Next_Main_File;
   end Next_Main_Lib_File;

   ----------------------
   -- Next_Main_Source --
   ----------------------

   function Next_Main_Source return File_Name_Type is
      Main_File : File_Name_Type := Next_Main_File;

   begin
      pragma Assert (In_Compiler or else In_Make);
      return Main_File;
   end Next_Main_Source;

   --------------------------------------
   -- Get_Primary_Src_Search_Directory --
   --------------------------------------

   function Get_Primary_Src_Search_Directory return String_Ptr is
   begin
      return Src_Search_Directories.Table (Primary_Directory);
   end Get_Primary_Src_Search_Directory;

   ------------------------
   -- Set_Main_File_Name --
   ------------------------

   procedure Set_Main_File_Name (Name : String) is
   begin
      Number_File_Names := Number_File_Names + 1;
      File_Names (Number_File_Names) := new String'(Name);
   end Set_Main_File_Name;

   ------------------------------
   -- Normalize_Directory_Name --
   ------------------------------

   function Normalize_Directory_Name (Directory : String) return String_Ptr is
      Result : String_Ptr;

   begin
      if Directory'Length = 0 then
         Result := new String'(Hostparm.Normalized_CWD);

      elsif Directory (Directory'Last) = Directory_Separator
        or else Directory (Directory'Last) = '/'
      then
         Result := new String'(Directory);
      else
         Result := new String (1 .. Directory'Length + 1);
         Result (1 .. Directory'Length) := Directory;
         Result (Directory'Length + 1) := Directory_Separator;
      end if;

      return Result;
   end Normalize_Directory_Name;

   ---------------------
   -- Number_Of_Files --
   ---------------------

   function Number_Of_Files return Int is
   begin
      return Number_File_Names;
   end Number_Of_Files;

   --------------------------
   -- OS_Time_To_GNAT_Time --
   --------------------------

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type is
      GNAT_Time : Time_Stamp_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (T, Y, Mo, D, H, Mn, S);
      Make_Time_Stamp
        (Year    => Nat (Y),
         Month   => Nat (Mo),
         Day     => Nat (D),
         Hour    => Nat (H),
         Minutes => Nat (Mn),
         Seconds => Nat (S),
         TS      => GNAT_Time);

      return GNAT_Time;
   end OS_Time_To_GNAT_Time;

   -----------------------
   -- Read_Library_Info --
   -----------------------

   function Read_Library_Info
     (Lib_File  : File_Name_Type;
      Fatal_Err : Boolean := False)
      return      Text_Buffer_Ptr
   is
      Lib_FD : File_Descriptor;
      --  The file descriptor for the current library file. A negative value
      --  indicates failure to open the specified source file.

      Text : Text_Buffer_Ptr;
      --  Allocated text buffer.

   begin
      Current_Full_Lib_Name := Find_File (Lib_File, Library);
      Current_Full_Obj_Name := Object_File_Name (Current_Full_Lib_Name);

      if Current_Full_Lib_Name = No_File then
         if Fatal_Err then
            Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));
         else
            Current_Full_Obj_Stamp := Empty_Time_Stamp;
            return null;
         end if;
      end if;

      Get_Name_String (Current_Full_Lib_Name);
      Name_Buffer (Name_Len + 1) := Ascii.NUL;

      --  Open the library FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.

      Lib_FD := Open_Read (Name_Buffer'Address, Binary);

      if Lib_FD = Invalid_FD then
         if Fatal_Err then
            Fail ("Cannot open: ",  Name_Buffer (1 .. Name_Len));
         else
            Current_Full_Obj_Stamp := Empty_Time_Stamp;
            return null;
         end if;
      end if;

      --  Check for object file consistency if requested

      if Opt.Check_Object_Consistency then
         Current_Full_Lib_Stamp := File_Stamp (Current_Full_Lib_Name);
         Current_Full_Obj_Stamp := File_Stamp (Current_Full_Obj_Name);

         if Current_Full_Obj_Stamp (1) = ' ' then

            --  When the library is readonly, always assume that
            --  the object is consistent.

            if Is_Readonly_Library (Current_Full_Lib_Name) then
               Current_Full_Obj_Stamp := Current_Full_Lib_Stamp;

            elsif Fatal_Err then
               Get_Name_String (Current_Full_Obj_Name);
               Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));

            else
               Current_Full_Obj_Stamp := Empty_Time_Stamp;
               return null;
            end if;
         end if;

         --  Object file exists, compare object and ALI time stamps

         if Current_Full_Lib_Stamp > Current_Full_Obj_Stamp then
            if Fatal_Err then
               Get_Name_String (Current_Full_Obj_Name);
               Fail ("Bad time stamp: ", Name_Buffer (1 .. Name_Len));
            else
               Current_Full_Obj_Stamp := Empty_Time_Stamp;
               return null;
            end if;
         end if;
      end if;

      --  Read data from the file

      declare
         Len : Integer := Integer (File_Length (Lib_FD));
         --  Length of source file text. If it doesn't fit in an integer
         --  we're probably stuck anyway (>2 gigs of source seems a lot!)

         Actual_Len : Integer := 0;

         Lo : Text_Ptr := 0;
         --  Low bound for allocated text buffer

         Hi : Text_Ptr := Text_Ptr (Len);
         --  High bound for allocated text buffer. Note length is Len + 1
         --  which allows for extra EOF character at the end of the buffer.

      begin
         --  Allocate text buffer. Note extra character at end for EOF

         Text := new Text_Buffer (Lo .. Hi);

         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Lib_FD, Text (Hi)'Address, Len);
            Hi := Hi + Text_Ptr (Actual_Len);
            exit when Actual_Len = Len or Actual_Len <= 0;
         end loop;

         Text (Hi) := EOF;
      end;

      --  Read is complete, close file and we are done

      Close (Lib_FD);
      return Text;

   end Read_Library_Info;

   ----------------------
   -- Read_Source_File --
   ----------------------

   procedure Read_Source_File
     (N   : File_Name_Type;
      Lo  : in Source_Ptr;
      Hi  : out Source_Ptr;
      Src : out Source_Buffer_Ptr)
   is
      Source_File_FD : File_Descriptor;
      --  The file descriptor for the current source file. A negative value
      --  indicates failure to open the specified source file.

      Len : Integer;
      --  Length of file. Assume no more than 2 gigabytes of source!

      Actual_Len : Integer;

   begin
      Current_Full_Source_Name  := Find_File (N, Source);
      Current_Full_Source_Stamp := File_Stamp (Current_Full_Source_Name);

      if Current_Full_Source_Name = No_File then

         --  If we were trying to access the main file and we could not
         --  find it we have an error.

         if N = Current_Main then
            Get_Name_String (N);
            Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));
         end if;

         Src := null;
         return;
      end if;

      Get_Name_String (Current_Full_Source_Name);
      Name_Buffer (Name_Len + 1) := Ascii.NUL;

      --  Open the source FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.

      Source_File_FD := Open_Read (Name_Buffer'Address, Binary);

      if Source_File_FD = Invalid_FD then
         Src := null;
         return;
      end if;

      --  Prepare to read data from the file

      Len := Integer (File_Length (Source_File_FD));

      --  Set Hi so that length is one more than the physical length,
      --  allowing for the extra EOF character at the end of the buffer

      Hi := Lo + Source_Ptr (Len);

      --  Do the actual read operation

      declare
         subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
         --  Physical buffer allocated

         type Actual_Source_Ptr is access Actual_Source_Buffer;
         --  This is the pointer type for the physical buffer allocated

         Actual_Ptr : Actual_Source_Ptr := new Actual_Source_Buffer;
         --  And this is the actual physical buffer

      begin
         --  Allocate source buffer, allowing extra character at end for EOF

         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Source_File_FD, Actual_Ptr (Hi)'Address, Len);
            Hi := Hi + Source_Ptr (Actual_Len);
            exit when Actual_Len = Len or Actual_Len <= 0;
         end loop;

         Actual_Ptr (Hi) := EOF;

         --  Now we need to work out the proper virtual origin pointer to
         --  return. This is exactly Actual_Ptr (0)'Address, but we have
         --  to be careful to suppress checks to compute this address.

         declare
            pragma Suppress (All_Checks);

            function To_Source_Buffer_Ptr is new
              Unchecked_Conversion (Address, Source_Buffer_Ptr);

         begin
            Src := To_Source_Buffer_Ptr (Actual_Ptr (0)'Address);
         end;
      end;

      --  Read is complete, get time stamp and close file and we are done

      Close (Source_File_FD);

   end Read_Source_File;

   ---------------------
   -- Smart_Find_File --
   ---------------------

   function Smart_Find_File
     (N : File_Name_Type;
      T : File_Type)
      return File_Name_Type
   is
      Full_File_Name : File_Name_Type;

   begin
      if not File_Cache_Enabled then
         return Find_File (N, T);
      end if;

      Full_File_Name := File_Name_Hash_Table.Get (N);

      if Full_File_Name = No_File then
         Full_File_Name := Find_File (N, T);
         File_Name_Hash_Table.Set (N, Full_File_Name);
      end if;

      return Full_File_Name;
   end Smart_Find_File;

   ----------------------
   -- Smart_File_Stamp --
   ----------------------

   function Smart_File_Stamp
     (N    : File_Name_Type;
      T    : File_Type)
      return Time_Stamp_Type
   is
      Time_Stamp : Time_Stamp_Type;

   begin
      if not File_Cache_Enabled then
         return File_Stamp (Find_File (N, T));
      end if;

      Time_Stamp := File_Stamp_Hash_Table.Get (N);

      if Time_Stamp (1) = ' ' then
         Time_Stamp := File_Stamp (Smart_Find_File (N, T));
         File_Stamp_Hash_Table.Set (N, Time_Stamp);
      end if;

      return Time_Stamp;
   end Smart_File_Stamp;

   ----------------------
   -- Source_File_Data --
   ----------------------

   procedure Source_File_Data (Cache : Boolean) is
   begin
      File_Cache_Enabled := Cache;
   end Source_File_Data;

   -----------------------
   -- Source_File_Stamp --
   -----------------------

   function Source_File_Stamp (N : File_Name_Type) return Time_Stamp_Type is
   begin
      return Smart_File_Stamp (N, Source);
   end Source_File_Stamp;

   ---------------------
   -- Strip_Directory --
   ---------------------

   function Strip_Directory (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      declare
         S : String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Fptr : Natural := S'First;

      begin
         for J in reverse S'Range loop
            if S (J) = Directory_Separator
              or else S (J) = '/'
            then
               Fptr := J + 1;
               exit;
            end if;
         end loop;

         if Fptr = S'First then
            return Name;
         end if;

         Name_Buffer (1 .. S'Last - Fptr + 1) := S (Fptr .. S'Last);
         Name_Len :=  S'Last - Fptr + 1;
         return Name_Find;
      end;
   end Strip_Directory;

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            return Name_Enter;
         end if;
      end loop;

      return Name;
   end Strip_Suffix;

   -----------------------
   -- Stub_Output_Start --
   -----------------------

   --  For now does nothing, should process -o switch ???

   procedure Stub_Output_Start is
   begin
      null;
   end Stub_Output_Start;

   ----------------------
   -- Stub_Output_Stop --
   ----------------------

   --  For now does nothing, should process -o switch ???

   procedure Stub_Output_Stop is
   begin
      null;
   end Stub_Output_Stop;

   ---------------------
   -- C_String_Length --
   ---------------------

   function C_String_Length (S : Address) return Integer is
      function Strlen (S : Address) return Integer;
      pragma Import (C, Strlen, "strlen");

   begin
      if S = Null_Address then
         return 0;
      else
         return Strlen (S);
      end if;
   end C_String_Length;

   ---------------------------
   -- To_Canonical_Dir_Spec --
   ---------------------------

   function To_Canonical_Dir_Spec
     (Host_Dir     : String;
      Prefix_Style : Boolean)
      return         String_Access
   is
      function To_Canonical_Dir_Spec
        (Host_Dir    : Address;
         Prefix_Flag : Integer)
         return        Address;
      pragma Import (C, To_Canonical_Dir_Spec, "to_canonical_dir_spec");

      C_Host_Dir      : String (1 .. Host_Dir'Length + 1);
      Canonical_Dir_Addr : Address;
      Canonical_Dir_Len  : Integer;

   begin
      C_Host_Dir (1 .. Host_Dir'Length) := Host_Dir;
      C_Host_Dir (C_Host_Dir'Last)      := Ascii.NUL;

      if Prefix_Style then
         Canonical_Dir_Addr := To_Canonical_Dir_Spec (C_Host_Dir'Address, 1);
      else
         Canonical_Dir_Addr := To_Canonical_Dir_Spec (C_Host_Dir'Address, 0);
      end if;
      Canonical_Dir_Len := C_String_Length (Canonical_Dir_Addr);

      if Canonical_Dir_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Canonical_Dir_Addr, Canonical_Dir_Len);
      end if;

   exception
      when others =>
         Fail ("erroneous directory spec: ", Host_Dir);
         return null;
   end To_Canonical_Dir_Spec;

   ----------------------------
   -- To_Canonical_File_Spec --
   ----------------------------

   function To_Canonical_File_Spec
     (Host_File : String)
      return      String_Access
   is
      function To_Canonical_File_Spec (Host_File : Address) return Address;
      pragma Import (C, To_Canonical_File_Spec, "to_canonical_file_spec");

      C_Host_File      : String (1 .. Host_File'Length + 1);
      Canonical_File_Addr : Address;
      Canonical_File_Len  : Integer;

   begin
      C_Host_File (1 .. Host_File'Length) := Host_File;
      C_Host_File (C_Host_File'Last)      := Ascii.NUL;

      Canonical_File_Addr := To_Canonical_File_Spec (C_Host_File'Address);
      Canonical_File_Len  := C_String_Length (Canonical_File_Addr);

      if Canonical_File_Len = 0 then
         return null;
      else
         return To_Path_String_Access
                  (Canonical_File_Addr, Canonical_File_Len);
      end if;

   exception
      when others =>
         Fail ("erroneous file spec: ", Host_File);
         return null;
   end To_Canonical_File_Spec;

   ----------------------------
   -- To_Canonical_Path_Spec --
   ----------------------------

   function To_Canonical_Path_Spec
     (Host_Path : String)
      return      String_Access
   is
      function To_Canonical_Path_Spec (Host_Path : Address) return Address;
      pragma Import (C, To_Canonical_Path_Spec, "to_canonical_path_spec");

      C_Host_Path         : String (1 .. Host_Path'Length + 1);
      Canonical_Path_Addr : Address;
      Canonical_Path_Len  : Integer;

   begin
      C_Host_Path (1 .. Host_Path'Length) := Host_Path;
      C_Host_Path (C_Host_Path'Last)      := Ascii.NUL;

      Canonical_Path_Addr := To_Canonical_Path_Spec (C_Host_Path'Address);
      Canonical_Path_Len  := C_String_Length (Canonical_Path_Addr);

      --  Return a null string (vice a null) for zero length paths, for
      --  compatibility with getenv().

      return To_Path_String_Access (Canonical_Path_Addr, Canonical_Path_Len);

   exception
      when others =>
         Fail ("erroneous path spec: ", Host_Path);
         return null;
   end To_Canonical_Path_Spec;

   ---------------------------
   -- To_Host_Dir_Spec --
   ---------------------------

   function To_Host_Dir_Spec
     (Canonical_Dir : String;
      Prefix_Style  : Boolean)
      return          String_Access
   is
      function To_Host_Dir_Spec
        (Canonical_Dir : Address;
         Prefix_Flag   : Integer)
         return          Address;
      pragma Import (C, To_Host_Dir_Spec, "to_host_dir_spec");

      C_Canonical_Dir : String (1 .. Canonical_Dir'Length + 1);
      Host_Dir_Addr   : Address;
      Host_Dir_Len    : Integer;

   begin
      C_Canonical_Dir (1 .. Canonical_Dir'Length) := Canonical_Dir;
      C_Canonical_Dir (C_Canonical_Dir'Last)      := Ascii.NUL;

      if Prefix_Style then
         Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 1);
      else
         Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 0);
      end if;
      Host_Dir_Len := C_String_Length (Host_Dir_Addr);

      if Host_Dir_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Host_Dir_Addr, Host_Dir_Len);
      end if;
   end To_Host_Dir_Spec;

   ----------------------------
   -- To_Host_File_Spec --
   ----------------------------

   function To_Host_File_Spec
     (Canonical_File : String)
      return           String_Access
   is
      function To_Host_File_Spec (Canonical_File : Address) return Address;
      pragma Import (C, To_Host_File_Spec, "to_host_file_spec");

      C_Canonical_File      : String (1 .. Canonical_File'Length + 1);
      Host_File_Addr : Address;
      Host_File_Len  : Integer;

   begin
      C_Canonical_File (1 .. Canonical_File'Length) := Canonical_File;
      C_Canonical_File (C_Canonical_File'Last)      := Ascii.NUL;

      Host_File_Addr := To_Host_File_Spec (C_Canonical_File'Address);
      Host_File_Len  := C_String_Length (Host_File_Addr);

      if Host_File_Len = 0 then
         return null;
      else
         return To_Path_String_Access
                  (Host_File_Addr, Host_File_Len);
      end if;
   end To_Host_File_Spec;

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer)
      return      String_Access
   is
      subtype Path_String is String (1 .. Path_Len);
      type    Path_String_Access is access Path_String;

      function Address_To_Access is new
        Unchecked_Conversion (Source => Address,
                              Target => Path_String_Access);

      Path_Access : Path_String_Access := Address_To_Access (Path_Addr);

      Return_Val  : String_Access;

   begin
      Return_Val := new String (1 .. Path_Len);

      for J in 1 .. Path_Len loop
         Return_Val (J) := Path_Access (J);
      end loop;

      return Return_Val;
   end To_Path_String_Access;

   -----------------
   -- Tree_Create --
   -----------------

   procedure Tree_Create is
      Dot_Index : Natural;

   begin
      pragma Assert (In_Compiler);
      Get_Name_String (Current_Main);

      Dot_Index := 0;
      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Dot_Index := J;
            exit;
         end if;
      end loop;

      --  Should be impossible to not have an extension

      if Dot_Index = 0 then
         pragma Assert (False);
         raise Program_Error;
      end if;

      --  Change *.ads to *.ats and *.adb to *.atb

      Name_Buffer (Dot_Index + 2) := 't';
      Name_Buffer (Dot_Index + 4) := Ascii.NUL;
      Name_Len := Dot_Index + 3;
      Create_File_And_Check (Output_FD, Binary);

      Tree_Write_Initialize (Output_FD);
   end Tree_Create;

   ----------------
   -- Tree_Close --
   ----------------

   procedure Tree_Close is
   begin
      pragma Assert (In_Compiler);
      Tree_Write_Terminate;
      Close (Output_FD);
   end Tree_Close;

   -----------------------
   -- Write_Binder_Info --
   -----------------------

   procedure Write_Binder_Info (Info : String) is
   begin
      pragma Assert (In_Binder);
      Write_With_Check (Info'Address, Info'Length);
      Write_With_Check (EOL'Address, 1);
   end Write_Binder_Info;

   ------------------------
   -- Write_Library_Info --
   ------------------------

   procedure Write_Library_Info (Info : String) is
   begin
      pragma Assert (In_Compiler);
      Write_With_Check (Info'Address, Info'Length);
      Write_With_Check (EOL'Address, 1);
   end Write_Library_Info;

   -----------------------
   -- Find_Program_Name --
   -----------------------

   procedure Find_Program_Name is
      Command_Name : String (1 .. Len_Arg (0));
      Cindex1 : Integer := Command_Name'First;
      Cindex2 : Integer := Command_Name'Last;

   begin
      Fill_Arg (Command_Name'Address, 0);

      --  The program name might be specified by a full path name. However,
      --  we don't want to print that all out in an error message, so the
      --  path might need to be stripped away. In addition to the default
      --  directory_separator allow the '/' to act as separator since this
      --  is allowed in MS-DOS, Windows 95/NT, and OS2 ports. On VMS, the
      --  situation is more complicated because there are two characters to
      --  check for.

      for J in reverse Cindex1 .. Cindex2 loop
         if Command_Name (J) = Directory_Separator
           or else Command_Name (J) = '/'
           or else (Hostparm.OpenVMS
                    and then (Command_Name (J) = ']'
                              or else Command_Name (J) = ':'))
         then
            Cindex1 := J + 1;
            exit;
         end if;
      end loop;

      for J in reverse Cindex1 .. Cindex2 loop
         if Command_Name (J) = '.' then
            Cindex2 := J - 1;
            exit;
         end if;
      end loop;

      Name_Len := Cindex2 - Cindex1 + 1;
      Name_Buffer (1 .. Name_Len) := Command_Name (Cindex1 .. Cindex2);
   end Find_Program_Name;

   ------------------
   -- Program_Name --
   ------------------

   function Program_Name (Nam : String) return String_Access is
      Res : String_Access;

   begin
      --  Get the name of the current program being executed

      Find_Program_Name;

      --  Find the target prefix if any, for the cross compilation case
      --  for instance in "alpha-dec-vxworks-gcc" the target prefix is
      --  "alpha-dec-vxworks-"

      while Name_Len > 0  loop
         if Name_Buffer (Name_Len) = '-' then
            exit;
         end if;

         Name_Len := Name_Len - 1;
      end loop;

      --  Create the new program name

      Res := new String (1 .. Name_Len + Nam'Length);
      Res.all (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      Res.all (Name_Len + 1 .. Name_Len + Nam'Length) := Nam;
      return Res;
   end Program_Name;

   ------------------------
   -- Write_Program_Name --
   ------------------------

   procedure Write_Program_Name is
      Save_Buffer : String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

   begin

      Find_Program_Name;

      --  Convert the name to lower case so error messages are the same on
      --  all systems.

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) in 'A' .. 'Z' then
            Name_Buffer (J) :=
              Character'Val (Character'Pos (Name_Buffer (J)) + 32);
         end if;
      end loop;

      Write_Str (Name_Buffer (1 .. Name_Len));

      --  Restore Name_Buffer which was clobbered by the call to
      --  Find_Program_Name

      Name_Len := Save_Buffer'Last;
      Name_Buffer (1 .. Name_Len) := Save_Buffer;
   end Write_Program_Name;

   ----------------------
   -- Write_With_Check --
   ----------------------

   procedure Write_With_Check (A  : Address; N  : Integer) is
      Ignore : Boolean;

   begin
      if N = Write (Output_FD, A, N) then
         return;

      else
         Write_Str ("error: disk full writing ");
         Write_Name_Decoded (Output_File_Name);
         Write_Eol;
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Ascii.Nul;
         Delete_File (Name_Buffer'Address, Ignore);
         Exit_Program (E_Fatal);
      end if;
   end Write_With_Check;

   -----------------------
   -- Write_Xref_Output --
   -----------------------

   procedure Write_Xref_Info (Info : String; Eol : Boolean := True) is
   begin
      pragma Assert (In_Compiler);
      Write_With_Check (Info'Address, Info'Length);

      if Eol then
         Write_With_Check (Osint.EOL'Address, 1);
      end if;
   end Write_Xref_Info;

end Osint;
