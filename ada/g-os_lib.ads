------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . O S _ L I B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

--  This package types and procedures for interfacing to the underlying OS.
--  It is used by the GNAT compiler and by tools associated with the GNAT
--  compiler, and therefore works for the various OS-s to which GNAT has
--  been ported.  This package will undoubtedly grow as new services are
--  needed by various tools.

--  This package tends to use fairly low-level Ada in order to not bring
--  in large portions of the RTL.  For example, functions return access
--  to string as part of avoiding functions returning unconstrained types;
--  types related to dates are defined here instead of using the types
--  from Calendar, since use of Calendar forces linking in of tasking code.

with System;

package GNAT.OS_Lib is
pragma Elaborate_Body (OS_Lib);

   type String_Access is access all String;

   ---------------------
   -- Time/Date Stuff --
   ---------------------

   --  The OS's notion of time is represented by the private type OS_Time.
   --  This is the type returned by the File_Time_Stamp functions to obtain
   --  the time stamp of a specified file. Functions and a procedure (modeled
   --  after the similar subprograms in package Calendar) are provided for
   --  extracting information from a value of this type. Although these are
   --  called GM, the intention is not that they provide GMT times in all
   --  cases but rather the actual (time-zone independent) time stamp of the
   --  file (of course in Unix systems, this *is* in GMT form).

   type OS_Time is private;

   subtype Year_Type   is Integer range 1900 .. 2099;
   subtype Month_Type  is Integer range    1 ..   12;
   subtype Day_Type    is Integer range    1 ..   31;
   subtype Hour_Type   is Integer range    0 ..   23;
   subtype Minute_Type is Integer range    0 ..   59;
   subtype Second_Type is Integer range    0 ..   59;

   function GM_Year    (Date : OS_Time) return Year_Type;
   function GM_Month   (Date : OS_Time) return Month_Type;
   function GM_Day     (Date : OS_Time) return Day_Type;
   function GM_Hour    (Date : OS_Time) return Hour_Type;
   function GM_Minute  (Date : OS_Time) return Minute_Type;
   function GM_Second  (Date : OS_Time) return Second_Type;

   procedure GM_Split
     (Date    : OS_Time;
      Year    : out Year_Type;
      Month   : out Month_Type;
      Day     : out Day_Type;
      Hour    : out Hour_Type;
      Minute  : out Minute_Type;
      Second  : out Second_Type);

   ----------------
   -- File Stuff --
   ----------------

   --  These routines give access to the open/creat/close/read/write level
   --  of I/O routines in the typical C library (these functions are not
   --  part of the ANSI C standard, but are typically available in all
   --  systems). See also package Interfaces.C_Streams for access to the
   --  stream level routines.

   type File_Descriptor is private;
   --  Corresponds to the int file handle values used in the C routines,

   Standin  : constant File_Descriptor;
   Standout : constant File_Descriptor;
   Standerr : constant File_Descriptor;
   --  File descriptors for standard input output files

   Invalid_FD : constant File_Descriptor;
   --  File descriptor returned when error in opening/creating file;

   type Mode is (Binary, Text);
   for Mode'Size use Integer'Size;
   for Mode use (Binary => 0, Text => 1);
   --  Used in all the Open and Create calls to specify if the file is to be
   --  opened in binary mode or text mode. In systems like Unix, this has no
   --  effect, but in systems capable of text mode translation, the use of
   --  Text as the mode parameter causes the system to do CR/LF translation
   --  and also to recognize the DOS end of file character on input. The use
   --  of Text where appropriate allows programs to take a portable Unix view
   --  of DOs-format files and process them appropriately.

   function Open_Read
     (Name  : System.Address;
      Fmode : Mode)
      return  File_Descriptor;
   pragma Import (C, Open_Read, "open_read");
   --  Open file Name (NUL-terminated) for reading, returning file descriptor
   --  File descriptor returned is Invalid_FD if file cannot be opened.

   function Open_Read_Write
     (Name  : System.Address;
      Fmode : Mode)
      return  File_Descriptor;
   pragma Import (C, Open_Read_Write, "open_rw");
   --  Open file Name (NUL-terminated) for both reading and writing,
   --  returning file descriptor. File descriptor returned is Invalid_FD if
   --  file cannot be opened.

   function Create_File
     (Name  : System.Address;
      Fmode : Mode)
      return  File_Descriptor;
   pragma Import (C, Create_File, "open_create");
   --  Creates new file with given name (NUL-terminated) for writing, returning
   --  file descriptor for subsequent use in Write calls. File descriptor
   --  returned is Invalid_FD if file cannot be successfully created.

   function Create_New_File
     (Name  : System.Address;
      Fmode : Mode)
      return  File_Descriptor;
   pragma Import (C, Create_New_File, "open_new");
   --  Create new file with given name (NUL-terminated) for writing,
   --  returning file descriptor for subsequent use in Write calls.  This
   --  differs from Create_File in that it fails if the file already exists.
   --  File descriptor returned is Invalid_FD if the file exists or cannot
   --  be created.

   Temp_File_Len : constant Integer := 12;
   --  Length of name returned by Create_Temp_File call (GNAT-XXXXXX & NUL)

   subtype Temp_File_Name is String (1 .. Temp_File_Len);
   --  String subtype set by Create_Temp_File

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Temp_File_Name);
   --  Create and open for writing a temporary file.  The name of the
   --  file and the File Descriptor are returned.  The File Descriptor
   --  returned is Invalid_FD in the case of failure. No mode parameter
   --  is provided. Since this is a temporary file, there is no point in
   --  doing text translation on it.

   procedure Close (FD : File_Descriptor);
   pragma Import (C, Close, "close");
   --  Close file referenced by FD

   procedure Delete_File (Name : System.Address; Success : out Boolean);
   --  Deletes a file. Name is the address of the NUL-terminated file name
   --  Success is set True or False indicating if the delete is successful.

   function Read
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer)
   return Integer;
   pragma Import (C, Read, "read");
   --  Read N bytes to address A from file referenced by FD. Returned value
   --  is count of bytes actually read, which can be less than N at EOF.

   function Write
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer)
      return Integer;
   pragma Import (C, Write, "write");
   --  Write N bytes from address A to file referenced by FD. The returned
   --  value is the number of bytes written, which can be less than N if
   --  a disk full condition was detected.

   function File_Length (FD : File_Descriptor) return Long_Integer;
   pragma Import (C, File_Length, "file_length");
   --  Get length of file from file descriptor FD

   function File_Time_Stamp (Name : String) return OS_Time;
   --  Given the name of a file, Name, obtains and returns the time stamp.
   --  This function can be used for an unopend file.

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time;
   --  Get time stamp of file from file descriptor FD

   function Is_Regular_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing
   --  regular file. Returns True if so, False otherwise.

   function Is_Directory (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of a directory.
   --  Returns True if so, False otherwise.

   function Is_Writable_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing
   --  file that is writable. Returns True if so, False otherwise.

   function Locate_Exec_On_Path
     (Exec_Name : String)
      return String_Access;
   --  Try to locate an executable whose name is given by Exec_Name in the
   --  directories listed in the environment Path.  If the Exec_Name doesn't
   --  have the executable suffix, it will be appended before the search.
   --  Otherwise works like Locate_Regular_File below.

   function Locate_Regular_File
     (File_Name : String;
      Path      : String)
      return String_Access;
   --  Try to locate a regular file whose name is given by File_Name in the
   --  directories listed in  Path.  If a file is found, its full pathname is
   --  returned; otherwise, a null pointer is returned. If the File_Name given
   --  is an absolute pathname, then Locate_Regular_File just checks that the
   --  file exists and is a regular file.  Otherwise, the Path argument is
   --  parsed according to OS conventions, and for each directory in the Path
   --  a check is made if File_Name is a relative pathname of a regular file
   --  from that directory.

   function Get_Debuggable_Suffix return String_Access;
   --  Return the debuggable suffix convention. Usually this is the same as
   --   the convention for Get_Executable_Suffix.

   function Get_Executable_Suffix return String_Access;
   --  Return the executable suffix convention.

   function Get_Object_Suffix return String_Access;
   --  Return the object suffix convention.

   ------------------
   -- Subprocesses --
   ------------------

   type Argument_List is array (Positive range <>) of String_Access;
   --  Type used for argument list in call to Spawn. The lower bound
   --  of the array should be 1, and the length of the array indicates
   --  the number of arguments.

   type Argument_List_Access is access all Argument_List;
   --  Type used to return an Argument_List without dragging in secondary
   --  stack.

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean);
   --  The first parameter of function Spawn is the full path name of the
   --  executable. The second parameter contains the arguments to be passed
   --  to the program. Success is false if the program could not be spawned
   --  or its execution completed unsuccessfully. Note that the caller will
   --  be blocked until the execution of the spawned program is complete.

   type Process_Id is private;
   --  A private type used to identify a process activated by the following
   --  non-blocking call. The only meaningful operation on this type is a
   --  comparison for equality.

   Invalid_Pid : constant Process_Id;
   --  A special value used to indicate errors, as described below.

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List)
      return         Process_Id;
   --  This is a non blocking call. The Process_Id of the spawned process
   --  is returned. Parameters are to be used as in Spawn. If Invalid_Id
   --  is returned the program could not be spawned.

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean);
   --  Wait for the completion of any of the processes created by previous
   --  calls to Non_Blocking_Spawn. The caller will be suspended until one
   --  of these processes terminates (normally or abnormally). If any of
   --  these subprocesses terminates prior to the call to Wait_Process (and
   --  has not been returned by a previous call to Wait_Process), then the
   --  call to Wait_Process is immediate. Pid identifies the process that
   --  has terminated (matching the value returned from Non_Blocking_Spawn).
   --  Success is set to True if this sub-process terminated successfully.
   --  If Pid = Invalid_Id, there were no subprocesses left to wait on.

   function Argument_String_To_List
     (Arg_String : String)
      return Argument_List_Access;
   --  Take a string that is a program and it's arguments and parse it into
   --  an Argument_List.

   -------------------
   -- Miscellaneous --
   -------------------

   function Getenv (Name : String) return String_Access;
   --  Get the value of the environment variable. Returns the empty
   --  string if the environment variable does not exist.

   procedure OS_Exit  (Status : Integer);
   pragma Import (C, OS_Exit, "os_exit");
   --  Exit to OS with given status code (program is terminated)

   procedure OS_Abort;
   pragma Import (C, OS_Abort, "abort");
   --  Exit to OS signalling an abort (traceback or other appropriate
   --  diagnostic information should be given if possible, or entry made
   --  to the debugger if that is possible).

   Directory_Separator : constant Character;
   --  The character that is used to separate parts of a pathname.

   Path_Separator      : constant Character;
   --  The character to separate paths in an environment variable value.

private
   function Get_Dirsep_Char return Character;
   pragma Import (C, Get_Dirsep_Char, "Get_Dirsep_Char");
   Directory_Separator : constant Character := Get_Dirsep_Char;

   function Get_Pathsep_Char return Character;
   pragma Import (C, Get_Pathsep_Char, "Get_Pathsep_Char");
   Path_Separator      : constant Character := Get_Pathsep_Char;

   type OS_Time is new Long_Integer;

   type File_Descriptor is new Integer;

   Standin    : constant File_Descriptor :=  0;
   Standout   : constant File_Descriptor :=  1;
   Standerr   : constant File_Descriptor :=  2;
   Invalid_FD : constant File_Descriptor := -1;

   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;

end GNAT.OS_Lib;
