------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . O S _ L I B                           --
--                                                                          --
--                                 B o d y                                  --
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

with Unchecked_Conversion;
with System; use System;

package body GNAT.OS_Lib is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of a C string.  Does check for null address
   --  (returns 0).

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean;
      Pid          : out Process_Id;
      Blocking     : Boolean);
   --  Internal routine to implement the to Spawn (blocking and non blocking)
   --  routines. If Blocking is set to True then the spawn is blocking
   --  otherwise it is non blocking. In this latter case the Pid contains
   --  the process id number. The first three parameters are as in Spawn.

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer)
      return String_Access;
   --  Converts a C String to an Ada String.  Are we doing this to avoid
   --  withing Interfaces.C.Strings ???

   -----------------------------
   -- Argument_String_To_List --
   -----------------------------

   function Argument_String_To_List
     (Arg_String : String)
      return Argument_List_Access
   is
      Max_Args : Integer := Arg_String'Length;
      New_Argv : Argument_List (1 .. Max_Args);
      New_Argc : Natural := 0;
      Idx      : Integer;

   begin
      Idx := Arg_String'First;

      loop
         declare
            Quoted   : Boolean := False;
            Backqd   : Boolean := False;
            Old_Idx  : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  A vanilla space is the end of an argument

               if not Backqd and then not Quoted
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not Backqd and then not Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif not Backqd and then Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc := New_Argc + 1;
            New_Argv (New_Argc) :=
              new String'(Arg_String (Old_Idx .. Idx - 1));
            Idx := Idx + 1;
         end;

         exit when Idx > Arg_String'Last;
      end loop;

      return new Argument_List'(New_Argv (1 .. New_Argc));
   end Argument_String_To_List;

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

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Temp_File_Name)
   is
      function Get_Temp_Name (T : Address) return Address;
      pragma Import (C, Get_Temp_Name, "mktemp");

   begin
      Name := "GNAT-XXXXXX" & Ascii.NUL;

      --  Check for NULL pointer returned by C

      if Get_Temp_Name (Name'Address) = Null_Address then
         FD := -1;
      else
         FD := Create_New_File (Name'Address, Binary);
      end if;
   end Create_Temp_File;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : Address; Success : out Boolean) is
      R : Integer;

      function unlink (A : Address) return Integer;
      pragma Import (C, unlink, "unlink");

   begin
      R := unlink (Name);
      Success := (R = 0);
   end Delete_File;

   ----------------------
   -- File_Time_Stamp  --
   ----------------------

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time is
      function File_Time (FD    : File_Descriptor) return OS_Time;
      pragma Import (C, File_Time, "file_time_fd");

   begin
      return File_Time (FD);
   end File_Time_Stamp;

   ----------------------
   -- File_Time_Stamp  --
   ----------------------

   function File_Time_Stamp (Name : String) return OS_Time is

      function File_Time (Name : Address) return OS_Time;
      pragma Import (C, File_Time, "file_time_name");

      F_Name : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (Name'Length + 1)  := Ascii.NUL;
      return File_Time (F_Name'Address);
   end File_Time_Stamp;

   ----------------------------
   -- Get_Debuggable_Suffix  --
   ----------------------------

   function Get_Debuggable_Suffix return String_Access is

      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "get_debuggable_suffix_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin

      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Debuggable_Suffix;

   ----------------------------
   -- Get_Executable_Suffix  --
   ----------------------------

   function Get_Executable_Suffix return String_Access is

      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "get_executable_suffix_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin

      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Executable_Suffix;

   ------------------------
   -- Get_Object_Suffix  --
   ------------------------

   function Get_Object_Suffix return String_Access is

      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "get_object_suffix_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin

      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Object_Suffix;

   ------------
   -- Getenv --
   ------------

   function Getenv (Name : String) return String_Access is

      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "get_env_value_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Env_Value_Ptr    : Address;
      Env_Value_Length : Integer;
      F_Name           : String (1 .. Name'Length + 1);
      Result           : String_Access;

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (Name'Length + 1)  := Ascii.NUL;

      Get_Env_Value_Ptr
        (F_Name'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

      Result := new String (1 .. Env_Value_Length);

      if Env_Value_Length > 0 then
         Strncpy (Result.all'Address, Env_Value_Ptr, Env_Value_Length);
      end if;

      return Result;
   end Getenv;

   ------------
   -- GM_Day --
   ------------

   function GM_Day (Date : OS_Time) return Day_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return D;
   end GM_Day;

   -------------
   -- GM_Hour --
   -------------

   function GM_Hour (Date : OS_Time) return Hour_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return H;
   end GM_Hour;

   ---------------
   -- GM_Minute --
   ---------------

   function GM_Minute (Date : OS_Time) return Minute_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mn;
   end GM_Minute;

   --------------
   -- GM_Month --
   --------------

   function GM_Month (Date : OS_Time) return Month_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mo;
   end GM_Month;

   ---------------
   -- GM_Second --
   ---------------

   function GM_Second (Date : OS_Time) return Second_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return S;
   end GM_Second;

   --------------
   -- GM_Split --
   --------------

   procedure GM_Split
     (Date   : OS_Time;
      Year   : out Year_Type;
      Month  : out Month_Type;
      Day    : out Day_Type;
      Hour   : out Hour_Type;
      Minute : out Minute_Type;
      Second : out Second_Type)
   is
      procedure To_GM_Time
        (P_Time_T, P_Year, P_Month, P_Day, P_Hours, P_Mins, P_Secs : Address);
      pragma Import (C, To_GM_Time, "to_gm_time");

      T  : OS_Time := Date;
      Y  : Integer;
      Mo : Integer;
      D  : Integer;
      H  : Integer;
      Mn : Integer;
      S  : Integer;

   begin
      To_GM_Time (T'Address, Y'Address, Mo'Address, D'Address, H'Address,
                  Mn'Address, S'Address);
      Year   := Y + 1900;
      Month  := Mo + 1;
      Day    := D;
      Hour   := H;
      Minute := Mn;
      Second := S;
   end GM_Split;

   -------------
   -- GM_Year --
   -------------

   function GM_Year (Date : OS_Time) return Year_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Y;
   end GM_Year;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Name : String) return Boolean is

      function Is_Directory (Name : Address) return Integer;
      pragma Import (C, Is_Directory, "is_directory");

      F_Name : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (Name'Length + 1)  := Ascii.NUL;
      return Is_Directory (F_Name'Address) /= 0;
   end Is_Directory;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is

      function Is_Regular_File (Name : Address) return Integer;
      pragma Import (C, Is_Regular_File, "is_regular_file");

      F_Name : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (Name'Length + 1)  := Ascii.NUL;
      return Is_Regular_File (F_Name'Address) /= 0;
   end Is_Regular_File;

   ----------------------
   -- Is_Writable_File --
   ----------------------

   function Is_Writable_File (Name : String) return Boolean is

      function Is_Writable_File (Name : Address) return Integer;
      pragma Import (C, Is_Writable_File, "is_writable_file");

      F_Name : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (Name'Length + 1) := Ascii.NUL;
      return Is_Writable_File (F_Name'Address) /= 0;
   end Is_Writable_File;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path
     (Exec_Name : String)
      return      String_Access
   is
      function Locate_Exec_On_Path (C_Exec_Name : Address) return Address;
      pragma Import (C, Locate_Exec_On_Path, "locate_exec_on_path");

      C_Exec_Name  : String (1 .. Exec_Name'Length + 1);
      Path_Addr    : Address;
      Path_Len     : Integer;

   begin
      C_Exec_Name (1 .. Exec_Name'Length)   := Exec_Name;
      C_Exec_Name (C_Exec_Name'Last)        := Ascii.NUL;

      Path_Addr := Locate_Exec_On_Path (C_Exec_Name'Address);
      Path_Len  := C_String_Length (Path_Addr);

      if Path_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Path_Addr, Path_Len);
      end if;
   end Locate_Exec_On_Path;

   -------------------------
   -- Locate_Regular_File --
   -------------------------

   function Locate_Regular_File
     (File_Name : String;
      Path      : String)
      return      String_Access
   is
      function Locate_Regular_File
        (C_File_Name, Path_Val : Address) return Address;
      pragma Import (C, Locate_Regular_File, "locate_regular_file");

      C_File_Name  : String (1 .. File_Name'Length + 1);
      Path_Val     : String (1 .. Path'Length + 1);
      Path_Addr    : Address;
      Path_Len     : Integer;

   begin
      C_File_Name (1 .. File_Name'Length)   := File_Name;
      C_File_Name (C_File_Name'Last)        := Ascii.NUL;
      Path_Val  (1 .. Path'Length)          := Path;
      Path_Val  (Path_Val'Last)             := Ascii.NUL;

      Path_Addr := Locate_Regular_File (C_File_Name'Address, Path_Val'Address);
      Path_Len  := C_String_Length (Path_Addr);

      if Path_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Path_Addr, Path_Len);
      end if;
   end Locate_Regular_File;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List)
      return         Process_Id
   is
      Junk : Boolean;
      Pid  : Process_Id;

   begin
      Spawn_Internal (Program_Name, Args, Junk, Pid, Blocking => False);
      return Pid;
   end Non_Blocking_Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean)
   is
      Junk : Process_Id;

   begin
      Spawn_Internal (Program_Name, Args, Success, Junk, Blocking => True);
   end Spawn;

   --------------------
   -- Spawn_Internal --
   --------------------

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean;
      Pid          : out Process_Id;
      Blocking     : Boolean)
   is
      Arg_List : array (1 .. Args'Length + 2) of Address;

      Arg : String_Access;

      function Portable_Spawn (Args : Address) return Integer;
      pragma Import (C, Portable_Spawn, "portable_spawn");

      function Portable_No_Block_Spawn (Args : Address) return Process_Id;
      pragma Import (C, Portable_No_Block_Spawn, "portable_no_block_spawn");

   begin
      Arg := new String (1 .. Program_Name'Length + 1);
      Arg (1 .. Program_Name'Length) := Program_Name;
      Arg (Arg'Last)                 := Ascii.NUL;
      Arg_List (1)                   := Arg.all'Address;

      for J in 1 .. Args'Length loop
         Arg := new String (1 .. Args (J + Args'First - 1)'Length + 1);
         Arg (1 .. Arg'Last - 1) := Args (J + Args'First - 1).all;
         Arg (Arg'Last) := Ascii.NUL;
         Arg_List (J + 1) := Arg.all'Address;
      end loop;

      Arg_List (Arg_List'Last) := Null_Address;

      if Blocking then
         Pid     := Invalid_Pid;
         Success := (Portable_Spawn (Arg_List'Address) = 0);
      else
         Pid     := Portable_No_Block_Spawn (Arg_List'Address);
         Success := (Pid /= Invalid_Pid);
      end if;

   end Spawn_Internal;

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer)
      return String_Access is

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

   ------------------
   -- Wait_Process --
   ------------------

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean) is
      Status : Integer;

      function Portable_Wait (S : Address) return Process_Id;
      pragma Import (C, Portable_Wait, "portable_wait");

   begin
      Pid := Portable_Wait (Status'Address);
      Success := (Status = 0);
   end Wait_Process;

end GNAT.OS_Lib;
