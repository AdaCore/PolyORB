------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T _ S U I T E . R U N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with GNAT.Directory_Operations;
with GNAT.IO_Aux;
with GNAT.OS_Lib;
with GNAT.Regpat;

package body Test_Suite.Run is

   use Ada.Strings.Unbounded;

   use GNAT.Directory_Operations;
   use GNAT.IO_Aux;
   use GNAT.OS_Lib;
   use GNAT.Regpat;

   Executable_Suffix : String renames Get_Executable_Suffix.all;

   ---------
   -- Run --
   ---------

   function Run
     (Output           : Test_Suite_Output'Class;
      Exe              : Executable;
      Exec_In_Base_Dir : Boolean;
      First_Arg        : String;
      Item_To_Match    : Regexp_Array;
      Call_Backs       : Analyze_CB_Array;
      Timeout          : Integer)
     return Boolean
   is
      Fd : Process_Descriptor;

      Command : constant String := "./" & To_String (Exe.Command);

      Configuration_Filename : constant String := To_String (Exe.Conf);

      Arg_Length : Natural;

      Result : Expect_Match;

      Match  : Match_Array (0 .. 0);

      Test_Result : Boolean;

      Initial_Dir : constant Dir_Name_Str := Get_Current_Dir;

   begin
      --  Setting environment

      if Is_Directory (Configuration_Filename) then
         --  If no configuration filename is set,
         --  Configuration_Filename denotes a directory, which means
         --  there is no environment to set.

         Log (Output, "No environment to set, resetting POLYORB_CONF.");
         Setenv ("POLYORB_CONF", "");

      else
         if not File_Exists (Configuration_Filename)
           or else not Is_Readable_File (Configuration_Filename)
         then
            Log (Output, "Cannot read configuration file: "
                 & Configuration_Filename);
            Log (Output, "Aborting test");
            return False;
         end if;

         Log (Output, "Setting environment: " & Configuration_Filename);
         Setenv ("POLYORB_CONF", Configuration_Filename);
      end if;

      --  Test the executable actually exists

      if not Is_Regular_File (Normalize_Pathname
                                (Initial_Dir & Command & Executable_Suffix))
      then
         Log (Output, Normalize_Pathname (Initial_Dir
                & Command & Executable_Suffix) & " does not exist !");
         Log (Output, "Aborting test");

         return False;
      end if;

      --  Launch Test

      Log (Output, "Running: " & Command);

      Separator (Output);

      if Exe.Args = null then
         Arg_Length := 1;
      else
         Arg_Length := 1 + Exe.Args.all'Length;
      end if;

      declare
         Argument_List : GNAT.OS_Lib.Argument_List (1 .. Arg_Length);

      begin
         --  Compute argument list

         Argument_List (1) := new String'(First_Arg);

         if Exe.Args /= null then
            Argument_List (2 .. Argument_List'Last) := Exe.Args.all;
         end if;

         if Exec_In_Base_Dir then
            --  Change to base directory to allow the test to read its
            --  own configuration files.

            Log (Output, "Changing to test base directory "
                 & Dir_Name (Command));

            Change_Dir (Dir_Name (Command));

            --  Spawn executable

            Non_Blocking_Spawn
              (Descriptor  => Fd,
               Command     => "./" & Base_Name (Command),
               Args        => Argument_List,
               Buffer_Size => 4096,
               Err_To_Out  => True);

            Change_Dir (Initial_Dir);

         else
            --  Spawn executable

            Non_Blocking_Spawn
              (Descriptor  => Fd,
               Command     => Command,
               Args        => Argument_List,
               Buffer_Size => 4096,
               Err_To_Out  => True);
         end if;
      end;

      --  Redirect Output

      Initialize_Filter (Output);
      Add_Filter (Fd, Output_Filter'Access, GNAT.Expect.Output);

      --  Process test output

      begin
         Expect (Fd, Result, Item_To_Match, Match, Timeout);
      exception
         when GNAT.Expect.Process_Died =>

            --  The process may normally exit, or die because of an
            --  internal error. We cannot judge at this stage.

            Log (Output, "==> Process terminated abnormally <==");
            Test_Result := False;

            Close (Fd);
            return Test_Result;

         when GNAT.Expect.Invalid_Process =>

            --  The process was invalid, exit

            Log (Output, "==> Invalid process <==");
            Test_Result := False;

            Close (Fd);
            return Test_Result;
      end;

      --  Parse output

      if Integer (Result) in Item_To_Match'Range then
         Test_Result := Call_Backs (Integer (Result))
           (Expect_Out (Fd) (Match (0).First .. Match (0).Last));

      elsif Result = Expect_Timeout then
         Log (Output, "==> Time out ! <==");
         Test_Result := False;

      else
         Log (Output, "==> Unexpected output ! <==");
         Test_Result := False;
      end if;

      --  Clean up

      Close (Fd);
      return Test_Result;

   exception
      when others =>
         begin
            Close (Fd);
         exception
            when GNAT.Expect.Invalid_Process =>
               --  If we didn't successfully complete the Non_Blocking_Spawn,
               --  we want to reraise the original exception, rather than the
               --  error from Close.
               null;
         end;

         if Exec_In_Base_Dir then
            Change_Dir (Initial_Dir);
         end if;

         raise;
   end Run;

   -------------------
   -- Parse_Success --
   -------------------

   function Parse_Success (First_Arg : String) return Boolean is
      pragma Unreferenced (First_Arg);

   begin
      return True;
   end Parse_Success;

   -------------------
   -- Parse_Failure --
   -------------------

   function Parse_Failure (First_Arg : String) return Boolean is
      pragma Unreferenced (First_Arg);

   begin
      return False;
   end Parse_Failure;

end Test_Suite.Run;
