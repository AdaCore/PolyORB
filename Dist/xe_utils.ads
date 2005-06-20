------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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

--  This package provides several global variables, routines and
--  exceptions of general use.

with GNAT.OS_Lib; use GNAT.OS_Lib;
pragma Elaborate_All (GNAT.OS_Lib);
with XE_Types;    use XE_Types;

package XE_Utils is

   ----------------------
   -- Global Variables --
   ----------------------

   Root          : constant String := "dsa";
   Cfg_Suffix    : constant String := ".cfg";
   Obj_Suffix    : constant String := Get_Object_Suffix.all;
   Exe_Suffix    : constant String := Get_Executable_Suffix.all;
   ALI_Suffix    : constant String := ".ali";
   ADB_Suffix    : constant String := ".adb";
   ADS_Suffix    : constant String := ".ads";
   Cfg_Suffix_Id : File_Name_Type;
   Obj_Suffix_Id : File_Name_Type;
   Exe_Suffix_Id : File_Name_Type;
   ALI_Suffix_Id : File_Name_Type;
   ADB_Suffix_Id : File_Name_Type;
   ADS_Suffix_Id : File_Name_Type;
   Stub_Dir_Name : File_Name_Type;
   PWD_Id        : File_Name_Type;
   Stub_Dir      : String_Access;
   I_Current_Dir : String_Access;
   E_Current_Dir : String_Access;
   I_Stub_Dir    : String_Access;

   Part_Main_Src_Name : File_Name_Type;
   Part_Main_ALI_Name : File_Name_Type;
   Part_Main_Obj_Name : File_Name_Type;

   Part_Prj_File_Name : File_Name_Type;

   No_Args : constant Argument_List (1 .. 0) := (others => null);

   procedure Initialize;
   --  Initialize global variables, global flags, ...

   ------------------------------
   -- String and Name Handling --
   ------------------------------

   function Id (S : String) return Name_Id;
   --  Add S into name table and return id.

   function Quote (N : Name_Id) return Name_Id;
   --  Make a string containing N and return it as a Name_Id.

   function "&" (L : Name_Id; R : Name_Id) return Name_Id;
   function "&" (L : Name_Id; R : String) return Name_Id;

   function No (N : Name_Id) return Boolean;
   function Present (N : Name_Id) return Boolean;

   procedure Capitalize (S : in out String);
   function Capitalize (N : Name_Id) return Name_Id;
   function Capitalize (S : String) return String;
   --  Capitalize string or name id

   function  To_Lower   (C : Character) return Character;
   procedure To_Lower   (S : in out String);
   procedure To_Lower   (N : in out Name_Id);
   function  To_Lower   (N : Name_Id) return Name_Id;

   function Name (N : Name_Id) return Name_Id;
   --  Remove any encoded info from unit name (%s or %b)

   ------------------------------------
   -- Command Line Argument Handling --
   ------------------------------------

   procedure Scan_Dist_Arg (Argv : String);
   --  Process one command line argument

   procedure Scan_Dist_Args (Args : String);
   --  Split Args into a list of arguments according to usual shell
   --  splitting semantics, and process each argument using Scan_Dist_Arg.

   function More_Source_Files return Boolean;
   function Next_Main_Source return Name_Id;
   function Number_Of_Files return Natural;

   procedure Show_Dist_Args;
   --  Output processed command line switches (for debugging purposes)

   --------------------
   -- Error Handling --
   --------------------

   Fatal_Error         : exception;   --  Operating system error
   Scanning_Error      : exception;   --  Error during scanning
   Parsing_Error       : exception;   --  Error during parsing
   Matching_Error      : exception;   --  Error on overloading
   Partitioning_Error  : exception;   --  Error during partitionning
   Compilation_Error   : exception;   --  Error during compilation
   Usage_Error         : exception;   --  Command line error
   Not_Yet_Implemented : exception;

   type Exit_Code_Type is
     (E_Success,    -- No warnings or errors
      E_Fatal);     -- Fatal (serious) error

   procedure Exit_Program (Code : Exit_Code_Type);
   --  Call exit() with return code

   procedure Write_Missing_File (Fname : File_Name_Type);
   --  Output an error message to indicate that Fname is missing

   -----------------------
   --  Command Handling --
   -----------------------

   type File_Name_List is array (Natural range <>) of File_Name_Type;

   procedure Execute
     (Command   : String_Access;
      Arguments : Argument_List;
      Success   : out Boolean);

   procedure Build
     (Library    : File_Name_Type;
      Arguments  : Argument_List;
      Fatal      : Boolean := True;
      Silent     : Boolean := True);
   --  Execute gnat make and add gnatdist link flags

   procedure Compile
     (Source    : File_Name_Type;
      Arguments : Argument_List;
      Fatal     : Boolean := True;
      Silent    : Boolean := True);
   --  Execute gnat compile and add gnatdist gcc flags

   procedure List
     (Sources   : File_Name_List;
      Arguments : Argument_List;
      Output    : out File_Name_Type;
      Fatal     : Boolean := True);
   --  List source info into Output and raise Fatal Error if not
   --  successful. The user has to close Output afterwards.

end XE_Utils;
