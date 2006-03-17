------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                                F L A G S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Types; use Types;

package Flags is

   Main_Source     : Types.Name_Id := Types.No_Name;
   --  IDL source name

   Keep_TMP_Files  : Boolean       := False;
   --  True when we want to keep temporary files ganerated durin the
   --  compilation process

   Print_On_Stdout : Boolean       := False;
   --  True when we want to generate sources in the standard output

   Print_Full_Tree : Boolean       := False;
   --  Output tree

   Preprocess_Only : Boolean       := False;
   --  True when we only preprocess the IDL source file and output it

   Compile_Only    : Boolean       := False;
   --  True when we only compile the IDL source file and exit

   D_Scopes        : Boolean       := False;

   --  Preprocessor arguments (including -I...)
   CPP_Arg_Values : GNAT.OS_Lib.Argument_List (1 .. 64);
   CPP_Arg_Count  : Natural := 0;

   --  IAC search path (for imports and for preprocessor)
   IAC_Search_Paths : GNAT.OS_Lib.Argument_List (1 .. 64);
   IAC_Search_Count : Natural := 0;

   --  The output directory
   Output_Directory : String_Ptr := null;

   procedure Add_CPP_Flag (S : String);
   --  Add argument S to the preprocessor flags

   procedure Add_IAC_Search_Path (S : String);
   --  Add argument S to the search path

   procedure Scan_Flags;
   --  Scan arguments from command line and update flags above

end Flags;
