------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ F L A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2006 Free Software Foundation, Inc.           --
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

--  This package contains the flags available for GNATDIST as well as
--  those used by GNATDIST and passed to GNATMAKE and GNATLS.

with GNAT.Table;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package XE_Flags is

   Keep_Tmp_Files     : Boolean;
   Verbose_Mode       : Boolean;
   Debug_Mode         : Boolean;
   Quiet_Mode         : Boolean;

   Readonly_Flag      : constant String_Access := new String'("-a");
   Bind_Only_Flag     : constant String_Access := new String'("-b");
   Compile_Only_Flag  : constant String_Access := new String'("-c");
   Object_Dir_Flag    : constant String_Access := new String'("-D");
   Dependencies_Flag  : constant String_Access := new String'("-d");
   Keep_Going_Flag    : constant String_Access := new String'("-k");
   Link_Only_Flag     : constant String_Access := new String'("-l");
   Output_Flag        : constant String_Access := new String'("-o");
   Project_File_Flag  : constant String_Access := new String'("-P");
   Quiet_Flag         : constant String_Access := new String'("-q");
   Verbose_Flag       : constant String_Access := new String'("-v");
   GLADE_List_Flag    : constant String_Access := new String'("-V");
   Semantic_Only_Flag : constant String_Access := new String'("-gnatc");
   Skel_Flag          : constant String_Access := new String'("-gnatzr");
   Stub_Flag          : constant String_Access := new String'("-gnatzc");
   Comp_Arg_Flag      : constant String_Access := new String'("-cargs");
   Bind_Arg_Flag      : constant String_Access := new String'("-bargs");
   Link_Arg_Flag      : constant String_Access := new String'("-largs");

   Project_File_Name_Present : Boolean := False;
   Project_File_Name         : String_Access;

   package Make_Switches is new GNAT.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100);

   package List_Switches is new GNAT.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100);

end XE_Flags;
