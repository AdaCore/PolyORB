--  The package Options is a part of ASIS-for-GNAT
--  Copyright (C) 1997-1998 Free Software Foundation, Inc.

--  This modified version is a part of the CIAO project.
--  Copyright (C) 1999 École nationale supérieure des télécommunications.

------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--                       G N A T S T U B . O P T I O N S                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--           Copyright (c) 1997-1998, Free Software Foundation, Inc.        --
--                                                                          --
-- Gnatstub is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnatstub is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Gnatstub is distributed as a part of the ASIS implementation for GNAT    --
-- (ASIS-for-GNAT).                                                         --
--                                                                          --
-- Gnatstub  was  originally  developed  by  Alexei Kuchumov  as a part of  --
-- collaboration  between  Software  Engineering  Laboratory of  the Swiss  --
-- Federal  Institute  of  Technology  in  Lausanne, Switzerland, and  the  --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no 7SUPJ048247, funding a project  "Development of  --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- Gnatstub  is  now  maintained  by  Ada  Core  Technologies  Inc          --
-- (http://www.gnat.com).                                                   --
------------------------------------------------------------------------------

--  This package defines variables for storing CIAO options and parameters,
--  as well as some internal parameters used by CIAO.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with CIAO.Types;  use CIAO.Types;

package CIAO.Options is

   Initialized : Boolean := False;
   --  set to True by Initialize, if initialization is successful

   -------------
   -- Options --
   -------------

   Indent : constant Natural := 3;
   --  Indentation in the outputted source, is not set from command line.

   --  The default values are used as initialization expressions

   Verbose_Mode : Boolean := False;
   --  If this flag is set ON, gnatstub generates the message about itself,
   --  including ASIS/GNAT version with which it is built

   Quiet_Mode : Boolean := False;
   --  If this flag is set ON, gnatstub does not generate a confirmation
   --  in case when the samle body has successfully been created

   Overwrite_Body : Boolean := False;
   --  Should an existing body be overwritten

   Overwrite_Tree : Boolean := False;
   --  in case, if the current directory already contains the tree file with
   --  the name corresponding to Gnatstub parameter, indicates whethere or not
   --  this file should be overwritten

   Delete_Tree : Boolean := True;
   --  indicates if Gnatstub should delete the tree file cerated by itself.

   Reuse_Tree : Boolean := False;
   --  indicates if the existing tree should be reused

   Indent_Level : Positive := 3;
   Min_Indent_Level : constant Positive := 1;
   Max_Indent_Level : constant Positive := 9;
   --  identation level

   ------------------------------
   -- File and Directory names --
   ------------------------------

   File_Name : String_Ptr;
   --  The name of file that contains processed unit. This is the only one
   --  obligatory parameter. Only one unit name may be given. The name
   --  should be the name of the sourve file, it has to follow the GNAT
   --  file name conventions (in particular, it has to have .ads suffix).
   --  the file name may or may not contain the path information.

   Short_File_Name : String_Ptr;
   --  File name without directory information

   Tree_Name : String_Ptr;
   --  we need it in more, then one routine, so we define it here
   IDL_Name  : String_Ptr;
   Marshall_Spec_Name : String_Ptr;
   Marshall_Body_Name : String_Ptr;
   Impl_Spec_Name : String_Ptr;
   Impl_Body_Name : String_Ptr;

   Destination_Dir : String_Ptr;
   --  directory to put the sampler body in

   Arg_List : Argument_List_Access;
   --  -I options from the Gnatstub command line transformed into the
   --  form appropriate for calling gcc to create the tree file

   I_Options : String_Ptr := new String'("");
   I_Options_Tmp : String_Ptr := new String'("");
   --  these two variables are needed to accomulate the '-I' options
   --  during the command line processing

   ----------------------
   -- Status variables --
   ----------------------

   Tree_Exists : Boolean := False;
   --  if the tree file has been created or has been found as existing
   --  during the initialization

   Dir_Count : Natural := 0;
   --  the number of '-I' options in command line

   File_Name_Len         : Natural;
   File_Name_First       : Natural;
   File_Name_Last        : Natural;
   Short_File_Name_Len   : Natural;
   Short_File_Name_First : Natural;
   Short_File_Name_Last  : Natural;
   --  To simplify dealing with the spec file name

end CIAO.Options;
