----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  This package defines variables for storing CIAO options and parameters,
--  as well as some internal parameters used by CIAO.

--  $Id: //droopi/main/compilers/ciao/ciao-options.ads#3 $

with GNAT.OS_Lib;

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

   Overwrite_IDL : Boolean := False;
   --  Indicates whether an existing IDL source file should be overwritten.

   Overwrite_Tree : Boolean := False;
   --  Indicates whether an existing tree file should be overwritten.

   Reuse_Tree : Boolean := False;
   --  Indicates whether an existing tree should be reused

   Delete_Tree : Boolean := False;
   --  When CIAO creates a tree file itself, indicates whether that
   --  file should be removed after use.

   Expand : Boolean := True;
   --  Indicates whether IDL tree expansion must be performed.

   Disp_Tree : Boolean := False;
   --  Indicates whether a textual IDL tree debug dump must be
   --  performed.

   Generate : Boolean := True;
   --  Indicates whether stub code generation must be performed.

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

   Arg_List : GNAT.OS_Lib.Argument_List_Access;
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
