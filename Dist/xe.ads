------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                                   X E                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            1.23                              --
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
with Table;
with Opt;
with ALI;    use ALI;
with Types;  use Types;
package XE is

   --  Several names are already predefined. For each of these names, a key
   --  is associated in the hash table. This allows to retrieve the nature
   --  of the name and especially its type. The key (an integer) is in
   --  one of the following ranges and therefore, the name corresponds to
   --  the image of an element in the enumeration type.

   --------------
   --  Keyword --
   --------------

   type Token_Type is
      (Tok_Unknown,
       Tok_String_Literal,  -- (1)  string literal
       Tok_Identifier,      -- (2)  identifer
       Tok_Dot,             -- (3)  .
       Tok_Apostrophe,      -- (4)  '
       Tok_Left_Paren,      -- (5)  (
       Tok_Right_Paren,     -- (6)  )
       Tok_Comma,           -- (7)  ,
       Tok_Colon_Equal,     -- (8)  :=
       Tok_Colon,           -- (9)  :
       Tok_Configuration,   -- (10) CONFIGURATION
       Tok_Pragma,          -- (11) PRAGMA
       Tok_Procedure,       -- (12) PROCEDURE
       Tok_Is,              -- (13) IS
       Tok_In,              -- (14) IN
       Tok_For,             -- (15) FOR
       Tok_Use,             -- (16) USE
       Tok_Function,        -- (17) FUNCTION
       Tok_End,             -- (18) END
       Tok_Begin,           -- (19) BEGIN
       Tok_Null,            -- (20) NULL
       Tok_Semicolon,       -- (21) ;
       Tok_Arrow,           -- (22) =>
       Tok_EOF              -- (23) end of file
       );

   Tkn_Wrong : constant Int := 100;
   Tkn_First : constant Int := Tkn_Wrong  + 1;
   Tkn_Last  : constant Int := Tkn_Wrong  + 23;
   --  Should match Token_Type length

   type Tkn_Type is new Int range Tkn_Wrong .. Tkn_Last;

   Wrong_Token : constant Tkn_Type := Tkn_Type'First;
   First_Token : constant Tkn_Type := Tkn_Type'Succ (Wrong_Token);
   Last_Token  : constant Tkn_Type := Tkn_Type'Last;

   type Token_List_Type is array (Positive range <>) of Token_Type;

   function  Get_Token (N : Name_Id) return Token_Type;
   procedure Set_Token (N : String; T : Token_Type);

   Reserved  : array (Token_Type) of Boolean := (others => False);

   ----------------
   -- Attributes --
   ----------------

   type Attribute_Type is
      (Att_Unknown,
       Att_Host,           --  (1) Host
       Att_Storage_Dir     --  (2) Storage directory
       );

   Attr_Wrong : constant Int := 200;
   Attr_First : constant Int := Attr_Wrong + 1;
   Attr_Last  : constant Int := Attr_Wrong + 2;
   --  Should match Attribute_Type length

   type Attr_Type is new Int range Attr_Wrong .. Attr_Last;

   Wrong_Attribute : constant Attr_Type := Attr_Type'First;
   First_Attribute : constant Attr_Type := Attr_Type'Succ (Wrong_Attribute);
   Last_Attribute  : constant Attr_Type := Attr_Type'Last;

   function  Get_Attribute (N : Name_Id) return Attribute_Type;
   procedure Set_Attribute (N : String; A : Attribute_Type);

   -------------
   -- Pragmas --
   -------------

   type Pragma_Type is
      (Pgm_Unknown,
       Pgm_Starter         --  (1) Starter
       );

   Prag_Wrong : constant Int := 300;
   Prag_First : constant Int := Prag_Wrong + 1;
   Prag_Last  : constant Int := Prag_Wrong + 1;
   --  Should match Pragma_Type length

   type Pragma_Id is new Int range Prag_Wrong .. Prag_Last;

   Wrong_Pragma : constant Pragma_Id := Pragma_Id'First;
   First_Pragma : constant Pragma_Id := Pragma_Id'Succ (Wrong_Pragma);
   Last_Pragma  : constant Pragma_Id := Pragma_Id'Last;

   function  Get_Pragma (N : Name_Id) return Pragma_Type;
   procedure Set_Pragma (N : String; P : Pragma_Type);

   type Starter_Method_Type is (Ada_Starter, Shell_Starter, None_Starter);

   Starter_Method : Starter_Method_Type := Shell_Starter;

   ---------------------
   -- Predefined_Type --
   ---------------------

   type Predefined_Type is
      (Pre_Type_Unknown,
       Pre_Type_Partition       --  (1) Partition
       );

   Pre_Type_Wrong : constant Int := 400;
   Pre_Type_First : constant Int := Pre_Type_Wrong + 1;
   Pre_Type_Last  : constant Int := Pre_Type_Wrong + 1;
   --  Should match Predefined_Type length

   type Pre_Type_Id is new Int range Pre_Type_Wrong .. Pre_Type_Last;

   Wrong_Pre_Type : constant Pre_Type_Id := Pre_Type_Id'First;
   First_Pre_Type : constant Pre_Type_Id := Pre_Type_Id'Succ (Wrong_Pre_Type);
   Last_Pre_Type  : constant Pre_Type_Id := Pre_Type_Id'Last;

   function  Get_Predefined_Type (N : Name_Id) return Predefined_Type;
   procedure Set_Predefined_Type (N : String; P : Predefined_Type);

   --------------
   -- PID_Type --
   --------------

   PID_Wrong : constant Int := 1_000_000;
   PID_Null  : constant Int := PID_Wrong + 1;
   PID_First : constant Int := PID_Null  + 1;
   PID_Last  : constant Int := 1_999_999;

   type PID_Type is new Int range PID_Wrong .. PID_Last;

   Wrong_PID : constant PID_Type := PID_Type'First;
   Null_PID  : constant PID_Type := PID_Type'Succ (Wrong_PID);
   First_PID : constant PID_Type := PID_Type'Succ (Null_PID);
   Last_PID  : constant PID_Type := PID_Type'Last;

   function  Get_PID  (N : Name_Id) return PID_Type;
   procedure Set_PID  (N : Name_Id; P : PID_Type);

   ---------------
   -- CUID_Type --
   ---------------

   CUID_Wrong : constant Int := 2_000_000;
   CUID_Null  : constant Int := CUID_Wrong + 1;
   CUID_First : constant Int := CUID_Null  + 1;
   CUID_Last  : constant Int := 2_999_999;

   type CUID_Type is new Int range CUID_Wrong .. CUID_Last;
   --  CUID = Configure Unit ID to differentiate from Unit_Id.
   --  Such units from the configuration langage can be unknown
   --  as ada units.

   Wrong_CUID : constant CUID_Type := CUID_Type'First;
   Null_CUID  : constant CUID_Type := CUID_Type'Succ (Wrong_CUID);
   First_CUID : constant CUID_Type := CUID_Type'Succ (Null_CUID);
   Last_CUID  : constant CUID_Type := CUID_Type'Last;

   function  Get_CUID  (N : Name_Id) return CUID_Type;
   procedure Set_CUID  (N : Name_Id; U : CUID_Type);

   -----------
   -- Names --
   -----------

   subtype Partition_Name_Type is Name_Id;
   No_Partition_Name : constant Partition_Name_Type := No_Name;

   subtype CUnit_Name_Type is Name_Id;
   No_CUnit_Name     : constant CUnit_Name_Type := No_Name;

   subtype Host_Name_Type is Name_Id;
   No_Host_Name      : constant Host_Name_Type := No_Name;

   type Host_Type is
      record
         Func : Boolean;
         Name : Host_Name_Type;
      end record;
   No_Host           : constant Host_Type := (False, No_Host_Name);

   subtype Storage_Dir_Name_Type is Name_Id;
   No_Storage_Dir    : constant Storage_Dir_Name_Type := No_Name;

   --  Default values
   Default_Host        : Host_Type := No_Host;
   Default_Storage_Dir : Storage_Dir_Name_Type := No_Storage_Dir;

   type Partition_Type is record
      Name            : Partition_Name_Type   := No_Partition_Name;
      Host            : Host_Type             := Default_Host;
      Storage_Dir     : Storage_Dir_Name_Type := Default_Storage_Dir;
      Main_Subprogram : Unit_Name_Type        := No_Name;
      First_Unit      : CUID_Type             := Null_CUID;
      Last_Unit       : CUID_Type             := Null_CUID;
      To_Build        : Boolean               := True;
   end record;

   package Partitions  is new Table
     (Table_Component_Type => Partition_Type,
      Table_Index_Type     => PID_Type,
      Table_Low_Bound      => First_PID,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Partition");

   type Conf_Unit_Type is record
      CUname    : CUnit_Name_Type := No_CUnit_Name;
      My_ALI    : ALI_Id;
      My_Unit   : Unit_Id;
      Partition : PID_Type        := Null_PID;
      Next      : CUID_Type       := Null_CUID;
   end record;

   package CUnit is new Table
     (Table_Component_Type => Conf_Unit_Type,
      Table_Index_Type     => CUID_Type,
      Table_Low_Bound      => First_CUID,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "CUnit");

   procedure Set_Unit_Id (N : Name_Id; U : Unit_Id);
   function  Get_Unit_Id (N : Name_Id) return Unit_Id;
   --  Return N name key if its value is in Unit_Id range, otherwise
   --  return No_Unit_Id.

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id);
   function  Get_ALI_Id (N : Name_Id) return ALI_Id;
   --  Return N name key if its value is in ALI_Id range, otherwise
   --  return No_ALI_Id.

   procedure Load_All_Units (From : Unit_Name_Type);
   --  Recursively update GNAT internal tables by downloading all Uname
   --  dependent units if available.

   procedure Add_Conf_Unit (CU : in CUnit_Name_Type; To : in PID_Type);
   --  Assign a Conf Unit to a partition. This unit is declared in the
   --  configuration file (it is not yet mapped to an ada unit).

   procedure Create_Partition
     (Name : in Partition_Name_Type);
   --  Create a new partition and store its PID in its name key.

   procedure Copy_Partition
     (Name : in Partition_Name_Type;
      Many : in Int);
   --  Create Many successive copies of partition Name.

   function Is_Set (Partition : in PID_Type) return Boolean;
   --  Some units have already been assigned to this partition.

   Configuration_File : Name_Id := No_Name;
   Configuration      : Name_Id := No_Name;
   --  Name of the configuration

   Main_Partition     : PID_Type  := Null_PID;
   --  Partition where the main procedure has been assigned.

   Main_Subprogram    : Name_Id        := No_Name;
   Main_Source_File   : File_Name_Type := No_Name;
   Main_ALI           : ALI_Id;
   Most_Recent_Stamp  : Time_Stamp_Type;
   Sources_Modified   : Boolean        := False;
   --  Several variables related to the main procedure.

   procedure Maybe_Most_Recent_Stamp (Stamp : Time_Stamp_Type);
   --  Maybe set Most_Recent_Stamp.

   Verbose_Mode       : Boolean renames Opt.Verbose_Mode;
   Quiet_Output       : Boolean renames Opt.Quiet_Output;
   No_Recompilation   : Boolean renames Opt.Dont_Execute;

   Fatal_Error        : exception;   --  Operating system error
   Scanning_Error     : exception;   --  Error during scanning
   Parsing_Error      : exception;   --  Error during parsing
   Partitioning_Error : exception;   --  Error during partitionning
   Usage_Error        : exception;   --  Command line error
   Not_Yet_Implemented : exception;

end XE;

