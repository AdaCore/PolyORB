------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                                   X E                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
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
       Tok_Return,          -- (23) return
       Tok_EOF,             -- (24) end of file
       Tok_Reserved         -- (25) Ada reserved keyword
       );

   Tkn_Wrong : constant Int := 100;
   Tkn_First : constant Int := Tkn_Wrong  + 1;
   Tkn_Last  : constant Int := Tkn_Wrong  + 25;
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
      (Attribute_Unknown,
       Attribute_Host,           --  (1) Host Name
       Attribute_Storage_Dir,    --  (2) Storage directory
       Attribute_Main,           --  (3) Main procedure
       Attribute_Command_Line    --  (4) Command line
       );

   Attr_Wrong : constant Int := 200;
   Attr_First : constant Int := Attr_Wrong + 1;
   Attr_Last  : constant Int := Attr_Wrong + 4;
   --  Should match Attribute_Type length

   type Attr_Type is new Int range Attr_Wrong .. Attr_Last;

   Wrong_Attribute : constant Attr_Type := Attr_Type'First;
   First_Attribute : constant Attr_Type := Attr_Type'Succ (Wrong_Attribute);
   Last_Attribute  : constant Attr_Type := Attr_Type'Last;

   function Convert (Item : Attribute_Type) return Int;
   function Convert (Item : Int) return Attribute_Type;

   -------------
   -- Pragmas --
   -------------

   type Pragma_Type is
      (Pragma_Unknown,
       Pragma_Starter,            --  (1) Starter
       Pragma_Import,             --  (2) Import
       Pragma_Boot_Server         --  (3) Boot_Server
       );

   Prag_Wrong : constant Int := 300;
   Prag_First : constant Int := Prag_Wrong + 1;
   Prag_Last  : constant Int := Prag_Wrong + 3;
   --  Should match Pragma_Type length

   type Pragma_Id is new Int range Prag_Wrong .. Prag_Last;

   Wrong_Pragma : constant Pragma_Id := Pragma_Id'First;
   First_Pragma : constant Pragma_Id := Pragma_Id'Succ (Wrong_Pragma);
   Last_Pragma  : constant Pragma_Id := Pragma_Id'Last;

   function Convert (Item : Pragma_Type) return Int;
   function Convert (Item : Int) return Pragma_Type;

   type Starter_Method_Type is (Ada_Starter, Shell_Starter, None_Starter);

   function Convert (Item : Starter_Method_Type) return Int;
   function Convert (Item : Int) return Starter_Method_Type;

   type Import_Method_Type  is (Ada_Import, Shell_Import, None_Import);

   function Convert (Item : Import_Method_Type) return Int;
   function Convert (Item : Int) return Import_Method_Type;

   function Convert (Item : Boolean) return Int;
   function Convert (Item : Int) return Boolean;

   Starter_Method    : Starter_Method_Type := Shell_Starter;
   Cross_Elaboration : Boolean             := False;

   ---------------------
   -- Predefined_Type --
   ---------------------

   type Predefined_Type is
      (Pre_Type_Unknown,
       Pre_Type_Partition,      --  (1)  Partition
       Pre_Type_Boolean,        --  (2)  Boolean
       Pre_Type_String,         --  (3)  String
       Pre_Type_Starter,        --  (4)  Type__Starter
       Pre_Type_Entity,         --  (5)  Type__Entity
       Pre_Type_Convention,     --  (6)  Type__Convention
       Pre_Type_Ada_Unit,       --  (7)  Type__Ada_Unit
       Pre_Type_Subprogram,     --  (8)  Type__Subprogram
       Pre_Type_Function,       --  (9)  Type__*_Function
       Pre_Type_Procedure       --  (10) Type__*_Procedure
       );

   Pre_Type_Wrong : constant Int := 400;
   Pre_Type_First : constant Int := Pre_Type_Wrong + 1;
   Pre_Type_Last  : constant Int := Pre_Type_Wrong + 10;
   --  Should match Predefined_Type length

   type Pre_Type_Id is new Int range Pre_Type_Wrong .. Pre_Type_Last;

   Wrong_Pre_Type : constant Pre_Type_Id := Pre_Type_Id'First;
   First_Pre_Type : constant Pre_Type_Id := Pre_Type_Id'Succ (Wrong_Pre_Type);
   Last_Pre_Type  : constant Pre_Type_Id := Pre_Type_Id'Last;

   function Convert (Item : in Predefined_Type) return Int;
   function Convert (Item : in Int) return Predefined_Type;

   -------------
   -- Node_Id --
   -------------

   type Node_Id          is new Int range 10_000 .. 20_000;
   type Type_Id          is new Node_Id;
   type Variable_Id      is new Node_Id;
   type Component_Id     is new Node_Id;
   type Parameter_Id     is new Node_Id;
   type Attribute_Id     is new Node_Id;
   type Statement_Id     is new Node_Id;
   type Subprogram_Id    is new Node_Id;
   type Configuration_Id is new Node_Id;

   Null_Node  : constant Node_Id := Node_Id'First;
   First_Node : constant Node_Id := Null_Node + 1;

   NN                 : constant Node_Id := Null_Node;
   Null_Type          : constant Type_Id          := Type_Id (NN);
   Null_Variable      : constant Variable_Id      := Variable_Id (NN);
   Null_Parameter     : constant Parameter_Id     := Parameter_Id (NN);
   Null_Component     : constant Component_Id     := Component_Id (NN);
   Null_Subprogram    : constant Subprogram_Id    := Subprogram_Id (NN);
   Null_Configuration : constant Configuration_Id := Configuration_Id (NN);

   Null_Name            : constant Types.Name_Id := Types.No_Name;

   Configuration_Node   : Configuration_Id;

   Partition_Type_Node      : Type_Id;
   Boolean_Type_Node        : Type_Id;
   String_Type_Node         : Type_Id;
   Starter_Type_Node        : Type_Id;
   Convention_Type_Node     : Type_Id;
   Ada_Unit_Type_Node       : Type_Id;
   Subprogram_Type_Node     : Type_Id;
   Main_Procedure_Type_Node : Type_Id;
   Host_Function_Type_Node  : Type_Id;

   Pragma_Starter_Node       : Subprogram_Id;
   Pragma_Import_Node        : Subprogram_Id;
   Pragma_Boot_Server_Node   : Subprogram_Id;

   function Get_Node_Name
     (Node : in Node_Id)
     return Name_Id;
   pragma Inline (Get_Node_Name);

   function  Is_Type
     (Node : in Node_Id)
     return Boolean;
   pragma Inline (Is_Type);

   function  Is_Variable
     (Node : in Node_Id)
     return Boolean;
   pragma Inline (Is_Variable);

   function  Is_Component
     (Node : in Node_Id)
      return Boolean;
   pragma Inline (Is_Component);

   function  Is_Subprogram
     (Node : in Node_Id)
      return Boolean;
   pragma Inline (Is_Subprogram);

   function  Is_Statement
     (Node : in Node_Id)
      return Boolean;
   pragma Inline (Is_Statement);

   function  Is_Configuration
     (Node : in Node_Id)
      return Boolean;
   pragma Inline (Is_Configuration);

   procedure Create_Type
     (Type_Node : out Type_Id;
      Type_Name : in  Name_Id);
   pragma Inline (Create_Type);

   procedure Create_Variable
     (Variable_Node : out Variable_Id;
      Variable_Name : in  Name_Id);
   pragma Inline (Create_Variable);

   procedure Create_Parameter
     (Parameter_Node : out Parameter_Id;
      Parameter_Name : in  Name_Id);
   pragma Inline (Create_Variable);

   procedure Create_Component
     (Component_Node : out Component_Id;
      Component_Name : in  Name_Id);
   pragma Inline (Create_Component);

   procedure Create_Subprogram
     (Subprogram_Node : out Subprogram_Id;
      Subprogram_Name : in  Name_Id);
   pragma Inline (Create_Subprogram);

   procedure Create_Statement
     (Statement_Node : out Statement_Id;
      Statement_Name : in  Name_Id);
   pragma Inline (Create_Subprogram);

   procedure Create_Configuration
     (Configuration_Node : out Configuration_Id;
      Configuration_Name : in  Name_Id);
   pragma Inline (Create_Configuration);

   -------------------
   -- Configuration --
   -------------------

   procedure First_Configuration_Declaration
     (Configuration_Node : in  Configuration_Id;
      Declaration_Node   : out Node_Id);

   procedure Next_Configuration_Declaration
     (Declaration_Node   : in out Node_Id);

   procedure Append_Configuration_Declaration
     (Configuration_Node : in Configuration_Id;
      Declaration_Node   : in Node_Id);

   ----------------
   -- Subprogram --
   ----------------

   procedure Subprogram_Is_A_Procedure
     (Subprogram_Node : in Subprogram_Id;
      Procedure_Node  : in Boolean);

   function Is_Subprogram_A_Procedure
     (Subprogram_Node : in Subprogram_Id)
     return Boolean;

   procedure Set_Subprogram_Mark
     (Subprogram_Node : in Subprogram_Id;
      Subprogram_Mark : in Int);

   function  Get_Subprogram_Mark
     (Subprogram_Node : Subprogram_Id)
      return Int;

   procedure Set_Parameter_Mark
     (Parameter_Node : in Parameter_Id;
      Parameter_Mark : in Int);

   function  Get_Parameter_Mark
     (Parameter_Node : Parameter_Id)
      return Int;

   procedure First_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : out Parameter_Id);

   procedure Next_Subprogram_Parameter
     (Parameter_Node  : in out Parameter_Id);

   procedure Add_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : in Parameter_Id);

   ----------
   -- Type --
   ----------

   procedure Type_Is_A_Structure
     (Type_Node : in Type_Id;
      Structure : in Boolean);

   function  Is_Type_A_Structure
     (Type_Node : in Type_Id)
     return Boolean;

   procedure Set_Type_Mark
     (Type_Node : in Type_Id;
      Type_Mark : in Int);

   function  Get_Type_Mark
     (Type_Node : Type_Id)
      return Int;

   procedure First_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : out Component_Id);

   procedure Next_Type_Component
     (Component_Node  : in out Component_Id);

   procedure Add_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : in Component_Id);

   --------------
   -- Variable --
   --------------

   procedure Set_Variable_Type
     (Variable_Node : in Variable_Id;
      Variable_Type : in Type_Id);

   function Get_Variable_Type
     (Variable_Node : in Variable_Id)
     return Type_Id;

   procedure Set_Variable_Value
     (Variable_Node : in Variable_Id;
      Value_Node    : in Variable_Id);

   function Get_Variable_Value
     (Variable_Node : in Variable_Id)
     return Variable_Id;

   procedure Set_Variable_Mark
     (Variable_Node : in Variable_Id;
      Variable_Mark : in Int);

   function  Get_Variable_Mark
     (Variable_Node : Variable_Id)
      return Int;

   procedure Variable_Is_A_Structure
     (Variable_Node : in Variable_Id;
      Structure     : in Boolean);

   function Is_Variable_A_Structure
     (Variable_Node : in Variable_Id)
     return Boolean;

   procedure First_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : out Component_Id);

   procedure Next_Variable_Component
     (Component_Node  : in out Component_Id);

   procedure Add_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : in Component_Id);

   ---------------
   -- Component --
   ---------------

   procedure Set_Component_Value
     (Component_Node : in Component_Id;
      Value_Node     : in Node_Id);

   function  Get_Component_Value
     (Component_Node : in Component_Id)
     return Node_Id;

   procedure Set_Component_Type
     (Component_Node : in Component_Id;
      Type_Node      : in Type_Id);

   function Get_Component_Type
     (Component_Node : in Component_Id)
     return Type_Id;

   procedure Component_Is_An_Attribute
     (Component_Node : in Component_Id;
      Attribute_Node : in Boolean);

   function Is_Component_An_Attribute
     (Component_Node : in Component_Id)
     return Boolean;

   procedure Set_Component_Mark
     (Component_Node : Component_Id;
      Component_Mark : Int);

   function Get_Component_Mark
     (Component_Node : Component_Id)
      return Int;

   ---------------
   -- Parameter --
   ---------------

   procedure Set_Parameter_Type
     (Parameter_Node : in Parameter_Id;
      Parameter_Type : in Type_Id);

   function Get_Parameter_Type
     (Parameter_Node : in Parameter_Id)
     return Type_Id;

   ---------------
   -- Statement --
   ---------------

   procedure Set_Subprogram_Call
     (Statement_Node  : in Statement_Id;
      Subprogram_Node : in Subprogram_Id);

   function  Get_Subprogram_Call
     (Statement_Node  : in Statement_Id)
      return Subprogram_Id;

   ------------------------------
   -- Parser Convention Naming --
   ------------------------------

   Conf_Ada_Unit  : Name_Id;
   Part_Main_Unit : Name_Id;
   Returned_Param : Name_Id;
   Procedure_Unit : Name_Id;
   Sub_Prog_Param : Name_Id;
   Procedure_Call : Name_Id;

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

   -------------
   -- Host_Id --
   -------------

   Host_Wrong : constant Int := 3_000_000;
   Host_Null  : constant Int := Host_Wrong + 1;
   Host_First : constant Int := Host_Null  + 1;
   Host_Last  : constant Int := 3_999_999;

   type Host_Id is new Int range Host_Wrong .. Host_Last;

   Wrong_Host : constant Host_Id := Host_Id'First;
   Null_Host  : constant Host_Id := Host_Id'Succ (Wrong_Host);
   First_Host : constant Host_Id := Host_Id'Succ (Null_Host);
   Last_Host  : constant Host_Id := Host_Id'Last;

   type Host_Type is
      record
         Static   : Boolean            := True;
         Import   : Import_Method_Type := None_Import;
         Name     : Host_Name_Type     := No_Name;
         External : Host_Name_Type     := No_Name;
      end record;

   package Hosts  is new Table
     (Table_Component_Type => Host_Type,
      Table_Index_Type     => Host_Id,
      Table_Low_Bound      => First_Host,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Host");

   subtype Main_Subprogram_Type is Name_Id;
   No_Main_Subprogram : constant Main_Subprogram_Type := No_Name;

   subtype Command_Line_Type is Name_Id;
   No_Command_Line   : constant Command_Line_Type := No_Name;

   subtype Storage_Dir_Name_Type is Name_Id;
   No_Storage_Dir    : constant Storage_Dir_Name_Type := No_Name;

   --  Default values
   Default_Main          : Main_Subprogram_Type   := No_Main_Subprogram;
   Default_Host          : Host_Id   := Null_Host;
   Default_Storage_Dir   : Storage_Dir_Name_Type := No_Storage_Dir;
   Default_Command_Line  : Command_Line_Type := No_Command_Line;

   type Partition_Type is record
      Name            : Partition_Name_Type   := No_Partition_Name;
      Host            : Host_Id               := Null_Host;
      Storage_Dir     : Storage_Dir_Name_Type := No_Storage_Dir;
      Command_Line    : Command_Line_Type     := No_Command_Line;
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
      My_ALI    : ALI_Id          := No_ALI_Id;
      My_Unit   : Unit_Id         := No_Unit_Id;
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

   function Already_Loaded (Unit : Name_Id) return Boolean;
   --  Check that this unit has not been previously loaded in order
   --  to avoid multiple entries in GNAT tables.

   procedure Load_All_Units (From : Unit_Name_Type);
   --  Recursively update GNAT internal tables by downloading all Uname
   --  dependent units if available.

   procedure Add_Conf_Unit (CU : in CUnit_Name_Type; To : in PID_Type);
   --  Assign a Conf Unit to a partition. This unit is declared in the
   --  configuration file (it is not yet mapped to an ada unit).

   procedure Create_Partition
     (Name : in  Partition_Name_Type;
      PID  : out PID_Type);
   --  Create a new partition and store its PID in its name key.

   procedure Copy_Partition
     (Name : in Partition_Name_Type;
      Many : in Int);
   --  Create Many successive copies of partition Name.

   procedure Show_Configuration;
   --  Report the current configuration.

   function Is_Set (Partition : in PID_Type) return Boolean;
   --  Some units have already been assigned to this partition.

   function Str_To_Id (S : String) return Name_Id;

   RCI_Name_Server    : constant String := "rns";

   Configuration_File : Name_Id := No_Name;
   Configuration      : Name_Id := No_Name;
   --  Name of the configuration

   Main_Partition     : PID_Type  := Null_PID;
   --  Partition where the main procedure has been assigned.

   Main_Subprogram    : Name_Id        := No_Name;
   Main_Source_File   : File_Name_Type := No_Name;
   Main_ALI           : ALI_Id;
   Most_Recent_Stamp  : Time_Stamp_Type;
   --  Several variables related to the main procedure.

   Protocol_Name      : Name_Id        := No_Name;
   Protocol_Data      : Name_Id        := No_Name;
   --  Several variables to build the boot server.

   procedure Maybe_Most_Recent_Stamp (Stamp : Time_Stamp_Type);
   --  Maybe set Most_Recent_Stamp.

   Verbose_Mode       : Boolean;
   Debug_Mode         : Boolean;
   Quiet_Output       : Boolean;
   No_Recompilation   : Boolean;
   Building_Script    : Boolean;

   Fatal_Error         : exception;   --  Operating system error
   Scanning_Error      : exception;   --  Error during scanning
   Parsing_Error       : exception;   --  Error during parsing
   Partitioning_Error  : exception;   --  Error during partitionning
   Usage_Error         : exception;   --  Command line error
   Not_Yet_Implemented : exception;

end XE;

