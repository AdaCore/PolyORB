------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                            X E _ S T D C N F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

with XE;        use XE;
with XE_Parse;  use XE_Parse;
with XE_Scan;   use XE_Scan;
with XE_Utils;  use XE_Utils;

package body XE_Stdcnf is

   --  This procedure contains the standard configuration which is loaded
   --  before the user configuration.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Attribute_Node : Attribute_Id;
      Variable_Node  : Variable_Id;
      Parameter_Node : Parameter_Id;
      Component_Node : Component_Id;

   begin

      --  As a naming convention, we use the reserved keyword "private"
      --  for the standard configuration name.

      Create_Configuration (Configuration_Node, Str_To_Id ("private"));

      --  type Boolean_Type is (False, True); (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("boolean"),
         Type_Kind    => Pre_Type_Boolean,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Boolean_Type_Node);

      Declare_Variable
        (Str_To_Id ("true"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, 1);

      Declare_Variable
        (Str_To_Id ("false"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, 0);

      --  type string (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("string"),
         Type_Kind    => Pre_Type_String,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => String_Type_Node);

      --  type integer (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("integer"),
         Type_Kind    => Pre_Type_Integer,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Integer_Type_Node);

      Declare_Variable
        (Str_To_Id ("local_termination"),
         Integer_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Local_Termination));

      Declare_Variable
        (Str_To_Id ("global_termination"),
         Integer_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Global_Termination));

      Declare_Variable
        (Str_To_Id ("deferred_termination"),
         Integer_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Deferred_Termination));

      --  type type__host_function (standard)
      --     function F (...: String) return String;

      Declare_Type
        (Type_Name    => Type_Prefix & Str_To_Id ("host_function"),
         Type_Kind    => Pre_Type_Function,
         Comp_Type    => String_Type_Node,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Host_Function_Type_Node);

      Declare_Type_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => ISN_Subpro_Par,
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => ISN_Return_Par,
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      --  type type__main_procedure (standard)
      --     procedure P

      Declare_Type
        (Type_Name    => Type_Prefix & Str_To_Id ("main_procedure"),
         Type_Kind    => Pre_Type_Procedure,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Main_Procedure_Type_Node);

      --  type type__ada_unit (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & Str_To_Id ("ada_unit"),
         Type_Kind    => Pre_Type_Ada_Unit,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => False,
         Type_Sloc    => Null_Location,
         Type_Node    => Ada_Unit_Type_Node);

      --  type Partition (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("partition"),
         Type_Kind    => Pre_Type_Partition,
         Comp_Type    => Ada_Unit_Type_Node,
         List_Size    => Unbounded,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Partition_Type_Node);

      --  Legal attribute : 'Main
      --  Legal attribute : 'Host
      --  Legal attribute : 'Storage_Dir
      --  Legal attribute : 'Command_Line

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("main"),
         Attr_Type_Node => Main_Procedure_Type_Node,
         Attribute_Kind => Attribute_Main,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("host"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Host,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("storage_dir"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Storage_Dir,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("command_line"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Command_Line,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("termination"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Termination,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("_leader"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Leader,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      --  type Channel (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("channel"),
         Type_Kind    => Pre_Type_Channel,
         Comp_Type    => Null_Type,
         List_Size    => 2,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Channel_Type_Node);

      Declare_Type_Component
        (Type_Node        => Channel_Type_Node,
         Component_Name   => Str_To_Id ("partition_1"),
         Comp_Type_Node   => Partition_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Channel_Type_Node,
         Component_Name   => Str_To_Id ("partition_2"),
         Comp_Type_Node   => Partition_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Attribute
        (Type_Node        => Channel_Type_Node,
         Attribute_Name   => Str_To_Id ("filter"),
         Attr_Type_Node   => String_Type_Node,
         Attribute_Kind   => Attribute_Filter,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      --  type Convention_Type is (Ada, Shell, None); (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & Str_To_Id ("convention"),
         Type_Kind    => Pre_Type_Convention,
         Comp_Type    => Null_Type,
         List_Size    => 0,
         Is_Frozen    => True,
         Type_Sloc    => Null_Location,
         Type_Node    => Convention_Type_Node);

      Declare_Variable
        (Str_To_Id ("ada"),
         Convention_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Convert (Ada_Import));

      Declare_Variable
        (Str_To_Id ("shell"),
         Convention_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Convert (Shell_Import));

      Declare_Variable
        (Str_To_Id ("none"),
         Convention_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Convert (None_Import));

      --  pragma starter ... or
      --  procedure pragma__starter
      --    (method : starter__type);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("starter"),
         Pragma_Starter,
         True,
         Null_Location,
         Pragma_Starter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("method"),
         Convention_Type_Node,
         Pragma_Starter_Node,
         Null_Location,
         Parameter_Node);

      --  pragma Import ... or
      --  procedure pragma__import
      --    (convention : convention__type;
      --     entity     : procedure;
      --     link_name  : string);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("import"),
         Pragma_Import,
         True,
         Null_Location,
         Pragma_Import_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("convention"),
         Convention_Type_Node,
         Pragma_Import_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("entity"),
         Ada_Unit_Type_Node,
         Pragma_Import_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("link_name"),
         String_Type_Node,
         Pragma_Import_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_server ... or
      --  procedure pragma__starter
      --    (method : starter__type);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("boot_server"),
         Pragma_Boot_Server,
         True,
         Null_Location,
         Pragma_Boot_Server_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_name"),
         String_Type_Node,
         Pragma_Boot_Server_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_data"),
         String_Type_Node,
         Pragma_Boot_Server_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_server ... or
      --  procedure pragma__starter
      --    (check : starter__type);

      Declare_Subprogram
        (Pragma_Prefix & Str_To_Id ("version"),
         Pragma_Version,
         True,
         Null_Location,
         Pragma_Version_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("check"),
         Boolean_Type_Node,
         Pragma_Version_Node,
         Null_Location,
         Parameter_Node);

   end Initialize;

end XE_Stdcnf;
