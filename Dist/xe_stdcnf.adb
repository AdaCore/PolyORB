------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                            X E _ S T D C N F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with Types;     use Types;
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

      --  type Boolean_Type is (False, True, Infinite);

      Declare_Type
        (Type_Name    => Str_To_Id ("boolean"),
         Type_Kind    => Pre_Type_Boolean,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Boolean_Type_Node);

      Declare_Variable
        (Str_To_Id ("true"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Btrue));

      Declare_Variable
        (Str_To_Id ("false"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Bfalse));

      Declare_Variable
        (Str_To_Id ("unknown boolean"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (Bunknown));

      --  type string (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("string"),
         Type_Kind    => Pre_Type_String,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => String_Type_Node);

      --  type integer (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("integer"),
         Type_Kind    => Pre_Type_Integer,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Integer_Type_Node);

      --  Type Termination. To easily retrieve the enumeration literal
      --  and their image.

      for T in Termination_Img'Range loop
         Declare_Variable
           (To_Lower (Termination_Img (T)),
            Integer_Type_Node,
            Null_Location,
            Variable_Node);
         Set_Scalar_Value (Variable_Node, Int (T));
      end loop;

      --  type Reconnection. To easily retrieve the enumeration literal
      --  and their image.

      for R in Reconnection_Img'Range loop
         Declare_Variable
           (To_Lower (Reconnection_Img (R)),
            Integer_Type_Node,
            Null_Location,
            Variable_Node);
         Set_Scalar_Value (Variable_Node, Int (R));
      end loop;

      --  type priority_policy. To easily retrieve the enumeration
      --  literal and their image.

      for R in Priority_Policy_Img'Range loop
         Declare_Variable
           (To_Lower (Priority_Policy_Img (R)),
            Integer_Type_Node,
            Null_Location,
            Variable_Node);
         Set_Scalar_Value (Variable_Node, Int (R));
      end loop;

      --  type type__host_function (standard)
      --     function F (...: String) return String;

      Declare_Type
        (Type_Name    => Type_Prefix & "host function",
         Type_Kind    => Pre_Type_Function,
         Composite    => True,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Host_Function_Type_Node);

      Declare_Type_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => Str_To_Id ("partition_name"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => Str_To_Id ("return parameter"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      --  type type__main_procedure (standard)
      --     procedure P

      Declare_Type
        (Type_Name    => Type_Prefix & "main procedure",
         Type_Kind    => Pre_Type_Procedure,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Main_Procedure_Type_Node);

      --  type type__ada_unit (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "ada unit",
         Type_Kind    => Pre_Type_Ada_Unit,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Ada_Unit_Type_Node);

      --  type Partition (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("partition"),
         Type_Kind    => Pre_Type_Partition,
         Composite    => True,
         Comp_Type    => Ada_Unit_Type_Node,
         Array_Len    => Infinite,
         Type_Sloc    => Null_Location,
         Type_Node    => Partition_Type_Node);

      --  type type__task_pool (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "task pool",
         Type_Kind    => Pre_Type_Task_Pool,
         Composite    => True,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Task_Pool_Type_Node);

      Declare_Type_Component
        (Type_Node        => Task_Pool_Type_Node,
         Component_Name   => Str_To_Id ("low_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Task_Pool_Type_Node,
         Component_Name   => Str_To_Id ("high_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Task_Pool_Type_Node,
         Component_Name   => Str_To_Id ("max_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      --  type type__location (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "location",
         Type_Kind    => Pre_Type_Location,
         Composite    => True,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Location_Type_Node);

      Declare_Type_Component
        (Type_Node        => Location_Type_Node,
         Component_Name   => Str_To_Id ("support_name"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      Declare_Type_Component
        (Type_Node        => Location_Type_Node,
         Component_Name   => Str_To_Id ("support_data"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location,
         Component_Node   => Component_Node);

      --  type type__location_list (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "location list",
         Type_Kind    => Pre_Type_Locations,
         Composite    => True,
         Comp_Type    => Location_Type_Node,
         Array_Len    => Infinite,
         Type_Sloc    => Null_Location,
         Type_Node    => Locations_Type_Node);

      --  Define attributes for partition

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
         Attribute_Kind => Attribute_Directory,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("directory"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Directory,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("reconnection"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Reconnection,
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
         Attribute_Name => Str_To_Id ("priority"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Priority,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Str_To_Id ("filter"),
         Attr_Type_Node   => String_Type_Node,
         Attribute_Kind   => Attribute_PFilter,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Str_To_Id ("task_pool"),
         Attr_Type_Node   => Task_Pool_Type_Node,
         Attribute_Kind   => Attribute_Task_Pool,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Str_To_Id ("self_location"),
         Attr_Type_Node   => Locations_Type_Node,
         Attribute_Kind   => Attribute_Protocol,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Str_To_Id ("self_location"),
         Attr_Type_Node   => Location_Type_Node,
         Attribute_Kind   => Attribute_Protocol,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Str_To_Id ("data_location"),
         Attr_Type_Node   => Location_Type_Node,
         Attribute_Kind   => Attribute_Storage,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("is boot partition"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Leader,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("passive"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Passive,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Str_To_Id ("allow_light_pcs"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Allow_Light_PCS,
         Attribute_Sloc => Null_Location,
         Attribute_Node => Attribute_Node);

      --  type Channel (standard)

      Declare_Type
        (Type_Name    => Str_To_Id ("channel"),
         Type_Kind    => Pre_Type_Channel,
         Composite    => True,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
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
         Attribute_Kind   => Attribute_CFilter,
         Attribute_Sloc   => Null_Location,
         Attribute_Node   => Attribute_Node);

      --  type Convention_Type is (Ada, Shell, None); (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "convention",
         Type_Kind    => Pre_Type_Convention,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
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
      --    (convention : type__convention);

      Declare_Subprogram
        (Pragma_Prefix & "starter",
         Pragma_Starter,
         True,
         Null_Location,
         Pragma_Starter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("convention"),
         Convention_Type_Node,
         Pragma_Starter_Node,
         Null_Location,
         Parameter_Node);

      --  pragma priority ... or
      --  procedure pragma__priority
      --    (propagate : type__priority_policy);

      Declare_Subprogram
        (Pragma_Prefix & "priority",
         Pragma_Priority,
         True,
         Null_Location,
         Pragma_Priority_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("policy"),
         Integer_Type_Node,
         Pragma_Priority_Node,
         Null_Location,
         Parameter_Node);

      --  pragma Import ... or
      --  procedure pragma__import
      --    (convention    : type__convention;
      --     entity        : type__procedure;
      --     external_name : type__string);

      Declare_Subprogram
        (Pragma_Prefix & "import",
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
        (Str_To_Id ("external_name"),
         String_Type_Node,
         Pragma_Import_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_server ... or
      --  procedure pragma__boot_server
      --    (protocol_name : type__string;
      --     protocol_data : type__string);

      Declare_Subprogram
        (Pragma_Prefix & "boot_server",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_name"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_data"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_server ... or
      --  procedure pragma__boot_server
      --    (location : type__location);

      Declare_Subprogram
        (Pragma_Prefix & "boot_server",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("location"),
         Location_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_server ... or
      --  procedure pragma__boot_server
      --    (locations : type__location__list);

      Declare_Subprogram
        (Pragma_Prefix & "boot_server",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("locations"),
         Locations_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_location ... or
      --  procedure pragma__boot_location
      --    (protocol_name : type__string;
      --     protocol_data : type__string);

      Declare_Subprogram
        (Pragma_Prefix & "boot_location",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_name"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("protocol_data"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_location ... or
      --  procedure pragma__boot_server
      --    (location : type__location);

      Declare_Subprogram
        (Pragma_Prefix & "boot_location",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("location"),
         Location_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma boot_location ... or
      --  procedure pragma__boot_server
      --    (locations : type__location__list);

      Declare_Subprogram
        (Pragma_Prefix & "boot_location",
         Pragma_Boot_Location,
         True,
         Null_Location,
         Pragma_Boot_Location_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("locations"),
         Locations_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location,
         Parameter_Node);

      --  pragma version ... or
      --  procedure pragma__version
      --    (check : boolean__type);

      Declare_Subprogram
        (Pragma_Prefix & "version",
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

      --  pragma registration_filter ... or
      --  procedure registration_filter
      --    (filter : type__string);

      Declare_Subprogram
        (Pragma_Prefix & "registration_filter",
         Pragma_Reg_Filter,
         True,
         Null_Location,
         Pragma_Reg_Filter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("filter"),
         String_Type_Node,
         Pragma_Reg_Filter_Node,
         Null_Location,
         Parameter_Node);

      --  pragma remote_shell ... or
      --  procedure pragma__remote_shell
      --    (command : type__string;
      --     options : type__string);

      Declare_Subprogram
        (Pragma_Prefix & "remote_shell",
         Pragma_Remote_Shell,
         True,
         Null_Location,
         Pragma_Remote_Shell_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("command"),
         String_Type_Node,
         Pragma_Remote_Shell_Node,
         Null_Location,
         Parameter_Node);

      Declare_Subprogram_Parameter
        (Str_To_Id ("options"),
         String_Type_Node,
         Pragma_Remote_Shell_Node,
         Null_Location,
         Parameter_Node);

   end Initialize;

end XE_Stdcnf;
