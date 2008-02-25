------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            X E _ S T D C N F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
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

with XE_Types;     use XE_Types;
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

      Variable_Node  : Variable_Id;

   begin

      --  As a naming convention, we use the reserved keyword "private"
      --  for the standard configuration name.

      Create_Configuration (Configuration_Node, Id ("private"));

      --  type Boolean_Type is (False, True, Infinite);

      Declare_Type
        (Type_Name    => Id ("boolean"),
         Type_Kind    => Pre_Type_Boolean,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Boolean_Type_Node);

      Declare_Variable
        (Id ("true"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (BTrue));

      Declare_Variable
        (Id ("false"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (BFalse));

      Declare_Variable
        (Id ("unknown boolean"),
         Boolean_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Int (BMaybe));

      --  type string (standard)

      Declare_Type
        (Type_Name    => Id ("string"),
         Type_Kind    => Pre_Type_String,
         Composite    => False,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => String_Type_Node);

      --  type integer (standard)

      Declare_Type
        (Type_Name    => Id ("integer"),
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
         Component_Name   => Id ("partition_name"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Component
        (Type_Node        => Host_Function_Type_Node,
         Component_Name   => Id ("return parameter"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location);

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
        (Type_Name    => Id ("partition"),
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
         Component_Name   => Id ("low_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Component
        (Type_Node        => Task_Pool_Type_Node,
         Component_Name   => Id ("high_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Component
        (Type_Node        => Task_Pool_Type_Node,
         Component_Name   => Id ("max_mark"),
         Comp_Type_Node   => Integer_Type_Node,
         Component_Sloc   => Null_Location);

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
         Component_Name   => Id ("support_name"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Component
        (Type_Node        => Location_Type_Node,
         Component_Name   => Id ("support_data"),
         Comp_Type_Node   => String_Type_Node,
         Component_Sloc   => Null_Location);

      --  type type__location_list (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "location list",
         Type_Kind    => Pre_Type_Locations,
         Composite    => True,
         Comp_Type    => Location_Type_Node,
         Array_Len    => Infinite,
         Type_Sloc    => Null_Location,
         Type_Node    => Locations_Type_Node);

      --  type type__string_list (standard)

      Declare_Type
        (Type_Name    => Type_Prefix & "string list",
         Type_Kind    => Pre_Type_Strings,
         Composite    => True,
         Comp_Type    => String_Type_Node,
         Array_Len    => Infinite,
         Type_Sloc    => Null_Location,
         Type_Node    => String_List_Type_Node);

      --  Define attributes for partition

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("main"),
         Attr_Type_Node => Main_Procedure_Type_Node,
         Attribute_Kind => Attribute_Main,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("host"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Host,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("storage_dir"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Directory,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("directory"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Directory,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("environment_variables"),
         Attr_Type_Node => String_List_Type_Node,
         Attribute_Kind => Attribute_Environment_Variables,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("reconnection"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Reconnection,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("command_line"),
         Attr_Type_Node => String_Type_Node,
         Attribute_Kind => Attribute_Command_Line,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("termination"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Termination,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("priority"),
         Attr_Type_Node => Integer_Type_Node,
         Attribute_Kind => Attribute_Priority,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Id ("filter"),
         Attr_Type_Node   => String_Type_Node,
         Attribute_Kind   => Attribute_PFilter,
         Attribute_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Id ("task_pool"),
         Attr_Type_Node   => Task_Pool_Type_Node,
         Attribute_Kind   => Attribute_Task_Pool,
         Attribute_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Id ("self_location"),
         Attr_Type_Node   => Locations_Type_Node,
         Attribute_Kind   => Attribute_Protocol,
         Attribute_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Id ("self_location"),
         Attr_Type_Node   => Location_Type_Node,
         Attribute_Kind   => Attribute_Protocol,
         Attribute_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Partition_Type_Node,
         Attribute_Name   => Id ("data_location"),
         Attr_Type_Node   => Location_Type_Node,
         Attribute_Kind   => Attribute_Storage,
         Attribute_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("is boot partition"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Leader,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("passive"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Passive,
         Attribute_Sloc => Null_Location);

      Declare_Type_Attribute
        (Type_Node      => Partition_Type_Node,
         Attribute_Name => Id ("allow_light_pcs"),
         Attr_Type_Node => Boolean_Type_Node,
         Attribute_Kind => Attribute_Allow_Light_PCS,
         Attribute_Sloc => Null_Location);

      --  type Channel (standard)

      Declare_Type
        (Type_Name    => Id ("channel"),
         Type_Kind    => Pre_Type_Channel,
         Composite    => True,
         Comp_Type    => Null_Type,
         Array_Len    => 0,
         Type_Sloc    => Null_Location,
         Type_Node    => Channel_Type_Node);

      Declare_Type_Component
        (Type_Node        => Channel_Type_Node,
         Component_Name   => Id ("partition_1"),
         Comp_Type_Node   => Partition_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Component
        (Type_Node        => Channel_Type_Node,
         Component_Name   => Id ("partition_2"),
         Comp_Type_Node   => Partition_Type_Node,
         Component_Sloc   => Null_Location);

      Declare_Type_Attribute
        (Type_Node        => Channel_Type_Node,
         Attribute_Name   => Id ("filter"),
         Attr_Type_Node   => String_Type_Node,
         Attribute_Kind   => Attribute_CFilter,
         Attribute_Sloc   => Null_Location);

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
        (Id ("ada"),
         Convention_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Convert (Ada_Import));

      Declare_Variable
        (Id ("shell"),
         Convention_Type_Node,
         Null_Location,
         Variable_Node);

      --  To easily retrieve the enumeration literal.
      Set_Scalar_Value (Variable_Node, Convert (Shell_Import));

      Declare_Variable
        (Id ("none"),
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
        (Id ("convention"),
         Convention_Type_Node,
         Pragma_Starter_Node,
         Null_Location);

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
        (Id ("policy"),
         Integer_Type_Node,
         Pragma_Priority_Node,
         Null_Location);

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
        (Id ("convention"),
         Convention_Type_Node,
         Pragma_Import_Node,
         Null_Location);

      Declare_Subprogram_Parameter
        (Id ("entity"),
         Ada_Unit_Type_Node,
         Pragma_Import_Node,
         Null_Location);

      Declare_Subprogram_Parameter
        (Id ("external_name"),
         String_Type_Node,
         Pragma_Import_Node,
         Null_Location);

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
        (Id ("protocol_name"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

      Declare_Subprogram_Parameter
        (Id ("protocol_data"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("location"),
         Location_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("locations"),
         Locations_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("protocol_name"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

      Declare_Subprogram_Parameter
        (Id ("protocol_data"),
         String_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("location"),
         Location_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("locations"),
         Locations_Type_Node,
         Pragma_Boot_Location_Node,
         Null_Location);

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
        (Id ("check"),
         Boolean_Type_Node,
         Pragma_Version_Node,
         Null_Location);

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
        (Id ("filter"),
         String_Type_Node,
         Pragma_Reg_Filter_Node,
         Null_Location);

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
        (Id ("command"),
         String_Type_Node,
         Pragma_Remote_Shell_Node,
         Null_Location);

      Declare_Subprogram_Parameter
        (Id ("options"),
         String_Type_Node,
         Pragma_Remote_Shell_Node,
         Null_Location);

   end Initialize;

end XE_Stdcnf;
