------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                   X E                                    --
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

with Fname;        use Fname;
with Output;       use Output;
with Osint;        use Osint;
with Namet;        use Namet;
with GNAT.Os_Lib;  use GNAT.Os_Lib;
with XE_Utils;     use XE_Utils;
with XE_Defs;      use XE_Defs;
with Types;

package body XE is

   use type Types.Name_Id;
   use type Types.Unit_Name_Type;
   use type Types.Int;

   subtype Name_Id        is Types.Name_Id;
   subtype Unit_Name_Type is Types.Unit_Name_Type;
   subtype Int            is Types.Int;

   type Node_Kind is
      (K_Null,
       K_List,
       K_Type,
       K_Subprogram,
       K_Statement,
       K_Component,
       K_Variable);

   type List_Kind is
      (K_Declaration_List,
       K_Parameter_List,
       K_Component_List);

   type Node_Type is
      record
         Kind    : Node_Kind;
         Loc_X   : Int;
         Loc_Y   : Int;
         Name    : Name_Id;
         Node_1  : Node_Id;
         Node_2  : Node_Id;
         Node_3  : Node_Id;
         Flag_1  : Boolean;
         Value   : Int;
      end record;

   --  list
   --     node_1 : next declaration
   --     node_2 : list head
   --     node_3 : list tail
   --     flag_1 : list or array
   --     value  : declaration list | parameter list | component list
   --  subprogram
   --     node_1 : next declaration
   --     node_2 : unused
   --     node_3 : parameter list
   --     flag_1 : is a procedure
   --     value  : used when pragma
   --  type
   --     node_1 : next declaration
   --     node_2 : array element type when type is a list
   --     node_3 : component list
   --     flag_1 : auto. allocation
   --     value  : predefined_type'pos
   --  variable
   --     node_1 : next declaration
   --     node_2 : variable type
   --     node_3 : component list
   --     flag_1 : unused
   --     value  : enumeration type value
   --  component
   --     node_1 : next component
   --     node_2 : component type
   --     node_3 : component value
   --     flag_1 : is an attribute
   --     value  : used when enumeration type
   --  statement
   --     node_1 : next declaration
   --     node_2 : subprogram call
   --     node_3 : unused
   --     flag_1 : unused
   --     value  : unused

   function Is_Of_Kind
     (Node : Node_Id;
      Kind : Node_Kind)
     return Boolean;

   function Is_Component_List
     (Node : Node_Id)
     return Boolean;

   function Is_Declaration_List
     (Node : Node_Id)
     return Boolean;

   function Is_Parameter_List
     (Node : Node_Id)
     return Boolean;

   procedure Create_Node
     (Node : out Node_Id;
      Name : in  Name_Id;
      Kind : in  Node_Kind);

   function Convert (Item : List_Kind) return Int;
   function Convert (Item : Int) return List_Kind;

   package Nodes is new Table
     (Table_Component_Type => Node_Type,
      Table_Index_Type     => Node_Id,
      Table_Low_Bound      => First_Node,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Nodes");

   Context_Root_Node   : Node_Id := Null_Node;
   Function_Type_Node  : Node_Id := Null_Node;
   Procedure_Type_Node : Node_Id := Null_Node;

   ---------------------------
   -- Add_Channel_Partition --
   ---------------------------

   procedure Add_Channel_Partition
     (Partition : in Partition_Name_Type; To : in CID_Type) is
      PID : PID_Type := Get_PID (Partition);
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": add partition ");
         Write_Name (Partition);
         Write_Str  (" to channel ");
         Write_Name (Channels.Table (To).Name);
         Write_Eol;
      end if;
      if Channels.Table (To).Lower = Null_PID then
         Channels.Table (To).Lower := PID;
      elsif PID > Channels.Table (To).Lower then
         Channels.Table (To).Upper := PID;
      else
         Channels.Table (To).Upper := Channels.Table (To).Lower;
         Channels.Table (To).Lower := PID;
      end if;
   end Add_Channel_Partition;

   -------------------
   -- Add_Conf_Unit --
   -------------------

   procedure Add_Conf_Unit
     (CU : in CUnit_Name_Type;
      To : in PID_Type) is
   begin

      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": configuring unit ");
         Write_Name (CU);
         Write_Str  (" on partition ");
         Write_Name (Partitions.Table (To).Name);
         Write_Eol;
      end if;

      --  Mark this configured unit as already partitioned.
      Set_PID (CU, To);

      --  The same unit can be multiply declared especially if
      --  this unit is a normal package.
      CUnit.Increment_Last;
      CUnit.Table (CUnit.Last).Partition := To;
      CUnit.Table (CUnit.Last).CUname    := CU;
      CUnit.Table (CUnit.Last).My_ALI    := No_ALI_Id;
      CUnit.Table (CUnit.Last).My_Unit   := No_Unit_Id;
      CUnit.Table (CUnit.Last).Next      := Null_CUID;

      --  Update partition single linked list of configured units.
      if Partitions.Table (To).First_Unit = Null_CUID then
         Partitions.Table (To).First_Unit := CUnit.Last;
      else
         CUnit.Table (Partitions.Table (To).Last_Unit).Next := CUnit.Last;
      end if;
      Partitions.Table (To).Last_Unit := CUnit.Last;

   end Add_Conf_Unit;

   ------------------------------
   -- Add_Subprogram_Parameter --
   ------------------------------

   procedure Add_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : in Parameter_Id) is
      Node  : Node_Id := Node_Id (Subprogram_Node);
      List  : Node_Id;
      Value : Node_Id := Node_Id (Parameter_Node);
   begin
      pragma Assert (Is_Subprogram (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Parameter_List (List));
      if Nodes.Table (List).Node_2 = Null_Node then
         Nodes.Table (List).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      else
         Nodes.Table (Nodes.Table (List).Node_2).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      end if;
   end Add_Subprogram_Parameter;

   ------------------------
   -- Add_Type_Component --
   ------------------------

   procedure Add_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : in Component_Id) is
      Node  : Node_Id := Node_Id (Type_Node);
      List  : Node_Id;
      Value : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Type (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Component_List (List));
      if Nodes.Table (List).Node_2 = Null_Node then
         Nodes.Table (List).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      else
         Nodes.Table (Nodes.Table (List).Node_2).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      end if;
   end Add_Type_Component;

   ----------------------------
   -- Add_Variable_Component --
   ----------------------------

   procedure Add_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : in Component_Id) is
      Node  : Node_Id := Node_Id (Variable_Node);
      List  : Node_Id;
      Value : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Variable (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Component_List (List));
      if Nodes.Table (List).Node_2 = Null_Node then
         Nodes.Table (List).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      else
         Nodes.Table (Nodes.Table (List).Node_2).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      end if;
   end Add_Variable_Component;

   --------------------
   -- Already_Loaded --
   --------------------

   function Already_Loaded (Unit : Name_Id) return Boolean is
   begin
      Get_Name_String (Unit);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'b';
      if Get_Name_Table_Info (Name_Find) /= 0 then
         return True;
      end if;
      Name_Buffer (Name_Len) := 's';
      if Get_Name_Table_Info (Name_Find) /= 0 then
         return True;
      end if;
      return False;
   end Already_Loaded;

   -----------------------------------
   -- Add_Configuration_Declaration --
   -----------------------------------

   procedure Add_Configuration_Declaration
     (Configuration_Node : in Configuration_Id;
      Declaration_Node   : in Node_Id) is
      Conf : Node_Id := Node_Id (Configuration_Node);
      Back : Node_Id;
   begin
      pragma Assert (Is_Configuration (Conf));
      if Nodes.Table (Conf).Node_3 = Null_Node then
         Nodes.Table (Conf).Node_1 := Null_Node;
         Nodes.Table (Conf).Node_2 := Declaration_Node;
         Nodes.Table (Conf).Node_3 := Declaration_Node;
      else
         Nodes.Table (Nodes.Table (Conf).Node_3).Node_1 := Declaration_Node;
         Nodes.Table (Conf).Node_3 := Declaration_Node;
      end if;
      if Is_Configuration (Declaration_Node) then
         Nodes.Table (Conf).Node_3 := Conf;
         Back := Nodes.Table (Declaration_Node).Node_3;
         if Back = Null_Node then
            Nodes.Table (Declaration_Node).Node_1 := Conf;
         else
            Nodes.Table (Declaration_Node).Node_1 :=
              Nodes.Table (Declaration_Node).Node_2;
            Nodes.Table (Back).Node_1 := Conf;
         end if;
      end if;
   end Add_Configuration_Declaration;

   -----------------------------
   -- Component_Is_An_Attribute --
   -----------------------------

   procedure Component_Is_An_Attribute
     (Component_Node : in Component_Id;
      Attribute_Node : in Boolean) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Flag_1 := Attribute_Node;
   end Component_Is_An_Attribute;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Attribute_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Attribute_Type is
   begin
      pragma Assert
        (Item in Int (Attribute_Type'First) .. Int (Attribute_Type'Last));
      return Attribute_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Import_Method_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Import_Method_Type is
   begin
      pragma Assert
        (Item in
         Int (Import_Method_Type'First) ..
         Int (Import_Method_Type'Last));
      return Import_Method_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Pragma_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Pragma_Type is
   begin
      pragma Assert
        (Item in Int (Pragma_Type'First) .. Int (Pragma_Type'Last));
      return Pragma_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Predefined_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Predefined_Type is
   begin
      pragma Assert
        (Item in Int (Predefined_Type'First) .. Int (Predefined_Type'Last));
      return Predefined_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Starter_Method_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Starter_Method_Type is
   begin
      pragma Assert
        (Item in
         Int (Starter_Method_Type'First) ..
         Int (Starter_Method_Type'Last));
      return Starter_Method_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Termination_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Termination_Type is
   begin
      pragma Assert
        (Item in
         Int (Termination_Type'First) ..
         Int (Termination_Type'Last));
      return Termination_Type (Item);
   end Convert;

   -------------
   -- Convert --
   -------------
   function Convert (Item : List_Kind) return Int is
   begin
      return Int (List_Kind'Pos (Item));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return List_Kind is
   begin
      return List_Kind'Val (Item);
   end Convert;

   ------------------
   -- Copy_Channel --
   ------------------

   procedure Copy_Channel
     (Name : in Channel_Name_Type;
      Many : in Int) is
      CID  : CID_Type;
      CCID : CID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create Channel ");
         Write_Name (Name);
         Write_Str  (" (");
         Write_Int  (Many);
         if Many > 1 then
            Write_Str (" copies)");
         else
            Write_Str (" copy)");
         end if;
         Write_Eol;
      end if;

      CCID := Get_CID (Name);
      for I in 1 .. Many loop
         Channels.Increment_Last;
         CID := Channels.Last;
         Set_CID (Name, CID);
         Channels.Table (CID).Name  := Channels.Table (CID).Name;

         --  This is stupid, but let's do it.
         Channels.Table (CID).Lower := Channels.Table (CID).Lower;
         Channels.Table (CID).Upper := Channels.Table (CID).Upper;
      end loop;
   end Copy_Channel;

   --------------------
   -- Copy_Partition --
   --------------------

   procedure Copy_Partition
     (Name : in Partition_Name_Type;
      Many : in Int) is
      PID  : PID_Type;
      CPID : PID_Type;
      CUID : CUID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create partition ");
         Write_Name (Name);
         Write_Str  (" (");
         Write_Int  (Many);
         if Many > 1 then
            Write_Str (" copies)");
         else
            Write_Str (" copy)");
         end if;
         Write_Eol;
      end if;

      CPID := Get_PID (Name);
      for I in 1 .. Many loop
         Partitions.Increment_Last;
         PID := Partitions.Last;
         Set_PID (Name, PID);
         Partitions.Table (PID).Name := Name;
         CUID := Partitions.Table (CPID).First_Unit;
         while CUID /= Null_CUID loop
            Add_Conf_Unit (CUnit.Table (CUID).CUname, PID);
            CUID := CUnit.Table (CUID).Next;
         end loop;
      end loop;
   end Copy_Partition;

   --------------------
   -- Create_Channel --
   --------------------

   procedure Create_Channel
     (Name : in Channel_Name_Type;
      CID  : out CID_Type) is
      Channel : CID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create channel ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Channels.Increment_Last;
      Channel := Channels.Last;
      Set_CID (Name, Channel);
      Channels.Table (Channel).Name            := Name;
      Channels.Table (Channel).Lower           := Null_PID;
      Channels.Table (Channel).Upper           := Null_PID;
      Channels.Table (Channel).Filter          := No_Filter_Name;
      CID := Channel;
   end Create_Channel;

   ----------------------
   -- Create_Component --
   ----------------------

   procedure Create_Component
     (Component_Node : out Component_Id;
      Component_Name : in  Name_Id) is
      Node : Node_Id;
   begin
      Create_Node (Node, Component_Name, K_Component);
      Component_Node := Component_Id (Node);
   end Create_Component;

   --------------------------
   -- Create_Configuration --
   --------------------------

   procedure Create_Configuration
     (Configuration_Node : out Configuration_Id;
      Configuration_Name : in  Name_Id) is
      Node : Node_Id;
   begin
      Create_Node (Node, Configuration_Name, K_List);
      Nodes.Table (Node).Value := Convert (K_Declaration_List);
      Configuration_Node := Configuration_Id (Node);
   end Create_Configuration;

   -----------------
   -- Create_Node --
   -----------------

   procedure Create_Node
     (Node : out Node_Id;
      Name : in  Name_Id;
      Kind : in  Node_Kind) is
   begin
      Nodes.Increment_Last;
      Nodes.Table (Nodes.Last).Kind     := Kind;
      Nodes.Table (Nodes.Last).Name     := Name;
      Nodes.Table (Nodes.Last).Node_1   := Null_Node;
      Nodes.Table (Nodes.Last).Node_2   := Null_Node;
      Nodes.Table (Nodes.Last).Node_3   := Null_Node;
      Nodes.Table (Nodes.Last).Flag_1   := True;
      Nodes.Table (Nodes.Last).Value    := 0;
      Node := Nodes.Last;
   end Create_Node;

   ----------------------
   -- Create_Parameter --
   ----------------------

   procedure Create_Parameter
     (Parameter_Node : out Parameter_Id;
      Parameter_Name : in  Name_Id) is
   begin
      Create_Node (Node_Id (Parameter_Node), Parameter_Name, K_Variable);
   end Create_Parameter;

   ----------------------
   -- Create_Partition --
   ----------------------

   procedure Create_Partition
     (Name : in Partition_Name_Type;
      PID  : out PID_Type) is
      Partition : PID_Type;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": create partition ");
         Write_Name (Name);
         Write_Eol;
      end if;

      Partitions.Increment_Last;
      Partition := Partitions.Last;
      Set_PID (Name, Partition);
      Partitions.Table (Partition).Name            := Name;
      Partitions.Table (Partition).Host            := Null_Host;
      Partitions.Table (Partition).Storage_Dir     := No_Storage_Dir;
      Partitions.Table (Partition).Command_Line    := No_Command_Line;
      Partitions.Table (Partition).Main_Subprogram := No_Name;
      Partitions.Table (Partition).Termination     := Unknown_Termination;
      Partitions.Table (Partition).First_Unit      := Null_CUID;
      Partitions.Table (Partition).Last_Unit       := Null_CUID;
      Partitions.Table (Partition).To_Build        := True;
      Partitions.Table (Partition).Most_Recent     := Configuration_File;
      PID := Partition;
   end Create_Partition;

   ----------------------
   -- Create_Statement --
   ----------------------

   procedure Create_Statement
     (Statement_Node : out Statement_Id;
      Statement_Name : in  Name_Id) is
      Node : Node_Id;
   begin
      Create_Node (Node, Statement_Name, K_Statement);
      Statement_Node := Statement_Id (Node);
   end Create_Statement;

   -----------------------
   -- Create_Subprogram --
   -----------------------

   procedure Create_Subprogram
     (Subprogram_Node : out Subprogram_Id;
      Subprogram_Name : in  Name_Id) is
      Node : Node_Id;
      List : Node_Id;
   begin
      Create_Node (Node, Subprogram_Name, K_Subprogram);
      Create_Node (List, Str_To_Id ("parameter__list"), K_List);
      Nodes.Table (List).Value := Convert (K_Parameter_List);
      Nodes.Table (Node).Node_3 := List;
      Subprogram_Node := Subprogram_Id (Node);
   end Create_Subprogram;

   -----------------
   -- Create_Type --
   -----------------

   procedure Create_Type
     (Type_Node : out Type_Id;
      Type_Name : in  Name_Id) is
   begin
      Create_Node (Node_Id (Type_Node), Type_Name, K_Type);
   end Create_Type;

   ---------------------
   -- Create_Variable --
   ---------------------

   procedure Create_Variable
     (Variable_Node : out Variable_Id;
      Variable_Name : in  Name_Id) is
   begin
      Create_Node (Node_Id (Variable_Node), Variable_Name, K_Variable);
   end Create_Variable;

   -------------------------------------
   -- First_Configuration_Declaration --
   -------------------------------------

   procedure First_Configuration_Declaration
     (Configuration_Node : in  Configuration_Id;
      Declaration_Node   : out Node_Id) is
      Node : Node_Id := Node_Id (Configuration_Node);
   begin
      pragma Assert (Is_Configuration (Node));
      Declaration_Node := Nodes.Table (Node).Node_2;
   end First_Configuration_Declaration;

   --------------------------------
   -- First_Subprogram_Parameter --
   --------------------------------

   procedure First_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : out Parameter_Id) is
      Node : Node_Id := Node_Id (Subprogram_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Subprogram (Node) or else Is_Statement (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Parameter_List (List));
      Parameter_Node := Parameter_Id (Nodes.Table (List).Node_1);
   end First_Subprogram_Parameter;

   --------------------------
   -- First_Type_Component --
   --------------------------

   procedure First_Type_Component
     (Type_Node       : in Type_Id;
      Component_Node  : out Component_Id) is
      Node : Node_Id := Node_Id (Type_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Type (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Component_List (List));
      Component_Node := Component_Id (Nodes.Table (List).Node_1);
   end First_Type_Component;

   ------------------------------
   -- First_Variable_Component --
   ------------------------------

   procedure First_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : out Component_Id) is
      Node : Node_Id := Node_Id (Variable_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Variable (Node));
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Component_List (List));
      Component_Node := Component_Id (Nodes.Table (List).Node_1);
   end First_Variable_Component;

   -----------------------
   -- Get_Absolute_Exec --
   -----------------------

   function Get_Absolute_Exec (P : in PID_Type) return Name_Id is
      Dir  : Name_Id := Partitions.Table (P).Storage_Dir;
      Name : Name_Id renames Partitions.Table (P).Name;
   begin

      if Dir = No_Storage_Dir then
         Dir := Default_Storage_Dir;
      end if;

      if Dir = No_Storage_Dir then

         --  No storage dir means current directory

         return PWD_Id & Name;

      else
         Get_Name_String (Dir);
         if Name_Buffer (1) /= Separator and then
           Name_Buffer (1) /= '/' then

            --  The storage dir is relative

            return PWD_Id & Dir & Dir_Sep_Id & Name;

         end if;

         --  Write the dir as it has been written

         return Dir & Dir_Sep_Id & Name;

      end if;

   end Get_Absolute_Exec;

   ----------------
   -- Get_ALI_Id --
   ----------------

   function Get_ALI_Id (N : Name_Id) return ALI_Id is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Int (ALI_Id'First) .. Int (ALI_Id'Last) =>
            null;
         when others =>
            Info := Int (No_ALI_Id);
      end case;
      return ALI_Id (Info);
   end Get_ALI_Id;

   ----------------------------
   -- Get_Array_Element_Type --
   ----------------------------

   function Get_Array_Element_Type
     (Array_Type_Node   : in Type_Id)
      return Type_Id is
      Node : Node_Id := Node_Id (Array_Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      return Type_Id (Nodes.Table (Node).Node_2);
   end Get_Array_Element_Type;

   -------------
   -- Get_CID --
   -------------

   function Get_CID (N : Name_Id) return CID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (CID_Type'First) .. Int (CID_Type'Last) then
         return CID_Type (Info);
      else
         return Null_CID;
      end if;
   end Get_CID;

   ----------------------
   -- Get_Command_Line --
   ----------------------

   function Get_Command_Line    (P : in PID_Type) return Command_Line_Type is
      Cmd : Command_Line_Type := Partitions.Table (P).Command_Line;
   begin

      if Cmd = No_Command_Line then
         Cmd := Default_Command_Line;
      end if;

      return Cmd;

   end Get_Command_Line;

   ------------------------
   -- Get_Component_Mark --
   ------------------------

   function Get_Component_Mark
     (Component_Node : Component_Id)
      return Int is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Value;
   end Get_Component_Mark;

   ------------------------
   -- Get_Component_Type --
   ------------------------

   function Get_Component_Type
     (Component_Node : in Component_Id)
      return Type_Id is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Type_Id (Nodes.Table (Node).Node_2);
   end Get_Component_Type;

   ------------------------
   -- Get_Component_Value --
   ------------------------

   function Get_Component_Value
     (Component_Node : in Component_Id)
      return Node_Id is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Has_Component_A_Value (Component_Node));
      return Nodes.Table (Node).Node_3;
   end Get_Component_Value;

   --------------
   -- Get_CUID --
   --------------

   function Get_CUID (N : Name_Id) return CUID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (CUID_Type'First) .. Int (CUID_Type'Last) then
         return CUID_Type (Info);
      else
         return Null_CUID;
      end if;
   end Get_CUID;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter          (C : CID_Type) return Name_Id is
      F : Name_Id := Channels.Table (C).Filter;
   begin

      if F = No_Filter_Name then
         F := Default_Filter;
      end if;
      return F;

   end Get_Filter;

   --------------
   -- Get_Host --
   --------------

   function Get_Host            (P : in PID_Type) return Name_Id is
      H : Host_Id := Partitions.Table (P).Host;
   begin

      if H = Null_Host then
         H := Default_Host;
      end if;

      if H /= Null_Host then
         if not Hosts.Table (H).Static then
            if Hosts.Table (H).Import = Shell_Import then
               return  Str_To_Id ("""`") &
                       Hosts.Table (H).External &
                       Str_To_Id (" ") &
                       Partitions.Table (P).Name &
                       Str_To_Id ("`""");
            elsif Hosts.Table (H).Import = Ada_Import then
               return  Hosts.Table (H).External &
                       Str_To_Id ("(") &
                       Partitions.Table (P).Name &
                       Str_To_Id (")");
            end if;
            raise Parsing_Error;
         else
            return Str_To_Id ("""") &
                   Hosts.Table (H).Name &
                   Str_To_Id ("""");
         end if;

      else
         return No_Name;

      end if;

   end Get_Host;

   -------------------------
   -- Get_Main_Subprogram --
   -------------------------

   function Get_Main_Subprogram
     (P : in PID_Type)
      return Main_Subprogram_Type is
      Main : Main_Subprogram_Type := Partitions.Table (P).Main_Subprogram;
   begin

      if Main = No_Main_Subprogram then
         Main := Default_Main;
      end if;

      return Main;

   end Get_Main_Subprogram;

   -------------------
   -- Get_Node_Name --
   -------------------

   function  Get_Node_Name
     (Node : in Node_Id)
     return Name_Id is
   begin
      return Nodes.Table (Node).Name;
   end Get_Node_Name;

   -------------------
   -- Get_Node_SLOC --
   -------------------

   procedure Set_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : in Int;
      Loc_Y : in Int) is
   begin
      Nodes.Table (Node).Loc_X := Loc_X;
      Nodes.Table (Node).Loc_Y := Loc_Y;
   end Set_Node_SLOC;

   -------------------
   -- Get_Node_SLOC --
   -------------------

   procedure Get_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : out Int;
      Loc_Y : out Int) is
   begin
      Loc_X := Nodes.Table (Node).Loc_X;
      Loc_Y := Nodes.Table (Node).Loc_Y;
   end Get_Node_SLOC;

   -------------------------
   -- Get_Parameter_Mark --
   -------------------------

   function  Get_Parameter_Mark
     (Parameter_Node : in Parameter_Id)
     return Int is
      Node : Node_Id := Node_Id (Parameter_Node);
   begin
      pragma Assert (Is_Variable (Node));
      return Nodes.Table (Node).Value;
   end Get_Parameter_Mark;

   ------------------------
   -- Get_Parameter_Type --
   ------------------------

   function Get_Parameter_Type
     (Parameter_Node : in Parameter_Id)
     return Type_Id is
   begin
      return Get_Variable_Type (Variable_Id (Parameter_Node));
   end Get_Parameter_Type;

   -----------------------
   -- Get_Partition_Dir --
   -----------------------

   function Get_Partition_Dir (P : in PID_Type) return File_Name_Type is
   begin
      return DSA_Dir &
        Dir_Sep_Id & Configuration &
        Dir_Sep_Id & Partitions.Table (P).Name;
   end Get_Partition_Dir;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (N : Name_Id) return PID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (PID_Type'First) .. Int (PID_Type'Last) then
         return PID_Type (Info);
      else
         return Null_PID;
      end if;
   end Get_PID;

   -----------------------
   -- Get_Relative_Exec --
   -----------------------

   function Get_Relative_Exec (P : in PID_Type) return Name_Id is
      Dir  : Name_Id := Partitions.Table (P).Storage_Dir;
      Name : Name_Id renames Partitions.Table (P).Name;
   begin

      if Dir = No_Storage_Dir then
         Dir := Default_Storage_Dir;
      end if;

      if Dir = No_Storage_Dir then

         --  No storage dir means current directory

         return Name;

      else

         --  The storage dir is relative

         return Dir & Dir_Sep_Id & Name;

      end if;

   end Get_Relative_Exec;

   ---------------------
   -- Get_Storage_Dir --
   ---------------------

   function Get_Storage_Dir (P : in PID_Type) return Storage_Dir_Name_Type is
      Storage_Dir : Storage_Dir_Name_Type := Partitions.Table (P).Storage_Dir;
   begin

      if Storage_Dir = No_Storage_Dir then
         Storage_Dir := Default_Storage_Dir;
      end if;

      return Storage_Dir;

   end Get_Storage_Dir;

   ------------------------
   -- Get_Subprogram_Call --
   ------------------------

   function  Get_Subprogram_Call
     (Statement_Node  : in Statement_Id)
      return Subprogram_Id is
      Node : Node_Id := Node_Id (Statement_Node);
   begin
      pragma Assert (Is_Statement (Node));
      return Subprogram_Id (Nodes.Table (Node).Node_2);
   end Get_Subprogram_Call;

   -------------------------
   -- Get_Subprogram_Mark --
   -------------------------

   function  Get_Subprogram_Mark
     (Subprogram_Node : in Subprogram_Id)
     return Int is
      Node : Node_Id := Node_Id (Subprogram_Node);
   begin
      pragma Assert (Is_Subprogram (Node));
      return Nodes.Table (Node).Value;
   end Get_Subprogram_Mark;

   ---------------------
   -- Get_Termination --
   ---------------------

   function Get_Termination
     (P : in PID_Type)
      return Termination_Type is
   begin
      return Partitions.Table (P).Termination;
   end Get_Termination;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (N : Name_Id) return Token_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      if Info in Int (Token_Type'First) .. Int (Token_Type'Last) then
         return Token_Type (Info);
      else
         return Tok_Unknown;
      end if;
   end Get_Token;

   -------------------
   -- Get_Type_Mark --
   -------------------

   function  Get_Type_Mark
     (Type_Node : in Type_Id)
     return Int is
      Node : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      return Nodes.Table (Node).Value;
   end Get_Type_Mark;

   -----------------
   -- Get_Unit_Id --
   -----------------

   function Get_Unit_Id (N : Name_Id) return Unit_Id is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Int (Unit_Id'First) .. Int (Unit_Id'Last) =>
            null;
         when others =>
            Info := Int (No_Unit_Id);
      end case;
      return Unit_Id (Info);
   end Get_Unit_Id;

   --------------------
   -- Get_Unit_Sfile --
   --------------------

   function Get_Unit_Sfile (U : in Unit_Id) return File_Name_Type is
   begin
      Get_Name_String (Unit.Table (U).Sfile);
      Name_Len := Name_Len - 4;
      return Name_Find;
   end Get_Unit_Sfile;

   -----------------------
   -- Get_Variable_Mark --
   -----------------------

   function  Get_Variable_Mark
     (Variable_Node : Variable_Id)
      return Int is
      Node : Node_Id := Node_Id (Variable_Node);
   begin
      pragma Assert (Is_Variable (Node));
      return Nodes.Table (Node).Value;
   end Get_Variable_Mark;

   -----------------------
   -- Get_Variable_Type --
   -----------------------

   function Get_Variable_Type
     (Variable_Node : in Variable_Id)
      return Type_Id is
      Node : Node_Id := Node_Id (Variable_Node);
   begin
      pragma Assert (Is_Variable (Node));
      return Type_Id (Nodes.Table (Node).Node_2);
   end Get_Variable_Type;

   ------------------------
   -- Get_Variable_Value --
   ------------------------

   function Get_Variable_Value
     (Variable_Node : in Variable_Id)
     return Variable_Id is
      Node  : Node_Id := Node_Id (Variable_Node);
   begin
      return Variable_Id (Nodes.Table (Node).Node_3);
   end Get_Variable_Value;

   ---------------------------
   -- Has_Component_A_Value --
   ---------------------------

   function Has_Component_A_Value
     (Component_Node : Component_Id)
     return Boolean is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Node_3 /= Null_Node;
   end Has_Component_A_Value;

   ---------------------
   -- Is_Array_A_List --
   ---------------------

   function Is_Array_A_List
     (Array_Type_Node   : in Type_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Array_Type_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Type (Node));
      pragma Assert (Get_Array_Element_Type (Array_Type_Node) /= Null_Type);
      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Component_List (List));
      return Nodes.Table (List).Flag_1;
   end Is_Array_A_List;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Component);
   end Is_Component;

   -----------------------------
   -- Is_Component_An_Attribute --
   -----------------------------

   function Is_Component_An_Attribute
     (Component_Node : in Component_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Component_An_Attribute;

   -----------------------
   -- Is_Component_List --
   -----------------------

   function Is_Component_List
     (Node : in Node_Id)
      return Boolean is
   begin
      return Nodes.Table (Node).Kind = K_List and then
             Convert (Nodes.Table (Node).Value) = K_Component_List;
   end Is_Component_List;

   ----------------------
   -- Is_Configuration --
   ----------------------

   function Is_Configuration (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_List) and then
             Convert (Nodes.Table (Node).Value) = K_Declaration_List;
   end Is_Configuration;

   -------------------------
   -- Is_Declaration_List --
   -------------------------

   function Is_Declaration_List
     (Node : in Node_Id)
      return Boolean is
   begin
      return Nodes.Table (Node).Kind = K_List and then
             Convert (Nodes.Table (Node).Value) = K_Declaration_List;
   end Is_Declaration_List;

   ----------------
   -- Is_Of_Kind --
   ----------------

   function Is_Of_Kind
     (Node : in Node_Id;
      Kind : in Node_Kind)
      return Boolean is
   begin
      pragma Assert (Node /= Null_Node);
      return Nodes.Table (Node).Kind = Kind;
   end Is_Of_Kind;

   -----------------------
   -- Is_Parameter_List --
   -----------------------

   function Is_Parameter_List
     (Node : in Node_Id)
      return Boolean is
   begin
      return Nodes.Table (Node).Kind = K_List and then
             Convert (Nodes.Table (Node).Value) = K_Parameter_List;
   end Is_Parameter_List;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Partition : PID_Type) return Boolean is
   begin
      return Partitions.Table (Partition).Last_Unit /= Null_CUID;
   end Is_Set;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Statement);
   end Is_Statement;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_Subprogram);
   end Is_Subprogram;

   -------------------------------
   -- Is_Subprogram_A_Procedure --
   -------------------------------

   function Is_Subprogram_A_Procedure
     (Subprogram_Node : in Subprogram_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Subprogram_Node);
   begin
      pragma Assert (Is_Subprogram (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Subprogram_A_Procedure;

   -------------
   -- Is_Type --
   -------------

   function Is_Type (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_Type);
   end Is_Type;

   --------------------
   -- Is_Type_Frozen --
   --------------------

   function Is_Type_Frozen
     (Type_Node : Type_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Type_Frozen;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (Node : Node_Id)  return Boolean is
   begin
      pragma Assert (Node /= Null_Node);
      return Is_Of_Kind (Node, K_Variable);
   end Is_Variable;

   --------------------
   -- Load_All_Units --
   --------------------

   procedure Load_All_Units (From : Unit_Name_Type) is
      File : File_Name_Type;
      Lib  : File_Name_Type;
      Text : Text_Buffer_Ptr;
   begin
      if Verbose_Mode then
         Write_Program_Name;
         Write_Str  (": loading all units from ");
         Write_Name (From);
         Write_Eol;
      end if;
      if Already_Loaded (From) then
         return;
      end if;
      File := From & ADB_Suffix;
      if Full_Source_Name (File) = No_Name then
         File := From & ADS_Suffix;
         if Full_Source_Name (File) = No_Name then
            Write_Program_Name;
            Write_Str (": no spec or body found for unit ");
            Write_Name (From);
            Write_Eol;
            raise Fatal_Error;
         end if;
      end if;
      Lib  := Lib_File_Name (File);
      Text := Read_Library_Info (Lib);
      if Text = null then
         Write_Missing_File (Lib);
         raise Fatal_Error;
      end if;
      Read_ALI (Scan_ALI (Lib, Text));
   end Load_All_Units;

   ------------------------------------
   -- Next_Configuration_Declaration --
   ------------------------------------

   procedure Next_Configuration_Declaration
     (Declaration_Node   : in out Node_Id) is
   begin
      Declaration_Node := Nodes.Table (Declaration_Node).Node_1;
   end Next_Configuration_Declaration;

   -------------------------------
   -- Next_Subprogram_Parameter --
   -------------------------------

   procedure Next_Subprogram_Parameter
     (Parameter_Node  : in out Parameter_Id) is
      Node : Node_Id := Node_Id (Parameter_Node);
   begin
      pragma Assert (Is_Variable (Node));
      Parameter_Node := Parameter_Id (Nodes.Table (Node).Node_1);
   end Next_Subprogram_Parameter;

   -------------------------
   -- Next_Type_Component --
   -------------------------

   procedure Next_Type_Component
     (Component_Node  : in out Component_Id) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Type_Component;

   -----------------------------
   -- Next_Variable_Component --
   -----------------------------

   procedure Next_Variable_Component
     (Component_Node  : in out Component_Id) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Variable_Component;

   ----------------
   -- Set_ALI_Id --
   ----------------

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id) is
   begin
      Set_Name_Table_Info (N, Int (A));
   end Set_ALI_Id;

   --------------------
   -- Set_Array_Type --
   --------------------

   procedure Set_Array_Type
     (Array_Type_Node   : in Type_Id;
      Element_Type_Node : in Type_Id;
      Array_Is_A_List   : in Boolean) is
      Node : Node_Id := Node_Id (Array_Type_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Node_2 := Node_Id (Element_Type_Node);
      Create_Node (List, Str_To_Id ("pragma__n__array"), K_List);
      Nodes.Table (List).Value := Convert (K_Component_List);
      Nodes.Table (List).Flag_1 := Array_Is_A_List;
      Nodes.Table (Node).Node_3 := List;
   end Set_Array_Type;

   -------------
   -- Set_CID --
   -------------

   procedure Set_CID (N : Name_Id; C : CID_Type) is
   begin
      Set_Name_Table_Info (N, Int (C));
   end Set_CID;

   ------------------------
   -- Set_Component_Mark --
   ------------------------

   procedure Set_Component_Mark
     (Component_Node : Component_Id;
      Component_Mark : Int) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Value := Component_Mark;
   end Set_Component_Mark;

   ------------------------
   -- Set_Component_Type --
   ------------------------

   procedure Set_Component_Type
     (Component_Node : in Component_Id;
      Type_Node      : in Type_Id) is
      Node  : Node_Id := Node_Id (Component_Node);
      Ntype : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Component (Node));
      pragma Assert (Is_Type (Ntype));
      Nodes.Table (Node).Node_2 := Ntype;
   end Set_Component_Type;

   -------------------------
   -- Set_Component_Value --
   -------------------------

   procedure Set_Component_Value
     (Component_Node : in Component_Id;
      Value_Node     : in Node_Id) is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Node_3 := Value_Node;
   end Set_Component_Value;

   -------------
   -- Set_CUID --
   -------------

   procedure Set_CUID (N : Name_Id; U : CUID_Type) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_CUID;

   ------------------------
   -- Set_Parameter_Mark --
   ------------------------

   procedure Set_Parameter_Mark
     (Parameter_Node : in Parameter_Id;
      Parameter_Mark : in Int) is
      Node : Node_Id := Node_Id (Parameter_Node);
   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Value := Parameter_Mark;
   end Set_Parameter_Mark;

   ------------------------
   -- Set_Parameter_Type --
   ------------------------

   procedure Set_Parameter_Type
     (Parameter_Node : in Parameter_Id;
      Parameter_Type : in Type_Id) is
   begin
      Set_Variable_Type (Variable_Id (Parameter_Node), Parameter_Type);
   end Set_Parameter_Type;

   -------------
   -- Set_PID --
   -------------

   procedure Set_PID (N : Name_Id; P : PID_Type) is
   begin
      Set_Name_Table_Info (N, Int (P));
   end Set_PID;

   ------------------------
   -- Set_Subprogram_Call --
   ------------------------

   procedure Set_Subprogram_Call
     (Statement_Node  : in Statement_Id;
      Subprogram_Node : in Subprogram_Id) is
      Statement  : Node_Id := Node_Id (Statement_Node);
      Subprogram : Node_Id := Node_Id (Subprogram_Node);
   begin
      pragma Assert (Is_Statement  (Statement) and then
                     Is_Subprogram (Subprogram));
      Nodes.Table (Statement).Node_2 := Subprogram;
   end Set_Subprogram_Call;

   -------------------------
   -- Set_Subprogram_Mark --
   -------------------------

   procedure Set_Subprogram_Mark
     (Subprogram_Node : in Subprogram_Id;
      Subprogram_Mark : in Int) is
      Node : Node_Id := Node_Id (Subprogram_Node);
   begin
      pragma Assert (Is_Subprogram (Node));
      Nodes.Table (Node).Value := Subprogram_Mark;
   end Set_Subprogram_Mark;

   ---------------
   -- Set_Token --
   ---------------

   procedure Set_Token (N : String; T : Token_Type) is
      Name  : Name_Id;
   begin
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Int (T));
      Reserved (T) := True;
   end Set_Token;

   -------------------
   -- Set_Type_Mark --
   -------------------

   procedure Set_Type_Mark
     (Type_Node : in Type_Id;
      Type_Mark : in Int) is
      Node : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Value := Type_Mark;
   end Set_Type_Mark;

   -----------------
   -- Set_Unit_Id --
   -----------------

   procedure Set_Unit_Id (N : Name_Id; U : Unit_Id) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_Unit_Id;

   -----------------------
   -- Set_Variable_Mark --
   -----------------------

   procedure Set_Variable_Mark
     (Variable_Node : in Variable_Id;
      Variable_Mark : in Int) is
      Node : Node_Id := Node_Id (Variable_Node);
   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Value := Variable_Mark;
   end Set_Variable_Mark;

   -----------------------
   -- Set_Variable_Type --
   -----------------------

   procedure Set_Variable_Type
     (Variable_Node : in Variable_Id;
      Variable_Type : in Type_Id) is
      Node : Node_Id := Node_Id (Variable_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Variable (Node));
      pragma Assert (Is_Type (Node_Id (Variable_Type)));
      Nodes.Table (Node).Node_2 := Node_Id (Variable_Type);
      if Get_Array_Element_Type (Variable_Type) /= Null_Type then
         Create_Node (List, Str_To_Id ("record"), K_List);
         Nodes.Table (List).Value := Convert (K_Component_List);
         Nodes.Table (Node).Node_3 := List;
      end if;
   end Set_Variable_Type;

   ------------------------
   -- Set_Variable_Value --
   ------------------------

   procedure Set_Variable_Value
     (Variable_Node : in Variable_Id;
      Value_Node    : in Variable_Id) is
      Node  : Node_Id := Node_Id (Variable_Node);
   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Node_3 := Node_Id (Value_Node);
   end Set_Variable_Value;

   -------------------------
   --  Show_Configuration --
   -------------------------

   procedure Show_Configuration is

      Main         : Main_Subprogram_Type;
      Host         : Host_Id;
      Storage_Dir  : Storage_Dir_Name_Type;
      Command_Line : Command_Line_Type;

   begin
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str (" ---- Configuration report ----");
      Write_Eol;
      Write_Str (" ------------------------------");
      Write_Eol;
      Write_Str ("Configuration :");
      Write_Eol;
      Write_Str ("   Name        : ");
      Write_Name (Configuration);
      Write_Eol;

      Write_Str ("   Main        : ");
      Write_Name (Main_Subprogram);
      Write_Eol;

      Write_Str ("   Starter     : ");
      case Starter_Method is
         when Ada_Starter =>
            Write_Str ("Ada code");
         when Shell_Starter =>
            Write_Str ("shell script");
         when None_Starter =>
            Write_Str ("none");
      end case;
      Write_Eol;

      if Protocol_Name /= No_Name then
         Write_Str  ("   Protocol    : ");
         Write_Name (Protocol_Name);
         Write_Str  ("://");
         Write_Name (Protocol_Data);
         Write_Eol;
      end if;
      Write_Eol;

      for P in Partitions.First .. Partitions.Last loop
         declare
            I : Partition_Type renames Partitions.Table (P);
            U : CUID_Type;
         begin
            Write_Str ("Partition ");
            Write_Name (I.Name);
            Write_Eol;

            Main := Get_Main_Subprogram (P);
            if Main /= No_Main_Subprogram then
               Write_Str ("   Main        : ");
               Write_Name (Main);
               Write_Eol;
            end if;

            Host := I.Host;
            if Host = Null_Host then
               Host := Default_Host;
            end if;

            if Host /= Null_Host then
               Write_Str ("   Host        : ");
               if Hosts.Table (Host).Static then
                  Write_Name (Hosts.Table (Host).Name);
               else
                  Write_Str ("function call :: ");
                  Write_Name (Hosts.Table (Host).External);
                  case Hosts.Table (Host).Import is
                     when None_Import =>
                        null;
                     when Ada_Import =>
                        Write_Str (" (ada)");
                     when Shell_Import =>
                        Write_Str (" (shell)");
                  end case;
               end if;
               Write_Eol;
            end if;

            Storage_Dir := Get_Storage_Dir (P);
            if Storage_Dir /= No_Storage_Dir then
               Write_Str ("   Storage     : ");
               Write_Name (Storage_Dir);
               Write_Eol;
            end if;

            Command_Line := Get_Command_Line (P);
            if Command_Line /= No_Command_Line then
               Write_Str ("   Command     : ");
               Write_Name (Command_Line);
               Write_Eol;
            end if;

            if Get_Termination (P) /= Unknown_Termination then
               Write_Str ("   Termination : ");
               case Get_Termination (P) is
                  when Local_Termination =>
                     Write_Str ("local");
                  when Global_Termination =>
                     Write_Str ("global");
                  when Deferred_Termination =>
                     Write_Str ("deferred");
                  when Unknown_Termination =>
                     null;
               end case;
               Write_Eol;
            end if;

            if I.First_Unit /= Null_CUID then
               Write_Str ("   Units       : ");
               Write_Eol;
               U := I.First_Unit;
               while U /= Null_CUID loop
                  Write_Str ("             - ");
                  Write_Name (CUnit.Table (U).CUname);
                  if Unit.Table (CUnit.Table (U).My_Unit).RCI then
                     Write_Str (" (rci) ");
                  else
                     Write_Str (" (normal)");
                  end if;
                  Write_Eol;
                  U := CUnit.Table (U).Next;
               end loop;
               Write_Eol;
            end if;
         end;
      end loop;
      Write_Str (" -------------------------------");
      Write_Eol;
      if Channels.First <= Channels.Last then
         Write_Eol;
         declare
            P : PID_Type;
            F : Name_Id;
         begin
            for C in Channels.First .. Channels.Last loop
               Write_Str  ("Channel ");
               Write_Name (Channels.Table (C).Name);
               Write_Eol;
               Write_Str     ("   Partition 1 : ");
               P := Channels.Table (C).Lower;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               Write_Str     ("   Partition 2 : ");
               P := Channels.Table (C).Upper;
               Write_Name (Partitions.Table (P).Name);
               Write_Eol;
               F := Get_Filter (C);
               if F /= No_Filter_Name then
                  Write_Str  ("   Filter      : ");
                  Write_Name (F);
                  Write_Eol;
               end if;
               Write_Eol;
            end loop;
         end;
         Write_Str (" -------------------------------");
         Write_Eol;
      end if;

   end Show_Configuration;

   ---------------
   -- Str_To_Id --
   ---------------

   function Str_To_Id (S : String) return Name_Id is
   begin
      Name_Buffer (1 .. S'Length) := S;
      Name_Len := S'Length;
      return Name_Find;
   end Str_To_Id;

   -------------------------------
   -- Subprogram_Is_A_Procedure --
   -------------------------------

   procedure Subprogram_Is_A_Procedure
     (Subprogram_Node : in Subprogram_Id;
      Procedure_Node  : in Boolean) is
      Node : Node_Id := Node_Id (Subprogram_Node);
   begin
      pragma Assert (Is_Subprogram (Node));
      Nodes.Table (Node).Flag_1 := Procedure_Node;
   end Subprogram_Is_A_Procedure;

   --------------------
   -- Type_Is_Frozen --
   --------------------

   procedure Type_Is_Frozen
     (Type_Node  : in Type_Id;
      Extensible : in Boolean) is
      Node : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Flag_1 := Extensible;
   end Type_Is_Frozen;

   ------------------
   -- More_Recent_Stamp --
   ------------------

   procedure More_Recent_Stamp (P : in PID_Type; F : in File_Name_Type) is
   begin
      if More_Recent (F, Partitions.Table (P).Most_Recent) then
         if Debug_Mode then
            Write_Program_Name;
            Write_Str   (": ");
            Write_Name  (F);
            Write_File_Stamp (F);
            Write_Str   (", ");
            Write_Name  (Partitions.Table (P).Name);
            Write_Str   ("'s most recent file (previously ");
            Write_File_Stamp (Partitions.Table (P).Most_Recent);
            Write_Str   (")");
            Write_Eol;
         end if;
         Partitions.Table (P).Most_Recent := F;
      end if;
   end More_Recent_Stamp;

   ----------------
   -- Write_SLOC --
   ----------------

   procedure Write_SLOC (Node : Node_Id) is
      X, Y : Int;
   begin
      Get_Node_SLOC (Node, X, Y);
      Write_Name (Configuration_File);
      Write_Str (":");
      Write_Int (X);
      Write_Str (":");
      Write_Int (Y);
      Write_Str (": ");
   end Write_SLOC;

end XE;
