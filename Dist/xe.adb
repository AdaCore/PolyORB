------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                                  X E                                     --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
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
         Name    : Name_Id;
         Node_1  : Node_Id;
         Node_2  : Node_Id;
         Node_3  : Node_Id;
         Flag_1  : Boolean;
         Value   : Int;
      end record;

   --  list
   --     node_1 : next declaration head
   --     node_2 : declaration head
   --     node_3 : declaration tail
   --     flag_1 : unused
   --     value  : declaration list | parameter list | component list
   --  subprogram
   --     node_1 : next declaration
   --     node_2 :
   --     node_3 : parameter list
   --     flag_1 : is a procedure
   --     value  : unused
   --  type
   --     node_1 : next declaration
   --     node_2 : unused
   --     node_3 : component list | unused
   --     flag_1 : is structured
   --     value  : predefined_type'pos
   --  variable
   --     node_1 : next declaration
   --     node_2 : variable type
   --     node_3 : component list | unused
   --     flag_1 : is structured
   --     value  : used when enumeration type
   --  component
   --     node_1 : next component
   --     node_2 : component type
   --     node_3 : component value
   --     flag_1 : is an attribute
   --     value  : used when enumeration type
   --  statement
   --     node_1 : next declaration
   --     node_2 : subprogram call

   function Is_Of_Kind
     (Node : in Node_Id;
      Kind : in Node_Kind)
     return Boolean;

   function Is_Component_List
     (Node : in Node_Id)
     return Boolean;

   function Is_Parameter_List
     (Node : in Node_Id)
     return Boolean;

   function Is_Declaration_List
     (Node : in Node_Id)
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
      Table_Name           => "Node_Table");

   Context_Root_Node   : Node_Id := Null_Node;
   Function_Type_Node  : Node_Id := Null_Node;
   Procedure_Type_Node : Node_Id := Null_Node;

   First_Stamp : Boolean := True;

   -----------------------------
   -- Maybe_Most_Recent_Stamp --
   -----------------------------

   procedure Maybe_Most_Recent_Stamp (Stamp : Time_Stamp_Type) is
   begin
      if First_Stamp or else Stamp > Most_Recent_Stamp then
         First_Stamp := False;
         Most_Recent_Stamp := Stamp;
      end if;
   end Maybe_Most_Recent_Stamp;

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
         Write_Program_Name;
         Write_Str  (": ");
         Write_Name (Lib);
         Write_Str  (" not found");
         Write_Eol;
         raise Fatal_Error;
      end if;
      Read_ALI (Scan_ALI (Lib, Text));
   end Load_All_Units;

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
         Write_Str  (": create ");
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
      Partitions.Table (Partition).First_Unit      := Null_CUID;
      Partitions.Table (Partition).Last_Unit       := Null_CUID;
      Partitions.Table (Partition).To_Build        := True;
      PID := Partition;
   end Create_Partition;

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
         Write_Str  (": create ");
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
      if CPID = Null_PID or else
        CPID = Wrong_PID then
         Write_Program_Name;
         Write_Str (": gnatdist is going crazy");
         Write_Eol;
         raise Fatal_Error;
      end if;
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

      --  The configured unit name should not be a partition name.
      if Get_PID (CU) = Wrong_PID then
         Write_Program_Name;
         Write_Str  (": symbol ");
         Write_Name (CU);
         Write_Str  (" is already used");
         Write_Eol;
         raise Parsing_Error;
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

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Partition : PID_Type) return Boolean is
   begin
      return Partitions.Table (Partition).Last_Unit /= Null_CUID;
   end Is_Set;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (N : Name_Id) return PID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when 0 | PID_Null =>
            return Null_PID;
         when PID_First .. PID_Last =>
            return PID_Type (Info);
         when others =>
            return Wrong_PID;
      end case;
   end Get_PID;

   -------------
   -- Set_PID --
   -------------

   procedure Set_PID (N : Name_Id; P : PID_Type) is
   begin
      Set_Name_Table_Info (N, Int (P));
   end Set_PID;

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

   -----------------
   -- Set_Unit_Id --
   -----------------

   procedure Set_Unit_Id (N : Name_Id; U : Unit_Id) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_Unit_Id;

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

   ----------------
   -- Set_ALI_Id --
   ----------------

   procedure Set_ALI_Id (N : Name_Id; A : ALI_Id) is
   begin
      Set_Name_Table_Info (N, Int (A));
   end Set_ALI_Id;

   -------------
   -- Get_CUID --
   -------------

   function Get_CUID (N : Name_Id) return CUID_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when 0 | CUID_Null =>
            return Null_CUID;
         when CUID_First .. CUID_Last =>
            return CUID_Type (Info);
         when others =>
            return Wrong_CUID;
      end case;
   end Get_CUID;

   -------------
   -- Set_CUID --
   -------------

   procedure Set_CUID (N : Name_Id; U : CUID_Type) is
   begin
      Set_Name_Table_Info (N, Int (U));
   end Set_CUID;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (N : Name_Id) return Token_Type is
      Info : Int;
   begin
      Info := Get_Name_Table_Info (N);
      case Info is
         when Tkn_First .. Tkn_Last =>
            return Token_Type'Val
              (Info - Int (Wrong_Token) +
               Int (Token_Type'Pos (Token_Type'First)));
         when others =>
            return Tok_Unknown;
      end case;
   end Get_Token;

   ---------------
   -- Set_Token --
   ---------------

   procedure Set_Token (N : String; T : Token_Type) is
      Name  : Name_Id;
      Index : Int;
   begin
      Index := Int (Wrong_Token) +
               Int (Token_Type'Pos (T) -
                    Token_Type'Pos (Token_Type'First));
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Index);
      Reserved (T) := True;
   end Set_Token;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Boolean) return Int is
   begin
      return Int (Boolean'Pos (Item));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Boolean is
   begin
      return Boolean'Val (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Starter_Method_Type) return Int is
   begin
      return Int (Starter_Method_Type'Pos (Item));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Starter_Method_Type is
   begin
      return Starter_Method_Type'Val (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Import_Method_Type) return Int is
   begin
      return Int (Import_Method_Type'Pos (Item));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Import_Method_Type is
   begin
      return Import_Method_Type'Val (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Predefined_Type) return Int is
   begin
      return Int (Wrong_Pre_Type) +
             Int (Predefined_Type'Pos (Item) -
                  Predefined_Type'Pos (Predefined_Type'First));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Predefined_Type is
   begin
      if Item > Pre_Type_Last or else
         Item < Pre_Type_First then
         return Pre_Type_Unknown;
      else
         return Predefined_Type'Val
           (Item - Pre_Type_Wrong +
            Int (Predefined_Type'Pos (Predefined_Type'First)));
      end if;
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

   -------------
   -- Convert --
   -------------

   function Convert (Item : Attribute_Type) return Int is
   begin
      return Int (Wrong_Attribute) +
             Int (Attribute_Type'Pos (Item) -
                  Attribute_Type'Pos (Attribute_Type'First));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Attribute_Type is
   begin
      if Item > Attr_Last or else
         Item < Attr_First then
         return Attribute_Unknown;
      else
         return Attribute_Type'Val
           (Item - Attr_Wrong +
            Int (Attribute_Type'Pos (Attribute_Type'First)));
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Pragma_Type) return Int is
   begin
      return Int (Wrong_Pragma) +
             Int (Pragma_Type'Pos (Item) -
                  Pragma_Type'Pos (Pragma_Type'First));
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Pragma_Type is
   begin
      if Item > Prag_Last or else
         Item < Prag_First then
         return Pragma_Unknown;
      else
         return Pragma_Type'Val
           (Item - Prag_Wrong +
            Int (Pragma_Type'Pos (Pragma_Type'First)));
      end if;
   end Convert;

   ---------------
   -- Str_To_Id --
   ---------------

   function Str_To_Id (S : String) return Name_Id is
   begin
      Name_Buffer (1 .. S'Length) := S;
      Name_Len := S'Length;
      return Name_Find;
   end Str_To_Id;

   -------------------
   -- Get_Node_Name --
   -------------------

   function  Get_Node_Name
     (Node : in Node_Id)
     return Name_Id is
   begin
      return Nodes.Table (Node).Name;
   end Get_Node_Name;

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
   -- Is_Component_List --
   -----------------------

   function Is_Component_List
     (Node : in Node_Id)
      return Boolean is
   begin
      return Nodes.Table (Node).Kind = K_List and then
             Convert (Nodes.Table (Node).Value) = K_Component_List;
   end Is_Component_List;

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

   -------------
   -- Is_Type --
   -------------

   function Is_Type (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_Type);
   end Is_Type;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (Node : Node_Id)  return Boolean is
   begin
      pragma Assert (Node /= Null_Node);
      return Is_Of_Kind (Node, K_Variable);
   end Is_Variable;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Statement);
   end Is_Statement;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Component);
   end Is_Component;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_Subprogram);
   end Is_Subprogram;

   ----------------------
   -- Is_Configuration --
   ----------------------

   function Is_Configuration (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_List) and then
             Convert (Nodes.Table (Node).Value) = K_Declaration_List;
   end Is_Configuration;

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
   -- Create_Component --
   ----------------------

   procedure Create_Component
     (Component_Node : out Component_Id;
      Component_Name : in  Name_Id) is
   begin
      Create_Node (Node_Id (Component_Node), Component_Name, K_Component);
   end Create_Component;

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

   ------------------------------------
   -- Next_Configuration_Declaration --
   ------------------------------------

   procedure Next_Configuration_Declaration
     (Declaration_Node   : in out Node_Id) is
   begin
      Declaration_Node := Nodes.Table (Declaration_Node).Node_1;
   end Next_Configuration_Declaration;

   --------------------------------------
   -- Append_Configuration_Declaration --
   --------------------------------------

   procedure Append_Configuration_Declaration
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
   end Append_Configuration_Declaration;

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

   -------------------------
   -- Type_Is_A_Structure --
   -------------------------

   procedure Type_Is_A_Structure
     (Type_Node : in Type_Id;
      Structure : in Boolean) is
      Node : Node_Id := Node_Id (Type_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Flag_1 := Structure;
      Create_Node (List, Str_To_Id ("pragma__n__record"), K_List);
      Nodes.Table (List).Value := Convert (K_Component_List);
      Nodes.Table (Node).Node_3 := List;
   end Type_Is_A_Structure;

   -------------------------
   -- Is_Type_A_Structure --
   -------------------------

   function Is_Type_A_Structure
     (Type_Node : in Type_Id)
     return Boolean is
      Node : Node_Id := Node_Id (Type_Node);
   begin
      pragma Assert (Is_Type (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Type_A_Structure;

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

   -------------------------
   -- Next_Type_Component --
   -------------------------

   procedure Next_Type_Component
     (Component_Node  : in out Component_Id) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Type_Component;

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

   -----------------------------
   -- Variable_Is_A_Structure --
   -----------------------------

   procedure Variable_Is_A_Structure
     (Variable_Node : in Variable_Id;
      Structure     : in Boolean) is
      Node : Node_Id := Node_Id (Variable_Node);
      List : Node_Id;
   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Flag_1 := Structure;
      Create_Node (List, Str_To_Id ("pragma__n__record"), K_List);
      Nodes.Table (List).Value := Convert (K_Component_List);
      Nodes.Table (Node).Node_3 := List;
   end Variable_Is_A_Structure;

   -----------------------------
   -- Is_Variable_A_Structure --
   -----------------------------

   function Is_Variable_A_Structure
     (Variable_Node : in Variable_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Variable_Node);
   begin
      pragma Assert (Is_Variable (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Variable_A_Structure;

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
      if Is_Type_A_Structure (Variable_Type) then
         Create_Node (List, Str_To_Id ("record"), K_List);
         Nodes.Table (List).Value := Convert (K_Component_List);
         Nodes.Table (Node).Node_3 := List;
      end if;
   end Set_Variable_Type;

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

   -----------------------------
   -- Next_Variable_Component --
   -----------------------------

   procedure Next_Variable_Component
     (Component_Node  : in out Component_Id) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Variable_Component;

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

   -------------------------
   -- Set_Component_Value --
   -------------------------

   procedure Set_Component_Value
     (Component_Node : in Component_Id;
      Value_Node     : in Node_Id) is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      Nodes.Table (Node).Node_3 := Value_Node;
   end Set_Component_Value;

   ------------------------
   -- Get_Component_Value --
   ------------------------

   function Get_Component_Value
     (Component_Node : in Component_Id)
      return Node_Id is
      Node  : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Node_3;
   end Get_Component_Value;

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

   ------------------------
   -- Set_Parameter_Type --
   ------------------------

   procedure Set_Parameter_Type
     (Parameter_Node : in Parameter_Id;
      Parameter_Type : in Type_Id) is
   begin
      Set_Variable_Type (Variable_Id (Parameter_Node), Parameter_Type);
   end Set_Parameter_Type;

   ------------------------
   -- Get_Parameter_Type --
   ------------------------

   function Get_Parameter_Type
     (Parameter_Node : in Parameter_Id)
     return Type_Id is
   begin
      return Get_Variable_Type (Variable_Id (Parameter_Node));
   end Get_Parameter_Type;

   -------------------------
   --  Show_Configuration --
   -------------------------

   procedure Show_Configuration is

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
      Write_Str ("   Name     : ");
      Write_Name (Configuration);
      Write_Eol;

      Write_Str ("   Main     : ");
      Write_Name (Main_Subprogram);
      Write_Eol;

      Write_Str ("   Starter  : ");
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
         Write_Str  ("   Protocol : ");
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

            if I.Main_Subprogram /= No_Name then
               Write_Str ("   Main     : ");
               Write_Name (I.Main_Subprogram);
               Write_Eol;
            end if;

            Host := I.Host;
            if Host = Null_Host then
               Host := Default_Host;
            end if;

            if Host /= Null_Host then
               Write_Str ("   Host     : ");
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

            if I.Storage_Dir /= No_Storage_Dir then
               Storage_Dir := I.Storage_Dir;
            elsif Default_Storage_Dir /= No_Storage_Dir then
               Storage_Dir := Default_Storage_Dir;
            else
               Storage_Dir := No_Storage_Dir;
            end if;

            if Storage_Dir /= No_Storage_Dir then
               Write_Str ("   Storage  : ");
               Write_Name (Storage_Dir);
               Write_Eol;
            end if;

            if I.Command_Line /= No_Command_Line then
               Command_Line := I.Command_Line;
            elsif Default_Command_Line /= No_Command_Line then
               Command_Line := Default_Command_Line;
            else
               Command_Line := No_Command_Line;
            end if;

            if Command_Line /= No_Command_Line then
               Write_Str ("   Command  : ");
               Write_Name (Command_Line);
               Write_Eol;
            end if;

            if I.First_Unit /= Null_CUID then
               Write_Str ("   Units    : ");
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

   end Show_Configuration;

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
         if Name_Buffer (1) /= Separator then

            --  The storage dir is relative

            return PWD_Id & Dir & Dir_Sep_Id & Name;

         end if;

         --  Write the dir as it has been written

         return Dir & Dir_Sep_Id & Name;

      end if;

   end Get_Absolute_Exec;

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

   function Get_Command_Line    (P : in PID_Type) return Name_Id is
      Cmd : Command_Line_Type := Partitions.Table (P).Command_Line;
   begin

      if Cmd = No_Command_Line then
         Cmd := Default_Command_Line;
      end if;

      return Cmd;

   end Get_Command_Line;

   function Get_Main_Subprogram (P : in PID_Type) return Name_Id is
      Main : Main_Subprogram_Type := Partitions.Table (P).Main_Subprogram;
   begin

      if Main = No_Main_Subprogram then
         Main := Default_Main;
      end if;

      return Main;

   end Get_Main_Subprogram;

   function Get_Unit_Sfile (U : in Unit_Id) return File_Name_Type is
   begin
      Get_Name_String (Unit.Table (U).Sfile);
      Name_Len := Name_Len - 4;
      return Name_Find;
   end Get_Unit_Sfile;

end XE;







