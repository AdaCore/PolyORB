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

with Namet;        use Namet;
with Output;       use Output;
with XE_Utils;     use XE_Utils;

package body XE is

   type Node_Kind is
      (K_Configuration,
       K_List,
       K_Type,
       K_Subprogram,
       K_Statement,
       K_Component,
       K_Variable);

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

   function Is_List
     (Node : Node_Id)
     return Boolean;

   function Is_Of_Kind
     (Node : Node_Id;
      Kind : Node_Kind)
     return Boolean;

   procedure Create_Node
     (Node : out Node_Id;
      Name : in  Name_Id;
      Kind : in  Node_Kind);

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
      pragma Assert (Is_List (List));
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
      pragma Assert (Is_List (List));
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
      pragma Assert (Is_List (List));
      if Nodes.Table (List).Node_2 = Null_Node then
         Nodes.Table (List).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      else
         Nodes.Table (Nodes.Table (List).Node_2).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      end if;
   end Add_Variable_Component;

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

   ------------------------
   -- Set_Attribute_Kind --
   ------------------------

   procedure Set_Attribute_Kind
     (Component_Node : in Component_Id;
      Attribute_Kind : in Attribute_Type) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Value := Convert (Attribute_Kind);
   end Set_Attribute_Kind;

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

   ----------------------
   -- Create_Component --
   ----------------------

   procedure Create_Component
     (Component_Node : out Component_Id;
      Component_Name : in  Name_Id) is
   begin
      Create_Node (Node_Id (Component_Node), Component_Name, K_Component);
   end Create_Component;

   --------------------------
   -- Create_Configuration --
   --------------------------

   procedure Create_Configuration
     (Configuration_Node : out Configuration_Id;
      Configuration_Name : in  Name_Id) is
   begin
      Create_Node
        (Node_Id (Configuration_Node), Configuration_Name, K_Configuration);
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
      Nodes.Table (Nodes.Last).Flag_1   := False;
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
      pragma Assert (Is_List (List));
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
      pragma Assert (Is_List (List));
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
      pragma Assert (Is_List (List));
      Component_Node := Component_Id (Nodes.Table (List).Node_1);
   end First_Variable_Component;

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

   ------------------------------
   -- Is_Component_Initialized --
   ------------------------------

   function Is_Component_Initialized
     (Component_Node : Component_Id)
      return Boolean is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Component_Initialized;

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
      pragma Assert (Is_Component_Initialized (Component_Node));
      return Nodes.Table (Node).Node_3;
   end Get_Component_Value;

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
      pragma Assert (Is_List (List));
      return Nodes.Table (List).Flag_1;
   end Is_Array_A_List;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Component);
   end Is_Component;

   ------------------------
   -- Get_Attribute_Kind --
   ------------------------

   function Get_Attribute_Kind
     (Component_Node : in Component_Id)
      return Attribute_Type is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      return Convert (Nodes.Table (Node).Value);
   end Get_Attribute_Kind;

   ----------------------
   -- Is_Configuration --
   ----------------------

   function Is_Configuration (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_Configuration);
   end Is_Configuration;

   -------------
   -- Is_List --
   -------------

   function Is_List (Node : Node_Id) return Boolean is
   begin
      return Is_Of_Kind (Node, K_List);
   end Is_List;

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
      Nodes.Table (List).Flag_1 := Array_Is_A_List;
      Nodes.Table (Node).Node_3 := List;
   end Set_Array_Type;

   ------------------------------
   -- Component_Is_Initialized --
   ------------------------------

   procedure Component_Is_Initialized
     (Component_Node : Component_Id;
      Is_Initialized : Boolean) is
      Node : Node_Id := Node_Id (Component_Node);
   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Flag_1 := Is_Initialized;
   end Component_Is_Initialized;

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
      Nodes.Table (Node).Flag_1 := True;
      Nodes.Table (Node).Node_3 := Value_Node;
   end Set_Component_Value;

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
