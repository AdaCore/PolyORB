------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                   X E                                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with Table;
with Types;        use Types;
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
   --     node_1 : list head
   --     node_2 : list tail
   --     node_3 : component type
   --     flag_1 : unused
   --     value  : length
   --  subprogram
   --     node_1 : next declaration
   --     node_2 : appl. main procedure
   --     node_3 : parameter list
   --     flag_1 : is a procedure
   --     value  : pragma_type
   --  type
   --     node_1 : next declaration
   --     node_2 : array component type
   --     node_3 : component list
   --     flag_1 : unused
   --     value  : predefined_type
   --  variable
   --     node_1 : next declaration
   --     node_2 : variable type
   --     node_3 : component list | subprogram (1)
   --     flag_1 : initialized
   --     value  : when list: number of components; when scalar: value
   --  component
   --     node_1 : next component
   --     node_2 : component type
   --     node_3 : component value
   --     flag_1 : initialized
   --     value  : attribute_type
   --  statement
   --     node_1 : next declaration
   --     node_2 : subprogram call
   --     node_3 : unused
   --     flag_1 : unused
   --     value  : unused
   --  configuration
   --     node_1 : declaration head (in another way, next declaration)
   --     node_2 : declaration tail
   --     node_3 : previous configuration
   --     flag_1 : unused
   --     value  : unused
   --
   --  (1) Ada units are in our context enumeration literal, but in the
   --  context of Ada, there are also subprograms. When the variable type
   --  is Pre_Type_Ada_Unit, then the value of this variable is a
   --  subprogram.

   N_Anonymous_Variable : Int := 0;
   --  Count the number of created anonymous variables.

   package Nodes is new Table.Table
     (Table_Component_Type => Node_Type,
      Table_Index_Type     => Node_Id,
      Table_Low_Bound      => First_Node,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Nodes");

   procedure Create_Node
     (Node : out Node_Id;
      Name : in  Name_Id;
      Kind : in  Node_Kind);

   function Is_List
     (Node : Node_Id)
     return Boolean;

   function Is_Of_Kind
     (Node : Node_Id;
      Kind : Node_Kind)
     return Boolean;

   -----------------------------------
   -- Add_Configuration_Declaration --
   -----------------------------------

   procedure Add_Configuration_Declaration
     (Configuration_Node : in Configuration_Id;
      Declaration_Node   : in Node_Id)
   is
      CN : Node_Id := Node_Id (Configuration_Node);
      DN : Node_Id := Node_Id (Declaration_Node);

   begin
      pragma Assert (Is_Configuration (CN));

      if Is_Configuration (DN) then

         --  If Conf has already a configuration in its scope, then
         --  insert the new configuration in between. Then, link the
         --  last declaration of the new configuration with Conf.

         if Nodes.Table (CN).Node_3 /= Null_Node then
            Add_Configuration_Declaration
              (Configuration_Id (DN),
               Nodes.Table (CN).Node_3);
            Nodes.Table (DN).Node_3 := CN;

         else
            Nodes.Table (CN).Node_3 := DN;

            --  If the new configuration has declarations in it, then
            --  link the last declaration with Conf.

            if Nodes.Table (DN).Node_2 /= Null_Node then
               Nodes.Table (Nodes.Table (DN).Node_2).Node_1 := CN;

            else
               Nodes.Table (DN).Node_1 := CN;
            end if;
         end if;

      elsif Nodes.Table (CN).Node_2 /= Null_Node then

         --  In this case, Conf contains declarations. The last
         --  declaration of Conf may point to another
         --  configuration. When we add a declaration at the end of
         --  this configuration, we have to save the link to the other
         --  configuration in the new declaration node.

         Nodes.Table (DN).Node_1
           := Nodes.Table (Nodes.Table (CN).Node_2).Node_1;
         Nodes.Table (Nodes.Table (CN).Node_2).Node_1 := DN;
         Nodes.Table (CN).Node_2 := DN;

      else

         --  Conf is empty.

         Nodes.Table (DN).Node_1 := Nodes.Table (CN).Node_1;
         Nodes.Table (CN).Node_2 := DN;
         Nodes.Table (CN).Node_1 := DN;
      end if;

      --  When we add a configuration to another configuration we want the
      --  the following structure. This is done to ensure that we explore
      --  all the declarations by following node_1 of each node. To start
      --  from the beginning of the declarations of a configuration, we
      --  first jump to the previous configurations by following node_3
      --  of each configuration node.
      --
      --    conf-root            node               node
      --    +--------+        +--------+         +--------+
      --    | node 1 |------->| node 1 |-------->| node 1 |
      --    +--------+        +--------+         +--------+
      --    | node 2 |-       | node 2 |         | node 2 |
      --    +--------+ \      +--------+         +--------+
      --    | node 3 |  \     | node 3 |     --->| node 3 |
      --    +--------+   \    +--------+    /    +--------+
      --      |    ^      ------------------
      --      |    |
      --      |     \
      --      |      -------------------------------
      --      |                                     \
      --      v                                      |
      --    conf-decl            node               node
      --    +--------+        +--------+         +--------+
      --    | node 1 |------->| node 1 |-------->| node 1 |
      --    +--------+        +--------+         +--------+
      --    | node 2 |-       | node 2 |         | node 2 |
      --    +--------+  \     +--------+         +--------+
      --    | node 3 |   \    | node 3 |     --->| node 3 |
      --    +--------+    \   +--------+    /    +--------+
      --                   -----------------
   end Add_Configuration_Declaration;

   ------------------------------
   -- Add_Subprogram_Parameter --
   ------------------------------

   procedure Add_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : in Parameter_Id)
   is
      Node  : Node_Id := Node_Id (Subprogram_Node);
      List  : Node_Id;
      Value : Node_Id := Node_Id (Parameter_Node);

   begin
      pragma Assert (Is_Subprogram (Node));

      --  Get the list of the actual or formal parameters.

      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_List (List));

      if Nodes.Table (List).Node_1 = Null_Node then
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
      Component_Node  : in Component_Id)
   is
      TN : Node_Id := Node_Id (Type_Node);
      CL : Node_Id;
      CN : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Type (TN));

      CL := Nodes.Table (TN).Node_3;
      pragma Assert (Is_List (CL));

      if Nodes.Table (CL).Node_1 = Null_Node then
         Nodes.Table (CL).Node_1 := CN;
         Nodes.Table (CL).Node_2 := CN;

      else
         Nodes.Table (Nodes.Table (CL).Node_2).Node_1 := CN;
         Nodes.Table (CL).Node_2 := CN;
      end if;
   end Add_Type_Component;

   ----------------------------
   -- Add_Variable_Component --
   ----------------------------

   procedure Add_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : in Component_Id)
   is
      Node  : Node_Id := Node_Id (Variable_Node);
      List  : Node_Id;
      Value : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Variable (Node));

      List := Nodes.Table (Node).Node_3;
      pragma Assert (Is_List (List));

      if Nodes.Table (List).Node_1 = Null_Node then
         Nodes.Table (List).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;

      else
         Nodes.Table (Nodes.Table (List).Node_2).Node_1 := Value;
         Nodes.Table (List).Node_2 := Value;
      end if;
   end Add_Variable_Component;

   ------------------------------
   -- Component_Is_Initialized --
   ------------------------------

   procedure Component_Is_Initialized
     (Component_Node : in Component_Id;
      Is_Initialized : in Boolean)
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Flag_1 := Is_Initialized;
   end Component_Is_Initialized;

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

   function Convert (Item : Reconnection_Type) return Int is
   begin
      return Int (Item);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Item : Int) return Reconnection_Type is
   begin
      pragma Assert
        (Item in
         Int (Reconnection_Type'First) ..
         Int (Reconnection_Type'Last));
      return Reconnection_Type (Item);
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
   begin
      Create_Node (Node_Id (Statement_Node), Statement_Name, K_Statement);
   end Create_Statement;

   -----------------------
   -- Create_Subprogram --
   -----------------------

   procedure Create_Subprogram
     (Subprogram_Node : out Subprogram_Id;
      Subprogram_Name : in  Name_Id)
   is
      Node : Node_Id;
      List : Node_Id;

   begin
      Create_Node (Node, Subprogram_Name, K_Subprogram);
      Create_Node (List, No_Name, K_List);
      Nodes.Table (Node).Node_3 := List;
      Subprogram_Node := Subprogram_Id (Node);
   end Create_Subprogram;

   -----------------
   -- Create_Type --
   -----------------

   procedure Create_Type
     (Type_Node : out Type_Id;
      Type_Name : in  Name_Id)
   is
      Node : Node_Id;

   begin
      Create_Node (Node, Type_Name, K_Type);
      Type_Node := Type_Id (Node);
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
      Declaration_Node   : out Node_Id)
   is
      Conf : Node_Id := Node_Id (Configuration_Node);

   begin
      pragma Assert (Is_Configuration (Conf));

      --  Check that there is no configuration in front of the current
      --  configuration. If this is the case, then start from the
      --  previous configuration.

      if Nodes.Table (Conf).Node_3 /= Null_Node then
         First_Configuration_Declaration
           (Configuration_Id (Nodes.Table (Conf).Node_3),
            Declaration_Node);

      else
         Declaration_Node := Conf;
      end if;
   end First_Configuration_Declaration;

   --------------------------------
   -- First_Subprogram_Parameter --
   --------------------------------

   procedure First_Subprogram_Parameter
     (Subprogram_Node : in Subprogram_Id;
      Parameter_Node  : out Parameter_Id)
   is
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
      Component_Node  : out Component_Id)
   is
      TN : Node_Id := Node_Id (Type_Node);
      CL : Node_Id;

   begin
      pragma Assert (Is_Type (TN));
      CL := Nodes.Table (TN).Node_3;
      pragma Assert (Is_List (CL));
      Component_Node := Component_Id (Nodes.Table (CL).Node_1);
   end First_Type_Component;

   ------------------------------
   -- First_Variable_Component --
   ------------------------------

   procedure First_Variable_Component
     (Variable_Node   : in Variable_Id;
      Component_Node  : out Component_Id)
   is
      VN : Node_Id := Node_Id (Variable_Node);
      CL : Node_Id;

   begin
      pragma Assert (Is_Variable (VN));
      CL := Nodes.Table (VN).Node_3;
      pragma Assert (Is_List (CL));
      Component_Node := Component_Id (Nodes.Table (CL).Node_1);
   end First_Variable_Component;

   ------------------------------
   -- Get_Array_Component_Type --
   ------------------------------

   function Get_Array_Component_Type
     (Type_Node   : in Type_Id)
     return Type_Id
   is
      TN : Node_Id := Node_Id (Type_Node);

   begin
      pragma Assert (Is_Type_Composite (Type_Node));
      if  Get_Array_Length (Type_Node) = 0 then
         return Null_Type;
      else
         return Type_Id (Nodes.Table (TN).Node_2);
      end if;
   end Get_Array_Component_Type;

   ----------------------
   -- Get_Array_Length --
   ----------------------

   function Get_Array_Length
     (Type_Node : Type_Id)
     return Int
   is
      TN : Node_Id := Node_Id (Type_Node);
      CL : Node_Id;

   begin
      pragma Assert (Is_Type_Composite (Type_Node));
      CL := Nodes.Table (TN).Node_3;
      return Nodes.Table (CL).Value;
   end Get_Array_Length;

   ----------------------
   -- Get_Array_Length --
   ----------------------

   function Get_Array_Length
     (Variable_Node : Variable_Id)
     return Int
   is
      VN : Node_Id := Node_Id (Variable_Node);
      VT : Type_Id := Get_Variable_Type (Variable_Node);

   begin
      pragma Assert (Is_Variable (VN));
      pragma Assert (Get_Array_Length (VT) /= 0);
      return Nodes.Table (VN).Value;
   end Get_Array_Length;

   ------------------------
   -- Get_Attribute_Kind --
   ------------------------

   function Get_Attribute_Kind
     (Component_Node : in Component_Id)
     return Attribute_Type
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component (Node));
      return Convert (Nodes.Table (Node).Value);
   end Get_Attribute_Kind;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name
     (Component : Component_Id)
     return Name_Id is
   begin
      return Nodes.Table (Node_Id (Component)).Name;
   end Get_Component_Name;

   ------------------------
   -- Get_Component_Type --
   ------------------------

   function Get_Component_Type
     (Component_Node : in Component_Id)
     return Type_Id
   is
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
     return Variable_Id
   is
      Node  : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component_Initialized (Component_Node));
      Node := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Variable (Node));
      return Variable_Id (Node);
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

   procedure Get_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : out Int;
      Loc_Y : out Int) is
   begin
      Loc_X := Nodes.Table (Node).Loc_X;
      Loc_Y := Nodes.Table (Node).Loc_Y;
   end Get_Node_SLOC;

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
   -- Get_Parameter_Value --
   -------------------------

   function Get_Parameter_Value
     (Parameter_Node : Parameter_Id)
     return Variable_Id
   is
      Node  : Node_Id := Node_Id (Parameter_Node);

   begin
      pragma Assert (Is_Parameter_Initialized (Parameter_Node));
      Node := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Variable (Node));
      return Variable_Id (Node);
   end Get_Parameter_Value;

   ---------------------
   -- Get_Pragma_Kind --
   ---------------------

   function  Get_Pragma_Kind
     (Subprogram_Node : in Subprogram_Id)
     return Pragma_Type
   is
      Node : Node_Id := Node_Id (Subprogram_Node);

   begin
      pragma Assert (Is_Subprogram (Node));
      return Convert (Nodes.Table (Node).Value);
   end Get_Pragma_Kind;

   ----------------------
   -- Get_Scalar_Value --
   ----------------------

   function  Get_Scalar_Value
     (Variable_Node : Variable_Id)
     return Int
   is
      Node : Node_Id := Node_Id (Variable_Node);
      VT   : Type_Id;

   begin
      pragma Assert (Is_Variable_Initialized (Variable_Node));
      VT := Get_Variable_Type (Variable_Node);
      pragma Assert (not Is_Type_Composite (VT));
      return Nodes.Table (Node).Value;
   end Get_Scalar_Value;

   -------------------------
   -- Get_Subprogram_Call --
   -------------------------

   function  Get_Subprogram_Call
     (Statement_Node  : in Statement_Id)
     return Subprogram_Id
   is
      Node : Node_Id := Node_Id (Statement_Node);

   begin
      pragma Assert (Is_Statement (Node));
      return Subprogram_Id (Nodes.Table (Node).Node_2);
   end Get_Subprogram_Call;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (N : Name_Id)
     return Token_Type
   is
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
   -- Get_Type_Kind --
   -------------------

   function  Get_Type_Kind
     (Type_Node : in Type_Id)
     return Predefined_Type
   is
      Node : Node_Id := Node_Id (Type_Node);

   begin
      pragma Assert (Is_Type (Node));
      return Convert (Nodes.Table (Node).Value);
   end Get_Type_Kind;

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Variable : Variable_Id)
     return Name_Id is
   begin
      return Nodes.Table (Node_Id (Variable)).Name;
   end Get_Variable_Name;

   -----------------------
   -- Get_Variable_Type --
   -----------------------

   function Get_Variable_Type
     (Variable_Node : in Variable_Id)
     return Type_Id
   is
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
     return Variable_Id
   is
      Node  : Node_Id := Node_Id (Variable_Node);

   begin
      pragma Assert (Is_Variable_Initialized (Variable_Node));
      Node := Nodes.Table (Node).Node_3;
      pragma Assert (Is_Variable (Node) or else Is_Subprogram (Node));
      return Variable_Id (Node);
   end Get_Variable_Value;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Termination_Img
        := (Unknown_Termination  => Str_To_Id ("Undefined Termination"),
            Local_Termination    => Str_To_Id ("Local_Termination"),
            Global_Termination   => Str_To_Id ("Global_Termination"),
            Deferred_Termination => Str_To_Id ("Deferred_Termination"));

      Reconnection_Img
        := (Unknown_Reconnection  => Str_To_Id ("Undefined Reconnection"),
            Reject_On_Restart   => Str_To_Id ("Reject_On_Restart"),
            Block_Until_Restart => Str_To_Id ("Block_Until_Restart"),
            Fail_Until_Restart  => Str_To_Id ("Fail_Until_Restart"));

      Boolean_Img
        := (Bunknown  => Str_To_Id ("Undefined Boolean"),
            Bfalse    => Str_To_Id ("False"),
            Btrue     => Str_To_Id ("True"));
   end Initialize;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component (Node : Node_Id)  return Boolean is
   begin
      return Is_Of_Kind (Node, K_Component);
   end Is_Component;

   ------------------------------
   -- Is_Component_Initialized --
   ------------------------------

   function Is_Component_Initialized
     (Component_Node : Component_Id)
     return Boolean
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Component_Initialized;

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

   ------------------------------
   -- Is_Parameter_Initialized --
   ------------------------------

   function Is_Parameter_Initialized
     (Parameter_Node : in Parameter_Id)
     return Boolean
   is
      Node : Node_Id := Node_Id (Parameter_Node);

   begin
      pragma Assert (Is_Variable (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Parameter_Initialized;

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
     return Boolean
   is
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

   -----------------------
   -- Is_Type_Composite --
   -----------------------

   function Is_Type_Composite
     (Type_Node : in Type_Id)
     return Boolean
   is
      TN : Node_Id := Node_Id (Type_Node);

   begin
      pragma Assert (Is_Type (TN));
      return Nodes.Table (TN).Node_3 /= Null_Node;
   end Is_Type_Composite;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (Node : Node_Id)  return Boolean is
   begin
      pragma Assert (Node /= Null_Node);
      return Is_Of_Kind (Node, K_Variable);
   end Is_Variable;

   -----------------------------
   -- Is_Variable_Initialized --
   -----------------------------

   function Is_Variable_Initialized
     (Variable_Node : Variable_Id)
     return Boolean
   is
      Node : Node_Id := Node_Id (Variable_Node);

   begin
      pragma Assert (Is_Variable (Node));
      return Nodes.Table (Node).Flag_1;
   end Is_Variable_Initialized;

   ------------------
   -- Jump_Context --
   ------------------

   procedure Jump_Context
     (Context : in Context_Type) is
   begin
      if Debug_Mode then
         Write_Str ("delete [");
         Write_Int (Int (Context.Last_Node));
         Write_Str (" .. ");
         Write_Int (Int (Nodes.Last));
         Write_Str ("]");
         Write_Eol;
      end if;

      --  Cut the link to the next declarations because there are no
      --  longer meaningfull.

      Nodes.Table (Context.Last_Decl).Node_1 := Null_Node;

      --  Update the configuration tail.

      Nodes.Table (Context.Conf_Node).Node_2 := Context.Last_Decl;

      --  Remove the nodes created in the previous context.

      loop
         exit when Nodes.Last <= Node_Id (Context.Last_Node);
         Nodes.Decrement_Last;
      end loop;
      N_Anonymous_Variable := Context.Anonymous;
   end Jump_Context;

   ------------------------
   -- New_Component_Name --
   ------------------------

   function New_Component_Name
     (Variable_Node : Variable_Id)
     return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("C [");
      Add_Nat_To_Name_Buffer (Nat (Get_Array_Length (Variable_Node)));
      Add_Str_To_Name_Buffer ("]");
      return Name_Find;
   end New_Component_Name;

   -----------------------
   -- New_Variable_Name --
   -----------------------

   function New_Variable_Name return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("V_");
      Add_Nat_To_Name_Buffer (Nat (N_Anonymous_Variable));
      N_Anonymous_Variable := N_Anonymous_Variable + 1;
      return Name_Find;
   end New_Variable_Name;

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
     (Parameter_Node  : in out Parameter_Id)
   is
      Node : Node_Id := Node_Id (Parameter_Node);

   begin
      pragma Assert (Is_Variable (Node));
      Parameter_Node := Parameter_Id (Nodes.Table (Node).Node_1);
   end Next_Subprogram_Parameter;

   -------------------------
   -- Next_Type_Component --
   -------------------------

   procedure Next_Type_Component
     (Component_Node  : in out Component_Id)
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Type_Component;

   -----------------------------
   -- Next_Variable_Component --
   -----------------------------

   procedure Next_Variable_Component
     (Component_Node  : in out Component_Id)
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      Component_Node := Component_Id (Nodes.Table (Node).Node_1);
   end Next_Variable_Component;

   ------------------------------
   -- Parameter_Is_Initialized --
   ------------------------------

   procedure Parameter_Is_Initialized
     (Parameter_Node : in Parameter_Id;
      Is_Initialized : in Boolean)
   is
      Node : Node_Id := Node_Id (Parameter_Node);

   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Flag_1 := Is_Initialized;
   end Parameter_Is_Initialized;

   ------------------
   -- Save_Context --
   ------------------

   procedure Save_Context
     (Configuration : in Configuration_Id;
      Context       : out Context_Type) is
   begin
      Context.Last_Node := Nodes.Last;
      Context.Last_Decl := Nodes.Table (Node_Id (Configuration)).Node_2;
      Context.Conf_Node := Node_Id (Configuration);
      Context.Anonymous := N_Anonymous_Variable;
   end Save_Context;

   ------------------------------
   -- Set_Array_Component_Type --
   ------------------------------

   procedure Set_Array_Component_Type
     (Type_Node : in Type_Id;
      Comp_Type : in Type_Id)
   is
      Node : Node_Id := Node_Id (Type_Node);

   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Node_2 := Node_Id (Comp_Type);
   end Set_Array_Component_Type;

   ----------------------
   -- Set_Array_Length --
   ----------------------

   procedure Set_Array_Length
     (Type_Node    : in Type_Id;
      Array_Length : in Int)
   is
      TN : Node_Id := Node_Id (Type_Node);
      CL : Node_Id;

   begin
      pragma Assert (Is_Type_Composite (Type_Node));
      CL := Nodes.Table (TN).Node_3;
      Nodes.Table (CL).Value := Array_Length;
   end Set_Array_Length;

   ----------------------
   -- Set_Array_Length --
   ----------------------

   procedure Set_Array_Length
     (Variable_Node : in Variable_Id;
      Array_Length  : in Int)
   is
      VN : Node_Id := Node_Id (Variable_Node);
      VT : Type_Id;

   begin
      pragma Assert (Is_Variable (VN));
      VT := Get_Variable_Type (Variable_Node);
      pragma Assert (Is_Type_Composite (VT));
      pragma Assert (Get_Array_Length (VT) /= 0);
      Nodes.Table (VN).Value := Array_Length;
   end Set_Array_Length;

   ------------------------
   -- Set_Attribute_Kind --
   ------------------------

   procedure Set_Attribute_Kind
     (Component_Node : in Component_Id;
      Attribute_Kind : in Attribute_Type)
   is
      Node : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Value := Convert (Attribute_Kind);
   end Set_Attribute_Kind;

   ------------------------
   -- Set_Component_Type --
   ------------------------

   procedure Set_Component_Type
     (Component_Node : in Component_Id;
      Type_Node      : in Type_Id)
   is
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
      Value_Node     : in Variable_Id)
   is
      Node  : Node_Id := Node_Id (Component_Node);

   begin
      pragma Assert (Is_Component (Node));
      Nodes.Table (Node).Flag_1 := True;
      Nodes.Table (Node).Node_3 := Node_Id (Value_Node);
   end Set_Component_Value;

   -------------------
   -- Set_Node_SLOC --
   -------------------

   procedure Set_Node_SLOC
     (Node  : in Node_Id;
      Loc_X : in Int;
      Loc_Y : in Int) is
   begin
      Nodes.Table (Node).Loc_X := Loc_X;
      Nodes.Table (Node).Loc_Y := Loc_Y;
   end Set_Node_SLOC;

   ------------------------
   -- Set_Parameter_Type --
   ------------------------

   procedure Set_Parameter_Type
     (Parameter_Node : in Parameter_Id;
      Parameter_Type : in Type_Id) is
   begin
      Set_Variable_Type (Variable_Id (Parameter_Node), Parameter_Type);
   end Set_Parameter_Type;

   ---------------------
   -- Set_Pragma_Kind --
   ---------------------

   procedure Set_Pragma_Kind
     (Subprogram_Node : in Subprogram_Id;
      Pragma_Kind     : in Pragma_Type)
   is
      Node : Node_Id := Node_Id (Subprogram_Node);

   begin
      pragma Assert (Is_Subprogram (Node));
      Nodes.Table (Node).Value := Convert (Pragma_Kind);
   end Set_Pragma_Kind;

   ----------------------
   -- Set_Scalar_Value --
   -----------------------

   procedure Set_Scalar_Value
     (Variable_Node : in Variable_Id;
      Scalar_Value  : in Int)
   is
      Node : Node_Id := Node_Id (Variable_Node);

   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Flag_1 := True;
      Nodes.Table (Node).Value := Scalar_Value;
   end Set_Scalar_Value;

   ------------------------
   -- Set_Subprogram_Call --
   ------------------------

   procedure Set_Subprogram_Call
     (Statement_Node  : in Statement_Id;
      Subprogram_Node : in Subprogram_Id)
   is
      Statement  : Node_Id := Node_Id (Statement_Node);
      Subprogram : Node_Id := Node_Id (Subprogram_Node);

   begin
      pragma Assert (Is_Statement  (Statement) and then
                     Is_Subprogram (Subprogram));
      Nodes.Table (Statement).Node_2 := Subprogram;
   end Set_Subprogram_Call;

   ---------------
   -- Set_Token --
   ---------------

   procedure Set_Token
     (N : in String;
      T : in Token_Type)
   is
      Name  : Name_Id;

   begin
      Name_Len := N'Length;
      Name_Buffer (1 .. Name_Len) := N;
      Name := Name_Find;
      Set_Name_Table_Info (Name, Int (T));
      Reserved (T) := True;
   end Set_Token;

   -------------------
   -- Set_Type_Kind --
   -------------------

   procedure Set_Type_Kind
     (Type_Node : in Type_Id;
      Type_Kind : in Predefined_Type)
   is
      Node : Node_Id := Node_Id (Type_Node);

   begin
      pragma Assert (Is_Type (Node));
      Nodes.Table (Node).Value := Convert (Type_Kind);
   end Set_Type_Kind;

   -----------------------
   -- Set_Variable_Type --
   -----------------------

   procedure Set_Variable_Type
     (Variable_Node : in Variable_Id;
      Variable_Type : in Type_Id)
   is
      VN : Node_Id := Node_Id (Variable_Node);
      L  : Node_Id;

   begin
      pragma Assert (Is_Variable (VN));
      Nodes.Table (VN).Node_2 := Node_Id (Variable_Type);
      if Is_Type_Composite (Variable_Type) then
         Create_Node (L, No_Name, K_List);
         Nodes.Table (VN).Node_3 := L;
      end if;
   end Set_Variable_Type;

   ------------------------
   -- Set_Variable_Value --
   ------------------------

   procedure Set_Variable_Value
     (Variable_Node : in Variable_Id;
      Value_Node    : in Variable_Id)
   is
      Node  : Node_Id := Node_Id (Variable_Node);

   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Flag_1 := True;
      Nodes.Table (Node).Node_3 := Node_Id (Value_Node);
   end Set_Variable_Value;

   -------------------------------
   -- Subprogram_Is_A_Procedure --
   -------------------------------

   procedure Subprogram_Is_A_Procedure
     (Subprogram_Node : in Subprogram_Id;
      Procedure_Node  : in Boolean)
   is
      Node : Node_Id := Node_Id (Subprogram_Node);

   begin
      pragma Assert (Is_Subprogram (Node));
      Nodes.Table (Node).Flag_1 := Procedure_Node;
   end Subprogram_Is_A_Procedure;

   -----------------------
   -- Type_Is_Composite --
   -----------------------

   procedure Type_Is_Composite
     (Type_Node : in Type_Id;
      Composite : in Boolean)
   is
      TN : Node_Id := Node_Id (Type_Node);
      L  : Node_Id;

   begin
      pragma Assert (Is_Type (TN));
      if Composite then
         Create_Node (L, No_Name, K_List);
         Nodes.Table (TN).Node_3 := L;
      else
         Nodes.Table (TN).Node_3 := Null_Node;
      end if;
   end Type_Is_Composite;

   -----------------------------
   -- Variable_Is_Initialized --
   -----------------------------

   procedure Variable_Is_Initialized
     (Variable_Node  : in Variable_Id;
      Is_Initialized : in Boolean)
   is
      Node : Node_Id := Node_Id (Variable_Node);

   begin
      pragma Assert (Is_Variable (Node));
      Nodes.Table (Node).Flag_1 := Is_Initialized;
   end Variable_Is_Initialized;

   ----------------
   -- Write_SLOC --
   ----------------

   procedure Write_SLOC
     (Node : Node_Id)
   is
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
