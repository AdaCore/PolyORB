--  assign color to Category
--  find attribute in Component_Type
--  conflict with Name from Component_Type 0
--  conflict with Category from Component_Type 0
--  conflict with Provides from Component_Type 0
--  conflict with Requires from Component_Type 0
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 0
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 0
--  decide to assign 1
--  assign color to Next_Node
--  find attribute in Node_Id
--  conflict with Next_Node from Node_Id 0
--  decide to assign 1
--  assign color to First_Node
--  find attribute in List_Id
--  conflict with First_Node from List_Id 0
--  conflict with Last_Node from List_Id 0
--  decide to assign 1
--  assign color to Last_Node
--  find attribute in List_Id
--  conflict with First_Node from List_Id 1
--  conflict with Last_Node from List_Id 0
--  decide to assign 2
--  assign color to Items
--  find attribute in Package_Items
--  conflict with Items from Package_Items 0
--  conflict with Properties from Package_Items 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 2
--  assign color to Properties
--  find attribute in Package_Items
--  conflict with Items from Package_Items 2
--  conflict with Properties from Package_Items 0
--  conflict with Next_Node from Node_Id 1
--  find attribute in Component_Type
--  conflict with Name from Component_Type 0
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 0
--  conflict with Requires from Component_Type 0
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 0
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 3
--  assign color to Name
--  find attribute in Package_Spec
--  conflict with Name from Package_Spec 0
--  conflict with Public_Package_Items from Package_Spec 0
--  conflict with Private_Package_Items from Package_Spec 0
--  conflict with Next_Node from Node_Id 1
--  find attribute in Component_Type
--  conflict with Name from Component_Type 0
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 0
--  conflict with Requires from Component_Type 0
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 2
--  assign color to Public_Package_Items
--  find attribute in Package_Spec
--  conflict with Name from Package_Spec 2
--  conflict with Public_Package_Items from Package_Spec 0
--  conflict with Private_Package_Items from Package_Spec 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 3
--  assign color to Private_Package_Items
--  find attribute in Package_Spec
--  conflict with Name from Package_Spec 2
--  conflict with Public_Package_Items from Package_Spec 3
--  conflict with Private_Package_Items from Package_Spec 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 4
--  assign color to Provides
--  find attribute in Component_Type
--  conflict with Name from Component_Type 2
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 0
--  conflict with Requires from Component_Type 0
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 4
--  assign color to Requires
--  find attribute in Component_Type
--  conflict with Name from Component_Type 2
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 4
--  conflict with Requires from Component_Type 0
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 5
--  assign color to Parameters
--  find attribute in Component_Type
--  conflict with Name from Component_Type 2
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 4
--  conflict with Requires from Component_Type 5
--  conflict with Parameters from Component_Type 0
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 6
--  assign color to Annexes
--  find attribute in Component_Type
--  conflict with Name from Component_Type 2
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 4
--  conflict with Requires from Component_Type 5
--  conflict with Parameters from Component_Type 6
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 0
--  conflict with Next_Node from Node_Id 1
--  decide to assign 7
--  assign color to Parent
--  find attribute in Component_Type_Ext
--  conflict with Parent from Component_Type_Ext 0
--  conflict with Name from Component_Type 2
--  conflict with Category from Component_Type 1
--  conflict with Provides from Component_Type 4
--  conflict with Requires from Component_Type 5
--  conflict with Parameters from Component_Type 6
--  conflict with Properties from Component_Type 3
--  conflict with Annexes from Component_Type 7
--  conflict with Next_Node from Node_Id 1
--  decide to assign 8
with GNAT.Table;
with Locations; use Locations;
with Types;     use Types;

package Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_AADL_Spec,
      K_Package_Items,
      K_Package_Spec,
      K_Component_Type,
      K_Component_Type_Ext);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  AADL_Spec
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_AADL_Spec (I : Natural; N : List_Id);

   --
   --  Package_Items
   --
   --    Next_Node                : Node_Id
   --    Items                    : List_Id
   --    Properties               : List_Id
   --

   procedure W_Package_Items (I : Natural; N : Node_Id);

   --
   --  Package_Spec
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Public_Package_Items     : Node_Id
   --    Private_Package_Items    : Node_Id
   --

   procedure W_Package_Spec (I : Natural; N : Node_Id);

   --
   --  Component_Type
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Category                 : Byte
   --    Provides                 : List_Id
   --    Requires                 : List_Id
   --    Parameters               : List_Id
   --    Properties               : List_Id
   --    Annexes                  : List_Id
   --

   procedure W_Component_Type (I : Natural; N : Node_Id);

   --
   --  Component_Type_Ext
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Category                 : Byte
   --    Provides                 : List_Id
   --    Requires                 : List_Id
   --    Parameters               : List_Id
   --    Properties               : List_Id
   --    Annexes                  : List_Id
   --    Parent                   : Node_Id
   --

   procedure W_Component_Type_Ext (I : Natural; N : Node_Id);


   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Items (N : Node_Id) return List_Id;
   procedure Set_Items (N : Node_Id; V : List_Id);

   function Properties (N : Node_Id) return List_Id;
   procedure Set_Properties (N : Node_Id; V : List_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Public_Package_Items (N : Node_Id) return Node_Id;
   procedure Set_Public_Package_Items (N : Node_Id; V : Node_Id);

   function Private_Package_Items (N : Node_Id) return Node_Id;
   procedure Set_Private_Package_Items (N : Node_Id; V : Node_Id);

   function Category (N : Node_Id) return Byte;
   procedure Set_Category (N : Node_Id; V : Byte);

   function Provides (N : Node_Id) return List_Id;
   procedure Set_Provides (N : Node_Id; V : List_Id);

   function Requires (N : Node_Id) return List_Id;
   procedure Set_Requires (N : Node_Id; V : List_Id);

   function Parameters (N : Node_Id) return List_Id;
   procedure Set_Parameters (N : Node_Id; V : List_Id);

   function Annexes (N : Node_Id) return List_Id;
   procedure Set_Annexes (N : Node_Id; V : List_Id);

   function Parent (N : Node_Id) return Node_Id;
   procedure Set_Parent (N : Node_Id; V : Node_Id);

   procedure W_Node (I : Natural; N : Node_Id);

   type Boolean_Array is array (1 .. 0) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 8) of Int;

   type Node_Entry is record
      Kind : Node_Kind;
      O : Byte_Array;
      L : Int_Array;
      Loc : Location;
   end record;

   Default_Node : constant Node_Entry :=
     (Node_Kind'First,
      (others => 0),
      (others => 0),
      No_Location);

   package Entries is new GNAT.Table
     (Node_Entry, Node_Id, No_Node + 1, 1000, 100);

end Nodes;
