pragma Warnings (Off);
with Debug; use Debug;
with Locations; use Locations;
with Types; use Types;
package body Nodes is

   use Entries;

   function Kind (N : Node_Id) return Node_Kind is
   begin
      return Table (Node_Id (N)).Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind) is
   begin
      Table (Node_Id (N)).Kind := V;
   end Set_Kind;


   function Loc (N : Node_Id) return Location is
   begin
      return Table (Node_Id (N)).Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location) is
   begin
      Table (Node_Id (N)).Loc := V;
   end Set_Loc;


   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Node_Id
        or else Table (Node_Id (N)).Kind = K_Package_Items
        or else Table (Node_Id (N)).Kind = K_Package_Spec
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return Node_Id (Table (Node_Id (N)).L (1));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Node_Id
        or else Table (Node_Id (N)).Kind = K_Package_Items
        or else Table (Node_Id (N)).Kind = K_Package_Spec
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (1) := Int (V);
   end Set_Next_Node;


   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_List_Id
        or else Table (Node_Id (N)).Kind = K_AADL_Spec);

      return Node_Id (Table (Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_List_Id
        or else Table (Node_Id (N)).Kind = K_AADL_Spec);

      Table (Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;


   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_List_Id
        or else Table (Node_Id (N)).Kind = K_AADL_Spec);

      return Node_Id (Table (Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_List_Id
        or else Table (Node_Id (N)).Kind = K_AADL_Spec);

      Table (Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;


   function Items (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Items);

      return List_Id (Table (Node_Id (N)).L (2));
   end Items;

   procedure Set_Items (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Items);

      Table (Node_Id (N)).L (2) := Int (V);
   end Set_Items;


   function Properties (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Items
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return List_Id (Table (Node_Id (N)).L (3));
   end Properties;

   procedure Set_Properties (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Items
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (3) := Int (V);
   end Set_Properties;


   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return Name_Id (Table (Node_Id (N)).L (2));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (2) := Int (V);
   end Set_Name;


   function Public_Package_Items (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec);

      return Node_Id (Table (Node_Id (N)).L (3));
   end Public_Package_Items;

   procedure Set_Public_Package_Items (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec);

      Table (Node_Id (N)).L (3) := Int (V);
   end Set_Public_Package_Items;


   function Private_Package_Items (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec);

      return Node_Id (Table (Node_Id (N)).L (4));
   end Private_Package_Items;

   procedure Set_Private_Package_Items (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Package_Spec);

      Table (Node_Id (N)).L (4) := Int (V);
   end Set_Private_Package_Items;


   function Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return Byte (Table (Node_Id (N)).O (1));
   end Category;

   procedure Set_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).O (1) := Byte (V);
   end Set_Category;


   function Provides (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return List_Id (Table (Node_Id (N)).L (4));
   end Provides;

   procedure Set_Provides (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (4) := Int (V);
   end Set_Provides;


   function Requires (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return List_Id (Table (Node_Id (N)).L (5));
   end Requires;

   procedure Set_Requires (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (5) := Int (V);
   end Set_Requires;


   function Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return List_Id (Table (Node_Id (N)).L (6));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (6) := Int (V);
   end Set_Parameters;


   function Annexes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return List_Id (Table (Node_Id (N)).L (7));
   end Annexes;

   procedure Set_Annexes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (7) := Int (V);
   end Set_Annexes;


   function Parent (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      return Node_Id (Table (Node_Id (N)).L (8));
   end Parent;

   procedure Set_Parent (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Node_Id (N)).Kind = K_Component_Type_Ext);

      Table (Node_Id (N)).L (8) := Int (V);
   end Set_Parent;


   procedure W_Node (I : Natural; N : Node_Id) is
   begin
      case Kind (N) is
         when K_AADL_Spec =>
            W_AADL_Spec
              (I,
               List_Id (N));
         when K_Package_Items =>
            W_Package_Items
              (I,
               Node_Id (N));
         when K_Package_Spec =>
            W_Package_Spec
              (I,
               Node_Id (N));
         when K_Component_Type =>
            W_Component_Type
              (I,
               Node_Id (N));
         when K_Component_Type_Ext =>
            W_Component_Type_Ext
              (I,
               Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_AADL_Spec (I : Natural; N : List_Id) is
   begin
      W_Node_Header
        (I,
         Node_Id (N));
      W_Node_Attribute
        (I,
         "First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        (I,
         "Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_AADL_Spec;

   procedure W_Package_Items (I : Natural; N : Node_Id) is
   begin
      W_Node_Header
        (I,
         Node_Id (N));
      W_Node_Attribute
        (I,
         "Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        (I,
         "Items",
         "List_Id",
         Image (Items (N)),
         Int (Items (N)));
      W_Node_Attribute
        (I,
         "Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
   end W_Package_Items;

   procedure W_Package_Spec (I : Natural; N : Node_Id) is
   begin
      W_Node_Header
        (I,
         Node_Id (N));
      W_Node_Attribute
        (I,
         "Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        (I,
         "Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        (I,
         "Public_Package_Items",
         "Node_Id",
         Image (Public_Package_Items (N)),
         Int (Public_Package_Items (N)));
      W_Node_Attribute
        (I,
         "Private_Package_Items",
         "Node_Id",
         Image (Private_Package_Items (N)),
         Int (Private_Package_Items (N)));
   end W_Package_Spec;

   procedure W_Component_Type (I : Natural; N : Node_Id) is
   begin
      W_Node_Header
        (I,
         Node_Id (N));
      W_Node_Attribute
        (I,
         "Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        (I,
         "Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        (I,
         "Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        (I,
         "Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        (I,
         "Provides",
         "List_Id",
         Image (Provides (N)),
         Int (Provides (N)));
      W_Node_Attribute
        (I,
         "Requires",
         "List_Id",
         Image (Requires (N)),
         Int (Requires (N)));
      W_Node_Attribute
        (I,
         "Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        (I,
         "Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
   end W_Component_Type;

   procedure W_Component_Type_Ext (I : Natural; N : Node_Id) is
   begin
      W_Node_Header
        (I,
         Node_Id (N));
      W_Node_Attribute
        (I,
         "Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        (I,
         "Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        (I,
         "Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        (I,
         "Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        (I,
         "Provides",
         "List_Id",
         Image (Provides (N)),
         Int (Provides (N)));
      W_Node_Attribute
        (I,
         "Requires",
         "List_Id",
         Image (Requires (N)),
         Int (Requires (N)));
      W_Node_Attribute
        (I,
         "Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        (I,
         "Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
      W_Node_Attribute
        (I,
         "Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
   end W_Component_Type_Ext;


end Nodes;
