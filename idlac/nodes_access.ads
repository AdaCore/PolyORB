with GNAT.Table;
with Nodes;      use Nodes;
with Types;      use Types;

package Nodes_Access is

   type Node_Kind is
      (K_Forward,
       K_Named,
       K_Repository,
       K_Scope);

   --
   --  Forward
   --
   --     Kind                      : Node_Kind
   --     Original_Node             : Node_Id
   --     Parent                    : Node_Id
   --     Enclosing_Scope           : Node_Id
   --     Loc                       : Location
   --     Definition                : Identifier_Definition_Acc
   --     Identifier_List           : Identifier_Definition_List
   --     Identifier_Table          : Storage
   --     Unimplemented_Forwards    : Node_List
   --

   function Make_Forward return Node_Id;

   --
   --  Named
   --
   --     Kind                      : Node_Kind
   --     Original_Node             : Node_Id
   --     Parent                    : Node_Id
   --     Enclosing_Scope           : Node_Id
   --     Loc                       : Location
   --     Definition                : Identifier_Definition_Acc
   --

   function Make_Named return Node_Id;

   --
   --  Repository
   --
   --     Kind                      : Node_Kind
   --     Original_Node             : Node_Id
   --     Parent                    : Node_Id
   --     Enclosing_Scope           : Node_Id
   --     Loc                       : Location
   --     Definition                : Identifier_Definition_Acc
   --     Identifier_List           : Identifier_Definition_List
   --     Identifier_Table          : Storage
   --     Unimplemented_Forwards    : Node_List
   --     Contents                  : Node_List
   --

   function Make_Repository return Node_Id;

   --
   --  Scope
   --
   --     Kind                      : Node_Kind
   --     Original_Node             : Node_Id
   --     Parent                    : Node_Id
   --     Enclosing_Scope           : Node_Id
   --     Loc                       : Location
   --     Definition                : Identifier_Definition_Acc
   --     Identifier_List           : Identifier_Definition_List
   --     Identifier_Table          : Storage
   --

   function Make_Scope return Node_Id;

   function Contents (N : Node_Id) return Node_List;
   procedure Set_Contents (N : Node_Id; V : Node_List);

   function Definition (N : Node_Id) return Identifier_Definition_Acc;
   procedure Set_Definition (N : Node_Id; V : Identifier_Definition_Acc);

   function Enclosing_Scope (N : Node_Id) return Node_Id;
   procedure Set_Enclosing_Scope (N : Node_Id; V : Node_Id);

   function Identifier_List (N : Node_Id) return Identifier_Definition_List;
   procedure Set_Identifier_List (N : Node_Id; V : Identifier_Definition_List);

   function Identifier_Table (N : Node_Id) return Storage;
   procedure Set_Identifier_Table (N : Node_Id; V : Storage);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Original_Node (N : Node_Id) return Node_Id;
   procedure Set_Original_Node (N : Node_Id; V : Node_Id);

   function Parent (N : Node_Id) return Node_Id;
   procedure Set_Parent (N : Node_Id; V : Node_Id);

   function Unimplemented_Forwards (N : Node_Id) return Node_List;
   procedure Set_Unimplemented_Forwards (N : Node_Id; V : Node_List);


private

   type Node_Type is limited record
      Contents                  : Node_List                ;
      Definition                : Identifier_Definition_Acc;
      Enclosing_Scope           : Node_Id                  := No_Node;
      Identifier_List           : Identifier_Definition_List;
      Identifier_Table          : Storage                  ;
      Kind                      : Node_Kind                ;
      Loc                       : Location                 ;
      Original_Node             : Node_Id                  := No_Node;
      Parent                    : Node_Id                  := No_Node;
      Unimplemented_Forwards    : Node_List                ;
   end record;

   type Node_Access is access Node_Type;

   package Nodes_Table is
      new GNAT.Table (Table_Component_Type => Node_Access,
                      Table_Index_Type     => Node_Id,
                      Table_Low_Bound      => 1,
                      Table_Initial        => 1024,
                      Table_Increment      => 100);

end Nodes_Access;
