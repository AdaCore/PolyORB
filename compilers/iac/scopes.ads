with GNAT.Table;

with Types; use Types;

package Scopes is

   type Scope_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Scope_Depth : constant Int := -1;
   package Scope_Stack is
      new GNAT.Table (Scope_Stack_Entry, Int, No_Scope_Depth + 1, 10, 10);

   procedure Initialize;

   procedure Push_Scope (S : Node_Id);
   procedure Pop_Scope;

   function  Current_Scope return Node_Id;

   function Node_In_Scope (N : Node_Id; S : Node_Id) return Node_Id;
   function Node_In_Current_Scope (N : Node_Id) return Node_Id;
   --  Search into scope S (or current scope) for an identifier N.

   function Current_Node (N : Node_Id) return Node_Id;

   procedure Make_Node_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True);
   procedure Make_Enclosed_Nodes_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True);

   procedure Enter_Name_In_Scope (N : Node_Id);

   Root      : Node_Id;
   Root_Name : Name_Id;

end Scopes;

