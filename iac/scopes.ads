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
   --  Handle special scoping rules for types names (see 3.15.3). The
   --  potential scope of a type name extends over all its enclosing
   --  scopes out to the enclosing non-module scope. Remove nodes
   --  from their homonym chains (used to apply visibility rules).

   function  Current_Scope return Node_Id;
   --  Return current scope

   function Node_Explicitly_In_Scope (N : Node_Id; S : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier N in scope
   --  S. This node must be explicitly declared in S and not imported
   --  because of special scoping rules.

   function Node_Implicitly_In_Scope (N : Node_Id; S : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier N in scope
   --  S. This node can be implicitly declared in S that is explicitly
   --  declared or imported because of special scoping rules.

   function Visible_Node (N : Node_Id) return Node_Id;
   --  Find the currently visible definition for a given identifier,
   --  that is to say the first entry in the visibility chain
   --  (implemented using the homonyms chain).

   procedure Make_Node_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True);

   procedure Make_Enclosed_Nodes_Visible
     (E : Node_Id; Visible : Boolean; Immediately : Boolean := True);

   procedure Enter_Name_In_Scope (N : Node_Id);

   Root      : Node_Id;
   Root_Name : Name_Id;

end Scopes;


