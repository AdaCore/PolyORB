with GNAT.Table;

with Types; use Types;

package Scopes is

   type Scope_Stack_Entry is record
      Entity       : Entity_Id;
      Scoped_Names : Entity_Id;
   end record;

   No_Scope_Depth : constant Int := -1;
   package Scope_Stack is
      new GNAT.Table (Scope_Stack_Entry, Int, No_Scope_Depth + 1, 10, 10);

   procedure Initialize;

   procedure Push_Scope (E : Entity_Id);
   procedure Pop_Scope;

   function  Current_Scope return Entity_Id;
   function  Current_Scope_Depth return Int;
   function  Scoped_Names return Entity_Id;
   procedure Set_Scoped_Names (E : Entity_Id);

   function Entity_In_Scope (N : Node_Id; S : Entity_Id) return Entity_Id;
   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id;
   --  Search into scope S (or current scope) for an identifier N.

   function Current_Visible_Entity (N : Node_Id) return Entity_Id;

   procedure Make_Entity_Visible (E : Entity_Id; V : Boolean);
   procedure Make_Enclosed_Entities_Visible (E : Entity_Id; V : Boolean);

   procedure Enter_Name_In_Scope (N : Node_Id);

   function Enclosed_Entities (E : Entity_Id) return List_Id;

   Root      : Entity_Id;
   Root_Name : Name_Id;

end Scopes;


