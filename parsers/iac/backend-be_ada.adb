with Types; use Types;
with Nodes; use Nodes;

pragma Warnings (Off);
with Backend.BE_Ada.Nodes;
with Debug; use Debug;
pragma Warnings (On);


with Output; use Output;
with Errors; use Errors;
with Locations; use Locations;
with Namet; use Namet;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
with Ada.Strings.Unbounded;

package body Backend.BE_Ada is

   Ada_Packages : List_Id;

   package BE renames Backend.BE_Ada.Nodes;

   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Specification (E : Node_Id);
   function G_Package (E : Node_Id) return Node_Id;
   function Package_Name (E : Node_Id) return String;
   function Full_Package_Name (E : Node_Id) return String;
   function Visit_Interface (E : Node_Id)return Node_Id;
   function Visite_Module (E : Node_Id) return Node_Id;

   use Inheritance_Stack;

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      null;
   end Configure;

   --------------
   --  Generate --
   --------------

   procedure Generate (E : Node_Id) is
      D : Node_Id;
   begin

      Set_Standard_Output;
      Ada_Packages := New_List (BE.K_Ada_Package_List, No_Location);
      case Kind (E) is
         when K_Specification =>
            Generate_Specification (E);
         when others =>
            Write_Line ("Others");
      end case;



      D := BE.First_Node (Ada_Packages);
      while Present (D) loop
         Write_Line (Full_Package_Name (D));
         D := BE.Next_Node (D);
      end loop;
   end Generate;


   procedure Generate_Type_Declaration (E : Node_Id)  is
      pragma Unreferenced (E);
   begin
      --   Write_Line ("Type Definition");
      null;
   end Generate_Type_Declaration;


   function Visite_Module (E : Node_Id) return Node_Id is
   begin
      return G_Package (E);
   end Visite_Module;

   ----------------------------
   -- Generate_Specification --
   ----------------------------

   procedure Generate_Specification (E : Node_Id) is
      List_Def : List_Id;
      D      : Node_Id;
      Ada_Node : Node_Id;

   begin
      List_Def := Definitions (E);
      D := First_Node (List_Def);
      while Present (D) loop
         case Kind (D) is

            when K_Module =>
               Ada_Node := Visite_Module (D);
               Append_Node_To_List (Ada_Node, Ada_Packages);
               Push_Package (Ada_Node);
               Generate_Specification (D); -- Visit definitions of module
               Pop_Package;

            when K_Type_Declaration =>
               Generate_Type_Declaration (D);

            when K_Interface_Declaration =>
               Ada_Node := Visit_Interface (D);

            when others =>
               Display_Error ("Definition not recongnized");

         end case;
         D := Next_Node (D);
      end loop;

   end Generate_Specification;

   -----------------------
   --  Gererate Module  --
   -----------------------



   function G_Package (E : Node_Id) return Node_Id is

      Node : Node_Id;
      Id : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Ada_Name : Name_Id;
      Parent_Node : Node_Id;

   begin
      Node := New_Node (BE.K_Ada_Packages, No_Location);
      Id := New_Node (BE.K_Ada_Identifier, No_Location);
      Ada_Name := Map_Id_Name_Idl2Ada (Name (Identifier (E)));
      BE.Set_Name (Id, Ada_Name);
      BE.Set_Identifier (Node, Id);
      Parent_Node := Current_Package;
      BE.Set_Parent (Node, Parent_Node);
      case Kind (E) is
         when K_Module =>
            Pkg_Spec := No_Node;
            BE.Set_Package_Spec (Node, Pkg_Spec);
            Pkg_Body := No_Node;
            BE.Set_Package_Body (Node, Pkg_Body);
         when K_Interface_Declaration =>
            null;
         when others =>
            Write_Line ("In progress....G_Package");
      end case;
      return Node;
   end G_Package;


   -------------------
   -- Current_Scope --
   -------------------

   function Current_Package return Node_Id is
   begin
      if Last = No_Inheritance_Depth then
         return No_Node;
      else
         return Table (Last).Node;
      end if;
   end Current_Package;

   procedure Push_Package (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Node := E;
   end Push_Package;

   procedure Pop_Package is
   begin
      if Last > No_Inheritance_Depth then
         Decrement_Last;
      else
         null;  --  maybe it's better to raise an exception.
      end if;
   end Pop_Package;

   function Package_Name (E : Node_Id) return String is
   begin
      return Get_Name_String (BE.Name (BE.Identifier (E)));
   end Package_Name;

   function Full_Package_Name (E : Node_Id) return String is
      use Ada.Strings.Unbounded;
      Scope_Mark : constant String := ".";
      Full_Name : Unbounded_String;
      P : Node_Id;
   begin
      Full_Name := To_Unbounded_String (Package_Name (E));
      P := BE.Parent (E);
      while P /= No_Node loop

         Insert (Full_Name, 1, Package_Name (P) & Scope_Mark);
         P := BE.Parent (P);
      end loop;
      return To_String (Full_Name);
   end Full_Package_Name;

   function Visit_Interface (E : Node_Id) return Node_Id is
      pragma Unreferenced (E);
   begin
      return No_Node;
   end Visit_Interface;

end Backend.BE_Ada;
