with Types; use Types;
with Nodes; use Nodes;

pragma Warnings (Off);
with Backend.BE_Ada.Nodes;

pragma Warnings (On);


with Output; use Output;
with Errors; use Errors;
with Locations; use Locations;
with Namet; use Namet;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
with Ada.Strings.Unbounded;
with Backend.BE_Ada.Debug; use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Namet;
package body Backend.BE_Ada is

   Ada_Packages : List_Id;

   package BE renames Backend.BE_Ada.Nodes;
   package BE_Namet renames Backend.BE_Ada.Namet;

   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Specification (E : Node_Id);
   function G_Package (E : Node_Id) return Node_Id;
   function Package_Name (E : Node_Id) return String;
   function Full_Package_Name (E : Node_Id) return String;
   pragma Unreferenced (Full_Package_Name);
   function Visit_Interface (E : Node_Id)return Node_Id;
   function Visite_Module (E : Node_Id) return Node_Id;
   function All_Inheritance_Interface_Abstract (L : List_Id) return Boolean;
   procedure Insert_Type_Ref (L : List_Id; Inheritance_List : List_Id;
                                           Interface_Is_Abstract : Boolean);
   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Name : String) return Node_Id;
   procedure Insert_Base_Type;
   procedure Declare_Base_Type (Type_Str : String; K : BE.Node_Kind);
   procedure Run_Test_Procedure; --   Just for making tests.
   procedure Insert_Attr_SetGet (N : Node_Id; L : List_Id);



   use Inheritance_Stack;
   --------------
   --  Generate --
   --------------

   procedure Generate (E : Node_Id) is

   begin
      BE_Namet.Initialize;
      Insert_Base_Type;
      Set_Standard_Output;
      Ada_Packages := New_List (BE.K_Ada_Package_List, No_Location);
      case Kind (E) is
         when K_Specification =>
            Generate_Specification (E);
         when others =>
            Write_Line ("Others");
      end case;

      W_List_Id (Ada_Packages);
      Run_Test_Procedure; --   Just for making tests.
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
               Generate_Specification (D); -- Visit  definitions of the module
               Pop_Package;

            when K_Type_Declaration =>
               Generate_Type_Declaration (D);

            when K_Interface_Declaration =>
               Ada_Node := Visit_Interface (D);
               Append_Node_To_List (Ada_Node, Ada_Packages);
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
      Pkg : Node_Id;
      Pkg_Spec : Node_Id;
      Pkg_Body : Node_Id;
      Public_Decl : List_Id;
      I_Spec : List_Id;
      I_Body : List_Id;
      N : Node_Id;
      pragma Unreferenced (Pkg_Body, I_Body);
   begin
      Pkg := G_Package (E);
      --   Package creation
      Pkg_Spec := New_Node (BE.K_Ada_Package_Spec, No_Location);
      I_Spec := Interface_Spec (E);
      Public_Decl := New_List (BE.K_List_Id, No_Location);
      BE.Set_Ada_Public (Pkg_Spec, Public_Decl);
      Insert_Type_Ref (Public_Decl, I_Spec, False);
      if I_Spec /= No_List then
         N := BE.First_Node (I_Spec);
         while Present (N) loop
            case Kind (N) is
               when K_Attribute_Declaration =>
                  Insert_Attr_SetGet (N, Public_Decl);
               when others =>
                  DE ("Visit_Interface : Fonctionnalite pas encore imp!");
            end case;

         end loop;
      end if;


      --   Type reference insertion
      BE.Set_Package_Spec (Pkg, Pkg_Spec);

      I_Body := Interface_Body (E);

      return Pkg;
   end Visit_Interface;

   procedure Insert_Type_Ref
     (L : List_Id;
      Inheritance_List : List_Id;
      Interface_Is_Abstract : Boolean) is


      Type_Ref_Node : Node_Id;
      Type_Ref_Name : Node_Id;
      Ref_Str : constant String := "Ref";
      Abstract_Ref_Str : constant String := "Abstract_Ref";
      Local_Ref_Str   : constant String := "Local_Ref";
      Ancestor_Type_Abstract : constant String := "CORBA.AbstractBase.Ref";
      Ancestor_Type : constant String := "CORBA.Object.Ref";
      pragma Unreferenced (Local_Ref_Str, Type_Ref_Name);
   begin
      if (Inheritance_List = No_List)
        or
        (All_Inheritance_Interface_Abstract (Inheritance_List))
      then
         if Interface_Is_Abstract then
            Type_Ref_Node :=
              Make_Ada_Typedef_Node (Abstract_Ref_Str, Ancestor_Type_Abstract);
         else
            Type_Ref_Node := Make_Ada_Typedef_Node (Ref_Str, Ancestor_Type);
            Append_Node_To_List (Type_Ref_Node, L);
         end if;
      else
         null; --   in contruction.
      end if;
   end Insert_Type_Ref;

   function All_Inheritance_Interface_Abstract (L : List_Id) return Boolean is
      N : Node_Id;
      All_Abstract : Boolean := True;
   begin
      if L /= No_List then
         N := First_Node (L);
         while Present (N)  loop
            if Is_Abstract (N) then
               All_Abstract := False;
               exit;
            end if;
            N := Next_Node (N);
         end loop;
      end if;
      return All_Abstract;

   end All_Inheritance_Interface_Abstract;



   function Make_Ada_Typedef_Node
     (Identifier_Name : String;
      Type_Name : String) return Node_Id is
      --  Declaration
      Id_Node : Node_Id;
      Type_Node : Node_Id;
      Type_Def : Node_Id;
   begin
      Id_Node := New_Node (BE.K_Ada_Identifier, No_Location);
      BE_Namet.Set_Str_To_Name_Buffer (Identifier_Name);
      BE.Set_Name (Id_Node, BE_Namet.Name_Find);
      Type_Node := New_Node (BE.K_Ada_Identifier, No_Location);
      BE_Namet.Set_Str_To_Name_Buffer (Type_Name);
      BE.Set_Name (Type_Node, BE_Namet.Name_Find);
      Type_Def := New_Node (BE.K_Type_Declaration, No_Location);
      BE.Set_Identifier (Type_Def, Id_Node);
      BE.Set_Type_Spec (Type_Def, Type_Node);
      BE.Set_With_Null_Record (Type_Def, true);
      return Type_Def;
   end Make_Ada_Typedef_Node;

   procedure Insert_Base_Type is
   begin

      Declare_Base_Type ("Float", BE.K_Float);
      Declare_Base_Type ("Double", BE.K_Double);
      Declare_Base_Type ("Long Double", BE.K_Long_Double);
      Declare_Base_Type ("Short", BE.K_Short);
      Declare_Base_Type ("Long", BE.K_Long);
      Declare_Base_Type ("Long Long", BE.K_Long_Long);
      Declare_Base_Type ("Unsigned Short", BE.K_Unsigned_Short);
      Declare_Base_Type ("Unsigned Long", BE.K_Unsigned_Long);
      Declare_Base_Type ("Unsigned Long Long", BE.K_Unsigned_Long_Long);
      Declare_Base_Type ("Char", BE.K_Char);
      Declare_Base_Type ("Wide Char", BE.K_Wide_Char);
      Declare_Base_Type ("Octet", BE.K_Octet);
      Declare_Base_Type ("Boolean", BE.K_Boolean);
      Declare_Base_Type ("String", BE.K_String);

   end Insert_Base_Type;

   procedure Declare_Base_Type (Type_Str : String; K : BE.Node_Kind) is
      E : Node_Id;
      N : Name_Id;
   begin

      --  Create a fake node located at the beginning of the
      --  specification (current token location).

      E := New_Node (K, No_Location);

      --  Accumulate token names and store node id as table info

      BE_Namet.Set_Str_To_Name_Buffer (Type_Str);
      N := BE_Namet.Name_Find;
      BE_Namet.Set_Name_Table_Info (N, Int (E));
      Set_Image (Base_Type (E), N);
   end Declare_Base_Type;



   procedure Run_Test_Procedure is
   begin
      --   Write_Line (Image (BE.K_Ada_Packages));
      null;
   end Run_Test_Procedure;

   procedure Insert_Attr_SetGet (N : Node_Id; L : List_Id) is
      pragma Unreferenced (L);
   begin
      if Is_Readonly (N) then
         null;
      else
         null;
      end if;
   end Insert_Attr_SetGet;



end Backend.BE_Ada;


