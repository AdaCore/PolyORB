with Locations; use Locations;
--   with Output; use Output;
--   with Debug; use Debug;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
with Namet; use Namet;
with Ada.Strings.Unbounded;
--   with Backend.BE_Ada.Namet;



package body Backend.BE_Ada.Nutils is

   use Inheritance_Stack;

   --------------------------
   --  Append_List_To_List --
   --------------------------
   procedure Append_List_To_List (L1 : List_Id; L2 : in out List_Id) is
      N : Node_Id;
   begin
      if L1 = No_List then
         return;
      end if;
      N := First_Node (L1);
      while Present (N) loop
         Append_Node_To_List (N, L2);
         N := Next_Node (N);
      end loop;
   end Append_List_To_List;

   -------------------------
   -- Append_Node_To_List --
   -------------------------
   procedure Append_Node_To_List (E : Node_Id; L : in out List_Id) is
      Last : Node_Id;
      List_Kind : Node_Kind;
   begin
      if L = No_List then
         case Kind (E) is
            when K_Ada_Packages =>
               List_Kind := K_Ada_Package_List;
            when others =>
               List_Kind := K_List_Id;
         end case;
         L := New_List (List_Kind, No_Location);
      end if;

      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;


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
      return Get_Name_String (Name (Identifier (E)));
   end Package_Name;


   function Full_Package_Name (E : Node_Id) return String is
      use Ada.Strings.Unbounded;
      Scope_Mark : constant String := ".";
      Full_Name : Unbounded_String;
      P : Node_Id;
   begin
      Full_Name := To_Unbounded_String (Package_Name (E));
      P := Parent (E);
      while P /= No_Node loop

         Insert (Full_Name, 1, Package_Name (P) & Scope_Mark);
         P := Parent (P);
      end loop;
      return To_String (Full_Name);
   end Full_Package_Name;


   function New_Node
     (Kind : Node_Kind;
      Loc : Location)
     return Node_Id is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   function New_List
     (Kind : Node_Kind;
      Loc  : Location)
     return List_Id is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;
   begin
      C := First_Node (L);
      if C = E then
         Set_First_Node (L, Next_Node (E));
         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));
               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;


   function Map_Identifier_Name_2Ada (N : Name_Id) return Name_Id is
      Str : constant String := Get_Name_String (N);

   begin
      Set_Str_To_Name_Buffer (Map_Identifier_Name_2Ada (Str));

      return Name_Find;
   end Map_Identifier_Name_2Ada;

   function Map_Identifier_Name_2Ada (N : String) return String is
      Str : String := N;
      First : Integer := Str'First;
   begin
      while First <= Str'Last
        and then Str (First) = '_' loop
         First := First + 1;
      end loop;

      for I in First .. Str'Last loop
         if Str (I) = '_'
           and then I < Str'Last
           and then Str (I + 1) = '_' then
            Str (I + 1) := 'U';
         end if;
      end loop;
      if Str (Str'Last) = '_' then
         return (Str (First .. Str'Last) & 'U');
      else
         return (Str (First .. Str'Last));
      end if;

   end Map_Identifier_Name_2Ada;

   function Mk_Node_Ada_Argument
     (I : Node_Id; T : Node_Id; M : Mode_Id) return Node_Id is

      Ada_Argument_Node : Node_Id;
   begin
      Ada_Argument_Node := New_Node (K_Ada_Argument, No_Location);
      Set_Identifier (Ada_Argument_Node, I);
      Set_Type_Spec (Ada_Argument_Node, T);
      Set_Argument_Mode (Ada_Argument_Node, M);

      return Ada_Argument_Node;
   end Mk_Node_Ada_Argument;


   function Mk_Node_Ada_Argument_List (L : List_Id) return List_Id is
      pragma Unreferenced (L);
   begin
      return No_List;
   end Mk_Node_Ada_Argument_List;

   -------------------------------
   --  Make Node Ada Identifier --
   -------------------------------
   function Mk_Node_Ada_Identifier (Name : Name_Id) return Node_Id is
      Node : Node_Id;
   begin
      Node := New_Node (K_Ada_Identifier, No_Location);
      Set_Name (Node, Map_Identifier_Name_2Ada (Name));
      return Node;
   end Mk_Node_Ada_Identifier;

   -------------------------------
   --  Make Node Ada Identifier --
   -------------------------------
   function Mk_Node_Ada_Identifier (Name : String) return Node_Id is
   begin

      Set_Str_To_Name_Buffer (Map_Identifier_Name_2Ada (Name));
      return Mk_Node_Ada_Identifier (Name_Find);
   end Mk_Node_Ada_Identifier;


   function Mk_Node_Ada_Function_Spec
     (Function_Id : Node_Id;
      Arg_List : List_Id; Return_Type : Node_Id) return Node_Id is
      Ada_Function_Spec_Node : Node_Id;
   begin
      Ada_Function_Spec_Node := New_Node (K_Ada_Function_Spec, No_Location);
      Set_Identifier (Ada_Function_Spec_Node, Function_Id);
      Set_Argument_List (Ada_Function_Spec_Node, Arg_List);
      Set_Type_Spec (Ada_Function_Spec_Node, Return_Type);
      return Ada_Function_Spec_Node;
   end Mk_Node_Ada_Function_Spec;

   function Mk_Node_Ada_Function
     (Function_Spec : Node_Id; Decl : List_Id;
                               Funct_Body : List_Id) return Node_Id is
      pragma Unreferenced (Function_Spec, Decl, Funct_Body);
   begin
      return No_Node;
   end Mk_Node_Ada_Function;


   function Mk_Node_Ada_Procedure_Spec
     (Procedure_Id : Node_Id;
      Arg_List : List_Id) return Node_Id is

      Ada_Procedure_Spec_Node : Node_Id;
   begin
      Ada_Procedure_Spec_Node := New_Node
        (K_Ada_Procedure_Spec, No_Location);
      Set_Identifier (Ada_Procedure_Spec_Node, Procedure_Id);
      Set_Argument_List (Ada_Procedure_Spec_Node, Arg_List);

      return Ada_Procedure_Spec_Node;
   end Mk_Node_Ada_Procedure_Spec;

   function Mk_Node_Ada_Procedure
     (Proc_Spec : Node_Id; Decl : List_Id;
                           Proc_Body : List_Id) return Node_Id is
      pragma Unreferenced (Proc_Spec, Decl, Proc_Body);
   begin
      return No_Node;
   end Mk_Node_Ada_Procedure;

   --------------------------------------
   -- Mk_Node_Simple_Derived_Type_Def  --
   --------------------------------------
   function Mk_Node_Simple_Derived_Type_Def
     (Identifier_Node : Node_Id; Type_Spec_Node : Node_Id) return Node_Id is

      Node : Node_Id;
      Nested_Node : Node_Id;
   begin
      Node := New_Node (K_Type_Declaration, No_Location);
      Nested_Node := New_Node (K_Derived_Type_Definition, No_Location);
      Set_Identifier (Nested_Node, Type_Spec_Node);
      Set_Is_Abstract (Nested_Node, False);
      Set_Identifier (Node, Identifier_Node);
      Set_Type_Spec (Node, Nested_Node);
      return Node;
   end Mk_Node_Simple_Derived_Type_Def;



end Backend.BE_Ada.Nutils;
