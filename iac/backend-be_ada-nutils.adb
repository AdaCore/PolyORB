with Locations; use Locations;
--   with Output; use Output;
--   with Debug; use Debug;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
with Namet; use Namet;
with Backend.BE_Ada.Namet;



package body Backend.BE_Ada.Nutils is



   package BE_Namet renames Backend.BE_Ada.Namet;


   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;
   begin
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


   function Map_Id_Name_Idl2Ada (N : Name_Id) return Name_Id is
      Str : String := Get_Name_String (N);
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
         BE_Namet.Set_Str_To_Name_Buffer (Str (First .. Str'Last) & 'U');
      else
         BE_Namet.Set_Str_To_Name_Buffer (Str (First .. Str'Last));
      end if;

      return BE_Namet.Name_Find;
   end Map_Id_Name_Idl2Ada;

   function Mk_Node_Ada_Argument
     (I : Node_Id; T : Node_Id; M : Mode_Id) return Node_Id is
      pragma Unreferenced (I, T, M);
   begin
      return No_Node;
   end Mk_Node_Ada_Argument;

   function Mk_Node_Ada_Argument_List (L : List_Id) return List_Id is
      pragma Unreferenced (L);
   begin
      return No_List;
   end Mk_Node_Ada_Argument_List;


   function Mk_Node_Ada_Function_Spec
     (Arg_List : List_Id; Return_Type : Node_Id) return Node_Id is
      pragma Unreferenced (Arg_List, Return_Type);
   begin
      return No_Node;
   end Mk_Node_Ada_Function_Spec;

   function Mk_Node_Ada_Function
     (Function_Spec : Node_Id; Decl : List_Id;
                               Funct_Body : List_Id) return Node_Id is
      pragma Unreferenced (Function_Spec, Decl, Funct_Body);
   begin
      return No_Node;
   end Mk_Node_Ada_Function;


   function Mk_Node_Ada_Procedure_Spec
     (Arg_List : List_Id) return Node_Id is
      pragma Unreferenced (Arg_List);
   begin
      return No_Node;
   end Mk_Node_Ada_Procedure_Spec;

   function Mk_Node_Ada_Procedure
     (Proc_Spec : Node_Id; Decl : List_Id;
                           Proc_Body : List_Id) return Node_Id is
      pragma Unreferenced (Proc_Spec, Decl, Proc_Body);
   begin
      return No_Node;
   end Mk_Node_Ada_Procedure;



end Backend.BE_Ada.Nutils;
