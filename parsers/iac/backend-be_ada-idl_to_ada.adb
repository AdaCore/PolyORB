------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--            B A C K E N D . B E _ A D A . I D L _ T O _ A D A             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Types;     use Types;
with Values;    use Values;
with Locations; use Locations;

with Frontend.Nodes;             use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Expand;      use Backend.BE_Ada.Expand;

package body Backend.BE_Ada.IDL_To_Ada is

   Setter : constant Character := 'S';

   package BEN renames Backend.BE_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   function Base_Type_TC
     (K : FEN.Node_Kind)
     return Node_Id
   is
   begin
      case K is
         when FEN.K_Float               => return RE (RE_TC_Float);
         when FEN.K_Double              => return RE (RE_TC_Double);
         when FEN.K_Long_Double         => return RE (RE_TC_Long_Double);
         when FEN.K_Short               => return RE (RE_TC_Short);
         when FEN.K_Long                => return RE (RE_TC_Long);
         when FEN.K_Long_Long           => return RE (RE_TC_Long_Long);
         when FEN.K_Unsigned_Short      => return RE (RE_TC_Unsigned_Short);
         when FEN.K_Unsigned_Long       => return RE (RE_TC_Unsigned_Long);
         when FEN.K_Unsigned_Long_Long  => return RE
            (RE_TC_Unsigned_Long_Long);
         when FEN.K_Char                => return RE (RE_TC_Char);
         when FEN.K_Wide_Char           => return RE (RE_TC_WChar);
         when FEN.K_String              => return RE (RE_TC_String);
         when FEN.K_Wide_String         => return RE (RE_TC_Wide_String);
         when FEN.K_Boolean             => return RE (RE_TC_Boolean);
         when FEN.K_Octet               => return RE (RE_TC_Octet);
         when FEN.K_Object              => return RE (RE_TC_Object_0);
         when FEN.K_Any                 => return RE (RE_TC_Any);
         when others                    =>
            raise Program_Error;
      end case;
   end Base_Type_TC;

   ---------------------
   -- Bind_FE_To_Impl --
   ---------------------

   procedure Bind_FE_To_Impl
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Impl_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Impl;

   -----------------------
   -- Bind_FE_To_Helper --
   -----------------------

   procedure Bind_FE_To_Helper
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Helper_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Helper;

   ---------------------
   -- Bind_FE_To_Skel --
   ---------------------

   procedure Bind_FE_To_Skel
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Skel_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Skel;

   ---------------------
   -- Bind_FE_To_Stub --
   ---------------------

   procedure Bind_FE_To_Stub
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Stub_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Stub;

   -------------------
   -- Bind_FE_To_TC --
   -------------------

   procedure Bind_FE_To_TC
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_TC_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_TC;

   -------------------------
   -- Bind_FE_To_From_Any --
   -------------------------

   procedure Bind_FE_To_From_Any
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_From_Any_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_From_Any;

   -----------------------
   -- Bind_FE_To_To_Any --
   -----------------------

   procedure Bind_FE_To_To_Any
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_To_Any_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_To_Any;

   -----------------------
   -- Bind_FE_To_To_Ref --
   -----------------------

   procedure Bind_FE_To_To_Ref
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_To_Ref_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_To_Ref;

   -------------------------
   -- Bind_FE_To_U_To_Ref --
   -------------------------

   procedure Bind_FE_To_U_To_Ref
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_U_To_Ref_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_U_To_Ref;

   -------------------------
   -- Bind_FE_To_Type_Def --
   -------------------------

   procedure Bind_FE_To_Type_Def
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Type_Def_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Type_Def;

   ------------------------
   -- Bind_FE_To_Forward --
   ------------------------

   procedure Bind_FE_To_Forward
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Forward_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Forward;

   -------------------------------
   -- Bind_FE_To_Instanciations --
   -------------------------------

   procedure Bind_FE_To_Instanciations
     (F : Node_Id;
      Stub_Package_Node : Node_Id := No_Node;
      Stub_Type_Node : Node_Id := No_Node;
      Helper_Package_Node : Node_Id := No_Node;
      TC_Node : Node_Id := No_Node;
      From_Any_Node : Node_Id := No_Node;
      To_Any_Node : Node_Id := No_Node)
   is
      N : Node_Id;
      I : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      I := BE_Ada_Instanciations (N);

      if No (I) then
         I := New_Node (BEN.K_BE_Ada_Instanciations);
      end if;

      if Present (Stub_Package_Node) then
         Set_Stub_Package_Node (I, Stub_Package_Node);
      end if;

      if Present (Stub_Type_Node) then
         Set_Stub_Type_Node (I, Stub_Type_Node);
      end if;

      if Present (Helper_Package_Node) then
         Set_Helper_Package_Node (I, Helper_Package_Node);
      end if;

      if Present (TC_Node) then
         Set_TC_Node (I, TC_Node);
      end if;

      if Present (From_Any_Node) then
         Set_From_Any_Node (I, From_Any_Node);
      end if;

      if Present (To_Any_Node) then
         Set_To_Any_Node (I, To_Any_Node);
      end if;

      BEN.Set_BE_Ada_Instanciations (N, I);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (I, F);
   end Bind_FE_To_Instanciations;

   ---------------------------
   -- Bind_FE_To_Marshaller --
   ---------------------------

   procedure Bind_FE_To_Marshaller
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Marshaller_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Marshaller;

   -----------------------------
   -- Bind_FE_To_Unmarshaller --
   -----------------------------

   procedure Bind_FE_To_Unmarshaller
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Unmarshaller_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Unmarshaller;

   -------------------------
   -- Bind_FE_To_Set_Args --
   -------------------------

   procedure Bind_FE_To_Set_Args
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Set_Args_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Set_Args;

   ------------------
   -- Is_Base_Type --
   ------------------

   function Is_Base_Type
     (N : Node_Id)
     return Boolean
   is
   begin
      if FEN.Kind (N) in  FEN.K_Float .. FEN.K_Value_Base then
         return True;
      else
         return False;
      end if;
   end Is_Base_Type;

   --------------------
   -- Is_Object_Type --
   --------------------

   function Is_Object_Type
     (E : Node_Id)
     return Boolean
   is
   begin
      if FEN.Kind (E) = K_Object then
         return True;
      end if;

      if FEN.Kind (E) /= K_Scoped_Name then
         return False;
      end if;

      if FEN.Kind (Reference (E)) = K_Interface_Declaration
        or else
        FEN.Kind (Reference (E)) = K_Forward_Interface_Declaration
      then
         return True;
      end if;

      if FEN.Kind (Reference (E)) = K_Simple_Declarator
        or else
        FEN.Kind (Reference (E)) = K_Complex_Declarator
      then
         return Is_Object_Type (Type_Spec (Declaration (Reference (E))));
      end if;

      return False;

   end Is_Object_Type;

   ----------------------
   -- Is_N_Parent_Of_M --
   ----------------------

   function Is_N_Parent_Of_M
     (N : Node_Id;
      M : Node_Id)
     return Boolean
   is
      X : Node_Id := N;
      Y : Node_Id := M;
   begin
      if No (Y) then
         return False;
      else
         if FEN.Kind (X) = K_Identifier then
            X := Corresponding_Entity (X);
         end if;

         if FEN.Kind (Y) = K_Identifier then
            Y := Corresponding_Entity (Y);
         end if;

         if X = Y then
            return True;
         elsif FEN.Kind (Y) = K_Specification
           or else FEN.Kind (Scope_Entity (Identifier (Y))) = K_Specification
         then
            return False;
         else
            return Is_N_Parent_Of_M (X, Scope_Entity (Identifier (Y)));
         end if;
      end if;
   end Is_N_Parent_Of_M;

   -------------------
   -- Link_BE_To_FE --
   -------------------

   procedure Link_BE_To_FE
     (BE : Node_Id;
      FE : Node_Id)
   is
   begin
      Set_FE_Node (BE, FE);
   end Link_BE_To_FE;

   ------------------------------
   -- Map_Accessor_Declaration --
   ------------------------------

   function Map_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
     return Node_Id
   is
      Parameter  : Node_Id;
      Parameters : List_Id;
      Param_Type : Node_Id;
      Attr_Name  : Name_Id;
      Result     : Node_Id;
      Container  : constant Node_Id := Scope_Entity (Identifier (Attribute));
   begin
      Parameters := New_List (K_Parameter_Profile);

      --  Add the dispatching parameter to the parameter profile

      Parameter  := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)),
         Map_Ref_Type (Container));
      Append_Node_To_List (Parameter, Parameters);

      Param_Type := Map_Designator
        (Type_Spec (Declaration (Attribute)));
      Set_FE_Node (Param_Type, Type_Spec (Declaration (Attribute)));

      --  For the setter subprogram, add the second parameter To.

      if Accessor = Setter then
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_To)), Param_Type);
         Append_Node_To_List (Parameter, Parameters);
         Param_Type := No_Node;
      end if;

      --  At this point, Param_Type is the returned type. No_Node
      --  stands for the setter. Otherwise it is the getter.

      Attr_Name := To_Ada_Name (IDL_Name (FEN.Identifier (Attribute)));
      Set_Str_To_Name_Buffer ("Set_");
      Name_Buffer (1) := Accessor;
      Get_Name_String_And_Append (Attr_Name);
      Result := Make_Subprogram_Specification
        (Make_Defining_Identifier (Name_Find), Parameters, Param_Type);

      --  Link the generated node with the Frontend node

      Link_BE_To_FE (Result, Identifier (Attribute));
      return Result;
   end Map_Accessor_Declaration;

   ------------------------------------
   -- Map_Declarator_Type_Designator --
   ------------------------------------

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id
   is
      Designator : Node_Id;
      Decl_Name  : Name_Id;
      Type_Node  : Node_Id;
   begin
      Designator := Map_Designator (Type_Decl);

      --  When the declarator is complex, the component type is an
      --  array type.

      if Kind (Declarator) = K_Complex_Declarator then
         Decl_Name := To_Ada_Name (IDL_Name (FEN.Identifier (Declarator)));
         Get_Name_String (Decl_Name);
         Add_Str_To_Name_Buffer ("_Array");
         Decl_Name := Name_Find;
         Type_Node := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier (Decl_Name),
            Type_Definition     => Make_Array_Type_Definition
            (Map_Range_Constraints
             (FEN.Array_Sizes (Declarator)), Designator));
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (Type_Node),
            (Defining_Identifier
             (Main_Package
              (Current_Entity))));

         --  We make a link between the identifier and the type declaration.
         --  This link is useful for the generation of the From_Any and To_Any
         --  functions and the TC_XXX constant necessary for user defined
         --  types.

         Bind_FE_To_Type_Def (FEN.Identifier (Declarator), Type_Node);
         Append_Node_To_List
           (Type_Node,
            Visible_Part (Current_Package));
         Designator := New_Node (K_Designator);
         Set_Defining_Identifier
           (Designator, Defining_Identifier (Type_Node));
         Set_Correct_Parent_Unit_Name
           (Designator,
            (Defining_Identifier
             (Main_Package
              (Current_Entity))));
      end if;

      return Designator;
   end Map_Declarator_Type_Designator;

   -----------------------------
   -- Map_Defining_Identifier --
   -----------------------------

   function Map_Defining_Identifier (Entity : Node_Id) return Node_Id is

      I      : Node_Id := Entity;
      Result : Node_Id;

   begin
      if FEN.Kind (Entity) /= FEN.K_Identifier then
         I := FEN.Identifier (Entity);
      end if;

      Result := Make_Defining_Identifier (IDL_Name (I));
      if Present (BE_Node (I))
        and then Present (Stub_Node (BE_Node (I)))
        and then BEN.Kind (Stub_Node (BE_Node (I))) = K_IDL_Unit
      then
         Set_Corresponding_Node
           (Result, Main_Package (Stub_Node (BE_Node (I))));
      end if;

      return Result;
   end Map_Defining_Identifier;

   --------------------
   -- Map_Designator --
   --------------------

   function Map_Designator
     (Entity : Node_Id)
     return Node_Id
   is
      P : Node_Id;
      N : Node_Id;
      K : FEN.Node_Kind;
      R : Node_Id;
      B : Node_Id;
      Ref_Type_Node : Node_Id;
   begin
      K := FEN.Kind (Entity);

      if K = FEN.K_Scoped_Name then
         R := Reference (Entity);

         --  The routine below verifies wether the scoped name designs a CORBA
         --  Entity declared in orb.idl, in which case, it returns the
         --  correponding runtime entity

         N := Map_Predefined_CORBA_Entity (Entity);
         if Present (N) then
            return N;
         end if;

         if Kind (R) = K_Specification then
            return No_Node;
         end if;

         --  Handling the case where R is not a base type nor a user defined
         --  type but an Interface type :
         --  interface myType {...}
         --  In this case we do not return the identifier of the interface name
         --  but the identifier to the Ref type defined in the stub package
         --  relative to the interface.

         N := New_Node (K_Designator);
         if Kind (R) = FEN.K_Interface_Declaration then

            --  Getting the node of the Ref type declaration.

            Ref_Type_Node := Type_Def_Node (BE_Node (Identifier (R)));
            Set_Defining_Identifier
              (N,
               Copy_Node (Defining_Identifier (Ref_Type_Node)));
            Set_FE_Node (N, R);
            P := R;
         elsif Kind (R) = FEN.K_Forward_Interface_Declaration then

            --  Getting the node of the Ref type declaration.

            Ref_Type_Node := Type_Def_Node (BE_Node (Identifier (R)));
            Set_Defining_Identifier
              (N,
               Copy_Node (Defining_Identifier (Ref_Type_Node)));
            Set_FE_Node (N, R);

            Set_Correct_Parent_Unit_Name
              (N,
               Defining_Identifier
               (Stub_Package_Node
                (BE_Ada_Instanciations
                 (BE_Node
                  (Identifier
                   (R))))));
            P := No_Node;
         else
            Set_Defining_Identifier (N, Map_Defining_Identifier (R));
            Set_FE_Node (N, R);
            P := Scope_Entity (Identifier (R));
         end if;

         if Present (P) then
            if Kind (P) = K_Specification then
               B := Defining_Identifier_To_Designator
                 (Defining_Identifier
                  (Main_Package
                   (Stub_Node
                    (BE_Node
                     (Identifier
                      (P))))));
               Set_FE_Node (B, P);
               Set_Correct_Parent_Unit_Name
                 (N, B);
            else
               Set_Correct_Parent_Unit_Name (N, Map_Designator (P));
            end if;
         end if;

      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         N := RE (Convert (K));
         Set_FE_Node (N, Entity);

      else
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, Map_Defining_Identifier (Entity));

         if K = FEN.K_Interface_Declaration
           or else K = FEN.K_Module
         then
            P := Scope_Entity (Identifier (Entity));
            Set_FE_Node (N, Entity);
            Set_Correct_Parent_Unit_Name (N, Map_Designator (P));

         elsif K = FEN.K_Specification then
            return No_Node;
         end if;
      end if;

      P := Parent_Unit_Name (N);
      if Present (P) then
         Add_With_Package (P);
      end if;

      return N;
   end Map_Designator;

   ------------------------------------
   -- Map_Fully_Qualified_Identifier --
   ------------------------------------

   function Map_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id is
      N : Node_Id;
      P : Node_Id;
      I : Node_Id;

   begin
      I := FEN.Identifier (Entity);
      Get_Name_String (IDL_Name (I));

      if Kind (Entity) = K_Specification then
         Add_Str_To_Name_Buffer ("_IDL_File");
      end if;

      N := Make_Defining_Identifier (Name_Find);
      P := FEN.Scope_Entity (I);

      if Present (P)
        and then FEN.Kind (P) /= FEN.K_Specification
      then
         if FEN.Kind (P) = FEN.K_Operation_Declaration then
            I := FEN.Identifier (P);
            P := FEN.Scope_Entity (I);
         end if;

         Set_Correct_Parent_Unit_Name (N, Map_Fully_Qualified_Identifier (P));
      end if;

      return N;
   end Map_Fully_Qualified_Identifier;

   --------------------------
   -- Map_Get_Members_Spec --
   --------------------------

   function Map_Get_Members_Spec
     (Member_Type : Node_Id)
     return Node_Id
   is
      Profile   : List_Id;
      Parameter : Node_Id;
      N         : Node_Id;
   begin
      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_From)),
         RE (RE_Exception_Occurrence));
      Append_Node_To_List (Parameter, Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_To)),
         Member_Type,
         Mode_Out);
      Append_Node_To_List (Parameter, Profile);

      N := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_Get_Members)),
         Profile,
         No_Node);

      return N;
   end Map_Get_Members_Spec;

   ------------------
   -- Map_IDL_Unit --
   ------------------

   function Map_IDL_Unit (Entity : Node_Id) return Node_Id is
      P : Node_Id;
      N : Node_Id;
      M : Node_Id;  -- Main Package;
      D : Node_Id;
      L : List_Id;
      I : Node_Id;

   begin
      P := New_Node (K_IDL_Unit, Identifier (Entity));
      L := New_List (K_Packages);
      Set_Packages (P, L);
      I := Map_Fully_Qualified_Identifier (Entity);

      --  We don't generate code for imported entities

      Set_Generate_Code (P, not Imported (Entity));

      --  Main package

      M := Make_Package_Declaration (I);
      Set_IDL_Unit (M, P);
      Set_Main_Package (P, M);
      Append_Node_To_List (M, L);

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Defining_Identifier (Name_Find);
      Set_Correct_Parent_Unit_Name (N, I);
      D := Make_Package_Declaration (N);
      Set_IDL_Unit (D, P);
      Set_Parent (D, M);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (Entity) = K_Interface_Declaration then

         if not FEN.Is_Abstract_Interface (Entity) then

            --  No CDR, Skel or Impl packages are generated for abstract
            --  interfaces.

            if not FEN.Is_Local_Interface (Entity) then

               --  No CDR or Skel packages are generated for local interfaces

               --  Skeleton package

               Set_Str_To_Name_Buffer ("Skel");
               N := Make_Defining_Identifier (Name_Find);
               Set_Correct_Parent_Unit_Name (N, I);
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_Skeleton_Package (P, D);
               Append_Node_To_List (D, L);

               --  CDR package

               Set_Str_To_Name_Buffer ("CDR");
               N := Make_Defining_Identifier (Name_Find);
               Set_Correct_Parent_Unit_Name (N, I);
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_CDR_Package (P, D);
               Append_Node_To_List (D, L);
            end if;

            --  Implementation package

            Set_Str_To_Name_Buffer ("Impl");
            N := Make_Defining_Identifier (Name_Find);
            Set_Correct_Parent_Unit_Name (N, I);
            D := Make_Package_Declaration (N);
            Set_IDL_Unit (D, P);
            Set_Parent (D, M);
            Set_Implementation_Package (P, D);

            if Impl_Packages_Gen then
               Append_Node_To_List (D, L);
            end if;
         end if;
      end if;

      return P;
   end Map_IDL_Unit;

   -------------------
   -- Map_Impl_Type --
   -------------------

   function Map_Impl_Type
     (Entity : Node_Id)
     return Node_Id
   is
      pragma Assert
        (FEN.Kind (Entity) = K_Interface_Declaration
         or else FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ref_Type : Node_Id;
   begin
      if Is_Local_Interface (Entity) then

         --  Here, we use a runtime entity instead of a T_XXX because the
         --  casing rules in the type name are not standard and have to be
         --  registered

         Ref_Type := Defining_Identifier (RE (RE_LocalObject));
      else
         Ref_Type := Make_Defining_Identifier (TN (T_Object));
      end if;

      return Ref_Type;
   end Map_Impl_Type;

   ----------------------------
   -- Map_Impl_Type_Ancestor --
   ----------------------------

   function Map_Impl_Type_Ancestor
     (Entity : Node_Id)
     return Node_Id
   is
      pragma Assert
        (FEN.Kind (Entity) = K_Interface_Declaration
         or else FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ancestor : Node_Id;
   begin
      if Is_Local_Interface (Entity) then
         Ancestor := RE (RE_Object_2);
      else
         Ancestor := RE (RE_Servant_Base);
      end if;

      return Ancestor;
   end Map_Impl_Type_Ancestor;

   ----------------------------
   -- Map_Members_Definition --
   ----------------------------

   function Map_Members_Definition (Members : List_Id) return List_Id is
      Components            : List_Id;
      Member                : Node_Id;
      Declarator            : Node_Id;
      Member_Type           : Node_Id;
      Component_Declaration : Node_Id;
   begin
      Components := New_List (K_Component_List);
      Member := First_Entity (Members);
      while Present (Member) loop
         Declarator := First_Entity (Declarators (Member));
         Member_Type := Type_Spec (Member);
         while Present (Declarator) loop
            Component_Declaration := Make_Component_Declaration
              (Map_Defining_Identifier (FEN.Identifier (Declarator)),
               Map_Declarator_Type_Designator (Member_Type, Declarator));
            Bind_FE_To_Stub
              (Identifier (Declarator), Component_Declaration);
            Append_Node_To_List
              (Component_Declaration, Components);
            Declarator := Next_Entity (Declarator);
         end loop;
         Member := Next_Entity (Member);
      end loop;
      return Components;
   end Map_Members_Definition;

   ------------------------------
   -- Map_Narrowing_Designator --
   ------------------------------

   function Map_Narrowing_Designator
     (E         : Node_Id;
      Unchecked : Boolean)
     return Node_Id
   is
   begin
      case Unchecked is
         when True =>
            if Is_Abstract_Interface (E) then
               return Make_Defining_Identifier
                 (SN (S_Unchecked_To_Abstract_Ref));
            elsif Is_Local_Interface (E) then
               return Make_Defining_Identifier
                 (SN (S_Unchecked_To_Local_Ref));
            else
               return Make_Defining_Identifier
                 (SN (S_Unchecked_To_Ref));
            end if;
         when False =>
            if Is_Abstract_Interface (E) then
               return Make_Defining_Identifier
                 (SN (S_To_Abstract_Ref));
            elsif Is_Local_Interface (E) then
               return Make_Defining_Identifier
                 (SN (S_To_Local_Ref));
            else
               return Make_Defining_Identifier
                 (SN (S_To_Ref));
            end if;
      end case;
   end Map_Narrowing_Designator;

   ---------------------------
   -- Map_Range_Constraints --
   ---------------------------

   function Map_Range_Constraints
     (Array_Sizes : List_Id)
     return List_Id
   is
      L : List_Id;
      S : Node_Id;
      R : Node_Id;
      V : Value_Type;

   begin
      L := New_List (K_Range_Constraints);
      S := FEN.First_Entity (Array_Sizes);
      while Present (S) loop

         --  The range constraints may be :
         --  * Literal values
         --  * Previously declared constants (concretly, scoped names)

         R := New_Node (K_Range_Constraint);
         Set_First (R, Make_Literal (Int0_Val));
         if FEN.Kind (S) = K_Scoped_Name then
            V := Value (FEN.Value (Reference (S)));
            V.IVal := V.IVal - 1;
         else
            V := Value (FEN.Value (S));
            V.IVal := V.IVal - 1;
         end if;
         Set_Last (R, Make_Literal (New_Value (V)));
         Append_Node_To_List (R, L);
         S := FEN.Next_Entity (S);
      end loop;
      return L;
   end Map_Range_Constraints;

   ------------------
   -- Map_Ref_Type --
   ------------------

   function Map_Ref_Type
     (Entity : Node_Id)
     return Node_Id
   is
      pragma Assert
        (FEN.Kind (Entity) = K_Interface_Declaration
         or else FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ref_Type : Node_Id;
   begin
      if Is_Abstract_Interface (Entity) then
         Ref_Type := Make_Defining_Identifier (TN (T_Abstract_Ref));
      elsif Is_Local_Interface (Entity) then
         Ref_Type := Make_Defining_Identifier (TN (T_Local_Ref));
      else
         Ref_Type := Make_Defining_Identifier (TN (T_Ref));
      end if;

      return Ref_Type;
   end Map_Ref_Type;

   ---------------------------
   -- Map_Ref_Type_Ancestor --
   ---------------------------

   function Map_Ref_Type_Ancestor
     (Entity : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      pragma Assert
        (FEN.Kind (Entity) = K_Interface_Declaration
         or else FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ancestor : Node_Id;
   begin
      if Is_Abstract_Interface (Entity) then

         --  The abstract interfaces should inherit from CORBA.AbstractBase.Ref
         --  to allow passing interfaces and valuTypes.
         --  Since the code generation for valuType is not performed by IAC
         --  It is useless (for now) to make the abstract interfaces inherit
         --  from CORBA.AbstractBase.Ref and it causes problems when compiling
         --  current generated code.
         --  Ancestor := RE (RE_Ref_1);

         --  XXX : To be replaced when the ValuTypes are implemented
         Ancestor := RE (RE_Ref_2, Withed);
      else
         Ancestor := RE (RE_Ref_2, Withed);
      end if;

      return Ancestor;
   end Map_Ref_Type_Ancestor;

   --------------------------------
   -- Map_Repository_Declaration --
   --------------------------------

   function Map_Repository_Declaration (Entity : Node_Id) return Node_Id is

      procedure Fetch_Prefix
        (Entity     : in  Node_Id;
         Parent     : in  Node_Id;
         Prefix     : out Name_Id;
         Has_Prefix : out Boolean);

      procedure Get_Repository_String
        (Entity                : Node_Id;
         First_Recursion_Level : Boolean := True;
         Found_Prefix          : Boolean := False);

      ------------------
      -- Fetch_Prefix --
      ------------------

      procedure Fetch_Prefix
        (Entity     : in  Node_Id;
         Parent     : in  Node_Id;
         Prefix     : out Name_Id;
         Has_Prefix : out Boolean)
      is
         Prefixes : constant List_Id := Type_Prefixes (Parent);
         P        : Node_Id;
      begin
         Prefix     := No_Name;
         Has_Prefix := False;

         if not FEU.Is_Empty (Prefixes) then
            P := First_Entity (Prefixes);
            while Present (P) loop

               --  By this test, we check at the same time that :
               --  * The Entity and the prefix are declared in the same file
               --  * The prefix is defined before the declaration of Entity

               if FEN.Loc (P) < FEN.Loc (Entity) then
                  Prefix := IDL_Name (P);
                  Has_Prefix := True;
                  exit;
               end if;
               P := Next_Entity (P);
            end loop;
         end if;

      end Fetch_Prefix;

      ---------------------------
      -- Get_Repository_String --
      ---------------------------

      procedure Get_Repository_String
        (Entity                : Node_Id;
         First_Recursion_Level : Boolean := True;
         Found_Prefix          : Boolean := False) is

         I          : Node_Id;
         S          : Node_Id;
         Prefix     : Name_Id;
         Has_Prefix : Boolean := Found_Prefix;
         Name       : Name_Id;

      begin

         --  The explicit definition of a type ID disables the effects of the
         --  type prefix and the type version explicit definitions, the
         --  conflicts being checked in the analyze phase of the frontend.

         if First_Recursion_Level
           and then Present (FEN.Type_Id (Entity))
         then
            Get_Name_String (IDL_Name (FEN.Type_Id (Entity)));
            return;
         end if;

         --  For entity kinds modules, interfaces and valutypes, the prefix
         --  fetching begins from the entity itself. For the rest of kind, the
         --  fetchin statrts from the parent entity

         if First_Recursion_Level
           and then (FEN.Kind (Entity) = K_Module
                     or else FEN.Kind (Entity) = K_Interface_Declaration
                     or else FEN.Kind (Entity) = K_Value_Declaration)
           and then not FEU.Is_Empty (Type_Prefixes (Entity))
         then
            Has_Prefix := True;
            Name := Name_Find; --  Backup
            Prefix := IDL_Name (Last_Entity (Type_Prefixes (Entity)));
            Prefix := Add_Suffix_To_Name ("/", Prefix);
            Name   := Add_Suffix_To_Name
              (Get_Name_String (Prefix),
               Name);
            Get_Name_String (Name); --  Restore
            Prefix := No_Name;
         end if;

         I := FEN.Identifier (Entity);

         --  The potential scope is used to determin the entity parent

         S := Potential_Scope (I);

         if Present (S) then

            --  We check if the scope entity S has a prefix whose declaration
            --  occurs before the Entity.

            if not Has_Prefix then
               Name := Name_Find; --  Backup
               Fetch_Prefix (Entity, S, Prefix, Has_Prefix);
               if Prefix /= No_Name then
                  Prefix := Add_Suffix_To_Name ("/", Prefix);
                  Name   := Add_Suffix_To_Name
                    (Get_Name_String (Prefix),
                     Name);
               end if;
               Get_Name_String (Name); --  Restore
            end if;

            --  Then we continue building the string for the

            if FEN.Kind (S) /= FEN.K_Specification then
               Get_Repository_String (S, False, Has_Prefix);
               Add_Char_To_Name_Buffer ('/');
            end if;
         end if;

         Get_Name_String_And_Append (FEN.IDL_Name (I));
      end Get_Repository_String;

      I : Name_Id;
      V : Value_Id;

   begin

      --  Building the RepositoryId designator

      Name_Len := 0;
      case FEN.Kind (Entity) is
         when FEN.K_Interface_Declaration
           | FEN.K_Module =>
            null;

         when FEN.K_Structure_Type
           | FEN.K_Simple_Declarator
           | FEN.K_Complex_Declarator
           | FEN.K_Enumeration_Type
           | FEN.K_Exception_Declaration
           | FEN.K_Operation_Declaration
           | FEN.K_Union_Type =>
            Get_Name_String
              (To_Ada_Name (FEN.IDL_Name (FEN.Identifier (Entity))));
            Add_Char_To_Name_Buffer ('_');

         when others =>
            raise Program_Error;
      end case;
      Add_Str_To_Name_Buffer ("Repository_Id");
      I := Name_Find;

      --  Building the RepositoryId string value

      Set_Str_To_Name_Buffer ("IDL:");
      Get_Repository_String (Entity);
      if No (FEN.Type_Id (Entity)) then
         Add_Char_To_Name_Buffer (':');
         if Present (Type_Version (Entity)) then
            Get_Name_String_And_Append (FEN.IDL_Name (Type_Version (Entity)));
         else

            --  Extract from the CORBA 3.0 spec ($10.7.5.3):
            --  "If no version pragma is supplied for a definition, version
            --   1.0 is assumed"

            Add_Str_To_Name_Buffer ("1.0");
         end if;
      end if;

      V := New_String_Value (Name_Find, False);
      return Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (I),
         Constant_Present    => True,
         Object_Definition   => RE (RE_String_2),
         Expression          => Make_Literal (V));
   end Map_Repository_Declaration;

   ----------------------
   -- Map_Variant_List --
   ----------------------

   function Map_Variant_List
     (Alternatives   : List_Id;
      Literal_Parent : Node_Id := No_Node)
     return List_Id
   is

      Alternative : Node_Id;
      Variants    : List_Id;
      Variant     : Node_Id;
      Choices     : List_Id;
      Choice      : Node_Id;
      Label       : Node_Id;
      Element     : Node_Id;
      Identifier  : Node_Id;

   begin
      Variants := New_List (K_Variant_List);
      Alternative := First_Entity (Alternatives);
      while Present (Alternative) loop
         Variant := New_Node (K_Variant);
         Choices := New_List (K_Discrete_Choice_List);
         Set_Discrete_Choices (Variant, Choices);
         Label   := First_Entity (Labels (Alternative));
         Element := FEN.Element (Alternative);
         while Present (Label) loop

            Choice := Make_Literal
              (Value             => FEN.Value (Label),
               Parent_Designator => Literal_Parent);
            Append_Node_To_List (Choice, Choices);
            Label := Next_Entity (Label);
         end loop;
         Identifier := FEN.Identifier (FEN.Declarator (Element));
         Set_Component
           (Variant,
            Make_Component_Declaration
              (Map_Defining_Identifier (Identifier),
               Map_Declarator_Type_Designator
               (Type_Spec (Element), FEN.Declarator (Element))));
         Append_Node_To_List (Variant, Variants);
         Alternative := Next_Entity (Alternative);
      end loop;
      return Variants;
   end Map_Variant_List;

   --  Bodies of the CORBA module handling routines

   ---------------------------------
   -- Get_CORBA_Predefined_Entity --
   ---------------------------------

   function Get_CORBA_Predefined_Entity
     (E      : Node_Id;
      Implem : Boolean := False)
     return RE_Id
   is
      Entity : Node_Id;
      E_Name : Name_Id;
      N      : Node_Id;
   begin
      if FEN.Kind (E) = K_Scoped_Name then
         Entity := Reference (E);
      else
         Entity := E;
      end if;
      E_Name := FEU.Fully_Qualified_Name (FEN.Identifier (Entity), ".");

      for R in CORBA_Predefined_RU'Range loop

         --  during the test phase, we don't "with" any package

         N := RU (R, False);
         if E_Name = Fully_Qualified_Name (N) then

            --  We return the Ref type or the Object.

            if Implem then
               return CORBA_Predefined_Implem_Table (R);
            else
               return CORBA_Predefined_RU_Table (R);
            end if;
         end if;
      end loop;

      for R in CORBA_Predefined_RE'Range loop
         N := RE (R, False);
         if E_Name = Fully_Qualified_Name (N) then
            return CORBA_Predefined_RE_Table (R);
         end if;
      end loop;

      return RE_Null;
   end Get_CORBA_Predefined_Entity;

   ----------------------------------
   -- Map_Predefined_CORBA_Entitiy --
   ----------------------------------

   function Map_Predefined_CORBA_Entity
     (E : Node_Id;
      Implem : Boolean := False)
     return Node_Id is
      R : RE_Id;
   begin
      R := Get_CORBA_Predefined_Entity (E, Implem);
      if R /= RE_Null then
         return (RE (R));
      else
         return No_Node;
      end if;
   end Map_Predefined_CORBA_Entity;

   -----------------------------
   -- Map_Predefined_CORBA_TC --
   -----------------------------

   function Map_Predefined_CORBA_TC (E : Node_Id) return Node_Id is
      R : RE_Id;
   begin
      R := Get_CORBA_Predefined_Entity (E);
      case R is
         when RE_Any =>
            return RE (RE_TC_Any);
         when RE_Identifier_0 =>
            return RE (RE_TC_Identifier);
         when RE_RepositoryId =>
            return RE (RE_TC_RepositoryId);
         when RE_ScopedName =>
            return RE (RE_TC_ScopedName);
         when RE_PolicyType =>
            return RE (RE_TC_PolicyType);
         when RE_Visibility =>
            return RE (RE_TC_Visibility);
         when RE_Float =>
            return RE (RE_TC_Float);
         when RE_Double =>
            return RE (RE_TC_Double);
         when RE_Long_Double =>
            return RE (RE_TC_Long_Double);
         when RE_Short =>
            return RE (RE_TC_Short);
         when RE_Long =>
            return RE (RE_TC_Long);
         when RE_Long_Long =>
            return RE (RE_TC_Long_Long);
         when RE_Unsigned_Short =>
            return RE (RE_TC_Unsigned_Short);
         when RE_Unsigned_Long =>
            return RE (RE_TC_Unsigned_Long);
         when RE_Unsigned_Long_Long =>
            return RE (RE_TC_Unsigned_Long_Long);
         when RE_Char =>
            return RE (RE_TC_Char);
         when RE_WChar =>
            return RE (RE_TC_WChar);
         when RE_String_0 =>
            return RE (RE_TC_String);
         when RE_Wide_String =>
            return RE (RE_TC_Wide_String);
         when RE_Boolean =>
            return RE (RE_TC_Boolean);
         when RE_Octet =>
            return RE (RE_TC_Octet);
         when RE_Ref_2 =>
            return RE (RE_TC_Object_0);
         when RE_Object =>
            return RE (RE_TC_TypeCode);

         when RE_AnySeq_2 =>
            return RE (RE_TC_AnySeq);
         when RE_FloatSeq_2 =>
            return RE (RE_TC_FloatSeq);
         when RE_DoubleSeq_2 =>
            return RE (RE_TC_DoubleSeq);
         when RE_LongDoubleSeq_2 =>
            return RE (RE_TC_LongDoubleSeq);
         when RE_ShortSeq_2 =>
            return RE (RE_TC_ShortSeq);
         when RE_LongSeq_2 =>
            return RE (RE_TC_LongSeq);
         when RE_LongLongSeq_2 =>
            return RE (RE_TC_LongLongSeq);
         when RE_UShortSeq_2 =>
            return RE (RE_TC_UShortSeq);
         when RE_ULongSeq_2 =>
            return RE (RE_TC_ULongSeq);
         when RE_ULongLongSeq_2 =>
            return RE (RE_TC_ULongLongSeq);
         when RE_CharSeq_2 =>
            return RE (RE_TC_CharSeq);
         when RE_WCharSeq_2 =>
            return RE (RE_TC_WCharSeq);
         when RE_StringSeq_2 =>
            return RE (RE_TC_StringSeq);
         when RE_WStringSeq_2 =>
            return RE (RE_TC_WStringSeq);
         when RE_BooleanSeq_2 =>
            return RE (RE_TC_BooleanSeq);
         when RE_OctetSeq_2 =>
            return RE (RE_TC_OctetSeq);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_TC;

   -----------------------------------
   -- Map_Predefined_CORBA_From_Any --
   -----------------------------------

   function Map_Predefined_CORBA_From_Any (E : Node_Id) return Node_Id is
      R : RE_Id;
   begin
      R := Get_CORBA_Predefined_Entity (E);
      case R is
         when RE_Any
           | RE_Float
           | RE_Double
           | RE_Long_Double
           | RE_Short
           | RE_Long
           | RE_Long_Long
           | RE_Unsigned_Short
           | RE_Unsigned_Long
           | RE_Unsigned_Long_Long
           | RE_Char
           | RE_WChar
           | RE_String_0
           | RE_Wide_String
           | RE_Boolean
           | RE_Octet
           | RE_Object =>
            return RE (RE_From_Any_0);

         when RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType =>
            return RE (RE_From_Any_2);

         when RE_Ref_2 =>
            return RE (RE_From_Any_1);

         when RE_AnySeq_2
           | RE_FloatSeq_2
           | RE_DoubleSeq_2
           | RE_LongDoubleSeq_2
           | RE_ShortSeq_2
           | RE_LongSeq_2
           | RE_LongLongSeq_2
           | RE_UShortSeq_2
           | RE_ULongSeq_2
           | RE_ULongLongSeq_2
           | RE_CharSeq_2
           | RE_WCharSeq_2
           | RE_StringSeq_2
           | RE_WStringSeq_2
           | RE_BooleanSeq_2
           | RE_OctetSeq_2 =>
            return RE (RE_From_Any_4);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_From_Any;

   ---------------------------------
   -- Map_Predefined_CORBA_To_Any --
   ---------------------------------

   function Map_Predefined_CORBA_To_Any (E : Node_Id) return Node_Id is
      R : RE_Id;
   begin
      R := Get_CORBA_Predefined_Entity (E);
      case R is
         when RE_Any
           | RE_Float
           | RE_Double
           | RE_Long_Double
           | RE_Short
           | RE_Long
           | RE_Long_Long
           | RE_Unsigned_Short
           | RE_Unsigned_Long
           | RE_Unsigned_Long_Long
           | RE_Char
           | RE_WChar
           | RE_String_0
           | RE_Wide_String
           | RE_Boolean
           | RE_Octet
           | RE_Object =>
            return RE (RE_To_Any_0);

         when RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType =>
            return RE (RE_To_Any_2);

         when RE_Ref_2 =>
            return RE (RE_To_Any_1);

         when RE_AnySeq_2
           | RE_FloatSeq_2
           | RE_DoubleSeq_2
           | RE_LongDoubleSeq_2
           | RE_ShortSeq_2
           | RE_LongSeq_2
           | RE_LongLongSeq_2
           | RE_UShortSeq_2
           | RE_ULongSeq_2
           | RE_ULongLongSeq_2
           | RE_CharSeq_2
           | RE_WCharSeq_2
           | RE_StringSeq_2
           | RE_WStringSeq_2
           | RE_BooleanSeq_2
           | RE_OctetSeq_2 =>
            return RE (RE_To_Any_4);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_To_Any;

   --  Inheritance related subprograms

   --  The two subprograms allow to get a new Int value or to view the old
   --  value in order to use it to mark inherited entities
   Mark : Int := 1;
   function Get_New_Int_Value return Int;
   function View_Old_Int_Value return Int;

   --  If two entities inherited from two parents have the same name,
   --  the second should not be added
   function Already_Inherited
     (Name      : Name_Id)
     return Boolean;

   --  This procedure generates a comment that indicates from which
   --  interface the entity we deal with is inherited.
   procedure Explaining_Comment
     (First_Name  : Name_Id;
      Second_Name : Name_Id;
      Message     : String);

   --  This function return True if parent is the first parent of Child or
   --  if it is one of the parents of the fist parent of Child
   function Is_Implicit_Parent
     (Parent : Node_Id;
      Child : Node_Id)
     return Boolean;

   procedure Map_Any_Converters
     (Type_Name : in  Name_Id;
      From_Any  : out Node_Id;
      To_Any    : out Node_Id);

   -----------------------
   -- Get_New_Int_Value --
   -----------------------

   function Get_New_Int_Value return Int is
   begin
      Mark := Mark + 1;
      return Mark;
   end Get_New_Int_Value;

   ------------------------
   -- View_Old_Int_Value --
   ------------------------

   function View_Old_Int_Value return Int is
   begin
      return Mark;
   end View_Old_Int_Value;

   ------------------------
   -- Explaining_Comment --
   ------------------------

   procedure Explaining_Comment
     (First_Name  : Name_Id;
      Second_Name : Name_Id;
      Message     : String)
   is
      Comment : Node_Id;
   begin
      Get_Name_String (First_Name);
      Add_Str_To_Name_Buffer (Message);
      Get_Name_String_And_Append (Second_Name);
      Comment := Make_Ada_Comment (Name_Find);
      Append_Node_To_List
        (Comment,
         Visible_Part (Current_Package));
   end Explaining_Comment;

   ------------------------
   -- Is_Implicit_Parent --
   ------------------------

   function Is_Implicit_Parent
     (Parent : Node_Id;
      Child : Node_Id)
     return Boolean
   is
      pragma Assert
        (Kind (Parent) = K_Interface_Declaration and then
         Kind (Child) = K_Interface_Declaration);
   begin
      if not FEU.Is_Empty (Interface_Spec (Child)) then
         return FEU.Is_Parent (Parent, Child, True)
           or else FEU.Is_Parent
           (Parent,
            Reference
            (First_Entity
             (Interface_Spec
              (Child))));
      end if;
      return False;
   end Is_Implicit_Parent;

   ------------------------
   -- Map_Any_Converters --
   ------------------------

   procedure Map_Any_Converters
     (Type_Name : in  Name_Id;
      From_Any  : out Node_Id;
      To_Any    : out Node_Id)
   is
      New_Type  : Node_Id;
      Profile   : List_Id;
      Parameter : Node_Id;
   begin
      New_Type := Make_Designator (Type_Name);
      Set_Correct_Parent_Unit_Name
        (New_Type,
         Expand_Designator
         (Main_Package
          (Current_Entity)));

      --  From_Any

      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Item)),
         RE (RE_Any));
      Append_Node_To_List (Parameter, Profile);
      From_Any := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_From_Any)),
         Profile,
         Defining_Identifier (New_Type));

      --  Setting the correct parent unit name, for the future calls of the
      --  subprogram

      Set_Correct_Parent_Unit_Name
        (Defining_Identifier (From_Any),
         Defining_Identifier (Helper_Package (Current_Entity)));

      --  To_Any

      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Item)),
         Defining_Identifier (New_Type));
      Append_Node_To_List (Parameter, Profile);
      To_Any := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_To_Any)),
         Profile, RE (RE_Any));

      --  Setting the correct parent unit name, for the future calls of the
      --  subprogram

      Set_Correct_Parent_Unit_Name
        (Defining_Identifier (To_Any),
         Defining_Identifier (Helper_Package (Current_Entity)));

   end Map_Any_Converters;

   ----------------------------------
   -- Map_Inherited_Entities_Specs --
   ----------------------------------

   procedure Map_Inherited_Entities_Specs
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_Two_Params_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False)
   is
      Par_Int                  : Node_Id;
      Par_Name                 : Name_Id;
      Do_Visit                 : Boolean := True;
      N                        : Node_Id;
      Actual_Current_Interface : Node_Id;
      Mark                     : Int;
      L                        : constant List_Id
        := Interface_Spec (Current_Interface);

   begin
      if FEU.Is_Empty (L) then
         return;
      end if;

      --  We get the node of the current interface (the interface who first
      --  called this subprogram

      Actual_Current_Interface := FEN.Corresponding_Entity
        (FE_Node (Current_Entity));

      if First_Recusrion_Level then

         --  it's important to get the new value before any inherited
         --  entity manipulation

         Mark := Get_New_Int_Value;

         if Stub or else Helper then

            --  Mapping of type definitions, constant declarations and
            --  exception declarations defined in the parents

            --  During the different recursion level, we must have access to
            --  the current interface we are visiting. So we don't use the
            --  parameter Current_Interface because its value changes depending
            --  on the recursion level.

            Map_Additional_Entities_Specs
              (Reference (First_Entity (L)),
               Actual_Current_Interface,
               Stub   => Stub,
               Helper => Helper);
         end if;

         Par_Int := Next_Entity (First_Entity (L));
      else
         Mark := View_Old_Int_Value;
         Par_Int := First_Entity (L);
      end if;

      while Present (Par_Int) loop

         --  We ensure that the interface is not visited twice and is not
         --  an implicit parent

         Par_Name := FEU.Fully_Qualified_Name
           (Identifier
            (Reference
             (Par_Int)));
         if Is_Implicit_Parent (Reference (Par_Int), Actual_Current_Interface)
           or else Get_Name_Table_Info (Par_Name) = Mark
         then
            Do_Visit := False;
         end if;
         Set_Name_Table_Info (Par_Name, Mark);

         if not Do_Visit then
            Do_Visit := True;
         else
            if Stub or else Helper then

               --  Mapping of type definitions, constant declarations and
               --  exception declarations defined in the parents

               --  During the different recursion level, we must have access to
               --  the current interface we are visiting. So we don't use the
               --  parameter Current_Interface because its value changes
               --  depending on the recursion level.

               Map_Additional_Entities_Specs
                 (Reference (Par_Int),
                  Actual_Current_Interface,
                  Stub   => Stub,
                  Helper => Helper);
            end if;
            if Stub
              or else Skel
              or else Impl
            then
               N := First_Entity (Interface_Body (Reference (Par_Int)));
               while Present (N) loop
                  case  FEN.Kind (N) is
                     when K_Operation_Declaration =>
                        if not Skel then

                           --  Adding an explaining comment

                           Explaining_Comment
                             (FEN.IDL_Name (Identifier (N)),
                              FEU.Fully_Qualified_Name
                              (Identifier (Reference (Par_Int)),
                               Separator => "."),
                              " : inherited from ");
                        end if;

                        Visit_Operation_Subp (N, False);

                     when others =>
                        null;
                  end case;
                  N := Next_Entity (N);
               end loop;
            end if;

            --  Get indirectly inherited entities

            Map_Inherited_Entities_Specs
              (Current_Interface     => Reference (Par_Int),
               First_Recusrion_Level => False,
               Visit_Operation_Subp  => Visit_Operation_Subp,
               Stub                  => Stub,
               Helper                => Helper,
               Skel                  => Skel,
               Impl                  => Impl);
         end if;
         Par_Int := Next_Entity (Par_Int);
      end loop;
   end Map_Inherited_Entities_Specs;

   -----------------------------------
   -- Map_Inherited_Entities_Bodies --
   -----------------------------------

   procedure Map_Inherited_Entities_Bodies
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_One_Param_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False)
   is
      Par_Int                  : Node_Id;
      Par_Name                 : Name_Id;
      Do_Visit                 : Boolean := True;
      N                        : Node_Id;
      Actual_Current_Interface : Node_Id;
      Mark                     : Int;
      L                        : constant List_Id
        := Interface_Spec (Current_Interface);
   begin
      if FEU.Is_Empty (L) then
         return;
      end if;

      --  We get the node of the current interface (the interface who first
      --  called this subprogram

      Actual_Current_Interface := FEN.Corresponding_Entity
        (FE_Node (Current_Entity));

      if First_Recusrion_Level
        and then not Skel
      then

         --  it's important to get the new value before any inherited
         --  entity manipulation

         Mark := Get_New_Int_Value;

         if Stub or else Helper then

            --  Mapping of type definitions, constant declarations and
            --  exception declarations defined in the parents

            --  During the different recursion level, we must have access to
            --  the current interface we are visiting. So we don't use the
            --  parameter Current_Interface because its value changes depending
            --  on the recursion level.

            Map_Additional_Entities_Bodies
              (Reference (First_Entity (L)),
               Actual_Current_Interface,
               Stub   => Stub,
               Helper => Helper);
         end if;
         Par_Int := Next_Entity (First_Entity (L));
      else
         Mark := View_Old_Int_Value;
         Par_Int := First_Entity (L);
      end if;

      while Present (Par_Int) loop

         --  We ensure that the interface is not visited twice and is not
         --  an implicit parent

         Par_Name := FEU.Fully_Qualified_Name
           (Identifier
            (Reference
             (Par_Int)));
         if Is_Implicit_Parent (Reference (Par_Int), Actual_Current_Interface)
           or else Get_Name_Table_Info (Par_Name) = Mark
         then
            Do_Visit := False;
         end if;
         Set_Name_Table_Info (Par_Name, Mark);

         if not Do_Visit then
            Do_Visit := True;
         else
            if Stub or else Helper then

               --  Mapping of type definitions, constant declarations and
               --  exception declarations defined in the parents

               --  During the different recursion level, we must have access to
               --  the current interface we are visiting. So we don't use the
               --  parameter Current_Interface because its value changes
               --  depending on the recursion level.

               Map_Additional_Entities_Bodies
                 (Reference (Par_Int),
                  Actual_Current_Interface,
                  Stub   => Stub,
                  Helper => Helper);
            end if;

            if Stub
              or else Skel
              or else Impl
            then
               N := First_Entity (Interface_Body (Reference (Par_Int)));
               while Present (N) loop
                  case  FEN.Kind (N) is
                     when K_Operation_Declaration =>
                        Visit_Operation_Subp (N);
                     when others =>
                        null;
                  end case;
                  N := Next_Entity (N);
               end loop;
            end if;

            --  Get indirectly inherited entities

            Map_Inherited_Entities_Bodies
              (Current_Interface     => Reference (Par_Int),
               First_Recusrion_Level => False,
               Visit_Operation_Subp  => Visit_Operation_Subp,
               Stub                  => Stub,
               Helper                => Helper,
               Skel                  => Skel,
               Impl                  => Impl);
         end if;
         Par_Int := Next_Entity (Par_Int);
      end loop;
   end Map_Inherited_Entities_Bodies;

   -----------------------
   -- Already_Inherited --
   -----------------------

   function Already_Inherited
     (Name      : Name_Id)
     return Boolean
   is
      Result : Boolean;
   begin
      if Get_Name_Table_Info (Name) = View_Old_Int_Value then
         Result := True;
      else
         Result := False;
         Set_Name_Table_Info (Name, View_Old_Int_Value);
      end if;
      return Result;
   end Already_Inherited;

   -----------------------------------
   -- Map_Additional_Entities_Specs --
   -----------------------------------

   procedure Map_Additional_Entities_Specs
     (Parent_Interface : Node_Id;
      Child_Interface  : Node_Id;
      Stub             : Boolean := False;
      Helper           : Boolean := False)
   is

      Entity   : Node_Id;
      From_Any : Node_Id;
      To_Any   : Node_Id;

   begin
      --  We do not handle predefined CORBA parents.
      if Present (Map_Predefined_CORBA_Entity (Parent_Interface)) then
         return;
      end if;

      Entity := First_Entity (Interface_Body (Parent_Interface));
      while Present (Entity) loop
         case  FEN.Kind (Entity) is
            when K_Type_Declaration =>
               declare
                  D             : Node_Id;
                  Original_Type : Node_Id;
                  New_Type      : Node_Id;
                  T             : Node_Id;
               begin
                  D := First_Entity (Declarators (Entity));
                  while Present (D) loop
                     if not FEU.Is_Redefined (D, Child_Interface) and then
                       not Already_Inherited
                       (IDL_Name (Identifier (D)))
                     then

                        --  Adding an explaining comment

                        Explaining_Comment
                          (FEN.IDL_Name (Identifier (D)),
                           FEU.Fully_Qualified_Name
                           (Identifier (Parent_Interface),
                            Separator => "."),
                           " : inherited from ");

                        if Stub then

                           --  Subtype declaration

                           Original_Type := Expand_Designator
                             (Type_Def_Node
                              (BE_Node
                               (Identifier
                                (D))));
                           New_Type := Make_Defining_Identifier
                             (To_Ada_Name
                              (IDL_Name
                               (Identifier
                                (D))));
                           T := Make_Full_Type_Declaration
                             (Defining_Identifier    =>
                                New_Type,
                              Type_Definition        =>
                                Make_Derived_Type_Definition
                              (Subtype_Indication    =>
                                 Original_Type,
                               Record_Extension_Part =>
                                 No_Node,
                               Is_Subtype => True),
                              Is_Subtype => True);
                           Set_Corresponding_Node (New_Type, T);
                           Append_Node_To_List
                             (T,
                              Visible_Part (Current_Package));
                        end if;
                        if Helper then
                           Map_Any_Converters
                             (To_Ada_Name
                              (IDL_Name
                               (Identifier
                                (D))),
                              From_Any,
                              To_Any);
                           Append_Node_To_List
                             (From_Any,
                              Visible_Part (Current_Package));
                           Append_Node_To_List
                             (To_Any,
                              Visible_Part (Current_Package));
                        end if;
                     end if;
                     D := Next_Entity (D);
                  end loop;
               end;

            when K_Structure_Type
              | K_Union_Type
              | K_Enumeration_Type =>
               if not FEU.Is_Redefined (Entity, Child_Interface)  and then
                 not Already_Inherited
                 (IDL_Name (Identifier (Entity)))
               then
                  declare
                     Original_Type : Node_Id;
                     New_Type      : Node_Id;
                     T             : Node_Id;
                  begin

                     --  Adding an explaining comment

                     Explaining_Comment
                       (FEN.IDL_Name (Identifier (Entity)),
                        FEU.Fully_Qualified_Name
                        (Identifier (Parent_Interface),
                         Separator => "."),
                        " : inherited from ");

                     if Stub then

                        --  Subtype declaration

                        Original_Type := Expand_Designator
                          (Type_Def_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        New_Type := Make_Defining_Identifier
                          (To_Ada_Name
                           (IDL_Name
                            (Identifier
                             (Entity))));
                        T := Make_Full_Type_Declaration
                          (Defining_Identifier    =>
                             New_Type,
                           Type_Definition        =>
                             Make_Derived_Type_Definition
                           (Subtype_Indication    =>
                              Original_Type,
                            Record_Extension_Part =>
                              No_Node,
                            Is_Subtype => True),
                           Is_Subtype => True);
                        Set_Corresponding_Node (New_Type, T);
                        Append_Node_To_List
                          (T,
                           Visible_Part (Current_Package));
                     end if;
                     if Helper then
                        Map_Any_Converters
                          (To_Ada_Name
                           (IDL_Name
                            (Identifier
                             (Entity))),
                           From_Any,
                           To_Any);
                        Append_Node_To_List
                          (From_Any,
                           Visible_Part (Current_Package));
                        Append_Node_To_List
                          (To_Any,
                           Visible_Part (Current_Package));
                     end if;
                  end;
               end if;
            when K_Constant_Declaration =>
               if not FEU.Is_Redefined (Entity, Child_Interface) and then
                 not Already_Inherited
                 (IDL_Name (Identifier (Entity)))
               then
                  declare
                     Original_Constant : Node_Id;
                     New_Constant      : Node_Id;
                     C                 : Node_Id;
                  begin
                     if Stub then

                        --  Adding an explaining comment

                        Explaining_Comment
                          (FEN.IDL_Name (Identifier (Entity)),
                           FEU.Fully_Qualified_Name
                           (Identifier (Parent_Interface),
                            Separator => "."),
                           " : inherited from ");

                        --  Generate a "renamed" variable.

                        Original_Constant := Expand_Designator
                          (Stub_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        New_Constant := Make_Defining_Identifier
                          (To_Ada_Name
                           (FEN.IDL_Name
                            (Identifier
                             (Entity))));
                        C := Make_Object_Declaration
                          (Defining_Identifier =>
                             New_Constant,
                           Constant_Present    =>
                             False, --  Yes, False
                           Object_Definition   =>
                             Map_Designator (Type_Spec (Entity)),
                           Renamed_Object      =>
                             Original_Constant);
                        Append_Node_To_List
                          (C,
                           Visible_Part (Current_Package));
                     end if;
                  end;
               end if;

            when K_Exception_Declaration =>
               if not FEU.Is_Redefined (Entity, Child_Interface) and then
                 not Already_Inherited
                 (IDL_Name (Identifier (Entity)))
               then
                  declare
                     Original_Exception : Node_Id;
                     New_Exception      : Node_Id;
                     C                  : Node_Id;
                     Original_Type      : Node_Id;
                     New_Type           : Node_Id;
                     T                  : Node_Id;
                     N                  : Node_Id;
                  begin
                     --  Adding an explaining comment

                     Explaining_Comment
                       (FEN.IDL_Name (Identifier (Entity)),
                        FEU.Fully_Qualified_Name
                        (Identifier (Parent_Interface),
                         Separator => "."),
                        " : inherited from ");

                     if Stub then

                        --  Generate a "renamed" exception

                        Original_Exception := Expand_Designator
                          (Stub_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        New_Exception := Make_Defining_Identifier
                          (To_Ada_Name
                           (FEN.IDL_Name
                            (Identifier
                             (Entity))));
                        C := Make_Exception_Declaration
                          (Defining_Identifier =>
                             New_Exception,
                           Renamed_Exception   =>
                             Original_Exception);
                        Append_Node_To_List
                          (C,
                           Visible_Part (Current_Package));

                        --  Generate the "_Members" subtype

                        Original_Type := Expand_Designator
                          (Type_Def_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        New_Type := Make_Defining_Identifier
                          (BEN.Name
                           (Defining_Identifier
                            (Original_Type)));
                        T := Make_Full_Type_Declaration
                          (Defining_Identifier    =>
                             New_Type,
                           Type_Definition        =>
                             Make_Derived_Type_Definition
                           (Subtype_Indication    =>
                              Original_Type,
                            Record_Extension_Part =>
                              No_Node,
                            Is_Subtype => True),
                           Is_Subtype => True);
                        Set_Corresponding_Node (New_Type, T);
                        Append_Node_To_List
                          (T,
                           Visible_Part (Current_Package));

                        --  Generate the Get_Members procedure spec

                        N := Map_Get_Members_Spec (Expand_Designator (T));

                        Append_Node_To_List
                          (N, Visible_Part (Current_Package));
                     end if;
                     if Helper then
                        Map_Any_Converters
                          (BEN.Name
                           (Defining_Identifier
                            (Type_Def_Node
                             (BE_Node
                              (Identifier
                               (Entity))))),
                           From_Any,
                           To_Any);
                        Append_Node_To_List
                          (From_Any,
                           Visible_Part (Current_Package));
                        Append_Node_To_List
                          (To_Any,
                           Visible_Part (Current_Package));
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
         Entity := Next_Entity (Entity);
      end loop;
   end Map_Additional_Entities_Specs;

   ------------------------------------
   -- Map_Additional_Entities_Bodies --
   ------------------------------------

   procedure Map_Additional_Entities_Bodies
     (Parent_Interface : Node_Id;
      Child_Interface  : Node_Id;
      Stub             : Boolean := False;
      Helper           : Boolean := False)
   is
      Entity   : Node_Id;
      From_Any : Node_Id;
      To_Any   : Node_Id;
   begin
      --  We do not handle predefined CORBA parents.
      if Present (Map_Predefined_CORBA_Entity (Parent_Interface)) then
         return;
      end if;

      Entity := First_Entity (Interface_Body (Parent_Interface));
      while Present (Entity) loop
         case  FEN.Kind (Entity) is
            when K_Type_Declaration =>
               declare
                  D             : Node_Id;
               begin
                  if Helper then
                     D := First_Entity (Declarators (Entity));
                     while Present (D) loop
                        if not FEU.Is_Redefined (D, Child_Interface) and then
                          not Already_Inherited
                          (IDL_Name (Identifier (D)))
                        then
                           Map_Any_Converters
                             (To_Ada_Name
                              (IDL_Name
                               (Identifier
                                (D))),
                              From_Any,
                              To_Any);
                           Set_Renamed_Entity
                             (From_Any,
                              Expand_Designator
                              (From_Any_Node
                               (BE_Node
                                (Identifier
                                 (D)))));
                           Set_Renamed_Entity
                             (To_Any,
                              Expand_Designator
                              (To_Any_Node
                               (BE_Node
                                (Identifier
                                 (D)))));
                           Append_Node_To_List
                             (From_Any,
                              Statements (Current_Package));
                           Append_Node_To_List
                             (To_Any,
                              Statements (Current_Package));
                        end if;
                        D := Next_Entity (D);
                     end loop;
                  end if;
               end;

            when K_Structure_Type
              | K_Union_Type
              | K_Enumeration_Type =>
               if not FEU.Is_Redefined (Entity, Child_Interface) and then
                 not Already_Inherited
                 (IDL_Name (Identifier (Entity)))
               then
                  begin
                     if Helper then
                        Map_Any_Converters
                          (To_Ada_Name
                           (IDL_Name
                            (Identifier
                             (Entity))),
                           From_Any,
                           To_Any);
                        Set_Renamed_Entity
                          (From_Any,
                           Expand_Designator
                           (From_Any_Node
                            (BE_Node
                             (Identifier
                              (Entity)))));
                        Set_Renamed_Entity
                          (To_Any,
                           Expand_Designator
                           (To_Any_Node
                            (BE_Node
                             (Identifier
                              (Entity)))));
                        Append_Node_To_List
                          (From_Any,
                           Statements (Current_Package));
                        Append_Node_To_List
                          (To_Any,
                           Statements (Current_Package));
                     end if;
                  end;
               end if;

            when K_Exception_Declaration =>
               if not FEU.Is_Redefined (Entity, Child_Interface) and then
                 not Already_Inherited
                 (IDL_Name (Identifier (Entity)))
               then
                  declare
                     Original_Get_Members : Node_Id;
                     New_Member_Type      : Node_Id;
                     N                    : Node_Id;
                  begin
                     if Stub then

                        --  generate the renamed Get_Members

                        New_Member_Type :=
                          Expand_Designator
                          (Type_Def_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        Set_Correct_Parent_Unit_Name
                          (New_Member_Type,
                           Expand_Designator
                           (Main_Package
                            (Current_Entity)));
                        N := Map_Get_Members_Spec (New_Member_Type);

                        Original_Get_Members := Defining_Identifier
                          (Map_Get_Members_Spec
                           (Expand_Designator
                            (Type_Def_Node
                             (BE_Node
                              (Identifier
                               (Entity))))));

                        --  Setting the right parent unit name

                        Set_Correct_Parent_Unit_Name
                          (Original_Get_Members,
                           Expand_Designator
                           (BEN.Parent
                            (Type_Def_Node
                             (BE_Node
                              (Identifier
                               (Parent_Interface))))));
                        Set_Renamed_Entity (N, Original_Get_Members);
                        Append_Node_To_List
                          (N, Statements (Current_Package));
                     end if;
                     if Helper then
                        Map_Any_Converters
                          (BEN.Name
                           (Defining_Identifier
                            (Type_Def_Node
                             (BE_Node
                              (Identifier
                               (Entity))))),
                           From_Any,
                           To_Any);
                        Set_Renamed_Entity
                          (From_Any,
                           Expand_Designator
                           (From_Any_Node
                            (BE_Node
                             (Identifier
                              (Entity)))));
                        Set_Renamed_Entity
                          (To_Any,
                           Expand_Designator
                           (To_Any_Node
                            (BE_Node
                             (Identifier
                              (Entity)))));
                        Append_Node_To_List
                          (From_Any,
                           Statements (Current_Package));
                        Append_Node_To_List
                          (To_Any,
                           Statements (Current_Package));
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
         Entity := Next_Entity (Entity);
      end loop;
   end Map_Additional_Entities_Bodies;

   --  SII related subprograms bodies

   -----------------------------
   -- Map_Arg_Type_Identifier --
   -----------------------------

   function Map_Args_Type_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Args_Type");
      return Make_Defining_Identifier (Name_Find);
   end Map_Args_Type_Identifier;

   -------------------------
   -- Map_Args_Identifier --
   -------------------------

   function Map_Args_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Args");
      return Make_Defining_Identifier (Name_Find);
   end Map_Args_Identifier;

   -------------------------------
   -- Map_Marshaller_Identifier --
   -------------------------------

   function Map_Marshaller_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Marshaller");
      return Make_Defining_Identifier (Name_Find);
   end Map_Marshaller_Identifier;

   ---------------------------------
   -- Map_Unmarshaller_Identifier --
   ---------------------------------

   function Map_Unmarshaller_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Unmarshaller");
      return Make_Defining_Identifier (Name_Find);
   end Map_Unmarshaller_Identifier;

   -----------------------------
   -- Map_Set_Args_Identifier --
   -----------------------------

   function Map_Set_Args_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Set_Args");
      return Make_Defining_Identifier (Name_Find);
   end Map_Set_Args_Identifier;

end Backend.BE_Ada.IDL_To_Ada;
