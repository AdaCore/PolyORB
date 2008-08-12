------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      B A C K E N D . B E _ C O R B A _ A D A . I D L _ T O _ A D A       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;     use Namet;
with Values;    use Values;
with Locations; use Locations;
with Charset;   use Charset;
with Utils;     use Utils;

with Frontend.Nodes;   use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;

package body Backend.BE_CORBA_Ada.IDL_To_Ada is

   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;
   package FEU renames Frontend.Nutils;

   --  The entities below are used to avoid name collision when
   --  creating instantiated sequence packages

   function Get_Scope_Internal_Name
     (E_Name : Name_Id;
      E      : Node_Id)
     return Name_Id;
   --  Return an internal name useful for the `New_Index' routine

   function New_Index (E_Name : Name_Id; E : Node_Id) return Nat;
   --  Get a new index inside the scope E to resolve clashing on name
   --  E_Name.

   --  The 3 subprogram below handle the mapping of entity
   --  names for Sequence and Bounded String types

   function Get_Mapped_Entity_Name (T : Node_Id) return Name_Id;
   --  If the node T has already been mapped, return the mapped
   --  name

   procedure Link_Mapped_Entity_Name (E_Name : Name_Id; T : Node_Id);
   --  Makes a link between E_Name and T

   function Get_Internal_Name (T : Node_Id) return Name_Id;
   --  Returns a conventional Name_Id useful for the two subprogram
   --  above

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id;
   --  Map an Ada designator from Type_Decl according to the Ada
   --  mapping rules.

   ----------------------------
   -- Get_Mapped_Entity_Name --
   ----------------------------

   function Get_Mapped_Entity_Name (T : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (T) = K_Fixed_Point_Type   or else
                     FEN.Kind (T) = K_Sequence_Type      or else
                     FEN.Kind (T) = K_String_Type        or else
                     FEN.Kind (T) = K_Wide_String_Type);

      Internal_Name : constant Name_Id := Get_Internal_Name (T);
      Info          : constant Nat := Get_Name_Table_Info (Internal_Name);
   begin
      if Info /= 0 then
         return BEN.Name (Node_Id (Info));
      end if;

      return No_Name;
   end Get_Mapped_Entity_Name;

   -----------------------------
   -- Link_Mapped_Entity_Name --
   -----------------------------

   procedure Link_Mapped_Entity_Name
     (E_Name : Name_Id; T : Node_Id)
   is
      pragma Assert (FEN.Kind (T) = K_Fixed_Point_Type   or else
                     FEN.Kind (T) = K_Sequence_Type      or else
                     FEN.Kind (T) = K_String_Type        or else
                     FEN.Kind (T) = K_Wide_String_Type);

      Internal_Name : constant Name_Id := Get_Internal_Name (T);
      Info          : constant Node_Id
        := Make_Defining_Identifier (E_Name);
   begin
      Set_Name_Table_Info (Internal_Name, Int (Info));
   end Link_Mapped_Entity_Name;

   -----------------------
   -- Get_Internal_Name --
   -----------------------

   function Get_Internal_Name (T : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("Mapped_Entity%");
      Add_Nat_To_Name_Buffer (Nat (T));
      return Name_Find;
   end Get_Internal_Name;

   -----------------------------
   -- Get_Scope_Internal_Name --
   -----------------------------

   function Get_Scope_Internal_Name
     (E_Name : Name_Id;
      E      : Node_Id)
     return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("Scope%");
      Get_Name_String_And_Append (E_Name);
      Add_Char_To_Name_Buffer ('%');
      Add_Nat_To_Name_Buffer (Nat (E));
      Add_Char_To_Name_Buffer ('%');
      return Name_Find;
   end Get_Scope_Internal_Name;

   ---------------
   -- New_Index --
   ---------------

   function New_Index (E_Name : Name_Id; E : Node_Id) return Nat is
      Internal_Name : constant Name_Id := Get_Scope_Internal_Name (E_Name, E);
      Index         : Nat := Get_Name_Table_Info (Internal_Name);
   begin
      Index := Index + 1;
      Set_Name_Table_Info (Internal_Name, Index);
      return Index;
   end New_Index;

   ------------------
   -- Base_Type_TC --
   ------------------

   function Base_Type_TC (K : FEN.Node_Kind) return Node_Id is
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
            raise Program_Error with "Not a base type: "
              & FEN.Node_Kind'Image (K);
      end case;
   end Base_Type_TC;

   -------------------
   -- Bind_FE_To_BE --
   -------------------

   procedure Bind_FE_To_BE (F : Node_Id; B : Node_Id; W : Binding) is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      case W is
         when B_Impl =>
            Set_Impl_Node (N, B);

         when B_Stub =>
            Set_Stub_Node (N, B);

         when B_TC =>
            Set_TC_Node (N, B);

         when B_From_Any_Container =>
            Set_From_Any_Container_Node (N, B);

         when B_From_Any =>
            Set_From_Any_Node (N, B);

         when B_To_Any =>
            Set_To_Any_Node (N, B);

         when B_Raise_Excp =>
            Set_Raise_Excp_Node (N, B);

         when B_Initialize =>
            Set_Initialize_Node (N, B);

         when B_To_Ref =>
            Set_To_Ref_Node (N, B);

         when B_U_To_Ref =>
            Set_U_To_Ref_Node (N, B);

         when B_Type_Def =>
            Set_Type_Def_Node (N, B);

         when B_Forward =>
            Set_Forward_Node (N, B);

         when B_Unmarshaller =>
            Set_Unmarshaller_Node (N, B);

         when B_Marshaller =>
            Set_Marshaller_Node (N, B);

         when B_Buffer_Size =>
            Set_Buffer_Size_Node (N, B);

         when B_Instantiation =>
            Set_Instantiation_Node (N, B);

         when B_Pointer_Type =>
            Set_Pointer_Type_Node (N, B);

         when B_Aggr_Container =>
            Set_Aggr_Container_Node (N, B);

         when B_Clone =>
            Set_Clone_Node (N, B);

         when B_Finalize_Value =>
            Set_Finalize_Value_Node (N, B);

         when B_Get_Aggregate_Count =>
            Set_Get_Aggregate_Count_Node (N, B);

         when B_Set_Aggregate_Count =>
            Set_Set_Aggregate_Count_Node (N, B);

         when B_Get_Aggregate_Element =>
            Set_Get_Aggregate_Element_Node (N, B);

         when B_Set_Aggregate_Element =>
            Set_Set_Aggregate_Element_Node (N, B);

         when B_Unchecked_Get_V =>
            Set_Unchecked_Get_V_Node (N, B);

         when B_Wrap =>
            Set_Wrap_Node (N, B);

         when B_Element_Wrap =>
            Set_Element_Wrap_Node (N, B);

         when B_Args_In =>
            Set_Args_In_Node (N, B);

         when B_Args_Out =>
            Set_Args_Out_Node (N, B);

         when B_Access_Args_Out =>
            Set_Access_Args_Out_Node (N, B);

         when B_IR_Function =>
            Set_Ir_Function_Node (N, B);

         when B_Register_IR_Info =>
            Set_Register_Ir_Info_Node (N, B);
      end case;

      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_BE;

   ------------------
   -- Is_Base_Type --
   ------------------

   function Is_Base_Type (N : Node_Id) return Boolean is
   begin
      return FEN.Kind (N) in FEN.K_Float .. FEN.K_Value_Base;
   end Is_Base_Type;

   --------------------
   -- Is_Object_Type --
   --------------------

   function Is_Object_Type (E : Node_Id) return Boolean is
   begin
      --  Object types are basically, the CORBA::Object type,
      --  interface declaration and forward interface declarations.

      if FEN.Kind (E) = K_Object
        or else FEN.Kind (E) = K_Interface_Declaration
        or else FEN.Kind (E) = K_Forward_Interface_Declaration
      then
         return True;
      end if;

      --  If a type is defined basing on the 3 types listed above,
      --  then it must be a scoped name.

      if FEN.Kind (E) /= K_Scoped_Name then
         return False;
      end if;

      if FEN.Kind (Reference (E)) = K_Interface_Declaration
        or else FEN.Kind (Reference (E)) = K_Forward_Interface_Declaration
      then
         return True;
      end if;

      --  Handle multi level type definition recursively. Only simple
      --  declarators are considered as object types. Arrays of
      --  abjects are NOT objects.

      if FEN.Kind (Reference (E)) = K_Simple_Declarator then
         return Is_Object_Type (Type_Spec (Declaration (Reference (E))));
      end if;

      return False;
   end Is_Object_Type;

   ----------------------
   -- Is_N_Parent_Of_M --
   ----------------------

   function Is_N_Parent_Of_M (N : Node_Id; M : Node_Id) return Boolean is
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

   ------------------------
   -- Map_Container_Name --
   ------------------------

   function Map_Container_Name (E : Node_Id) return Name_Id is
      The_Name : constant Name_Id := To_Ada_Name (IDL_Name (Identifier (E)));
   begin
      Set_Str_To_Name_Buffer ("Content_Ü_");
      Get_Name_String_And_Append (The_Name);
      return Name_Find;
   end Map_Container_Name;

   ------------------------------------
   -- Map_Declarator_Type_Designator --
   ------------------------------------

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id
   is
      Designator : Node_Id;
   begin
      Designator := Map_Expanded_Name (Type_Decl);

      --  The expansion phase ensures there cannot be complex
      --  declarators inside structures, exceptions and unions.

      if Kind (Declarator) = K_Complex_Declarator then
         raise Program_Error with "Complex declarators in structures, "
           & "unions and exceptions have to be expanded";
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

      Result := Make_Defining_Identifier (To_Ada_Name (IDL_Name (I)));

      if Present (BE_Node (I))
        and then Present (Stub_Node (BE_Node (I)))
        and then BEN.Kind (Stub_Node (BE_Node (I))) = K_IDL_Unit
      then
         Set_Declaration_Node
           (Result, Main_Package (Stub_Node (BE_Node (I))));
      end if;

      return Result;
   end Map_Defining_Identifier;

   -----------------------
   -- Map_Expanded_Name --
   -----------------------

   function Map_Expanded_Name (Entity : Node_Id) return Node_Id is
      P : Node_Id;
      N : Node_Id;
      K : FEN.Node_Kind;
      R : Node_Id;
   begin
      K := FEN.Kind (Entity);

      if K = FEN.K_Scoped_Name then
         R := Reference (Entity);

         --  The routine below verifies whether the scoped name
         --  designs a CORBA Entity declared in orb.idl, in which
         --  case, it returns the corresponding runtime entity.

         N := Map_Predefined_CORBA_Entity (Entity);

         if Present (N) then
            return N;
         end if;

         if Kind (R) = K_Specification then
            return No_Node;
         end if;

         --  Handling the case where R is not a base type nor a user
         --  defined type but an interface type. In this case we do
         --  not return the identifier of the interface name but the
         --  identifier to the Ref type defined in the stub package
         --  relative to the interface.

         if Kind (R) = FEN.K_Interface_Declaration then
            --  Get the node of the Ref type declaration.

            N := Copy_Node
              (Defining_Identifier
               (Type_Def_Node
                (BE_Node
                 (Identifier
                  (R)))));
            Set_FE_Node (N, R);
            P := R;
         elsif Kind (R) = FEN.K_Forward_Interface_Declaration then
            --  Get the node of the Ref type declaration.

            N := Make_Selected_Component
              (Expand_Designator
               (Instantiation_Node
                (BE_Node
                 (Identifier
                  (R)))),
               Copy_Node
               (Defining_Identifier
                (Type_Def_Node
                 (BE_Node
                  (Identifier (R))))));
            Set_FE_Node (N, R);

            P := No_Node;
         else
            N := Map_Defining_Identifier (R);
            Set_FE_Node (N, R);
            P := Scope_Entity (Identifier (R));
         end if;

         if Present (P) then
            if Kind (P) = K_Specification then
               N := Make_Selected_Component
                 (Defining_Identifier
                  (Main_Package
                   (Stub_Node
                    (BE_Node
                     (Identifier
                      (P))))),
                  N);
            else
               N := Make_Selected_Component (Map_Expanded_Name (P), N);
            end if;
         end if;
      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         N := RE (Convert (K));
         Set_FE_Node (N, Entity);

      else
         N := Map_Defining_Identifier (Entity);

         if K = FEN.K_Interface_Declaration
           or else K = FEN.K_Module
         then
            P := Scope_Entity (Identifier (Entity));
            Set_FE_Node (N, Entity);

            N := Make_Selected_Component (Map_Expanded_Name (P), N);

         elsif K = FEN.K_Specification then
            return No_Node;
         end if;
      end if;

      P := Get_Parent_Unit_Name (N);

      if Present (P) then
         Add_With_Package (P);
      end if;

      return N;
   end Map_Expanded_Name;

   -------------------------
   -- Map_Fixed_Type_Name --
   -------------------------

   function Map_Fixed_Type_Name (F : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (F) = K_Fixed_Point_Type);

      F_Name : Name_Id;
      Info   : Nat;
      Index  : Nat;
   begin
      --  First of all, see whether we have already mapped the fixed
      --  point type F.

      F_Name := Get_Mapped_Entity_Name (F);

      if F_Name /= No_Name then
         return F_Name;
      end if;

      --  It's the first time we try to map the fixed point type F.

      Set_Str_To_Name_Buffer ("Fixed_");
      Add_Nat_To_Name_Buffer (Nat (N_Total (F)));
      Add_Char_To_Name_Buffer ('_');
      Add_Nat_To_Name_Buffer (Nat (N_Scale (F)));

      --  Now the sequence type name is almost built...

      F_Name := Name_Find;

      --  ... However we must resolve the conflicts that may occur
      --  with other fixed point type names

      Info := Get_Name_Table_Info (F_Name);

      if Info = Int (Main_Package (Current_Entity)) then
         Index := New_Index (F_Name, Main_Package (Current_Entity));
         Get_Name_String (F_Name);
         Add_Char_To_Name_Buffer ('_');
         Add_Nat_To_Name_Buffer (Index);
         F_Name := Name_Find;
      end if;

      Set_Name_Table_Info (F_Name, Int (Main_Package (Current_Entity)));

      --  Finally, we link F and F_Name

      Link_Mapped_Entity_Name (F_Name, F);

      return F_Name;
   end Map_Fixed_Type_Name;

   --------------------------------
   -- Map_Fixed_Type_Helper_Name --
   --------------------------------

   function Map_Fixed_Type_Helper_Name (F : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (F) = K_Fixed_Point_Type);

      Type_Name : constant Name_Id := Map_Fixed_Type_Name (F);
   begin
      Set_Str_To_Name_Buffer ("Helper_Ü_");
      Get_Name_String_And_Append (Type_Name);
      return Name_Find;
   end Map_Fixed_Type_Helper_Name;

   ------------------------------------
   -- Map_Fully_Qualified_Identifier --
   ------------------------------------

   function Map_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id
   is
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

         N := Make_Selected_Component
           (Map_Fully_Qualified_Identifier (P),
            N);
      end if;

      return N;
   end Map_Fully_Qualified_Identifier;

   --------------------------
   -- Map_Get_Members_Spec --
   --------------------------

   function Map_Get_Members_Spec (Member_Type : Node_Id) return Node_Id is
      Profile   : List_Id;
      Parameter : Node_Id;
      N         : Node_Id;
   begin
      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_From)),
         RE (RE_Exception_Occurrence));
      Append_To (Profile, Parameter);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_To)),
         Member_Type,
         Mode_Out);
      Append_To (Profile, Parameter);

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
      M : Node_Id;
      D : Node_Id;
      L : List_Id;
      I : Node_Id;
      Z : Node_Id;
      F : Node_Id;
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

      --  The main package is appended to the list (in order for the
      --  code to be generated) only if the user did not request to
      --  disable it.

      if not Disable_Client_Code_Gen then
         Append_To (L, M);
      end if;

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Selected_Component
        (I,
         Make_Defining_Identifier (Name_Find));
      D := Make_Package_Declaration (N);
      Set_IDL_Unit (D, P);
      Set_Parent (D, M);
      Set_Helper_Package (P, D);

      if not Disable_Client_Code_Gen then
         Append_To (L, D);
      end if;

      --  Internals package

      Set_Str_To_Name_Buffer ("Internals");
      N := Make_Selected_Component
        (Copy_Node (Defining_Identifier (D)),
         Make_Defining_Identifier (Name_Find));
      Z := Make_Package_Declaration (N);
      Set_IDL_Unit (Z, P);
      Set_Parent (Z, D);

      --  Internals is a subunit of Helper

      Set_Is_Subunit_Package (Package_Specification (Z), True);

      Append_To (Subunits (Package_Specification (D)),
        Package_Specification (Z));

      Append_To (Statements (Package_Implementation (D)),
        Package_Implementation (Z));

      --  As a subunit of Helper, Interals shares its list of withed
      --  packages with Helper.

      Set_Withed_Packages (Package_Specification (Z),
                           Withed_Packages (Package_Specification (D)));

      Set_Withed_Packages (Package_Implementation (Z),
                           Withed_Packages (Package_Implementation (D)));

      Set_Internals_Package (P, Z);

      --  Interface repository information package

      if IR_Info_Packages_Gen then
         Set_Str_To_Name_Buffer ("IR_Info");
         N := Make_Selected_Component
           (I,
            Make_Defining_Identifier (Name_Find));
         F := Make_Package_Declaration (N);
         Set_IDL_Unit (F, P);
         Set_Parent (F, M);
         Set_Ir_Info_Package (P, F);
         Append_To (L, F);
      end if;

      if Kind (Entity) = K_Interface_Declaration then

         if not FEN.Is_Abstract_Interface (Entity) then

            --  No CDR, Skel or Impl packages are generated for abstract
            --  interfaces.

            if not FEN.Is_Local_Interface (Entity) then

               --  No CDR or Skel packages are generated for local interfaces

               --  Skeleton package

               Set_Str_To_Name_Buffer ("Skel");
               N := Make_Selected_Component
                 (I,
                  Make_Defining_Identifier (Name_Find));
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_Skeleton_Package (P, D);
               Append_To (L, D);

               --  CDR package

               Set_Str_To_Name_Buffer ("CDR");
               N := Make_Selected_Component
                 (I,
                  Make_Defining_Identifier (Name_Find));
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_CDR_Package (P, D);

               if not Disable_Client_Code_Gen then
                  Append_To (L, D);
               end if;

               --  Aligned package

               Set_Str_To_Name_Buffer ("Aligned");
               N := Make_Selected_Component
                 (I,
                  Make_Defining_Identifier (Name_Find));
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_Aligned_Package (P, D);

               if not Disable_Client_Code_Gen then
                  Append_To (L, D);
               end if;

               --  Buffers package

               Set_Str_To_Name_Buffer ("Buffers");
               N := Make_Selected_Component
                 (I,
                  Make_Defining_Identifier (Name_Find));
               D := Make_Package_Declaration (N);
               Set_IDL_Unit (D, P);
               Set_Parent (D, M);
               Set_Buffers_Package (P, D);

               if not Disable_Client_Code_Gen then
                  Append_To (L, D);
               end if;
            end if;

            --  Implementation package

            Set_Str_To_Name_Buffer ("Impl");
            N := Make_Selected_Component
              (I,
               Make_Defining_Identifier (Name_Find));
            D := Make_Package_Declaration (N);
            Set_IDL_Unit (D, P);
            Set_Parent (D, M);
            Set_Implementation_Package (P, D);

            if Impl_Packages_Gen then
               Append_To (L, D);
            end if;
         end if;
      end if;

      return P;
   end Map_IDL_Unit;

   ----------------------------
   -- Map_Impl_Type_Ancestor --
   ----------------------------

   function Map_Impl_Type_Ancestor (Entity : Node_Id) return Node_Id is
      pragma Assert
        (FEN.Kind (Entity) = K_Interface_Declaration or else
         FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ancestor : Node_Id;
   begin
      if Is_Local_Interface (Entity) then
         Ancestor := RE (RE_Object_2);
      else
         Ancestor := RE (RE_Servant_Base);
      end if;

      return Ancestor;
   end Map_Impl_Type_Ancestor;

   ----------------------
   -- Map_Indices_Name --
   ----------------------

   function Map_Indices_Name (D : Node_Id) return Name_Id is
      The_Name : constant Name_Id := To_Ada_Name (IDL_Name (Identifier (D)));
   begin
      Set_Str_To_Name_Buffer ("Indices_Ü_");
      Get_Name_String_And_Append (The_Name);
      return Name_Find;
   end Map_Indices_Name;

   ----------------------
   -- Map_Lengths_Name --
   ----------------------

   function Map_Lengths_Name (D : Node_Id) return Name_Id is
      The_Name : constant Name_Id := To_Ada_Name (IDL_Name (Identifier (D)));
   begin
      Set_Str_To_Name_Buffer ("Lengths_Ü_");
      Get_Name_String_And_Append (The_Name);
      return Name_Find;
   end Map_Lengths_Name;

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
            Bind_FE_To_BE (Identifier (Declarator),
                           Component_Declaration,
                           B_Stub);
            Append_To (Components, Component_Declaration);
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
   -- Map_Pointer_Type_Name --
   ---------------------------

   function Map_Pointer_Type_Name (E : Node_Id) return Name_Id is
      Type_Name : constant Name_Id := To_Ada_Name (IDL_Name (Identifier (E)));
   begin
      Set_Str_To_Name_Buffer ("Ptr_Ü_");
      Get_Name_String_And_Append (Type_Name);

      return Name_Find;
   end Map_Pointer_Type_Name;

   -----------------
   -- Map_IR_Name --
   -----------------

   function Map_IR_Name (E : Node_Id) return Name_Id is
      Prefix : constant String  := "IR_";
      Name   : constant Name_Id := To_Ada_Name (IDL_Name (Identifier (E)));
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Map_IR_Name;

   ------------------------
   -- Map_Cached_IR_Name --
   ------------------------

   function Map_Cached_IR_Name (E : Node_Id) return Name_Id is
      Prefix : constant String  := "Cached_";
      Name   : constant Name_Id := Map_IR_Name (E);
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Map_Cached_IR_Name;

   ---------------------------
   -- Map_Range_Constraints --
   ---------------------------

   function Map_Range_Constraints (Array_Sizes : List_Id) return List_Id is
      L : List_Id;
      S : Node_Id;
      R : Node_Id;
      V : Value_Type;

   begin
      L := New_List (K_Range_Constraints);
      S := FEN.First_Entity (Array_Sizes);

      while Present (S) loop

         --  If the array size is a previously declared constant (concretely, a
         --  scoped name), then use the value of that constant.

         if FEN.Kind (S) = K_Scoped_Name then
            V := Value (FEN.Value (Reference (S)));

         --  Otherwise, it's some other constant expression; use the
         --  expression's value.

         else
            V := Value (FEN.Value (S));
         end if;

         --  Subtract 1 for zero-based arrays

         V.IVal := V.IVal - 1;

         R := Make_Range_Constraint
           (Make_Literal (Int0_Val),
            Make_Literal (New_Value (V)));
         Append_To (L, R);
         S := FEN.Next_Entity (S);
      end loop;

      return L;
   end Map_Range_Constraints;

   ------------------
   -- Map_Ref_Type --
   ------------------

   function Map_Ref_Type (Entity : Node_Id) return Node_Id is
      pragma Assert (FEN.Kind (Entity) = K_Interface_Declaration or else
                     FEN.Kind (Entity) = K_Forward_Interface_Declaration);

      Ref_Type : Node_Id;
   begin
      if Is_Abstract_Interface (Entity) then
         Ref_Type := Make_Defining_Identifier (TN (T_Abstract_Ref));
      elsif Is_Local_Interface (Entity)
        and then FEN.Kind (Entity) = K_Interface_Declaration
      then
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
        (FEN.Kind (Entity) = K_Interface_Declaration or else
         FEN.Kind (Entity) = K_Forward_Interface_Declaration);
      Ancestor : Node_Id;
   begin
      if Is_Abstract_Interface (Entity) then

         --  The abstract interfaces should inherit from
         --  CORBA.AbstractBase.Ref to allow passing interfaces and
         --  ValueTypes.

         --  Since the code generation for ValueType is not performed
         --  by IAC, it is useless (for now) to make the abstract
         --  interfaces inherit from CORBA.AbstractBase.Ref and it
         --  causes problems when compiling current generated code.

         --  XXX : To be replaced by RE_Ref_1 when the ValueTypes are
         --  implemented.
         Ancestor := RE (RE_Ref_2, Withed);
      else
         Ancestor := RE (RE_Ref_2, Withed);
      end if;

      return Ancestor;
   end Map_Ref_Type_Ancestor;

   -----------------------------------
   -- Map_Repository_Id_Declaration --
   -----------------------------------

   function Map_Repository_Id_Declaration (Entity : Node_Id) return Node_Id is

      procedure Fetch_Prefix
        (Entity     : Node_Id;
         Parent     : Node_Id;
         Prefix     : out Name_Id;
         Has_Prefix : out Boolean);

      procedure Get_Repository_Id_String
        (Entity                : Node_Id;
         First_Recursion_Level : Boolean := True;
         Found_Prefix          : Boolean := False);

      ------------------
      -- Fetch_Prefix --
      ------------------

      procedure Fetch_Prefix
        (Entity     : Node_Id;
         Parent     : Node_Id;
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

      ------------------------------
      -- Get_Repository_Id_String --
      ------------------------------

      procedure Get_Repository_Id_String
        (Entity                : Node_Id;
         First_Recursion_Level : Boolean := True;
         Found_Prefix          : Boolean := False)
      is
         I          : Node_Id;
         S          : Node_Id;
         Prefix     : Name_Id;
         Has_Prefix : Boolean := Found_Prefix;
         Name       : Name_Id;

      begin

         --  The explicit definition of a type ID disables the effects
         --  of the type prefix and the type version explicit
         --  definitions, the conflicts being checked in the analyze
         --  phase of the frontend.

         if First_Recursion_Level
           and then Present (FEN.Type_Id (Entity))
         then
            Get_Name_String (IDL_Name (FEN.Type_Id (Entity)));
            return;
         end if;

         --  For entity kinds modules, interfaces and valutypes, the
         --  prefix fetching begins from the entity itself. For the
         --  rest of kind, the fetching starts from the parent entity.

         if First_Recursion_Level and then
           (FEN.Kind (Entity) = K_Module or else
            FEN.Kind (Entity) = K_Interface_Declaration or else
            FEN.Kind (Entity) = K_Value_Declaration) and then
           not FEU.Is_Empty (Type_Prefixes (Entity))
         then
            Has_Prefix := True;
            Name := Name_Find; --  Backup
            Prefix := IDL_Name (Last_Entity (Type_Prefixes (Entity)));
            Prefix := Add_Suffix_To_Name ("/", Prefix);
            Name   := Add_Suffix_To_Name (Get_Name_String (Prefix), Name);
            Get_Name_String (Name); --  Restore
            Prefix := No_Name;
         end if;

         I := FEN.Identifier (Entity);

         --  The potential scope is used to determine the entity parent

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
               Get_Repository_Id_String (S, False, Has_Prefix);
               Add_Char_To_Name_Buffer ('/');
            end if;
         end if;

         Get_Name_String_And_Append (FEN.IDL_Name (I));
      end Get_Repository_Id_String;

      I : Name_Id;
      V : Value_Id;
   begin
      I := Map_Repository_Id_Name (Entity);

      --  Building the Repository Id string value

      Set_Str_To_Name_Buffer ("IDL:");
      Get_Repository_Id_String (Entity);

      if No (FEN.Type_Id (Entity)) then
         Add_Char_To_Name_Buffer (':');

         Get_Name_String_And_Append (Map_Type_Version (Entity));
      end if;

      V := New_String_Value (Name_Find, False);
      return Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (I),
         Constant_Present    => True,
         Object_Definition   => RE (RE_String_2),
         Expression          => Make_Literal (V));
   end Map_Repository_Id_Declaration;

   ----------------------------
   -- Map_Repository_Id_Name --
   ----------------------------

   function Map_Repository_Id_Name (Entity : Node_Id) return Name_Id is
   begin
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

      return Name_Find;
   end Map_Repository_Id_Name;

   ----------------------
   -- Map_Type_Version --
   ----------------------

   function Map_Type_Version (Entity : Node_Id) return Name_Id is
      NB     : constant String := Name_Buffer (1 .. Name_Len);
      Result : Name_Id;
   begin
      if Present (Type_Version (Entity)) then
         Result := FEN.IDL_Name (Type_Version (Entity));
      else
         --  Extract from the CORBA 3.0 spec ($10.7.5.3):
         --  "If no version pragma is supplied for a definition, version
         --   1.0 is assumed"

         Set_Str_To_Name_Buffer ("1.0");
         Result := Name_Find;
      end if;

      --  Restore the name buffer

      Set_Str_To_Name_Buffer (NB);

      return Result;
   end Map_Type_Version;

   -----------------------------
   -- Map_Raise_From_Any_Name --
   -----------------------------

   function Map_Raise_From_Any_Name (Entity : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (Entity) = K_Exception_Declaration);

      Spg_Name : Name_Id := To_Ada_Name (IDL_Name (FEN.Identifier (Entity)));
   begin
      Set_Str_To_Name_Buffer ("Raise_");
      Get_Name_String_And_Append (Spg_Name);
      Add_Str_To_Name_Buffer ("_From_Any");
      Spg_Name := Name_Find;
      return Spg_Name;
   end Map_Raise_From_Any_Name;

   ---------------------------
   -- Map_Sequence_Pkg_Name --
   ---------------------------

   function Map_Sequence_Pkg_Name (S : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (S) = K_Sequence_Type);

      Bounded  : constant Boolean := Present (Max_Size (S));
      Elt_Type : constant Node_Id := Type_Spec (S);
      ET_Name  : Name_Id;
      S_Name   : Name_Id;
      R        : Node_Id;
      Info     : Nat;
      Index    : Nat;
   begin
      --  First of all, see whether we have already mapped the
      --  sequence type S.

      S_Name := Get_Mapped_Entity_Name (S);

      if S_Name /= No_Name then
         return S_Name;
      end if;

      --  It's the first time we try to map the sequence type S

      --  Get the full name of the sequence element type

      if Is_Base_Type (Elt_Type) then
         ET_Name := FEN.Image (Base_Type (Elt_Type));
      elsif FEN.Kind (Elt_Type) = K_Scoped_Name then
         R := Reference (Elt_Type);

         if False
           or else FEN.Kind (R) = K_Interface_Declaration
           or else FEN.Kind (R) = K_Forward_Interface_Declaration
           or else FEN.Kind (R) = K_Simple_Declarator
           or else FEN.Kind (R) = K_Complex_Declarator
           or else FEN.Kind (R) = K_Structure_Type
           or else FEN.Kind (R) = K_Union_Type
           or else FEN.Kind (R) = K_Enumeration_Type
         then
            ET_Name := FEU.Fully_Qualified_Name
              (FEN.Identifier (R), Separator => "_");
         else
            raise Program_Error;
         end if;
      else
         raise Program_Error;
      end if;

      --  If the type name consists of two or more words ("unsigned
      --  long"), replace spaces by underscores.

      Get_Name_String (ET_Name);

      for Index in 1 .. Name_Len loop
         if Name_Buffer (Index) = ' ' then
            Name_Buffer (Index) := '_';
         end if;
      end loop;

      ET_Name := Name_Find;

      --  A prefix specified by the CORBA Ada mapping specifications

      Set_Str_To_Name_Buffer ("IDL_SEQUENCE_");

      --  If the sequence is bounded, append the maximal length

      if Bounded then
         Add_Dnat_To_Name_Buffer
           (Dnat (Value (FEN.Value (Max_Size (S))).IVal));
         Add_Char_To_Name_Buffer ('_');
      end if;

      --  Append the element type name

      Get_Name_String_And_Append (ET_Name);

      --  If the sequence type spec is a forwarded entity we append an
      --  indication to the package name.

      if FEN.Kind (Elt_Type) = K_Scoped_Name then
         R := FEN.Reference (Elt_Type);

         if False
           or else FEN.Kind (R) = K_Forward_Interface_Declaration
           or else FEN.Kind (R) = K_Value_Forward_Declaration
           or else FEN.Kind (R) = K_Forward_Structure_Type
           or else FEN.Kind (R) = K_Forward_Union_Type
         then
            Add_Str_To_Name_Buffer ("_Forward");
         end if;
      end if;

      --  Now the sequence type name is almost built...

      S_Name := Name_Find;

      --  ... However we must resolve the conflicts that may occur
      --  with other sequence type names.

      Info := Get_Name_Table_Info (S_Name);

      if Info = Int (Main_Package (Current_Entity)) then
         Index := New_Index (S_Name, Main_Package (Current_Entity));
         Get_Name_String (S_Name);
         Add_Char_To_Name_Buffer ('_');
         Add_Nat_To_Name_Buffer (Index);
         S_Name := Name_Find;
      end if;

      Set_Name_Table_Info (S_Name, Int (Main_Package (Current_Entity)));

      --  Finally, we link S and S_Name

      Link_Mapped_Entity_Name (S_Name, S);

      return S_Name;
   end Map_Sequence_Pkg_Name;

   ----------------------------------
   -- Map_Sequence_Pkg_Helper_Name --
   ----------------------------------

   function Map_Sequence_Pkg_Helper_Name (S : Node_Id) return Name_Id is
      pragma Assert (FEN.Kind (S) = K_Sequence_Type);
   begin
      Get_Name_String (Map_Sequence_Pkg_Name (S));
      Add_Str_To_Name_Buffer ("_Helper");
      return Name_Find;
   end Map_Sequence_Pkg_Helper_Name;

   -------------------------
   -- Map_String_Pkg_Name --
   -------------------------

   function Map_String_Pkg_Name (S : Node_Id) return Name_Id is
      S_Name : Name_Id;
      Info   : Nat;
      Index  : Nat;
   begin
      pragma Assert (FEN.Kind (S) = K_String_Type        or else
                     FEN.Kind (S) = K_Wide_String_Type);
      --  First of all, see whether we have already mapped the string
      --  type S

      S_Name := Get_Mapped_Entity_Name (S);

      if S_Name /= No_Name then
         return S_Name;
      end if;

      --  It's the first time we try to map the string type S

      Set_Str_To_Name_Buffer ("Bounded_");

      --  Wide string types require additional suffix

      if FEN.Kind (S) = K_Wide_String_Type then
         Add_Str_To_Name_Buffer ("Wide_");
      end if;

      Add_Str_To_Name_Buffer ("String_");
      Add_Dnat_To_Name_Buffer
        (Dnat (Value (FEN.Value (Max_Size (S))).IVal));

      --  Now the string type name is almost built...

      S_Name := Name_Find;

      --  ... However we must resolve the conflicts that may occur
      --  with other sequence type names

      Info := Get_Name_Table_Info (S_Name);

      if Info = Int (Main_Package (Current_Entity)) then
         Index := New_Index (S_Name, Main_Package (Current_Entity));
         Get_Name_String (S_Name);
         Add_Char_To_Name_Buffer ('_');
         Add_Nat_To_Name_Buffer (Index);
         S_Name := Name_Find;
      end if;

      Set_Name_Table_Info (S_Name, Int (Main_Package (Current_Entity)));

      --  Finally, we link S and S_Name

      Link_Mapped_Entity_Name (S_Name, S);

      return S_Name;
   end Map_String_Pkg_Name;

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
              (Value  => FEN.Value (Label),
               Parent => Literal_Parent);
            Append_To (Choices, Choice);
            Label := Next_Entity (Label);
         end loop;

         Identifier := FEN.Identifier (FEN.Declarator (Element));
         Set_Component
           (Variant,
            Make_Component_Declaration
              (Map_Defining_Identifier (Identifier),
               Map_Declarator_Type_Designator
               (Type_Spec (Element), FEN.Declarator (Element))));
         Append_To (Variants, Variant);
         Alternative := Next_Entity (Alternative);
      end loop;

      return Variants;
   end Map_Variant_List;

   ---------------------
   -- Map_Choice_List --
   ---------------------

   procedure Map_Choice_List
     (Labels         :        List_Id;
      Literal_Parent :        Node_Id;
      Choices        :    out List_Id;
      Default_Met    : in out Boolean)
   is
      Label  : Node_Id;
      Choice : Node_Id;
   begin
      Choices := New_List (K_Discrete_Choice_List);

      --  Expansion guarantees that the "default:" case is isolated in
      --  a standalone alternative.

      Label   := First_Entity (Labels);

      if FEN.Value (Label) = No_Value then
         Default_Met := True;
      else
         while Present (Label) loop
            Choice := Make_Literal
              (Value  => FEN.Value (Label),
               Parent => Literal_Parent);
            Append_To (Choices, Choice);
            Label := Next_Entity (Label);
         end loop;
      end if;
   end Map_Choice_List;

   ---------------------------------
   -- Map_Wrap_Element_Identifier --
   ---------------------------------

   function Map_Wrap_Element_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (FEN.Kind (E) = K_Sequence_Type);
   begin
      Get_Name_String
        (BEN.Name
         (Defining_Identifier
          (Instantiation_Node
           (BE_Node
            (E)))));
      Add_Str_To_Name_Buffer ("_Element_Wrap");

      return Make_Defining_Identifier (Name_Find);
   end Map_Wrap_Element_Identifier;

   --  Bodies of the CORBA module handling routines

   ---------------------------------
   -- Get_Predefined_CORBA_Entity --
   ---------------------------------

   function Get_Predefined_CORBA_Entity
     (E      : Node_Id;
      Implem : Boolean := False;
      Wrap   : Boolean := False)
     return RE_Id
   is
      Entity : Node_Id;
      E_Name : Name_Id;
      Info   : Nat;
   begin
      if FEN.Kind (E) = K_Scoped_Name then
         Entity := Reference (E);
      else
         Entity := E;
      end if;

      Get_Name_String
        (To_Lower (FEU.Fully_Qualified_Name (FEN.Identifier (Entity), ".")));
      Add_Str_To_Name_Buffer (CORBA_Predefined_RU_Suffix);
      E_Name := Name_Find;
      Info := Get_Name_Table_Info (E_Name);

      if Info /= 0 then
         --  We have a match with a CORBA predefined unit

         if Implem then
            return CORBA_Predefined_RU_Implem_Table
                     (CORBA_Predefined_RU'Val (Info));
         else
            return CORBA_Predefined_RU_Table
                     (CORBA_Predefined_RU'Val (Info));
         end if;
      end if;

      Get_Name_String
        (To_Lower (FEU.Fully_Qualified_Name (FEN.Identifier (Entity), ".")));
      Add_Str_To_Name_Buffer (CORBA_Predefined_RE_Suffix);
      E_Name := Name_Find;
      Info := Get_Name_Table_Info (E_Name);

      if Info /= 0 then
         --  We have a match with a CORBA predefined entity

         --  In the case of IDL sequences, if the Wrap flag has been
         --  set, we return the corresponding .Sequence type for which
         --  a Wrap function has been generated.

         if Wrap then
            return CORBA_Predefined_RE_Wrap_Table
                     (CORBA_Predefined_RE'Val (Info));
         else
            return CORBA_Predefined_RE_Table
                     (CORBA_Predefined_RE'Val (Info));
         end if;
      end if;

      return RE_Null;
   end Get_Predefined_CORBA_Entity;

   ---------------------------------
   -- Map_Predefined_CORBA_Entity --
   ---------------------------------

   function Map_Predefined_CORBA_Entity
     (E      : Node_Id;
      Implem : Boolean := False;
      Wrap   : Boolean := False;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E, Implem, Wrap);

      if R /= RE_Null then
         return RE (R, Withed);
      else
         return No_Node;
      end if;
   end Map_Predefined_CORBA_Entity;

   -------------------------------
   -- Map_Predefined_CORBA_Type --
   -------------------------------

   function Map_Predefined_CORBA_Type
     (E      : Node_Id;
      Wrap   : Boolean := False;
      Withed : Boolean := True)
     return Node_Id
   is
      R :  RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E => E, Wrap => Wrap);

      case R is
         when RE_Null =>
            return No_Node;

         when RE_Get_Domain_Policy =>
            return RE (RE_Get_Domain_Policy_Args_Type, Withed);

         when RE_Get_Policy_Type =>
            return RE (RE_Get_Policy_Type_Args_Type, Withed);

         when RE_Copy =>
            return RE (RE_Copy_Args_Type, Withed);

         when others =>
            return RE (R, Withed);
      end case;
   end Map_Predefined_CORBA_Type;

   -------------------------------------
   -- Map_Predefined_CORBA_Initialize --
   -------------------------------------

   function Map_Predefined_CORBA_Initialize (E : Node_Id) return Node_Id is
      R : RE_Id;
      N : Node_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);
      N := New_Node (K_Node_Id);

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
           | RE_Object
           | RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType
           | RE_Ref_2 =>
            --  Return a dummy node and not "No_Node" because No_Nodes
            --  means that this is not a predefined CORBA type.

            return N;

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
            --  FIXME : For predefined CORBA Sequence type, once the
            --  CORBA.IDL_Sequence.Helper.Init package is added to the
            --  PolyORB source, return the corresponding
            --  <Type>Seq_Initialize procedure. For now we return a
            --  dummy node N.

            return N;

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_Initialize;

   --------------------------------------
   -- Map_Predefined_CORBA_IR_Function --
   --------------------------------------

   function Map_Predefined_CORBA_IR_Function (E : Node_Id) return Node_Id is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

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
           | RE_Object
           | RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType
           | RE_Ref_2
           | RE_AnySeq_2
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

            raise Program_Error with
              "IR info for predefined CORBA entites not implemented yet";

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_IR_Function;

   -----------------------------
   -- Map_Predefined_CORBA_TC --
   -----------------------------

   function Map_Predefined_CORBA_TC
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

      case R is
         when RE_Any =>
            return RE (RE_TC_Any, Withed);
         when RE_Identifier_0 =>
            return RE (RE_TC_Identifier, Withed);
         when RE_RepositoryId =>
            return RE (RE_TC_RepositoryId, Withed);
         when RE_ScopedName =>
            return RE (RE_TC_ScopedName, Withed);
         when RE_PolicyType =>
            return RE (RE_TC_PolicyType, Withed);
         when RE_Visibility =>
            return RE (RE_TC_Visibility, Withed);
         when RE_Float =>
            return RE (RE_TC_Float, Withed);
         when RE_Double =>
            return RE (RE_TC_Double, Withed);
         when RE_Long_Double =>
            return RE (RE_TC_Long_Double, Withed);
         when RE_Short =>
            return RE (RE_TC_Short, Withed);
         when RE_Long =>
            return RE (RE_TC_Long, Withed);
         when RE_Long_Long =>
            return RE (RE_TC_Long_Long, Withed);
         when RE_Unsigned_Short =>
            return RE (RE_TC_Unsigned_Short, Withed);
         when RE_Unsigned_Long =>
            return RE (RE_TC_Unsigned_Long, Withed);
         when RE_Unsigned_Long_Long =>
            return RE (RE_TC_Unsigned_Long_Long, Withed);
         when RE_Char =>
            return RE (RE_TC_Char, Withed);
         when RE_WChar =>
            return RE (RE_TC_WChar, Withed);
         when RE_String_0 =>
            return RE (RE_TC_String, Withed);
         when RE_Wide_String =>
            return RE (RE_TC_Wide_String, Withed);
         when RE_Boolean =>
            return RE (RE_TC_Boolean, Withed);
         when RE_Octet =>
            return RE (RE_TC_Octet, Withed);
         when RE_Ref_2 =>
            return RE (RE_TC_Object_0, Withed);
         when RE_Object =>
            return RE (RE_TC_TypeCode, Withed);
         when RE_Ref_11 =>
            return RE (RE_TC_DomainManager, Withed);
         when RE_Ref_6 =>
            return RE (RE_TC_Policy, Withed);

         when RE_AnySeq_2 =>
            return RE (RE_TC_AnySeq, Withed);
         when RE_FloatSeq_2 =>
            return RE (RE_TC_FloatSeq, Withed);
         when RE_DoubleSeq_2 =>
            return RE (RE_TC_DoubleSeq, Withed);
         when RE_LongDoubleSeq_2 =>
            return RE (RE_TC_LongDoubleSeq, Withed);
         when RE_ShortSeq_2 =>
            return RE (RE_TC_ShortSeq, Withed);
         when RE_LongSeq_2 =>
            return RE (RE_TC_LongSeq, Withed);
         when RE_LongLongSeq_2 =>
            return RE (RE_TC_LongLongSeq, Withed);
         when RE_UShortSeq_2 =>
            return RE (RE_TC_UShortSeq, Withed);
         when RE_ULongSeq_2 =>
            return RE (RE_TC_ULongSeq, Withed);
         when RE_ULongLongSeq_2 =>
            return RE (RE_TC_ULongLongSeq, Withed);
         when RE_CharSeq_2 =>
            return RE (RE_TC_CharSeq, Withed);
         when RE_WCharSeq_2 =>
            return RE (RE_TC_WCharSeq, Withed);
         when RE_StringSeq_2 =>
            return RE (RE_TC_StringSeq, Withed);
         when RE_WStringSeq_2 =>
            return RE (RE_TC_WStringSeq, Withed);
         when RE_BooleanSeq_2 =>
            return RE (RE_TC_BooleanSeq, Withed);
         when RE_OctetSeq_2 =>
            return RE (RE_TC_OctetSeq, Withed);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_TC;

   -----------------------------------
   -- Map_Predefined_CORBA_From_Any --
   -----------------------------------

   function Map_Predefined_CORBA_From_Any
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

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
            return RE (RE_From_Any_0, Withed);

         when RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType =>
            return RE (RE_From_Any_2, Withed);

         when RE_Ref_2 =>
            return RE (RE_From_Any_1, Withed);

         when RE_Ref_6 =>
            return RE (RE_From_Any_6, Withed);

         when RE_Ref_11 =>
            return RE (RE_From_Any_5, Withed);

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
            return RE (RE_From_Any_4, Withed);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_From_Any;

   ---------------------------------
   -- Map_Predefined_CORBA_To_Any --
   ---------------------------------

   function Map_Predefined_CORBA_To_Any
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

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
            return RE (RE_To_Any_0, Withed);

         when RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType =>
            return RE (RE_To_Any_2, Withed);

         when RE_Ref_2 =>
            return RE (RE_To_Any_1, Withed);

         when RE_Ref_6 =>
            return RE (RE_To_Any_6, Withed);

         when RE_Ref_11 =>
            return RE (RE_To_Any_5, Withed);

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
            return RE (RE_To_Any_4, Withed);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_To_Any;

   ---------------------------------
   -- Map_Predefined_CORBA_To_Ref --
   ---------------------------------

   function Map_Predefined_CORBA_To_Ref
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

      case R is
         when RE_Ref_6 =>
            return RE (RE_To_Ref_6, Withed);

         when RE_Ref_11 =>
            return RE (RE_To_Ref_5, Withed);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_To_Ref;

   -------------------------------------
   -- Map_Predefined_CORBA_Marshaller --
   -------------------------------------

   function Map_Predefined_CORBA_Marshaller
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      pragma Assert (FEN.Kind (E) = FEN.K_Operation_Declaration);

      R := Get_Predefined_CORBA_Entity (E);

      case R is
         when RE_Get_Domain_Policy =>
            return RE (RE_Get_Domain_Policy_Marshaller, Withed);

         when RE_Get_Policy_Type =>
            return RE (RE_Get_Policy_Type_Marshaller, Withed);

         when RE_Copy =>
            return RE (RE_Copy_Marshaller, Withed);

         when others =>
            return No_Node;
      end case;

   end Map_Predefined_CORBA_Marshaller;

   ---------------------------------------
   -- Map_Predefined_CORBA_Unmarshaller --
   ---------------------------------------

   function Map_Predefined_CORBA_Unmarshaller
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      pragma Assert (FEN.Kind (E) = FEN.K_Operation_Declaration);

      R := Get_Predefined_CORBA_Entity (E);

      case R is
         when RE_Get_Domain_Policy =>
            return RE (RE_Get_Domain_Policy_Unmarshaller, Withed);

         when RE_Get_Policy_Type =>
            return RE (RE_Get_Policy_Type_Unmarshaller, Withed);

         when RE_Copy =>
            return RE (RE_Copy_Unmarshaller, Withed);

         when others =>
            return No_Node;
      end case;

   end Map_Predefined_CORBA_Unmarshaller;

   -------------------------------
   -- Map_Predefined_CORBA_Wrap --
   -------------------------------

   function Map_Predefined_CORBA_Wrap
     (E      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      R : RE_Id;
   begin
      R := Get_Predefined_CORBA_Entity (E);

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
            return RE (RE_Wrap_2, Withed);

         when RE_Identifier_0
           | RE_RepositoryId
           | RE_ScopedName
           | RE_Visibility
           | RE_PolicyType =>
            --  These types are redefinitions of CORBA.String,
            --  CORBA.Short and CORBA.Unsigned_Long. In all cases, we
            --  use CORBA.Wrap.

            return RE (RE_Wrap_2, Withed);

         when RE_Ref_2 =>
            return RE (RE_Wrap_3, Withed);

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
            return RE (RE_Wrap_4, Withed);

         when others =>
            return No_Node;
      end case;
   end Map_Predefined_CORBA_Wrap;

   ----------------------------------------------
   -- Inheritance related internal subprograms --
   ----------------------------------------------

   Mark : Int := 1;
   function Get_New_Int_Value return Int;
   --  Get a new Int value

   function View_Old_Int_Value return Int;
   --  View the current int value without modifying it

   function Already_Inherited (Name : Name_Id) return Boolean;
   --  If two entities inherited from two parents have the same name,
   --  the second should not be added

   procedure Explaining_Comment
     (First_Name  : Name_Id;
      Second_Name : Name_Id;
      Message     : String);
   --  Generate a comment that indicates from which interface the
   --  entity we deal with is inherited.

   function Is_Implicit_Parent
     (Parent : Node_Id;
      Child  : Node_Id)
     return Boolean;
   --  Return True if parent is the first parent of Child or if it is
   --  one of the parents of the first parent of Child

   procedure Map_Any_Converters
     (Type_Name : Name_Id;
      From_Any  :    out Node_Id;
      To_Any    :    out Node_Id);
   --  Return the From_Any and the To_Any nodes corresponding to type
   --  'Type_Name'

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
      Append_To (Visible_Part (Current_Package), Comment);
   end Explaining_Comment;

   ------------------------
   -- Is_Implicit_Parent --
   ------------------------

   function Is_Implicit_Parent
     (Parent : Node_Id;
      Child  : Node_Id)
     return Boolean
   is
      pragma Assert (Kind (Parent) = K_Interface_Declaration and then
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
     (Type_Name : Name_Id;
      From_Any  :    out Node_Id;
      To_Any    :    out Node_Id)
   is
      New_Type  : Node_Id;
      Profile   : List_Id;
      Parameter : Node_Id;
   begin
      New_Type := Make_Selected_Component
        (Expand_Designator (Main_Package (Current_Entity)),
         Make_Identifier (Type_Name));

      --  From_Any

      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Item)),
         RE (RE_Any));
      Append_To (Profile, Parameter);

      From_Any := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_From_Any)),
         Profile,
         New_Type);

      --  To_Any

      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Item)),
         New_Type);
      Append_To (Profile, Parameter);
      To_Any := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_To_Any)),
         Profile, RE (RE_Any));
   end Map_Any_Converters;

   ----------------------------------
   -- Map_Inherited_Entities_Specs --
   ----------------------------------

   procedure Map_Inherited_Entities_Specs
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean                        := True;
      Visit_Operation_Subp  : Visit_Procedure_Two_Params_Ptr;
      Stub                  : Boolean                        := False;
      Helper                : Boolean                        := False;
      Skel                  : Boolean                        := False;
      Impl                  : Boolean                        := False)
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

      --  We get the node of the current interface (i.e. the interface
      --  who first called this subprogram).

      Actual_Current_Interface := FEN.Corresponding_Entity
        (FE_Node (Current_Entity));

      if First_Recusrion_Level then

         --  It is important to get the new value before any inherited
         --  entity manipulation.

         Mark := Get_New_Int_Value;

         if Stub or else Helper then

            --  The mapping of type definitions, constant declarations and
            --  exception declarations is defined in the parents.

            --  During the different recursion level, we must have
            --  access to the current interface we are visiting. So we
            --  don't use the parameter Current_Interface because its
            --  value changes depending on the recursion level.

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

               --  Mapping of type definitions, constant declarations
               --  and exception declarations defined in the parents

               --  During the different recursion level, we must have
               --  access to the current interface we are visiting. So
               --  we don't use the parameter Current_Interface
               --  because its value changes depending on the
               --  recursion level.

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
      First_Recusrion_Level : Boolean                       := True;
      Visit_Operation_Subp  : Visit_Procedure_One_Param_Ptr;
      Stub                  : Boolean                       := False;
      Helper                : Boolean                       := False;
      Skel                  : Boolean                       := False;
      Impl                  : Boolean                       := False)
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

      --  We get the node of the current interface (the interface who
      --  first called this subprogram.

      Actual_Current_Interface := FEN.Corresponding_Entity
        (FE_Node (Current_Entity));

      --  In case of first recursion level, we compute a new value to
      --  tag visited parents (in order to visit them only once), we
      --  also map the additional entities.

      if First_Recusrion_Level then
         --  It is important to get the new value before any inherited
         --  entity manipulation

         Mark := Get_New_Int_Value;

         if Stub or else Helper then

            --  Mapping of type definitions, constant declarations and
            --  exception declarations defined in the parents

            --  During the different recursion level, we must have
            --  access to the current interface we are visiting. So we
            --  don't use the parameter Current_Interface because its
            --  value changes depending on the recursion level.

            Map_Additional_Entities_Bodies
              (Reference (First_Entity (L)),
               Actual_Current_Interface,
               Stub   => Stub,
               Helper => Helper);
         end if;
      else
         Mark := View_Old_Int_Value;
      end if;

      --  In case of first recursion level and not a skel visiting, we
      --  begin by the second parent, in other cases, we begin by the
      --  first.

      if First_Recusrion_Level and then not Skel then
         Par_Int := Next_Entity (First_Entity (L));
      else
         Par_Int := First_Entity (L);
      end if;

      while Present (Par_Int) loop
         --  We ensure that the interface is not visited twice and is
         --  not an implicit parent

         Par_Name := FEU.Fully_Qualified_Name
           (Identifier
            (Reference
             (Par_Int)));

         if Get_Name_Table_Info (Par_Name) = Mark then
            Do_Visit := False;
         else
            Set_Name_Table_Info (Par_Name, Mark);
         end if;

         --  For Stubs, Helpers or Impls, primitives for the first
         --  parent are inherited by entities: we only add nodes from
         --  the others parents. Skel should handle all parents.

         if Is_Implicit_Parent (Reference (Par_Int), Actual_Current_Interface)
           and then not Skel
         then
            Do_Visit := False;
         end if;

         if not Do_Visit then
            Do_Visit := True;
         else
            if Stub or else Helper then

               --  Mapping of type definitions, constant declarations
               --  and exception declarations defined in the parents.

               --  During the different recursion level, we must have
               --  access to the current interface we are visiting. So
               --  we don't use the parameter Current_Interface
               --  because its value changes depending on the
               --  recursion level.

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

   function Already_Inherited (Name : Name_Id) return Boolean is
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
      --  We do not handle predefined CORBA parents. Here we use
      --  Get_Predefined_CORBA_Entity to avoid "withing" a useless
      --  package.

      if Get_Predefined_CORBA_Entity (Parent_Interface) /= RE_Null then
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
                           Set_Declaration_Node (New_Type, T);
                           Append_To (Visible_Part (Current_Package), T);
                        end if;

                        if Helper then
                           Map_Any_Converters
                             (To_Ada_Name
                              (IDL_Name
                               (Identifier
                                (D))),
                              From_Any,
                              To_Any);
                           Append_To
                             (Visible_Part (Current_Package), From_Any);
                           Append_To
                             (Visible_Part (Current_Package), To_Any);
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
                        Set_Declaration_Node (New_Type, T);
                        Append_To (Visible_Part (Current_Package), T);
                     end if;

                     if Helper then
                        Map_Any_Converters
                          (To_Ada_Name
                           (IDL_Name
                            (Identifier
                             (Entity))),
                           From_Any,
                           To_Any);
                        Append_To (Visible_Part (Current_Package), From_Any);
                        Append_To (Visible_Part (Current_Package), To_Any);
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
                             Map_Expanded_Name (Type_Spec (Entity)),
                           Renamed_Object      =>
                             Original_Constant);
                        Append_To (Visible_Part (Current_Package), C);
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
                        Append_To (Visible_Part (Current_Package), C);

                        --  Generate the "_Members" subtype

                        Original_Type := Expand_Designator
                          (Type_Def_Node
                           (BE_Node
                            (Identifier
                             (Entity))));
                        New_Type := Make_Defining_Identifier
                          (Get_Name
                           (Get_Base_Identifier
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
                        Set_Declaration_Node (New_Type, T);
                        Append_To (Visible_Part (Current_Package), T);

                        --  Generate the Get_Members procedure spec

                        N := Map_Get_Members_Spec (Expand_Designator (T));

                        Append_To (Visible_Part (Current_Package), N);
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
                        Append_To (Visible_Part (Current_Package), From_Any);
                        Append_To (Visible_Part (Current_Package), To_Any);
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
      --  We do not handle predefined CORBA parents. Here we use
      --  Get_Predefined_CORBA_Entity to avoid "withing" a useless
      --  package.

      if Get_Predefined_CORBA_Entity (Parent_Interface) /= RE_Null then
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
                           Append_To (Statements (Current_Package), From_Any);
                           Append_To (Statements (Current_Package), To_Any);
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
                        Append_To (Statements (Current_Package), From_Any);
                        Append_To (Statements (Current_Package), To_Any);
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

                        New_Member_Type := Make_Selected_Component
                          (Defining_Identifier (Main_Package (Current_Entity)),
                           Get_Base_Identifier
                           (Type_Def_Node
                            (BE_Node
                             (Identifier
                              (Entity)))));
                        N := Map_Get_Members_Spec (New_Member_Type);

                        Original_Get_Members := Make_Selected_Component
                          (Expand_Designator
                           (BEN.Parent
                            (Type_Def_Node
                             (BE_Node
                              (Identifier
                               (Parent_Interface))))),
                           Get_Base_Identifier
                           (Defining_Identifier
                            (Map_Get_Members_Spec
                             (Expand_Designator
                              (Type_Def_Node
                               (BE_Node
                                (Identifier
                                 (Entity))))))));
                        Set_Renamed_Entity (N, Original_Get_Members);
                        Append_To (Statements (Current_Package), N);
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
                        Append_To (Statements (Current_Package), From_Any);
                        Append_To (Statements (Current_Package), To_Any);
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;

         Entity := Next_Entity (Entity);
      end loop;
   end Map_Additional_Entities_Bodies;

   -------------------------------------------------
   -- Static request handling related subprograms --
   -------------------------------------------------

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

   --------------------------------
   -- Map_Buffer_Size_Identifier --
   --------------------------------

   function Map_Buffer_Size_Identifier (E : Node_Id) return Node_Id is
      pragma Assert (BEN.Kind (E) = K_Defining_Identifier);
   begin
      Get_Name_String (BEN.Name (E));
      Add_Str_To_Name_Buffer ("_Buffer_Size");
      return Make_Defining_Identifier (Name_Find);
   end Map_Buffer_Size_Identifier;

   ----------------------------------
   -- Map_Argument_Identifier_Name --
   ----------------------------------

   function Map_Argument_Identifier_Name
     (P : Name_Id;
      O : Name_Id)
     return Name_Id
   is
   begin
      Get_Name_String (O);
      Add_Str_To_Name_Buffer ("_Arg_Name_");
      Get_Name_String_And_Append (P);
      Add_Str_To_Name_Buffer ("_Ü");
      return Name_Find;
   end Map_Argument_Identifier_Name;

   -----------------------
   -- Map_Argument_Name --
   -----------------------

   function Map_Argument_Name (P : Name_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("Argument_");
      Get_Name_String_And_Append (P);
      Add_Str_To_Name_Buffer ("_Ü");
      return Name_Find;
   end Map_Argument_Name;

   -------------------------------
   -- Map_Argument_Content_Name --
   -------------------------------

   function Map_Argument_Content_Name (P : Name_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("Arg_CC_");
      Get_Name_String_And_Append (P);
      Add_Str_To_Name_Buffer ("_Ü");
      return Name_Find;
   end Map_Argument_Content_Name;

   ---------------------------
   -- Map_Argument_Any_Name --
   ---------------------------

   function Map_Argument_Any_Name (P : Name_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("Arg_Any_");
      Get_Name_String_And_Append (P);
      Add_Str_To_Name_Buffer ("_Ü");
      return Name_Find;
   end Map_Argument_Any_Name;

   --------------------------------
   -- Map_Result_Subprogram_Name --
   --------------------------------

   function Map_Result_Subprogram_Name (O : Name_Id) return Name_Id is
   begin
      Get_Name_String (O);
      Add_Str_To_Name_Buffer ("_Result_Ü");
      return Name_Find;
   end Map_Result_Subprogram_Name;

   --------------------------------
   -- Map_Result_Identifier_Name --
   --------------------------------

   function Map_Result_Identifier_Name (O : Name_Id) return Name_Id is
   begin
      Get_Name_String (O);
      Add_Str_To_Name_Buffer ("_Result_Name_Ü");
      return Name_Find;
   end Map_Result_Identifier_Name;

   --------------------------------
   -- Map_Operation_Name_Literal --
   --------------------------------

   function Map_Operation_Name_Literal (O : Node_Id) return Name_Id is
   begin
      --  Literals for attribute accessors must be prefixed by '_'
      --  (_Get_... and _Set_...). Attribute accessors are known by
      --  their null location (they are created at expansion time, so
      --  they do not have a location in the IDL file).

      Name_Len := 0;

      if FEN.Loc (Identifier (O)) = No_Location then
         Set_Char_To_Name_Buffer ('_');
      end if;

      Get_Name_String_And_Append (IDL_Name (Identifier (O)));
      return Name_Find;
   end Map_Operation_Name_Literal;

   -------------------------
   -- Cast_When_Necessary --
   -------------------------

   procedure Cast_When_Necessary
     (Ada_Node           : in out Node_Id;
      IDL_Immediate_Type :        Node_Id;
      IDL_Original_Type  :        Node_Id;
      Wrap               :        Boolean := False)
   is
      Is_Object : constant Boolean := Is_Object_Type (IDL_Original_Type);
   begin
      if Is_Object then
         --  Handle predefined CORBA objects

         if FEN.Kind (IDL_Original_Type) = K_Interface_Declaration or else
           FEN.Kind (IDL_Original_Type) = K_Forward_Interface_Declaration
         then
            declare
               R : constant RE_Id   := Get_Predefined_CORBA_Entity
                 (IDL_Original_Type);
            begin
               case R is
                  when RE_Object =>
                     if FEN.Kind (IDL_Immediate_Type) = K_Scoped_Name and then
                       IDL_Original_Type /= Reference (IDL_Immediate_Type)
                     then
                        Ada_Node := Make_Type_Conversion
                          (RE (RE_Object), Ada_Node);
                     end if;

                     return; --  Important

                  when others =>
                     null;
               end case;
            end;
         end if;

         --  General case

         if FEN.Kind (IDL_Immediate_Type) = K_Scoped_Name then
            Ada_Node := Make_Type_Conversion
              (RE (RE_Ref_2), Ada_Node);
         end if;
      else
         if FEN.Kind (IDL_Immediate_Type) = K_Scoped_Name and then
           FEN.Kind (Reference (IDL_Immediate_Type)) = K_Simple_Declarator
         then
            Ada_Node := Make_Type_Conversion
              (Get_Type_Definition_Node (IDL_Original_Type, Wrap), Ada_Node);
         end if;
      end if;
   end Cast_When_Necessary;

   ---------------------------------
   -- Get_From_Any_Container_Node --
   ---------------------------------

   function Get_From_Any_Container_Node (T : Node_Id) return Node_Id is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            return RE (RE_From_Any_1);
         else
            return RE (RE_From_Any_0);
         end if;
      end if;

      --  User type case

      case FEN.Kind (T) is
         when K_Enumeration_Type =>
            return Expand_Designator
              (From_Any_Container_Node (BE_Node (Identifier (T))));
         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_From_Any (T);
            begin
               --  If result is not null, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_From_Any_Container_Node (Reference (T));
            end;

         when others =>
            raise Program_Error with "Cannot get the additional From_Any "
              & "spec of a "
              & FEN.Node_Kind'Image (FEN.Kind (T))
              & " at position " & Image (FEN.Loc (T));
      end case;
   end Get_From_Any_Container_Node;

   -----------------------
   -- Get_From_Any_Node --
   -----------------------

   function Get_From_Any_Node
     (T      : Node_Id;
      Withed : Boolean := True)
     return Node_Id is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            return RE (RE_From_Any_1, Withed);
         else
            return RE (RE_From_Any_0, Withed);
         end if;
      end if;

      --  User type case

      case FEN.Kind (T) is
         when K_Fixed_Point_Type
           | K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Expand_Designator (From_Any_Node (BE_Node (T)), Withed);

         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_From_Any (T, Withed);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_From_Any_Node (Reference (T), Withed);
            end;

         when others =>
            return Expand_Designator
              (From_Any_Node (BE_Node (Identifier (T))), Withed);
      end case;
   end Get_From_Any_Node;

   -------------------------
   -- Get_Initialize_Node --
   -------------------------

   function Get_Initialize_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id
   is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         return No_Node;
      end if;

      --  General case

      case FEN.Kind (T) is
         when K_Fixed_Point_Type
           | K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Expand_Designator (Initialize_Node (BE_Node (T)));

         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_Initialize (T);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_Initialize_Node (Reference (T), Resolve_Forward);
            end;

         when K_Forward_Interface_Declaration =>
            if Resolve_Forward then
               return Get_Initialize_Node (Forward (T), Resolve_Forward);
            else
               return Expand_Designator
                 (Initialize_Node (BE_Node (Identifier (T))));
            end if;

         when others =>
            return Expand_Designator
              (Initialize_Node (BE_Node (Identifier (T))));
      end case;
   end Get_Initialize_Node;

   -----------------
   -- Get_TC_Node --
   -----------------

   function Get_TC_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id
   is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         return Base_Type_TC (FEN.Kind (T));
      end if;

      --  General case

      case FEN.Kind (T) is
         when K_Fixed_Point_Type
           | K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Expand_Designator (TC_Node (BE_Node (T)));

         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_TC (T);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_TC_Node (Reference (T), Resolve_Forward);
            end;

         when K_Forward_Interface_Declaration =>
            if Resolve_Forward then
               return Get_TC_Node (Forward (T), Resolve_Forward);
            else
               return Expand_Designator (TC_Node (BE_Node (Identifier (T))));
            end if;

         when others =>
            return Expand_Designator (TC_Node (BE_Node (Identifier (T))));
      end case;
   end Get_TC_Node;

   ---------------------
   -- Get_To_Any_Node --
   ---------------------

   function Get_To_Any_Node
     (T      : Node_Id;
      Withed : Boolean := True)
     return Node_Id is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            return RE (RE_To_Any_3, Withed);
         else
            return RE (RE_To_Any_0, Withed);
         end if;
      end if;

      --  General case

      case FEN.Kind (T) is
         when K_Fixed_Point_Type
           | K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Expand_Designator (To_Any_Node (BE_Node (T)), Withed);

         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_To_Any (T, Withed);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_To_Any_Node (Reference (T), Withed);
            end;

         when others =>
            return Expand_Designator
              (To_Any_Node (BE_Node (Identifier (T))), Withed);
      end case;
   end Get_To_Any_Node;

   ---------------------
   -- Get_To_Ref_Node --
   ---------------------

   function Get_To_Ref_Node
     (T      : Node_Id;
      Withed : Boolean := True)
     return Node_Id is
   begin
      case FEN.Kind (T) is
         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_To_Ref (T, Withed);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_To_Ref_Node (Reference (T), Withed);
            end;

         when others =>
            declare
               Orig_Type_Spec : constant Node_Id :=
                 FEU.Get_Original_Type_Specifier (T);
            begin
               return Expand_Designator
                 (To_Ref_Node (BE_Node (Identifier (Orig_Type_Spec))),
                  Withed);
            end;
      end case;
   end Get_To_Ref_Node;

   -------------------------
   -- Get_Marshaller_Node --
   -------------------------

   function Get_Marshaller_Node
     (O      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      Result : Node_Id;
   begin
      pragma Assert (FEN.Kind (O) = FEN.K_Operation_Declaration);

      Result := Map_Predefined_CORBA_Marshaller (O, Withed);

      if No (Result) then
         Result := Expand_Designator
           (Marshaller_Node
            (BE_Node
             (Identifier (O))));
      end if;

      return Result;
   end Get_Marshaller_Node;

   ---------------------------
   -- Get_Unmarshaller_Node --
   ---------------------------

   function Get_Unmarshaller_Node
     (O      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      Result : Node_Id;
   begin
      pragma Assert (FEN.Kind (O) = FEN.K_Operation_Declaration);

      Result := Map_Predefined_CORBA_Unmarshaller (O, Withed);

      if No (Result) then
         Result := Expand_Designator
           (Unmarshaller_Node
            (BE_Node
             (Identifier (O))));
      end if;

      return Result;
   end Get_Unmarshaller_Node;

   ------------------------------
   -- Get_Type_Definition_Node --
   ------------------------------

   function Get_Type_Definition_Node
     (T      : Node_Id;
      Wrap   : Boolean := False)
     return Node_Id
   is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         return RE (Convert (FEN.Kind (T)));
      end if;

      --  General case

      case FEN.Kind (T) is
         --  For sequence and bounded [wide] string types, the
         --  frontend node is linked to a type designator. For the
         --  remaining types, the frontend node is linked to full type
         --  definition.

         when K_Fixed_Point_Type =>
            return Expand_Designator (Type_Def_Node (BE_Node (T)));

         when K_Structure_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Complex_Declarator =>
            return Expand_Designator
              (Type_Def_Node (BE_Node (Identifier (T))));

         when K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Copy_Expanded_Name (Type_Def_Node (BE_Node (T)));

         when K_Scoped_Name =>
            --  Handle predefined CORBA entities

            declare
               Result : constant Node_Id := Map_Predefined_CORBA_Type
                 (T, Wrap => Wrap);
            begin
               if Present (Result) then
                  return Result;
               end if;

               return Get_Type_Definition_Node (Reference (T));
            end;

         when others =>
            --  Handle predefined CORBA entities

            declare
               Result         : constant Node_Id := Map_Predefined_CORBA_Type
                 (T, Wrap => Wrap);
               Orig_Type_Spec : constant Node_Id :=
                 FEU.Get_Original_Type_Specifier (T);
            begin
               if Present (Result) then
                  return Result;
               end if;

               --  If the wrap flag has been set, we return the
               --  ancestor type of T for which a 'Wrap' function has
               --  been generated.

               if Wrap then
                  return Get_Type_Definition_Node (Orig_Type_Spec);
               else
                  return Expand_Designator
                    (Type_Def_Node (BE_Node (Identifier (T))));
               end if;
            end;
      end case;
   end Get_Type_Definition_Node;

   --------------------------
   -- Get_IR_Function_Node --
   --------------------------

   function Get_IR_Function_Node
     (T      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
      function Get_Primitive (R : RE_Id) return Node_Id;
      --  Return a call to Get_Primitive for the named PrimitiveDef
      --  kind.

      -------------------
      -- Get_Primitive --
      -------------------

      function Get_Primitive (R : RE_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Subprogram_Call
           (RE (RE_Get_Primitive),
            New_List
            (Make_Subprogram_Call (RE (RE_Get_IR_Root), No_List),
             RE (R)));

         return N;
      end Get_Primitive;

   begin
      case FEN.Kind (T) is
         when K_Void =>
            return Get_Primitive (RE_pk_void);

         when K_Short =>
            return Get_Primitive (RE_pk_short);

         when K_Long =>
            return Get_Primitive (RE_pk_long);

         when K_Long_Long =>
            return Get_Primitive (RE_pk_longlong);

         when K_Unsigned_Short =>
            return Get_Primitive (RE_pk_ushort);

         when K_Unsigned_Long =>
            return Get_Primitive (RE_pk_ulong);

         when K_Unsigned_Long_Long =>
            return Get_Primitive (RE_pk_ulonglong);

         when K_Char =>
            return Get_Primitive (RE_pk_char);

         when K_Wide_Char =>
            return Get_Primitive (RE_pk_wchar);

         when K_Boolean =>
            return Get_Primitive (RE_pk_boolean);

         when K_Float =>
            return Get_Primitive (RE_pk_float);

         when K_Double =>
            return Get_Primitive (RE_pk_double);

         when K_Long_Double =>
            return Get_Primitive (RE_pk_longdouble);

         when K_String =>
            return Get_Primitive (RE_pk_string);

         when K_String_Type =>
            return Get_Primitive (RE_pk_string);

         when K_Wide_String =>
            return Get_Primitive (RE_pk_wstring);

         when K_Wide_String_Type =>
            return Get_Primitive (RE_pk_wstring);

         when K_Octet =>
            return Get_Primitive (RE_pk_octet);

         when K_Object =>
            return Get_Primitive (RE_pk_objref);

         when K_Any =>
            return Get_Primitive (RE_pk_any);

         when K_Fixed_Point_Type | K_Sequence_Type =>
            raise Program_Error;

         when K_Scoped_Name =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_IR_Function (T);
            begin
               --  If result is not nul, then we deal with a
               --  predefined CORBA entity.

               if Present (Result) then
                  return Result;
               end if;

               return Get_IR_Function_Node (Reference (T), Withed);
            end;

         when others =>
            return Expand_Designator
              (Ir_Function_Node (BE_Node (Identifier (T))), Withed);
      end case;
   end Get_IR_Function_Node;

   -------------------
   -- Get_Wrap_Node --
   -------------------

   function Get_Wrap_Node
     (T      : Node_Id;
      Withed : Boolean := True)
     return Node_Id
   is
   begin
      --  Base types case

      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            return RE (RE_Wrap_3, Withed);
         else
            return RE (RE_Wrap_2, Withed);
         end if;
      end if;

      --  General case

      case FEN.Kind (T) is
         when K_Fixed_Point_Type
           | K_Sequence_Type
           | K_String_Type
           | K_Wide_String_Type =>
            return Expand_Designator (Wrap_Node (BE_Node (T)), Withed);

         when K_Structure_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Complex_Declarator =>
            return Expand_Designator
              (Wrap_Node (BE_Node (Identifier (T))), Withed);

         when K_Scoped_Name =>
            --  Handle predefined CORBA entities

            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_Wrap (T, Withed);
            begin
               if Present (Result) then
                  return Result;
               end if;

               return Get_Wrap_Node (Reference (T), Withed);
            end;

         when K_Interface_Declaration
           | K_Forward_Interface_Declaration =>
            --  Handle predefined CORBA entities

            declare
               Result : constant Node_Id := Map_Predefined_CORBA_Wrap (T);
            begin
               if Present (Result) then
                  return Result;
               end if;

               --  Interfaces are CORBA.Object

               return RE (RE_Wrap_3, Withed);
            end;

         when others =>
            declare
               Result : constant Node_Id :=
                 Map_Predefined_CORBA_Wrap (T, Withed);
               Orig_Type_Spec : constant Node_Id :=
                 FEU.Get_Original_Type_Specifier (T);
            begin
               if Present (Result) then
                  return Result;
               end if;

               return Get_Wrap_Node (Orig_Type_Spec, Withed);
            end;
      end case;
   end Get_Wrap_Node;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Dep             : Node_Id;
      Dependency_List : List_Id;
      Dependency_Kind : Dependent_Entity;
      Optional        : Boolean := False)
   is

      function "=" (Name : Name_Id; Node : Node_Id) return Boolean;
      --  Shortcut to compare `Name' to the full name of `Node'

      function Is_Internal_Unit (Unit : Node_Id) return Boolean;
      --  Return True if `Unit' is an internal Ada unit

      ---------
      -- "=" --
      ---------

      function "=" (Name : Name_Id; Node : Node_Id) return Boolean is
      begin
         return Name = Fully_Qualified_Name (Node);
      end "=";

      ----------------------
      -- Is_Internal_Unit --
      ----------------------

      function Is_Internal_Unit (Unit : Node_Id) return Boolean is
      begin
         return BEN.Kind (Get_Declaration_Node (Unit))
           = K_Package_Instantiation;
      end Is_Internal_Unit;

      Dep_Name : Name_Id;
      V        : Value_Id;
      N        : Node_Id;
      M        : Node_Id;
      Append   : Boolean := False;
   begin
      if Is_Internal_Unit (Dep) then
         return;
      end if;

      Dep_Name := BEU.Fully_Qualified_Name (Dep);

      --  Particular case : We don't add dependencies on:

      --  * The Helper package itself

      if Dep_Name = Defining_Identifier
        (Helper_Package (Current_Entity))
      then
         return;

         --  First case : A dependency on CORBA.Object.Helper
         --  implies a dependency on CORBA.Object

      elsif Dep_Name = RU (RU_CORBA_Object_Helper, False) then
         Add_Dependency
           (RU (RU_CORBA_Object, False),
            Dependency_List,
            Dependency_Kind);
         return;

         --  Second case : We lower the case of these entities
         --  * CORBA
         --  * CORBA.Helper
         --  * CORBA.Object

      elsif Dep_Name = RU (RU_CORBA, False)
        or else Dep_Name = RU (RU_CORBA_Helper, False)
        or else Dep_Name = RU (RU_CORBA_Object, False)
      then
         Get_Name_String (Dep_Name);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Dep_Name := Name_Find;
         Append := True;

         --  Third case: Some PolyORB units have a customized
         --  initialization name

      elsif Dep_Name = RU (RU_PolyORB_Exceptions, False) then
         Set_Str_To_Name_Buffer ("exceptions");
         Dep_Name := Name_Find;
         Append := True;
      elsif Dependency_Kind /= D_Helper then
         --  Forth case: general case, we add the dependency for the
         --  given package except for *generated* Helpers that need
         --  not to depend upon each others

         Append := True;
      end if;

      --  If the dependency is optional, append a '?' suffix

      if Optional then
         Dep_Name := Add_Suffix_To_Name ("?", Dep_Name);
      end if;

      --  Check whether the dependency is already added

      M := First_Node (Dependency_List);
      while Present (M) loop
         if Values.Value (BEN.Value (M)).SVal = Dep_Name then
            Append := False;
         end if;
         M := Next_Node (M);
      end loop;

      --  Add the dependency if it belongs to the cases above

      if Append then
         V := New_String_Value (Dep_Name, False);
         N := Make_Literal (V);
         Append_To (Dependency_List, N);
      end if;
   end Add_Dependency;

end Backend.BE_CORBA_Ada.IDL_To_Ada;
