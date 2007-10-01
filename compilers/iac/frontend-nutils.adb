------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      F R O N T E N D . N U T I L S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with Errors; use Errors;
with Namet;  use Namet;
with Utils;  use Utils;

package body Frontend.Nutils is

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;
      Many : Int := Size (L);

   begin
      Last := Last_Entity (L);
      if No (Last) then
         Set_First_Entity (L, E);
      else
         Set_Next_Entity (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Entity (L, Last);
         Many := Many + 1;
         Last := Next_Entity (Last);
      end loop;
      Set_Size (L, Many);
   end Append_Node_To_List;

   -------------------------------
   -- Bind_Declarator_To_Entity --
   -------------------------------

   procedure Bind_Declarator_To_Entity (D : Node_Id; E : Node_Id) is
   begin
      Set_Declaration (D, E);
   end Bind_Declarator_To_Entity;

   --------------------------------
   -- Bind_Declarators_To_Entity --
   --------------------------------

   procedure Bind_Declarators_To_Entity (D : List_Id; E : Node_Id)
   is
      N : Node_Id := First_Entity (D);
   begin
      while Present (N) loop
         Set_Declaration (N, E);
         N := Next_Entity (N);
      end loop;
   end Bind_Declarators_To_Entity;

   -------------------------------
   -- Bind_Identifier_To_Entity --
   -------------------------------

   procedure Bind_Identifier_To_Entity (N : Node_Id; E : Node_Id) is
   begin
      Set_Identifier (E, N);
      Set_Corresponding_Entity (N, E);
   end Bind_Identifier_To_Entity;

   ----------------------
   -- Check_Identifier --
   ----------------------

   procedure Check_Identifier (Ref, Def : Node_Id) is
   begin
      if Present (Ref)
        and then Present (Def)
        and then IDL_Name (Ref) /= IDL_Name (Def)
      then
         Error_Loc  (1) := Loc  (Ref);
         Error_Name (1) := Name (Def);
         Error_Loc  (2) := Loc  (Def);
         DE ("bad casing of#declared!");
      end if;
   end Check_Identifier;

   -------------------
   -- First_Homonym --
   -------------------

   function First_Homonym (N : Node_Id) return Node_Id is
      HN : constant Name_Id := Name (N);
   begin
      return Node_Id (Get_Name_Table_Info (HN));
   end First_Homonym;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := Next_Entity (N);
   begin
      Set_Next_Entity (N, E);
      Set_Next_Entity (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := First_Entity (L);
      if Entity = N then
         Set_Next_Entity (E, Entity);
         Set_First_Entity (L, E);
      else
         while Present (Entity) loop
            exit when Next_Entity (Entity) = N;
            Entity := Next_Entity (Entity);
         end loop;
         Insert_After_Node (E, Entity);
      end if;
   end Insert_Before_Node;

   --------------------------
   -- Fully_Qualified_Name --
   --------------------------

   function Fully_Qualified_Name
     (E : Node_Id;
      Separator : String := "::")
     return Name_Id is
      P : Node_Id;
   begin
      pragma Assert (Kind (E) = K_Identifier);
      P := Potential_Scope (E);
      Name_Len := 0;
      if Present (P) and then Kind (P) /= K_Specification then
         Get_Name_String (Fully_Qualified_Name (Identifier (P), Separator));
         Add_Str_To_Name_Buffer (Separator);
      end if;
      Get_Name_String_And_Append (IDL_Name (E));
      return Name_Find;
   end Fully_Qualified_Name;

   ---------------------
   -- Is_A_Forward_Of --
   ---------------------

   function Is_A_Forward_Of (X, Y : Node_Id) return Boolean is
      KX : constant Node_Kind := Kind (X);
      KY : constant Node_Kind := Kind (Y);

   begin
      case KY is
         when K_Interface_Declaration
           | K_Forward_Interface_Declaration =>
            return KX = K_Forward_Interface_Declaration;

         when K_Structure_Type
            | K_Forward_Structure_Type =>
            return KX = K_Forward_Structure_Type;

         when K_Union_Type
            | K_Forward_Union_Type =>
            return KX = K_Forward_Union_Type;

         when K_Value_Declaration =>
            return KX = K_Value_Forward_Declaration
              and then not Is_Abstract_Value (X);

         when K_Value_Forward_Declaration =>
            return KX = K_Value_Forward_Declaration
              and then (Is_Abstract_Value (X) = Is_Abstract_Value (Y));

         when K_Abstract_Value_Declaration =>
            return KX = K_Value_Forward_Declaration
              and then Is_Abstract_Value (X);

         when others =>
            return False;
      end case;
   end Is_A_Forward_Of;

   ---------------------
   -- Is_A_Local_Type --
   ---------------------

   function Is_A_Local_Type (E : Node_Id) return Boolean is
   begin
      return Kind (E) = K_Interface_Declaration
        and then Is_Local_Interface (E);
   end Is_A_Local_Type;

   ---------------------
   -- Is_A_Non_Module --
   ---------------------

   function Is_A_Non_Module (E : Node_Id) return Boolean is
      K : constant Node_Kind := Kind (E);
   begin
      return K /= K_Module and then K /= K_Specification;
   end Is_A_Non_Module;

   ----------------
   -- Is_A_Scope --
   ----------------

   function Is_A_Scope (E : Node_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Module
           | K_Enumeration_Type
           | K_Specification
           | K_Structure_Type
           | K_Union_Type
           | K_Value_Declaration
           | K_Abstract_Value_Declaration
           | K_Exception_Declaration
           | K_Interface_Declaration =>
            return True;

         when others =>
            return False;
      end case;
   end Is_A_Scope;

   ---------------
   -- Is_A_Type --
   ---------------

   function Is_A_Type (E : Node_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Type_Declaration
           | K_Forward_Structure_Type
           | K_Structure_Type
           | K_Forward_Union_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Native_Type
           | K_Sequence_Type
           | K_String
           | K_Wide_String
           | K_Fixed_Point_Type
           | K_Float
           | K_Double
           | K_Long_Double
           | K_Short
           | K_Long
           | K_Long_Long
           | K_Unsigned_Short
           | K_Unsigned_Long
           | K_Unsigned_Long_Long
           | K_Char
           | K_Wide_Char
           | K_Boolean
           | K_Octet
           | K_Object
           | K_Any
           | K_Void
           | K_Value_Base =>
            return True;
         when others =>
            return False;
      end case;
   end Is_A_Type;

   -------------------------------
   -- Is_Attribute_Or_Operation --
   -------------------------------

   function Is_Attribute_Or_Operation (E : Node_Id) return Boolean is
      K : constant Node_Kind := Kind (E);
   begin
      return K = K_Attribute_Declaration
        or else K = K_Operation_Declaration;
   end Is_Attribute_Or_Operation;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Entity (L));
   end Is_Empty;

   -------------------------------------
   -- Is_Interface_Redefinable_Node --
   -------------------------------------

   function Is_Interface_Redefinable_Node (E : Node_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Type_Declaration
           | K_Constant_Declaration
           | K_Forward_Structure_Type
           | K_Structure_Type
           | K_Forward_Union_Type
           | K_Union_Type
           | K_Enumeration_Type
           | K_Native_Type
           | K_Sequence_Type
           | K_String
           | K_Wide_String
           | K_Fixed_Point_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Interface_Redefinable_Node;

   ---------------
   -- Is_Parent --
   ---------------

   function Is_Parent
     (Parent : Node_Id;
      Child  : Node_Id;
      First  : Boolean := False)
     return Boolean
   is
      pragma Assert
        (Kind (Parent) = K_Interface_Declaration and then
         Kind (Child) = K_Interface_Declaration);

      Result : Boolean := False;
      N      : Node_Id;
   begin
      if not Is_Empty (Interface_Spec (Child)) then
         N := First_Entity (Interface_Spec (Child));
         if First then
            return Parent = Reference (N);
         else
            while Present (N) loop
               if Parent = Reference (N) then
                  Result := True;
               end if;
               N := Next_Entity (N);
            end loop;
            return Result;
         end if;
      end if;
      return False;
   end Is_Parent;

   ------------------
   -- Is_Redefined --
   ------------------

   function Is_Redefined
     (Entity : Node_Id;
      In_Interface : Node_Id)
     return Boolean
   is
      E      : Node_Id;
      Name   : Name_Id;
   begin
      pragma Assert (Kind (In_Interface) = K_Interface_Declaration);

      --  This function handles only redefinition of Types (enum, struct...),
      --  constants and exception. The operation redefinition is not allowed
      --  in IDL.
      pragma Assert (Kind (Entity) = K_Simple_Declarator
                     or else Kind (Entity) = K_Complex_Declarator
                     or else Kind (Entity) = K_Enumeration_Type
                     or else Kind (Entity) = K_Structure_Type
                     or else Kind (Entity) = K_Union_Type
                     or else Kind (Entity) = K_Constant_Declaration
                     or else Kind (Entity) = K_Exception_Declaration);

      Name := IDL_Name (Identifier (Entity));
      E := First_Entity (Interface_Body (In_Interface));
      while Present (E) loop
         case (Kind (E)) is
            when K_Type_Declaration =>
               declare
                  D : Node_Id;
               begin
                  D := First_Entity (Declarators (E));
                  while Present (D) loop
                     if Name = IDL_Name (Identifier (D)) then
                        return True;
                     end if;
                     D := Next_Entity (D);
                  end loop;
               end;

            when K_Enumeration_Type
              | K_Structure_Type
              | K_Union_Type
              | K_Constant_Declaration
              | K_Exception_Declaration =>
               if Name = IDL_Name (Identifier (E)) then
                  return True;
               end if;

            when others =>
               null;
         end case;
         E := Next_Entity (E);
      end loop;

      return False;
   end Is_Redefined;

   -------------------------------
   -- Is_Multidimensional_Array --
   -------------------------------

   function Is_Multidimensional_Array (D : Node_Id) return Boolean is
      pragma Assert (Kind (D) = K_Complex_Declarator);

      Dim : constant Natural := Length (Array_Sizes (D));
   begin
      return Dim > 1;
   end Is_Multidimensional_Array;

   ----------------------------------
   -- Get_Original_Type_Declarator --
   ----------------------------------

   function Get_Original_Type_Declarator (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      case Kind (E) is
         when K_Simple_Declarator =>
            N := Type_Spec (Declaration (E));

            if Kind (N) = K_Scoped_Name then
               return Get_Original_Type_Declarator (Reference (N));
            else
               return E;
            end if;

         when K_Scoped_Name =>
            N := Reference (E);

            if Kind (N) = K_Simple_Declarator
              or else Kind (E) = K_Complex_Declarator
            then
               return Get_Original_Type_Declarator (N);
            else
               return N;
            end if;

         when others =>
            return E;
      end case;
   end Get_Original_Type_Declarator;

   -----------------------------------
   -- Get_Original_Type_Declaration --
   -----------------------------------

   function Get_Original_Type_Declaration (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      case Kind (E) is
         when K_Complex_Declarator | K_Simple_Declarator =>
            N := Type_Spec (Declaration (E));

            if Kind (N) = K_Scoped_Name then
               return Get_Original_Type_Declaration (N);
            else
               return Declaration (E);
            end if;

         when K_Scoped_Name =>
            N := Reference (E);

            if Kind (N) = K_Simple_Declarator
              or else Kind (E) = K_Complex_Declarator
            then
               return Get_Original_Type_Declaration (N);
            else
               return N;
            end if;

         when others =>
            return No_Node;
      end case;
   end Get_Original_Type_Declaration;

   ---------------------------------
   -- Get_Original_Type_Specifier --
   ---------------------------------

   function Get_Original_Type_Specifier (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      --  If 'E' is a declarator, we handle it, else, we handle its
      --  type spec.

      case Kind (E) is
         when K_Complex_Declarator =>
            --  We don't resolve the complex declarators at this point

            return E;

         when K_Simple_Declarator =>

            --  We resolve the declaration type spec

            return Get_Original_Type_Specifier (Type_Spec (Declaration (E)));

         when K_Scoped_Name =>
            --  We rewind type spec

            --  A scoped name type designates either a declarator or
            --  an object.

            N := Reference (E);

            if Kind (N) = K_Simple_Declarator then
               --  We resolve the declaration type spec

               if Kind (Declaration (N)) = K_Native_Type then
                  return N;
               else
                  return Get_Original_Type_Specifier
                    (Type_Spec
                     (Declaration (N)));
               end if;
            else
               return N;
            end if;

         when others =>
            return E;
      end case;
   end Get_Original_Type_Specifier;

   -------------------------
   -- Has_Local_Component --
   -------------------------

   function Has_Local_Component (E : Node_Id) return Boolean is
      --  Get the original type

      Orig_Type : constant Node_Id := Get_Original_Type_Specifier (E);
   begin
      case Kind (Orig_Type) is
         when K_Interface_Declaration | K_Forward_Interface_Declaration =>
            --  For interface type, simply verify that they are local
            --  interfaces.

            return Is_Local_Interface (Orig_Type);

         when K_Complex_Declarator =>
            --  For arrays, we see whether the element type has local
            --  components.

            return Has_Local_Component (Type_Spec (Declaration (Orig_Type)));

         when K_Structure_Type | K_Exception_Declaration =>
            --  For structures and exceptions, we see whether an
            --  element of the sequence has local components.

            declare
               Result : Boolean := False;
               M      : Node_Id := First_Entity (Members (Orig_Type));
            begin
               while Present (M) loop
                  Result := Result
                    or else Has_Local_Component (Type_Spec (M));

                  M := Next_Entity (M);
               end loop;

               return Result;
            end;

         when K_Union_Type =>
            --  For unions, we see whether an element of the sequence
            --  has local components.

            declare
               Result : Boolean := False;
               S      : Node_Id := First_Entity (Switch_Type_Body (Orig_Type));
            begin
               while Present (S) loop
                  Result := Result
                    or else Has_Local_Component (Type_Spec (Element (S)));

                  S := Next_Entity (S);
               end loop;

               return Result;
            end;

         when K_Sequence_Type =>
            --  For sequences, we see whether the element type has
            --  local components.

            return Has_Local_Component (Type_Spec (Orig_Type));

         when others =>
            return False;
      end case;
   end Has_Local_Component;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
   begin
      return Natural (Size (L));
   end Length;

   -------------------------------
   -- Make_Constant_Declaration --
   -------------------------------

   function Make_Constant_Declaration
     (Loc        : Location;
      Type_Spec  : Node_Id;
      Identifier : Node_Id;
      Expression : Node_Id)
     return Node_Id
   is
      N : constant Node_Id := New_Node (K_Constant_Declaration, Loc);
   begin
      Set_Type_Spec  (N, Type_Spec);
      Set_Identifier (N, Identifier);
      Set_Expression (N, Expression);

      return N;
   end Make_Constant_Declaration;

   ---------------------
   -- Make_Identifier --
   ---------------------

   function Make_Identifier
     (Loc      : Location;
      IDL_Name : Name_Id;
      Node     : Node_Id;
      Scope_Entity    : Node_Id)
     return Node_Id
   is
      N : constant Node_Id := New_Node (K_Identifier, Loc);
   begin
      Set_Name                 (N, To_Lower (IDL_Name));
      Set_IDL_Name             (N, IDL_Name);
      Set_Corresponding_Entity (N, Node);
      Set_Scope_Entity         (N, Scope_Entity);
      Set_Potential_Scope      (N, Scope_Entity);
      return N;
   end Make_Identifier;

   ----------------------
   -- Make_Scoped_Name --
   ----------------------

   function Make_Scoped_Name
     (Loc        : Location;
      Identifier : Node_Id;
      Parent     : Node_Id;
      Reference  : Node_Id)
     return Node_Id
   is
      N : constant Node_Id := New_Node (K_Scoped_Name, Loc);
   begin
      pragma Assert (Kind (Identifier) = K_Identifier);
      Set_Identifier    (N, Identifier);
      pragma Assert (Kind (Identifier) = K_Identifier);
      Set_Parent_Entity (N, Parent);
      Set_Reference     (N, Reference);

      return N;
   end Make_Scoped_Name;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (N : Node_Id) return Node_Id
   is
      L : Node_Id;
   begin
      Entries.Increment_Last;
      L := Entries.Last;
      Entries.Table (L) := Entries.Table (N);
      Set_Loc       (L, No_Location);
      Set_Next_Entity (L, No_Node);
      if Kind (L) = K_Identifier then
         Set_Homonym (L, No_Node);
      end if;
      return L;
   end New_Copy;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      Loc  : Location)
     return List_Id
   is
   begin
      return List_Id (New_Node (Kind, Loc));
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      Loc  : Location)
     return Node_Id
   is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   --------------
   -- Operator --
   --------------

   function Operator (E : Node_Id) return Operator_Type is
      O : Operator_Id;
   begin
      O := Nodes.Operator (E);
      return Operator_Type'Val (O);
   end Operator;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (T : Token_Type) return Mode_Id is
   begin
      return Token_Type'Pos (T) - Token_Type'Pos (T_In);
   end Parameter_Mode;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (M : Mode_Id) return Token_Type is
   begin
      return Token_Type'Val (M + Token_Type'Pos (T_In));
   end Parameter_Mode;

   ---------------------
   -- Get_Pragma_Type --
   ---------------------

   function Get_Pragma_Type (T : Token_Type) return Pragma_Type is
   begin
      return Token_Type'Pos (T) - Token_Type'Pos (T_Pragma_Id);
   end Get_Pragma_Type;

   ---------------------
   -- Get_Pragma_Type --
   ---------------------

   function Get_Pragma_Type (P : Pragma_Type) return Token_Type is
   begin
      return Token_Type'Val (P + Token_Type'Pos (T_Pragma_Id));
   end Get_Pragma_Type;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;
   begin
      C := First_Entity (L);
      if C = E then
         Set_First_Entity (L, Next_Entity (E));
         if Last_Entity (L) = E then
            Set_Last_Entity (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Entity (C) = E then
               Set_Next_Entity (C, Next_Entity (E));
               if Last_Entity (L) = E then
                  Set_Last_Entity (L, C);
               end if;
               exit;
            end if;
            C := Next_Entity (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   -----------------------
   -- Set_First_Homonym --
   -----------------------

   procedure Set_First_Homonym (N : Node_Id; V : Node_Id) is
   begin
      Set_Name_Table_Info (Name (N), Int (V));
   end Set_First_Homonym;

   ------------------
   -- Set_Operator --
   ------------------

   procedure Set_Operator (E : Node_Id; O : Operator_Type) is
      B : Byte;
   begin
      B := Operator_Type'Pos (O);
      Set_Operator (E, Operator_Id (B));
   end Set_Operator;

end Frontend.Nutils;
