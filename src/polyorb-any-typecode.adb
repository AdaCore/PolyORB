------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                          T Y P E C O D E  (separate unit)                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2016, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  TypeCode operations extracted from monolithic polyorb-any.adb
--  This is a separate compilation unit for the TypeCode nested package.
--  Extracted as part of RDB-004: Decompose polyorb-any God Class

separate (PolyORB.Any)
package body TypeCode is

   ---------------------------------
   -- Support for union typecodes --
   ---------------------------------

   --  Union typecodes need an efficient way of looking up the field index
   --  corresponding to a given label value. Depending on specific union
   --  types, this can be done as a (costly) linear scan of the type code,
   --  or through a lookup table.

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Union_TC_Map'Class,
      Name => Union_TC_Map_Ptr);

   type Member_Array is array (Unsigned_Long range <>) of Long;

   type Enum_Union_TC_Map (Enum_Last : Unsigned_Long)
     is new Union_TC_Map
   with record
      Members : Member_Array (0 .. Enum_Last);
   end record;

   overriding function Label_To_Member
     (Map   : access Enum_Union_TC_Map;
      Label : Any_Container'Class) return Long;
   --  Look up field pos from label pos

   -------------------------------
   -- Default complex typecodes --
   -------------------------------

   type Default_Aggregate_Content_Ptr is
     access all Default_Aggregate_Content'Class;

   function Parameters
     (TC : TypeCode.Object_Ptr) return Default_Aggregate_Content_Ptr;
   pragma Inline (Parameters);
   --  Return a pointer to the parameters of TC

   function Get_Parameter
     (Self  : Object_Ptr;
      Index : Types.Unsigned_Long) return Any_Container_Ptr;
   --  Extract the Index'th parameter from Self

   function Get_Parameter
     (Self  : Object_Ptr;
      Index : Types.Unsigned_Long) return Object_Ptr;
   --  Special version of Get_Parameter for the case where the parameter
   --  is itself a TypeCode.

   procedure Add_Parameter (Obj : Object_Ptr; Param : Any);
   --  Add Param to Obj. Raises Program_Error if Obj is frozen

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Local_Ref) return Boolean is
   begin
      return Equal (Object_Of (Left), Object_Of (Right));
   end Equal;

   function Equal (Left, Right : Object_Ptr) return Boolean is
      Nb_Param : Unsigned_Long;
   begin
      pragma Debug
        (C, O ("Equal (TypeCode): enter, Left = "
               & Image (Left) & ", Right = " & Image (Right)));

      --  Shortcut further tests when testing for the same object

      if Left = Right then
         pragma Debug (C, O ("Equal (TypeCode): end: True, same object"));
         return True;
      end if;

      if Kind (Left) /= Kind (Right) then
         pragma Debug (C,
           O ("Equal (TypeCode): end: False, different kinds"));
         return False;
      end if;

      pragma Debug (C, O ("Equal (TypeCode): parameter number comparison"));

      Nb_Param := Parameter_Count (Right);

      if Nb_Param /= Parameter_Count (Left) then
         pragma Debug (C,
           O ("Equal (TypeCode): end: False, different param counts"));
         return False;
      end if;

      if Nb_Param = 0 then
         pragma Debug (C, O ("Equal (TypeCode): end: True"));
         return True;
      end if;

      --  Recursive comparison

      pragma Debug (C, O ("Equal (TypeCode): recursive comparison"));

      for J in 0 .. Nb_Param - 1 loop
         if not "=" (Any_Container'Class'(Get_Parameter (Left, J).all),
                     Any_Container'Class'(Get_Parameter (Right, J).all))
         then
            pragma Debug (C,
              O ("Equal (TypeCode): end: False, param"
              & J'Img & " differs"));
            return False;
         end if;
      end loop;

      pragma Debug (C, O ("Equal (TypeCode): end: True, all params match"));
      return True;
   end Equal;

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter (Self  : Local_Ref; Param : Any) is
   begin
      Add_Parameter (Object_Of (Self), Param);
   end Add_Parameter;

   procedure Add_Parameter (Obj : Object_Ptr; Param : Any) is
   begin
      pragma Debug (C, O ("Add_Parameter: enter"));

      if Obj.Frozen then
         raise Program_Error with "TypeCode already frozen";
      end if;

      pragma Debug (C, O ("Add_Parameter: adding " & Image (Param)));

      if Obj.Parameters = null then
         Obj.Parameters :=
           Allocate_Default_Aggregate_Content (Tk_TypeCode);
      end if;

      Add_Aggregate_Element
        (Default_Aggregate_Content (Obj.Parameters.all),
         Get_Container (Param));
      pragma Debug (C, O ("Add_Parameter: end"));
   end Add_Parameter;

   ----------------------
   -- Build_Complex_TC --
   ----------------------

   function Build_Complex_TC
     (Kind       : TCKind;
      Parameters : Any_Array) return Local_Ref
   is
      Obj : constant Object_Ptr := new Object (Kind);
      Res : Local_Ref;
   begin
      Set (Res, Smart_Pointers.Entity_Ptr (Obj));
      for J in Parameters'Range loop
         TypeCode.Add_Parameter (Res, Parameters (J));
      end loop;
      Freeze (Obj);
      return Res;
   end Build_Complex_TC;

   -----------------------
   -- Build_Sequence_TC --
   -----------------------

   function Build_Sequence_TC
     (Element_TC : TypeCode.Local_Ref;
      Max        : Natural) return Local_Ref
   is
   begin
      return Build_Complex_TC (Tk_Sequence,
        (To_Any (Types.Unsigned_Long (Max)), To_Any (Element_TC)));
   end Build_Sequence_TC;

   ------------------------
   -- Concrete_Base_Type --
   ------------------------

   function Concrete_Base_Type (Self : Local_Ref) return Local_Ref is
   begin
      return To_Ref (Concrete_Base_Type (Object_Of (Self)));
   end Concrete_Base_Type;

   function Concrete_Base_Type (Self : Object_Ptr) return Object_Ptr is
   begin
      case Kind (Self) is
         when Tk_Value | Tk_Event =>
            return Get_Parameter (Self, 3);

         when others =>
            raise BadKind;
      end case;
   end Concrete_Base_Type;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Self : Local_Ref) return Local_Ref is
   begin
      return To_Ref (Content_Type (Object_Of (Self)));
   end Content_Type;

   function Content_Type (Self : Object_Ptr) return Object_Ptr is
   begin
      case Kind (Self) is
         when Tk_Sequence
           | Tk_Array =>
            return Get_Parameter (Self, 1);

         when Tk_Valuebox
           | Tk_Alias =>
            return Get_Parameter (Self, 2);

         when others =>
            raise BadKind;
      end case;
   end Content_Type;

   ---------------------
   -- Build_String_TC --
   ---------------------

   function Build_String_TC
     (Max : Types.Unsigned_Long) return TypeCode.Local_Ref
   is
   begin
      return Build_Complex_TC (Tk_String, (1 => To_Any (Max)));
   end Build_String_TC;

   ----------------------
   -- Build_Wstring_TC --
   ----------------------

   function Build_Wstring_TC
     (Max : Types.Unsigned_Long) return Local_Ref
   is
   begin
      return Build_Complex_TC (Tk_Wstring, (1 => To_Any (Max)));
   end Build_Wstring_TC;

   -------------------
   -- Default_Index --
   -------------------

   function Default_Index (Self : Local_Ref) return Types.Long is
   begin
      return Default_Index (Object_Of (Self));
   end Default_Index;

   function Default_Index (Self : Object_Ptr) return Types.Long is
   begin

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any.TypeCode to understand the magic
      --  numbers used here.

      case Kind (Self) is
         when Tk_Union =>
            return From_Any (Get_Parameter (Self, 3).all);

         when others =>
            raise BadKind;
      end case;
   end Default_Index;

   --------------------------
   -- Disable_Ref_Counting --
   --------------------------

   procedure Disable_Ref_Counting (Self : in out Object) is
   begin
      Smart_Pointers.Disable_Ref_Counting (Self);
   end Disable_Ref_Counting;

   ------------------------
   -- Discriminator_Type --
   ------------------------

   function Discriminator_Type (Self : Local_Ref) return Local_Ref is
   begin
      return To_Ref (Discriminator_Type (Object_Of (Self)));
   end Discriminator_Type;

   function Discriminator_Type (Self : Object_Ptr) return Object_Ptr is
   begin

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any.TypeCode to understand the magic
      --  numbers used here.

      case Kind (Self) is
         when Tk_Union =>
            return Get_Parameter (Self, 2);

         when others =>
            raise BadKind;
      end case;
   end Discriminator_Type;

   ---------------------
   -- Enumerator_Name --
   ---------------------

   function Enumerator_Name
     (Self : Local_Ref; Index : Unsigned_Long) return Types.Identifier
   is
   begin
      return Enumerator_Name (Object_Of (Self), Index);
   end Enumerator_Name;

   function Enumerator_Name
     (Self : Object_Ptr; Index : Unsigned_Long) return Types.Identifier
   is
      Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
   begin
      case Kind (Self) is

         when Tk_Enum =>
            if Param_Nb < Index + 3 then
               raise Bounds;
            end if;

            return Types.Identifier
              (Types.String'
                 (From_Any (Get_Parameter (Self, Index + 2).all)));

         when others =>
            raise BadKind;
      end case;
   end Enumerator_Name;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Local_Ref) return Boolean is
   begin
      return Equivalent (Object_Of (Left), Object_Of (Right));
   end Equivalent;

   function Equivalent (Left, Right : Object_Ptr) return Boolean is
      Nb_Param : constant Unsigned_Long := Member_Count (Left);
      pragma Assert (Nb_Param > 0);

      U_Left  : Object_Ptr := Left;
      U_Right : Object_Ptr := Right;
   begin
      --  comments are from the spec CORBA v2.3 - 10.7.1
      --  If the result of the kind operation on either TypeCode is
      --  tk_alias, recursively replace the TypeCode with the result of
      --  calling content_type, until the kind is no longer tk_alias.

      while Kind (U_Left) = Tk_Alias loop
         U_Left := Content_Type (U_Left);
      end loop;

      while Kind (U_Right) = Tk_Alias loop
         U_Right := Content_Type (U_Right);
      end loop;

      --  TypeCodes of differents kinds are never equivalent

      if Kind (U_Left) /= Kind (U_Right) then
         return False;
      end if;

      --  If the id operation is valid for the TypeCode kind, equivalent
      --  returns TRUE if the results of id for both TypeCodes are non-empty
      --  strings and both strings are equal. If both ids are non-empty but
      --  are not equal, then equivalent returns FALSE.

      case Kind (U_Left) is
         when
           Tk_Objref              |
           Tk_Struct              |
            Tk_Union              |
            Tk_Enum               |
            Tk_Value              |
            Tk_Valuebox           |
            Tk_Native             |
            Tk_Abstract_Interface |
            Tk_Except             =>

            declare
               Id_Left  : constant RepositoryId := Id (Left);
               Id_Right : constant RepositoryId := Id (Right);
               Null_RepositoryId : constant RepositoryId :=
                 RepositoryId'(To_PolyORB_String (""));
            begin
               if Id_Left /= Null_RepositoryId
                 and then Id_Right /= Null_RepositoryId
               then
                  return Id_Left = Id_Right;
               end if;
            end;

         when others =>
            null;
      end case;

      --  If either or both id is an empty string, or the TypeCode kind does
      --  not support the id operation, equivalent will perform structural
      --  comparison of the TypeCodes by comparing the results of the other
      --  TypeCode operations in the following bullet items (ignoring
      --  aliases as described in the first bullet.). The structural
      --  comparison only calls operations that are valid for the given
      --  TypeCode kind. If any of these operations do not return equal
      --  results, then equivalent returns FALSE. If all comparisons are
      --  equal, equivalent returns true.
      --    * The results of the name and member_name operations are ignored
      --      and not compared.
      --    * The results of the member_count operation are compared.

      case Kind (Left) is
         when
           Tk_Struct |
           Tk_Union  |
           Tk_Enum   |
           Tk_Value  |
           Tk_Except =>

            if Member_Count (Left) /= Member_Count (Right) then
               return False;
            end if;

         when others =>
            null;
      end case;

      --    * The results of the member_type operation for each member
      --      index are compared by recursively calling equivalent.

      case Kind (Left) is
         when
           Tk_Struct |
           Tk_Union  |
           Tk_Value  |
           Tk_Except =>

            for J in 0 .. Nb_Param - 1 loop
               if not Equivalent (Member_Type (Left, J),
                                  Member_Type (Right, J))
               then
                  return False;
               end if;
            end loop;

         when others =>
            null;
      end case;

      --    * The results of the member_label operation for each member
      --  index of a union TypeCode are compared for equality. Note that
      --  this means that unions whose members are not defined in the same
      --  order are not considered structurally equivalent.

      if Kind (Left) = Tk_Union then
         for J in 0 .. Nb_Param - 1 loop
            if Types.Long (J) /= Default_Index (Left)
              and then Types.Long (J) /= Default_Index (Right)
              and then Member_Label (Left, J).all
                    /= Member_Label (Right, J).all
            then
               return False;
            end if;
         end loop;
      end if;

      --    * The results of the discriminator_type operation are compared
      --  by recursively calling equivalent.

      if Kind (Left) = Tk_Union
        and then not Equivalent (Discriminator_Type (Left),
                                 Discriminator_Type (Right))
      then
         return False;
      end if;

      --    * The results of the default_index operation are compared.

      if Kind (Left) = Tk_Union
        and then Default_Index (Left) > -1
        and then Default_Index (Right) > -1
        and then Default_Index (Left) /= Default_Index (Right)
      then
         return False;
      end if;

      --    * The results of the length operation are compared.

      case Kind (Left) is
         when
           Tk_String   |
           Tk_Sequence |
           Tk_Array    =>

            if Length (Left) /= Length (Right) then
               return False;
            end if;

         when others =>
            null;
      end case;

      --    * The results of the discriminator_type operation are compared
      --  by recursively calling equivalent.

      case Kind (Left) is
         when
           Tk_Sequence |
           Tk_Array    |
           Tk_Valuebox =>

            if not Equivalent (Content_Type (Left),
                               Content_Type (Right))
            then
               return False;
            end if;
         when others =>
            null;
      end case;

      --    * The results of the digits and scale operations are compared.

      if Kind (Left) = Tk_Fixed then
         if Fixed_Digits (Left) /= Fixed_Digits (Right)
           or else Fixed_Scale (Left) /= Fixed_Scale (Right)
         then
            return False;
         end if;
      end if;

      --  not in spec but to be compared

      if Kind (Left) = Tk_Value then

         --  member_visibility

         for J in 0 .. Nb_Param - 1 loop
            if Member_Visibility (Left, J) /=
              Member_Visibility (Right, J)
            then
               return False;
            end if;
         end loop;

         --  type_modifier

         if Type_Modifier (Left) /= Type_Modifier (Right) then
            return False;
         end if;

         --  concrete base type

         if not Equivalent (Concrete_Base_Type (Left),
                            Concrete_Base_Type (Right))
         then
            return False;
         end if;
      end if;

      --  All structure parameters are equivalent

      return True;
   end Equivalent;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Obj : Object_Ptr) is
   begin
      Obj.Frozen := True;

      case Obj.Kind is
         when Tk_Union =>
            --  For a union typecode with enum discriminant, build a
            --  pos-to-field lookup table.

            declare
               Disc_TC   : constant Object_Ptr := Discriminator_Type (Obj);
               Disc_Kind : constant TCKind := Kind (Disc_TC);
               Enum_Len  : Unsigned_Long;
            begin
               case Disc_Kind is
                  when Tk_Enum | Tk_Boolean =>
                     if Disc_Kind = Tk_Boolean then
                        Enum_Len := 2;
                     else
                        Enum_Len := Member_Count (Disc_TC);
                     end if;

                     declare
                        Param_Nb     : constant Unsigned_Long :=
                          Parameter_Count (Obj);
                        Def_Index    : constant Long := Default_Index (Obj);
                        New_Map_Ptr  : constant Union_TC_Map_Ptr :=
                          new Enum_Union_TC_Map'
                            (Enum_Last => Enum_Len - 1,
                             Members   => (others => Def_Index));
                        New_Map      : Enum_Union_TC_Map
                        renames Enum_Union_TC_Map (New_Map_Ptr.all);
                     begin
                        --  Build lookup table

                        for J in 0 .. (Param_Nb - 4) / 3 - 1 loop

                           --  Note: we must skip the assignment for the
                           --  default member, because it has a dummy label
                           --  value.

                           if Long (J) /= Def_Index then
                              New_Map.Members
                                (Pos_From_Any
                                   (Member_Label (Obj, J).all)) := Long (J);
                           end if;
                        end loop;

                        --  Install new mapper in union typecode (replacing
                        --  the original scan-based mapper provided by the
                        --  typecode itself).

                        Obj.Map := New_Map_Ptr;
                     end;

                  when others =>
                     --  ??? For integer discriminants with an adequately
                     --  small range, a lookup-table-based mapper would
                     --  afford improved performance. Right now however
                     --  we still use the default linear scan mapper for
                     --  all types with non-enumerated discriminants.

                     null;
               end case;
            end;

         when others =>
            null;
      end case;
   end Freeze;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is
   begin
      pragma Debug (C, O ("Finalize (TypeCode.Object): enter"));

      if Self.Parameters /= null then
         Finalize_Value (Self.Parameters.all);
         Free (Self.Parameters);
      end if;

      if Self.Kind = Tk_Union
        and then Self.Map /= Union_TC_Map'Class (Self)'Unchecked_Access
      then
         Free (Union_TC_Map_Ptr (Self.Map));
      end if;

      pragma Debug (C, O ("Finalize (TypeCode.Object): leave"));
   end Finalize;

   ------------------
   -- Fixed_Digits --
   ------------------

   function Fixed_Digits (Self : Local_Ref) return Unsigned_Short is
   begin
      return Fixed_Digits (Object_Of (Self));
   end Fixed_Digits;

   function Fixed_Digits (Self : Object_Ptr) return Unsigned_Short is
   begin
      case Kind (Self) is
         when Tk_Fixed =>
            return From_Any (Get_Parameter (Self, 0).all);

         when others =>
            raise BadKind;
      end case;
   end Fixed_Digits;

   -----------------
   -- Fixed_Scale --
   -----------------

   function Fixed_Scale (Self : Local_Ref) return Short is
   begin
      return Fixed_Scale (Object_Of (Self));
   end Fixed_Scale;

   function Fixed_Scale (Self : Object_Ptr) return Short is
   begin
      case Kind (Self) is
         when Tk_Fixed =>
            return From_Any (Get_Parameter (Self, 1).all);

         when others =>
            raise BadKind;
      end case;
   end Fixed_Scale;

   -------------------
   -- Get_Parameter --
   -------------------

   function Get_Parameter
     (Self : Object_Ptr; Index : Unsigned_Long) return Any_Container_Ptr
   is
      Int_Index : constant Integer := Integer (Index);
      T         : Content_Table renames Parameters (Self).V;
   begin
      if Int_Index > Content_Tables.Last (T) then
         raise Bounds;
      end if;
      return T.Table (Int_Index);
   end Get_Parameter;

   function Get_Parameter
     (Self : Object_Ptr; Index : Unsigned_Long) return Object_Ptr
   is
      TC_Container : constant Any_Container_Ptr :=
        Get_Parameter (Self, Index);
   begin
      --  Here we have an Any that contains a TypeCode. We extract the
      --  inner TypeCode.Object_Ptr directly rather than doing a From_Any
      --  to avoid having to do a costly adjust operation on a
      --  TypeCode.Local_Ref.

      return Object_Of
        (Elementary_Any_TypeCode.Unchecked_Get_V
         (Elementary_Any_TypeCode.T_Content
          (TC_Container.The_Value.all)'Access).all);
   exception
      when E : Constraint_Error =>
         pragma Debug (C, O ("C_E (" & Ada.Exceptions.Exception_Message (E)
           & ") raised getting parameter" & Index'Img
           & " on a " & Kind (Self)'Img));
         pragma Debug (C,
           O ("Expected "
              & Ada.Tags.External_Tag
                  (Elementary_Any_TypeCode.T_Content'Tag)
              & ", got "
              & Ada.Tags.External_Tag (TC_Container.The_Value'Tag)));
         raise;
   end Get_Parameter;

   --------
   -- Id --
   --------

   function Id (Self : Local_Ref) return RepositoryId is
   begin
      return Id (Object_Of (Self));
   end Id;

   function Id (Self : Object_Ptr) return RepositoryId is
   begin
      case Kind (Self) is
         when Tk_Objref
           | Tk_Struct
           | Tk_Union
           | Tk_Enum
           | Tk_Alias
           | Tk_Except
           | Tk_Value
           | Tk_Valuebox
           | Tk_Native
           | Tk_Abstract_Interface
           | Tk_Local_Interface
           | Tk_Component
           | Tk_Home
           | Tk_Event =>
            return RepositoryId
              (Types.String'(From_Any (Get_Parameter (Self, 1).all)));

         when others =>
            raise BadKind;
      end case;
   end Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Set parameters of default complex typecodes

      Add_Parameter (PTC_String'Access,      To_Any (Unsigned_Long'(0)));

      Add_Parameter (PTC_Wide_String'Access, To_Any (Unsigned_Long'(0)));

      Add_Parameter (PTC_RootObject'Access, To_Any ("Object"));
      Add_Parameter (PTC_RootObject'Access, To_Any ("PolyORB:Object:1.0"));

      --  Do not ref count / garbage collect our library-level root TCs

      Smart_Pointers.Disable_Ref_Counting (PTC_Null);
      Smart_Pointers.Disable_Ref_Counting (PTC_Void);
      Smart_Pointers.Disable_Ref_Counting (PTC_Short);
      Smart_Pointers.Disable_Ref_Counting (PTC_Long);
      Smart_Pointers.Disable_Ref_Counting (PTC_Long_Long);
      Smart_Pointers.Disable_Ref_Counting (PTC_Unsigned_Short);
      Smart_Pointers.Disable_Ref_Counting (PTC_Unsigned_Long);
      Smart_Pointers.Disable_Ref_Counting (PTC_Unsigned_Long_Long);
      Smart_Pointers.Disable_Ref_Counting (PTC_Float);
      Smart_Pointers.Disable_Ref_Counting (PTC_Double);
      Smart_Pointers.Disable_Ref_Counting (PTC_Long_Double);
      Smart_Pointers.Disable_Ref_Counting (PTC_Boolean);
      Smart_Pointers.Disable_Ref_Counting (PTC_Char);
      Smart_Pointers.Disable_Ref_Counting (PTC_Wchar);
      Smart_Pointers.Disable_Ref_Counting (PTC_Octet);
      Smart_Pointers.Disable_Ref_Counting (PTC_Any);
      Smart_Pointers.Disable_Ref_Counting (PTC_TypeCode);
      Smart_Pointers.Disable_Ref_Counting (PTC_String);
      Smart_Pointers.Disable_Ref_Counting (PTC_Wide_String);
      Smart_Pointers.Disable_Ref_Counting (PTC_RootObject);
   end Initialize;

   ------------
   -- Is_Nil --
   ------------

   overriding function Is_Nil (Self : Local_Ref) return Boolean is
   begin
      return Smart_Pointers.Is_Nil (Smart_Pointers.Ref (Self));
   end Is_Nil;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Local_Ref) return TCKind is
   begin
      return Kind (Object_Of (Self));
   end Kind;

   function Kind (Self : Object_Ptr) return TCKind is
   begin
      --  An unset typecode reference is considered to be equivalent to a
      --  void typecode (this is a small optimization, so that personalities
      --  need not set a typecode for the reply of void-valued operations).

      if Self = null then
         return Tk_Void;
      else
         return Self.Kind;
      end if;
   end Kind;

   ---------------------
   -- Label_To_Member --
   ---------------------

   overriding function Label_To_Member
     (Map   : access Object;
      Label : Any_Container'Class) return Long
   is
      Encl_TC      : constant Object_Ptr := Object_Ptr (Map);
      Param_Nb     : constant Unsigned_Long :=
                       Parameter_Count (Encl_TC);
      Def_Index    : constant Long :=
                       Default_Index (Encl_TC);
      Member_Index : Long := -1;

   begin
      --  Default version of Label_To_Member: linear scan of typecode
      --  (horribly inefficient???)

      Parameters :
      for J in 0 .. (Param_Nb - 4) / 3 - 1 loop

         --  We ignore the default member, as its placeholder label could
         --  interfere with a non-default label.

         if Long (J) /= Def_Index
              and then
             Any_Container_Eq
               (Discriminator_Type (Encl_TC),
                Member_Label (Encl_TC, J).all,
                Label)
         then
            Member_Index := Long (J);
            exit Parameters;
         end if;
      end loop Parameters;

      if Member_Index = -1 then
         Member_Index := Def_Index;
      end if;

      return Member_Index;
   end Label_To_Member;

   overriding function Label_To_Member
     (Map   : access Enum_Union_TC_Map;
      Label : Any_Container'Class) return Long
   is
   begin
      return Map.Members (Pos_From_Any (Label));
   end Label_To_Member;

   ------------
   -- Length --
   ------------

   function Length (Self : Local_Ref) return Unsigned_Long is
   begin
      return Length (Object_Of (Self));
   end Length;

   function Length (Self : Object_Ptr) return Unsigned_Long is
      TK : constant TCKind := TypeCode.Kind (Self);
   begin
      case TK is
         when Tk_String
           | Tk_Wstring
           | Tk_Sequence
           | Tk_Array =>
            return From_Any (Get_Parameter (Self, 0).all);

         when others =>
            pragma Debug (C, O ("Length: no such attribute for " & TK'Img));
            raise BadKind;
      end case;
   end Length;

   ------------------
   -- Member_Count --
   ------------------

   function Member_Count (Self : Local_Ref) return Unsigned_Long is
   begin
      return Member_Count (Object_Of (Self));
   end Member_Count;

   function Member_Count (Self : Object_Ptr) return Unsigned_Long is
      Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
   begin

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any to understand the magic numbers
      --  returned here.

      case Kind (Self) is
         when Tk_Struct
           | Tk_Except =>
            return (Param_Nb / 2) - 1;

         when Tk_Union =>
            return (Param_Nb - 4) / 3;

         when Tk_Enum =>
            return Param_Nb - 2;

         when Tk_Value
           | Tk_Event =>
            return (Param_Nb - 4) / 3;

         when others =>
            raise BadKind;
      end case;
   end Member_Count;

   ------------------
   -- Member_Label --
   ------------------

   function Member_Label
     (Self : Local_Ref; Index : Unsigned_Long) return Any
   is
   begin
      return Member_Label (Object_Of (Self), Index);
   end Member_Label;

   ------------------
   -- Member_Label --
   ------------------

   function Member_Label
     (Self : Local_Ref; Index : Unsigned_Long) return Any_Container_Ptr
   is
   begin
      return Member_Label (Object_Of (Self), Index);
   end Member_Label;

   ------------------
   -- Member_Label --
   ------------------

   function Member_Label
     (Self : Object_Ptr; Index : Unsigned_Long) return Any
   is
      Result : Any;
   begin
      Set (Result,
        Smart_Pointers.Entity_Ptr
          (Any_Container_Ptr'(Member_Label (Self, Index))));
      return Result;
   end Member_Label;

   ------------------
   -- Member_Label --
   ------------------

   function Member_Label
     (Self : Object_Ptr; Index : Unsigned_Long) return Any_Container_Ptr
   is
      Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
   begin
      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any.TypeCode to understand the magic
      --  numbers used here.

      case Kind (Self) is
         when Tk_Union =>
            if Param_Nb < 3 * Index + 7 then
               raise Bounds;
            end if;
            return Get_Parameter (Self, 3 * Index + 4);

         when others =>
            raise BadKind;
      end case;
   end Member_Label;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name
     (Self : Local_Ref; Index : Unsigned_Long) return Identifier
   is
   begin
      return Member_Name (Object_Of (Self), Index);
   end Member_Name;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name
     (Self : Object_Ptr; Index : Unsigned_Long) return Identifier
   is
      Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      Res      : PolyORB.Types.String;
   begin

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any to understand the magic numbers used
      --  here.

      case Kind (Self) is
         when Tk_Struct
           | Tk_Except =>
            if Param_Nb < 2 * Index + 4 then
               raise Bounds;
            end if;
            Res := From_Any (Get_Parameter (Self, 2 * Index + 3).all);
            return Identifier (Res);

         when Tk_Union | Tk_Value | Tk_Event =>
            if Param_Nb < 3 * Index + 7 then
               raise Bounds;
            end if;
            Res := From_Any (Get_Parameter (Self, 3 * Index + 6).all);
            return Identifier (Res);

         when Tk_Enum =>
            if Param_Nb < Index + 3 then
               raise Bounds;
            end if;
            Res := From_Any (Get_Parameter (Self, Index + 2).all);
            return Identifier (Res);

         when others =>
            raise BadKind;
      end case;
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type
     (Self : Local_Ref; Index : Unsigned_Long) return Local_Ref
   is
   begin
      return To_Ref (Member_Type (Object_Of (Self), Index));
   end Member_Type;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type
     (Self : Object_Ptr; Index : Unsigned_Long) return Object_Ptr
   is
      Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      K        : constant TCKind := Kind (Self);
   begin
      pragma Debug (C, O ("Member_Type: enter, Kind is " & TCKind'Image (K)
        & Param_Nb'Img & " parameters"));

      --  See the big explanation after the declaration of TypeCode.Object
      --  in the private part of PolyORB.Any.TypeCode to understand the
      --  magic numbers used here.

      case K is
         when Tk_Struct
           |  Tk_Except =>
            if Param_Nb < 2 * Index + 4 then
               raise Bounds;
            end if;
            return Get_Parameter (Self, 2 * Index + 2);

         when Tk_Union | Tk_Value | Tk_Event =>
            if Param_Nb < 3 * Index + 7 then
               raise Bounds;
            end if;
            return Get_Parameter (Self, 3 * Index + 5);

         when others =>
            raise BadKind;
      end case;
   end Member_Type;

   ----------------------------
   -- Member_Type_With_Label --
   ----------------------------

   function Member_Type_With_Label
     (Self  : Local_Ref;
      Label : Any) return Local_Ref
   is
   begin
      return To_Ref (Member_Type_With_Label (Object_Of (Self),
        Get_Container (Label).all));
   end Member_Type_With_Label;

   ----------------------------
   -- Member_Type_With_Label --
   ----------------------------

   function Member_Type_With_Label
     (Self  : Object_Ptr;
      Label : Any) return Object_Ptr
   is
   begin
      return Member_Type_With_Label (Self, Get_Container (Label).all);
   end Member_Type_With_Label;

   ----------------------------
   -- Member_Type_With_Label --
   ----------------------------

   function Member_Type_With_Label
     (Self  : Local_Ref;
      Label : Any_Container'Class) return Local_Ref
   is
   begin
      return To_Ref (Member_Type_With_Label (Object_Of (Self), Label));
   end Member_Type_With_Label;

   ----------------------------
   -- Member_Type_With_Label --
   ----------------------------

   function Member_Type_With_Label
     (Self  : Object_Ptr;
      Label : Any_Container'Class) return Object_Ptr
   is
      Member_Index : Long;

   begin
      pragma Debug (C, O ("Member_Type_With_Label: enter"));

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any.TypeCode to understand the magic
      --  numbers used here.

      if Kind (Self) /= Tk_Union then
         raise BadKind;
      end if;

      Member_Index := Self.Map.Label_To_Member (Label);
      pragma Debug
        (C, O ("Member_Type_With_Label: found index " & Member_Index'Img));

      if Member_Index = -1 then

         --  No member with this label: return void type

         return PTC_Void'Access;
      end if;

      return Get_Parameter (Self, 3 * Unsigned_Long (Member_Index) + 5);
   end Member_Type_With_Label;

   -----------------------
   -- Member_Visibility --
   -----------------------

   function Member_Visibility
     (Self : Local_Ref; Index : Unsigned_Long) return Visibility is
   begin
      return Member_Visibility (Object_Of (Self), Index);
   end Member_Visibility;

   function Member_Visibility
     (Self : Object_Ptr; Index : Unsigned_Long) return Visibility is
   begin

      --  See comments after the declaration of TypeCode.Object in the
      --  private part of PolyORB.Any.TypeCode to understand the magic
      --  numbers used here.

      case Kind (Self) is
         when Tk_Value | Tk_Event =>
            declare
               Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
            begin
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;

               return Visibility
                 (Short'
                    (From_Any (Get_Parameter (Self, 3 * Index + 3).all)));
            end;

         when others =>
            raise BadKind;
      end case;
   end Member_Visibility;

   ----------
   -- Name --
   ----------

   function Name (Self : Local_Ref) return Identifier is
   begin
      return Name (Object_Of (Self));
   end Name;

   function Name (Self : Object_Ptr) return Identifier is
      TCK : constant TCKind := Kind (Self);
   begin
      case TCK is
         when Tk_Objref
           | Tk_Struct
           | Tk_Union
           | Tk_Enum
           | Tk_Alias
           | Tk_Except
           | Tk_Value
           | Tk_Valuebox
           | Tk_Native
           | Tk_Abstract_Interface
           | Tk_Local_Interface
           | Tk_Component
           | Tk_Home
           | Tk_Event =>
            declare
               Res : PolyORB.Types.String;
            begin
               Res := From_Any (Get_Parameter (Self, 0).all);
               return Identifier (Res);
            end;

         when others =>
            raise BadKind with TCK'Img & " TypeCode has no name";
      end case;
   end Name;

   ---------------
   -- Object_Of --
   ---------------

   function Object_Of (Self : Local_Ref) return Object_Ptr is
   begin
      return Object_Ptr (Entity_Of (Self));
   end Object_Of;

   ---------------------
   -- Parameter_Count --
   ---------------------

   function Parameter_Count (Self : Local_Ref) return Unsigned_Long is
   begin
      return Parameter_Count (Object_Of (Self));
   end Parameter_Count;

   function Parameter_Count (Self : Object_Ptr) return Unsigned_Long is
      use Content_Tables;
      ACC : constant Default_Aggregate_Content_Ptr := Parameters (Self);
   begin
      if ACC = null then
         return 0;
      else
         return Types.Unsigned_Long (Last (ACC.V) - First (ACC.V) + 1);
      end if;
   end Parameter_Count;

   ----------------
   -- Parameters --
   ----------------

   function Parameters
     (TC : TypeCode.Object_Ptr) return Default_Aggregate_Content_Ptr
   is
   begin
      return Default_Aggregate_Content_Ptr (TC.Parameters);
   end Parameters;

   -------------
   -- TC_Null --
   -------------

   function TC_Null return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Null'Access);
   end TC_Null;

   -------------
   -- TC_Void --
   -------------

   function TC_Void return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Void'Access);
   end TC_Void;

   --------------
   -- TC_Short --
   --------------

   function TC_Short return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Short'Access);
   end TC_Short;

   -------------
   -- TC_Long --
   -------------

   function TC_Long return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Long'Access);
   end TC_Long;

   ------------------
   -- TC_Long_Long --
   ------------------

   function TC_Long_Long return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Long_Long'Access);
   end TC_Long_Long;

   -----------------------
   -- TC_Unsigned_Short --
   -----------------------

   function TC_Unsigned_Short return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Unsigned_Short'Access);
   end TC_Unsigned_Short;

   ----------------------
   -- TC_Unsigned_Long --
   ----------------------

   function TC_Unsigned_Long return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Unsigned_Long'Access);
   end TC_Unsigned_Long;

   ---------------------------
   -- TC_Unsigned_Long_Long --
   ---------------------------

   function TC_Unsigned_Long_Long return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Unsigned_Long_Long'Access);
   end TC_Unsigned_Long_Long;

   --------------
   -- TC_Float --
   --------------

   function TC_Float return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Float'Access);
   end TC_Float;

   ---------------
   -- TC_Double --
   ---------------

   function TC_Double return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Double'Access);
   end TC_Double;

   --------------------
   -- TC_Long_Double --
   --------------------

   function TC_Long_Double return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Long_Double'Access);
   end TC_Long_Double;

   ----------------
   -- TC_Boolean --
   ----------------

   function TC_Boolean return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Boolean'Access);
   end TC_Boolean;

   -------------
   -- TC_Char --
   -------------

   function TC_Char return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Char'Access);
   end TC_Char;

   --------------
   -- TC_Wchar --
   --------------

   function TC_Wchar return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Wchar'Access);
   end TC_Wchar;

   --------------
   -- TC_Octet --
   --------------

   function TC_Octet return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Octet'Access);
   end TC_Octet;

   ------------
   -- TC_Any --
   ------------

   function TC_Any return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_Any'Access);
   end TC_Any;

   -----------------
   -- TC_TypeCode --
   -----------------

   function TC_TypeCode return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_TypeCode'Access);
   end TC_TypeCode;

   ---------------
   -- TC_String --
   ---------------

   function TC_String return Local_Ref is
   begin
      return To_Ref (PTC_String'Access);
   end TC_String;

   --------------------
   -- TC_Wide_String --
   --------------------

   function TC_Wide_String return Local_Ref is
   begin
      return To_Ref (PTC_Wide_String'Access);
   end TC_Wide_String;

   -------------------
   -- TC_RootObject --
   -------------------

   function TC_RootObject return TypeCode.Local_Ref is
   begin
      return To_Ref (PTC_RootObject'Access);
   end TC_RootObject;

   -------------------
   -- TCF_Principal --
   -------------------

   function TCF_Principal return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Principal));
   end TCF_Principal;

   ----------------
   -- TCF_Struct --
   ----------------

   function TCF_Struct return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Struct));
   end TCF_Struct;

   ---------------
   -- TCF_Union --
   ---------------

   function TCF_Union return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Union));
   end TCF_Union;

   --------------
   -- TCF_Enum --
   --------------

   function TCF_Enum return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Enum));
   end TCF_Enum;

   ---------------
   -- TCF_Alias --
   ---------------

   function TCF_Alias return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Alias));
   end TCF_Alias;

   ----------------
   -- TCF_Except --
   ----------------

   function TCF_Except return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Except));
   end TCF_Except;

   ----------------
   -- TCF_Object --
   ----------------

   function TCF_Object return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Objref));
   end TCF_Object;

   ---------------
   -- TCF_Fixed --
   ---------------

   function TCF_Fixed return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Fixed));
   end TCF_Fixed;

   ------------------
   -- TCF_Sequence --
   ------------------

   function TCF_Sequence return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Sequence));
   end TCF_Sequence;

   ---------------
   -- TCF_Array --
   ---------------

   function TCF_Array return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Array));
   end TCF_Array;

   ---------------
   -- TCF_Value --
   ---------------

   function TCF_Value return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Value));
   end TCF_Value;

   ------------------
   -- TCF_Valuebox --
   ------------------

   function TCF_Valuebox return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Valuebox));
   end TCF_Valuebox;

   ----------------
   -- TCF_Native --
   ----------------

   function TCF_Native return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Native));
   end TCF_Native;

   ----------------------------
   -- TCF_Abstract_Interface --
   ----------------------------

   function TCF_Abstract_Interface return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Abstract_Interface));
   end TCF_Abstract_Interface;

   -------------------------
   -- TCF_Local_Interface --
   -------------------------

   function TCF_Local_Interface return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Local_Interface));
   end TCF_Local_Interface;

   -------------------
   -- TCF_Component --
   -------------------

   function TCF_Component return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Component));
   end TCF_Component;

   --------------
   -- TCF_Home --
   --------------

   function TCF_Home return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Home));
   end TCF_Home;

   ---------------
   -- TCF_Event --
   ---------------

   function TCF_Event return TypeCode.Local_Ref is
   begin
      return To_Ref (new Object (Tk_Event));
   end TCF_Event;

   ------------
   -- To_Ref --
   ------------

   function To_Ref (Self : Object_Ptr) return Local_Ref is
      Result : Local_Ref;
   begin
      Set (Result, Smart_Pointers.Entity_Ptr (Self));
      return Result;
   end To_Ref;

   -------------------
   -- Type_Modifier --
   -------------------

   function Type_Modifier (Self : Local_Ref) return ValueModifier is
   begin
      return Type_Modifier (Object_Of (Self));
   end Type_Modifier;

   function Type_Modifier (Self : Object_Ptr) return ValueModifier is
   begin
      case Kind (Self) is
         when Tk_Value | Tk_Event =>
            return ValueModifier
              (Short'(From_Any (Get_Parameter (Self, 2).all)));

         when others =>
            raise BadKind;
      end case;
   end Type_Modifier;

end TypeCode;
