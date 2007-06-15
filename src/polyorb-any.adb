------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

with Ada.Strings.Fixed;

with PolyORB.Log;
with PolyORB.Utils.Dynamic_Tables;

with System.Address_Image;

package body PolyORB.Any is

   use PolyORB.Log;
   use PolyORB.Types;
   use type System.Address;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Free is
     new Ada.Unchecked_Deallocation (Content'Class, Content_Ptr);

   procedure Move_Any_Value (Dst_C, Src_C : in out Any_Container'Class);
   --  Transfer the value of Src_C to Dst_C; Src_C is empty upon return.
   --  Foreign status is transferred from Src_C to Dst_C. The previous contents
   --  of Dst_C are deallocated if appropriate.

   procedure Set_Value
     (C : in out Any_Container'Class; CC : Content_Ptr; Foreign : Boolean);
   --  Set the designated value of A to CC; set the Foreign indication as
   --  to the given value. Note that no type conformance check is performed!
   --  The previous value, and if applicable the associated storage, are
   --  deallocated.

   type Aggregate_Content_Ptr is access all Aggregate_Content'Class;

   --------------------
   -- Elementary_Any --
   --------------------

   package body Elementary_Any is

      type T_Content_Ptr is access all T_Content;

      procedure Free is new Ada.Unchecked_Deallocation (T, T_Ptr);

      procedure Kind_Check (C : Any_Container'Class);
      pragma Inline (Kind_Check);

      -----------
      -- Clone --
      -----------

      function Clone
        (CC   : T_Content;
         Into : Content_Ptr := null) return Content_Ptr
      is
      begin
         if Into /= null then
            T_Content (Into.all).V.all := CC.V.all;
            return Into;
         end if;
         return new T_Content'(Content with V => new T'(CC.V.all));
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      procedure Finalize_Value (CC : in out T_Content) is
      begin
         Free (CC.V);
      end Finalize_Value;

      --------------
      -- From_Any --
      --------------

      function From_Any (C : Any_Container'Class) return T is
      begin
         Kind_Check (C);
         return T_Content_Ptr (C.The_Value).V.all;
      end From_Any;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Value : Any_Container'Class;
         Index : Unsigned_Long) return T
      is
         CA_Ptr : constant Aggregate_Content_Ptr :=
                    Aggregate_Content_Ptr (Value.The_Value);
         M : aliased Mechanism := By_Value;

         PTC : aliased TypeCode.Object (Kind);
         CC  : constant T_Content :=
                 T_Content
                   (Get_Aggregate_Element
                    (CA_Ptr, PTC'Unchecked_Access, Index, M'Access));
      begin
         return CC.V.all;
      end Get_Aggregate_Element;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      function Get_Aggregate_Element
        (Value : Any;
         Index : Unsigned_Long) return T
      is
      begin
         return Get_Aggregate_Element (Get_Container (Value).all, Index);
      end Get_Aggregate_Element;

      ----------------
      -- Kind_Check --
      ----------------

      procedure Kind_Check (C : Any_Container'Class) is
      begin
         if TypeCode.Kind (Unwind_Typedefs (Get_Type_Obj (C))) /= Kind then
            raise Program_Error;
         end if;
      end Kind_Check;

      -------------------
      -- Set_Any_Value --
      -------------------

      procedure Set_Any_Value (X : T; C : in out Any_Container'Class) is
      begin
         Kind_Check (C);

         if C.The_Value = null then
            C.The_Value := new T_Content'(V => new T'(X));
            C.Foreign   := False;

         else
            T_Content_Ptr (C.The_Value).V.all := X;
         end if;

         C.Is_Finalized := False;
      end Set_Any_Value;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      function Unchecked_Get_V (X : access T_Content) return T_Ptr is
      begin
         return X.V;
      end Unchecked_Get_V;

      ----------
      -- Wrap --
      ----------

      function Wrap (X : access T) return Content'Class is
      begin
         return T_Content'(V => T_Ptr (X));
      end Wrap;

   end Elementary_Any;

   --  The following two bodies are needed early for elaboration of
   --  Elementary_Any instances

   ----------------
   -- From_Any_G --
   ----------------

   function From_Any_G (A : Any) return T is
   begin
      return From_Any (Get_Container (A).all);
   end From_Any_G;

   --------------
   -- To_Any_G --
   --------------

   function To_Any_G (X : T) return Any is
      A : Any;
   begin
      Set_Type (A, TC);
      Set_Any_Value (X, Get_Container (A).all);
      return A;
   end To_Any_G;

   ------------------------------
   -- Elementary_Any instances --
   ------------------------------

   package Elementary_Any_Octet is
     new Elementary_Any (Types.Octet, Tk_Octet);
   package Elementary_Any_Short is
     new Elementary_Any (Types.Short, Tk_Short);
   package Elementary_Any_Long is
     new Elementary_Any (Types.Long, Tk_Long);
   package Elementary_Any_Long_Long is
     new Elementary_Any (Types.Long_Long, Tk_Longlong);
   package Elementary_Any_UShort is
     new Elementary_Any (Types.Unsigned_Short, Tk_Ushort);
   package Elementary_Any_ULong is
     new Elementary_Any (Types.Unsigned_Long, Tk_Ulong);
   package Elementary_Any_ULong_Long is
     new Elementary_Any (Types.Unsigned_Long_Long, Tk_Ulonglong);
   package Elementary_Any_Boolean is
     new Elementary_Any (Types.Boolean, Tk_Boolean);
   package Elementary_Any_Char is
     new Elementary_Any (Types.Char, Tk_Char);
   package Elementary_Any_Wchar is
     new Elementary_Any (Types.Wchar, Tk_Widechar);
   package Elementary_Any_Float is
     new Elementary_Any (Types.Float, Tk_Float);
   package Elementary_Any_Double is
     new Elementary_Any (Types.Double, Tk_Double);
   package Elementary_Any_Long_Double is
     new Elementary_Any (Types.Long_Double, Tk_Longdouble);
   package Elementary_Any_String is
     new Elementary_Any (Types.String, Tk_String);
   package Elementary_Any_Wide_String is
     new Elementary_Any (Types.Wide_String, Tk_Wstring);

   package Elementary_Any_Bounded_String is
     new Elementary_Any (Ada.Strings.Superbounded.Super_String, Tk_String);
   package Elementary_Any_Bounded_Wide_String is
     new Elementary_Any (Ada.Strings.Wide_Superbounded.Super_String,
                         Tk_Wstring);

   package Elementary_Any_Any is
     new Elementary_Any (Any, Tk_Any);
   package Elementary_Any_TypeCode is
     new Elementary_Any (TypeCode.Local_Ref, Tk_TypeCode);

   ---------------------------------
   -- 'Aggregate' content wrapper --
   ---------------------------------

   --  While an aggregate is constructed, its contents are stored as a
   --  chained list.
   --  Once the construction is completed (i.e. the length of the list won't
   --  grow anymore), the list is converted to an array (to speed up access
   --  to random elements) and the aggegate is frozen (i.e. no elements can
   --  be added to it). Actually the freeze occurs the first time an element
   --  is retrieved through Get_Aggregate_Element.

   --  A list of Any contents (for construction of aggregates)

   package Content_Tables is new PolyORB.Utils.Dynamic_Tables
     (Table_Component_Type => Any_Container_Ptr,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 0,
      Table_Initial        => 8,
      Table_Increment      => 100);
   subtype Content_Table is Content_Tables.Instance;

   --  For complex types that could be defined in IDL, a Aggregate_Content
   --  will be used.
   --
   --  Complex types include Struct, Union, Enum, Sequence, Array, Except,
   --  Fixed, Value, Valuebox, Abstract_Interface. Here is the way the
   --  content_list is used in each case (See CORBA V2.3 - 15.3)

   --     - for Struct, Except: the elements are the values of each
   --       field in the order of the declaration
   --
   --     - for Union : the value of the switch element comes
   --       first. Then come all the values of the corresponding fields
   --
   --     - for Enum : an unsigned_long corresponding to the position
   --       of the value in the declaration is the only element
   --
   --     - for Array : all the elements of the array, one by one.
   --
   --     - for Sequence : the length first and then all the elements
   --       of the sequence, one by one. XXX Can't we get rid of the length?
   --       it is implicit already in the length of the aggregate
   --
   --     - for Fixed : XXX
   --     - for Value : XXX
   --     - for Valuebox : XXX
   --     - for Abstract_Interface : XXX

   -------------------------------
   -- Default_Aggregate_Content --
   -------------------------------

   --  Default generic implementation of aggregate content wrapper, based on
   --  a table of Any_Container accesses.

   type Default_Aggregate_Content (Kind : TCKind) is
     new Aggregate_Content with
   record
      V : Content_Table;
   end record;

   --  Content primitives

   function Clone
     (CC   : Default_Aggregate_Content;
      Into : Content_Ptr := null) return Content_Ptr;
   procedure Finalize_Value (CC : in out Default_Aggregate_Content);

   --  Aggregate_Content primitives

   function Get_Aggregate_Count
     (ACC : Default_Aggregate_Content) return Types.Unsigned_Long;

   procedure Set_Aggregate_Count
     (ACC   : in out Default_Aggregate_Content;
      Count : Types.Unsigned_Long);

   function Get_Aggregate_Element
     (ACC   : not null access Default_Aggregate_Content;
      TC    : TypeCode.Object_Ptr;
      Index : Types.Unsigned_Long;
      Mech  : not null access Mechanism) return Content'Class;

   procedure Set_Aggregate_Element
     (ACC    : in out Default_Aggregate_Content;
      TC     : TypeCode.Object_Ptr;
      Index  : Types.Unsigned_Long;
      From_C : in out Any_Container'Class);

   procedure Add_Aggregate_Element
     (ACC : in out Default_Aggregate_Content;
      El  : Any_Container_Ptr);

   function Allocate_Default_Aggregate_Content
     (Kind : TCKind) return Content_Ptr;
   --  Allocate and initialize a Aggregate_Content. The TCKind is that of the
   --  aggregate.

   procedure Deep_Deallocate (Table : in out Content_Table);
   --  Deallocate each content element of a content table

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Any) return Boolean is
      Res : Boolean;
   begin
      pragma Debug (O ("Equal (Any): enter, "
                       & Image (Left) & " =? " & Image (Right)));
      Res := "=" (Get_Container (Left).all, Get_Container (Right).all);
      pragma Debug (O ("Equal (Any): returning " & Res'Img));
      return Res;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Any_Container'Class) return Boolean is
      L_Type : constant TypeCode.Object_Ptr := Get_Type_Obj (Left);
      R_Type : constant TypeCode.Object_Ptr := Get_Type_Obj (Right);

      function Agg_Elements_Equal
        (TC           : TypeCode.Object_Ptr;
         L_ACC, R_ACC : access Aggregate_Content'Class;
         Index        : Types.Unsigned_Long) return Boolean;
      --  Compare the Index'th element of Left and Right, which are assumed
      --  to be aggregates. The expected type for both elements is TC.

      function Agg_Elements_Equal
        (TC           : TypeCode.Object_Ptr;
         L_ACC, R_ACC : access Aggregate_Content'Class;
         Index        : Types.Unsigned_Long) return Boolean
      is
         L_C  : Any_Container;
         R_C  : Any_Container;
         L_M  : aliased Mechanism := By_Value;
         L_CC : aliased Content'Class :=
                  Get_Aggregate_Element (L_ACC, TC, Index, L_M'Access);
         R_M  : aliased Mechanism := By_Value;
         R_CC : aliased Content'Class :=
                  Get_Aggregate_Element (R_ACC, TC, Index, R_M'Access);
      begin
         Set_Type (L_C, TC);
         Set_Value (L_C, L_CC'Unchecked_Access, Foreign => True);
         Set_Type (R_C, TC);
         Set_Value (R_C, R_CC'Unchecked_Access, Foreign => True);

         return "=" (L_C, R_C);
      end Agg_Elements_Equal;

   begin
      if not TypeCode.Equal (L_Type, R_Type) then
         return False;
      end if;

      pragma Debug (O ("Equal (Any): passed typecode test"));
      case TypeCode.Kind (Unwind_Typedefs (L_Type)) is
         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Equal (Any, Null or Void): end"));
            return True;

         when Tk_Short =>
            declare
               L : constant Short := From_Any (Left);
               R : constant Short := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Short): end"));
               return L = R;
            end;

         when Tk_Long =>
            declare
               L : constant Long := From_Any (Left);
               R : constant Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long): end"));
               return L = R;
            end;

         when Tk_Ushort =>
            declare
               L : constant Unsigned_Short := From_Any (Left);
               R : constant Unsigned_Short := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Ushort): end"));
               return L = R;
            end;

         when Tk_Ulong =>
            declare
               L : constant Unsigned_Long := From_Any (Left);
               R : constant Unsigned_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Ulong): end"));
               return L = R;
            end;

         when Tk_Float =>
            declare
               L : constant Types.Float := From_Any (Left);
               R : constant Types.Float := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Float): end"));
               return L = R;
            end;

         when Tk_Double =>
            declare
               L : constant Double := From_Any (Left);
               R : constant Double := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Double): end"));
               return L = R;
            end;

         when Tk_Boolean =>
            declare
               L : constant Boolean := From_Any (Left);
               R : constant Boolean := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Boolean): end"));
               return L = R;
            end;

         when Tk_Char =>
            declare
               L : constant Char := From_Any (Left);
               R : constant Char := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Char): end"));
               return L = R;
            end;

         when Tk_Octet =>
            declare
               L : constant Octet := From_Any (Left);
               R : constant Octet := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Octet): end"));
               return L = R;
            end;

         when Tk_Any =>
            declare
               L : constant Any := From_Any (Left);
               R : constant Any := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Any): end"));
               return "=" (L, R);
            end;

         when Tk_TypeCode =>
            declare
               L : constant TypeCode.Local_Ref := From_Any (Left);
               R : constant TypeCode.Local_Ref := From_Any (Right);
            begin
               if TypeCode.Kind (R) = Tk_Value then
                  pragma Debug (O ("Equal (Any, TypeCode) :" &
                                   " Skipping Tk_Value" &
                                   " typecode comparison"));
                  --  TODO/XXX Call a different equality procedure
                  --  to accomodate eventual circular references in
                  --  typecodes
                  pragma Debug (O ("Equal (Any, TypeCode) :" &
                                   " Tk_Value NOT IMPLEMENTED"));
                  raise Program_Error;
                  return True;
               else
                  pragma Debug (O ("Equal (Any, TypeCode): end"));
                  return TypeCode.Equal (R, L);
               end if;
            end;

         when Tk_Principal =>
            --  XXX : to be done
            pragma Debug (O ("Equal (Any, Principal): end"
                             & " NOT IMPLEMENTED -> TRUE"));
            return True;

         when Tk_Objref =>
            declare
--               L : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Left);
--               R : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, ObjRef): end"
                                & " NOT IMPLEMENTED -> TRUE"));
               --  XXX : is_equivalent has to be implemented
               return True;
               --  return CORBA.Object.Is_Equivalent (L, R);
            end;

         when Tk_Struct | Tk_Except =>

            --  1. Retrieve aggregate contents wrapper for Left and Right
            --  2. For each member in the aggregate, compare both values:
            --     2.1. Retrieve member type
            --     2.2. Retrieve contents wrapper on the stack
            --     2.3. Conjure up temporary Any's pointing to these wrappers,
            --          marked as foreign (no contents deallocation upon
            --          finalization)
            --     2.4. Recurse in Equal on temporary Anys

            declare
               List_Type : constant TypeCode.Object_Ptr :=
                             Unwind_Typedefs (L_Type);
               M_Type    : TypeCode.Object_Ptr;
               L_ACC : Aggregate_Content'Class
                         renames Aggregate_Content'Class (Left.The_Value.all);
               R_ACC : Aggregate_Content'Class
                         renames Aggregate_Content'Class (Right.The_Value.all);
            begin
               for J in 0 .. TypeCode.Member_Count (List_Type) - 1 loop
                  M_Type := TypeCode.Member_Type (List_Type, J);
                  if not Agg_Elements_Equal
                           (M_Type, L_ACC'Access, R_ACC'Access, J)
                  then
                     pragma Debug (O ("Equal (Any, struct/except): end"));
                     return False;
                  end if;
               end loop;
               pragma Debug (O ("Equal (Any, struct/except): end"));
               return True;
            end;

         when Tk_Union =>
            declare
               L_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Left.The_Value.all);
               R_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Right.The_Value.all);
               List_Type   : constant TypeCode.Object_Ptr :=
                               Unwind_Typedefs (L_Type);
               Switch_Type : constant TypeCode.Object_Ptr :=
                               TypeCode.Discriminator_Type (List_Type);
               Member_Type : TypeCode.Object_Ptr;
            begin
               pragma Assert (Get_Aggregate_Count (L_ACC) = 2);
               pragma Assert (Get_Aggregate_Count (R_ACC) = 2);

               --  First compares the switch value

               if not Agg_Elements_Equal
                        (Switch_Type, L_ACC'Access, R_ACC'Access, 0)
               then
                  pragma Debug (O ("Equal (Any, Union): "
                    & "switch differs, end"));
                  return False;
               end if;

               declare
                  Label_Mech : aliased Mechanism := By_Value;
                  Label_CC : aliased Content'Class :=
                               Get_Aggregate_Element
                                 (L_ACC'Access, Switch_Type, 0,
                                  Label_Mech'Access);
                  Label_C : Any_Container;
                  Res : Boolean;
               begin
                  Set_Type (Label_C, Switch_Type);
                  Set_Value
                    (Label_C, Label_CC'Unchecked_Access, Foreign => True);
                  Member_Type :=
                    TypeCode.Member_Type_With_Label (List_Type, Label_C);

                  Res := Agg_Elements_Equal
                           (Member_Type, L_ACC'Access, R_ACC'Access, 1);
                  pragma Debug (O ("Equal (Any, Union): end, " & Res'Img));
                  return Res;
               end;
            end;

         when Tk_Enum =>
            pragma Debug (O ("Equal (Any, Enum): end"));
            --  compares the only element of both aggregate : an unsigned long
            declare
               L_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Left.The_Value.all);
               R_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Right.The_Value.all);
            begin
               return Agg_Elements_Equal
                 (TypeCode.PTC_Unsigned_Long'Access,
                  L_ACC'Access, R_ACC'Access, 0);
            end;

         when Tk_Sequence
           | Tk_Array =>
            declare
               List_Type : constant TypeCode.Object_Ptr :=
                             Unwind_Typedefs (L_Type);

               Member_Type : constant TypeCode.Object_Ptr :=
                               TypeCode.Content_Type (List_Type);

               L_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Left.The_Value.all);
               R_ACC : Aggregate_Content'Class renames
                         Aggregate_Content'Class (Right.The_Value.all);
            begin
               --  for each member in the aggregate, compare both values

               for J in 0 .. TypeCode.Length (List_Type) - 1 loop
                  if not Agg_Elements_Equal
                           (Member_Type, L_ACC'Access, R_ACC'Access, J)
                  then
                     pragma Debug (O ("Equal (Any, sequence/array): end"));
                     return False;
                  end if;
               end loop;

               pragma Debug (O ("Equal (Any, sequence/array): end"));
               return True;
            end;

         when Tk_Fixed
           | Tk_Value
           | Tk_Valuebox
           | Tk_Abstract_Interface
           | Tk_Local_Interface
           | Tk_Component
           | Tk_Home
           | Tk_Event =>
            --  XXX : to be done
            pragma Debug (O ("Equal (Any, Fixed, Value, ValueBox, "
                             & "Abstract_Interface, Local_Interface, "
                             & "Component, Home or Event): end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;

         when Tk_String =>
            declare
               L : constant Standard.String := From_Any (Left);
               R : constant Standard.String := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, String): end"));
               return L = R;
            end;

         when Tk_Alias =>

            --  We should never be here, since the case statement uses the
            --  precise type of the anys, that is an unaliased type.

            pragma Debug (O ("Equal (Any, Alias): end with exception"));
            raise Program_Error;

         when Tk_Longlong =>
            declare
               L : constant Long_Long := From_Any (Left);
               R : constant Long_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long_Long): end"));
               return L = R;
            end;

         when Tk_Ulonglong =>
            declare
               L : constant Unsigned_Long_Long := From_Any (Left);
               R : constant Unsigned_Long_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Unsigned_Long_Long): end"));
               return L = R;
            end;

         when Tk_Longdouble =>
            declare
               L : constant Long_Double := From_Any (Left);
               R : constant Long_Double := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long_Double): end"));
               return L = R;
            end;

         when Tk_Widechar =>
            declare
               L : constant Wchar := From_Any (Left);
               R : constant Wchar := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Wchar): end"));
               return L = R;
            end;

         when Tk_Wstring =>
            declare
               L : constant Types.Wide_String := From_Any (Left);
               R : constant Types.Wide_String := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Wide_String): end"));
               return L = R;
            end;

         when Tk_Native =>
            --  XXX  to be done
            pragma Debug (O ("Equal (Any, Native): end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;
      end case;
   end "=";

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element
     (ACC : in out Aggregate_Content;
      El  : Any_Container_Ptr)
   is
   begin

      --  This is not supported by default

      raise Program_Error;
   end Add_Aggregate_Element;

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element
     (ACC : in out Default_Aggregate_Content;
      El  : Any_Container_Ptr)
   is
      use Content_Tables;
   begin
      pragma Assert (Initialized (ACC.V));

      Smart_Pointers.Inc_Usage (Smart_Pointers.Entity_Ptr (El));
      Increment_Last (ACC.V);
      ACC.V.Table (Last (ACC.V)) := El;
   end Add_Aggregate_Element;

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : Any)
   is
      CA_Ptr : constant Aggregate_Content_Ptr :=
                 Aggregate_Content_Ptr (Get_Container (Value).The_Value);
   begin
      pragma Debug (O ("Add_Aggregate_Element: enter"));
      Add_Aggregate_Element (CA_Ptr.all, Get_Container (Element));
      pragma Debug (O ("Add_Aggregate_Element: end"));
   end Add_Aggregate_Element;

   ----------------------------------------
   -- Allocate_Default_Aggregate_Content --
   ----------------------------------------

   function Allocate_Default_Aggregate_Content
     (Kind : TCKind) return Content_Ptr
   is
      Result : constant Aggregate_Content_Ptr :=
                 new Default_Aggregate_Content (Kind => Kind);
   begin
      Content_Tables.Initialize (Default_Aggregate_Content (Result.all).V);
      return Content_Ptr (Result);
   end Allocate_Default_Aggregate_Content;

   -----------
   -- Clone --
   -----------

   function Clone
     (CC   : No_Content;
      Into : Content_Ptr := null) return Content_Ptr is
   begin
      raise Program_Error;
      return null;
   end Clone;

   --  Clone function for Default_Aggregate_Content
   --  Caveat emptor: this function allocates a new container for each
   --  element of the aggregate, and sets its value by recursively cloning
   --  the contents of the original element. It is *extremely* costly! Also,
   --  it never supports direct in-place assignment.

   function Clone
     (CC   : Default_Aggregate_Content;
      Into : Content_Ptr := null) return Content_Ptr
   is
      use PolyORB.Smart_Pointers;
      use Content_Tables;
   begin
      if Into /= null then
         return null;
      end if;

      declare
         New_CC_P : constant Content_Ptr :=
                      Allocate_Default_Aggregate_Content (CC.Kind);
         New_CC   : Default_Aggregate_Content
                      renames Default_Aggregate_Content (New_CC_P.all);
      begin
         Set_Last (New_CC.V, Last (CC.V));
         for J in First (New_CC.V) .. Last (New_CC.V) loop

            --  Create a new any container, referenced by this aggregate

            New_CC.V.Table (J) := new Any_Container;
            Inc_Usage (Entity_Ptr (New_CC.V.Table (J)));

            --  Set its type and copy the value from the original element

            New_CC.V.Table (J).The_Type := CC.V.Table (J).The_Type;
            Set_Value (New_CC.V.Table (J).all,
                       Clone (CC.V.Table (J).The_Value.all), Foreign => False);
         end loop;
         return New_CC_P;
      end;
   end Clone;

   --------------
   -- Copy_Any --
   --------------

   function Copy_Any (Src : Any) return Any is
      Dst : Any;
   begin
      Set_Type (Dst, Get_Type_Obj (Src));
      Copy_Any_Value (Dst => Dst, Src => Src);
      return Dst;
   end Copy_Any;

   --------------------
   -- Copy_Any_Value --
   --------------------

   procedure Copy_Any_Value
     (Dst_C : in out Any_Container'Class;
      Src_C : Any_Container'Class);

   procedure Copy_Any_Value (Dst : Any; Src : Any) is
   begin
      Copy_Any_Value (Get_Container (Dst).all, Get_Container (Src).all);
   end Copy_Any_Value;

   procedure Copy_Any_Value
     (Dst_C : in out Any_Container'Class;
      Src_C : Any_Container'Class)
   is
      TC  : constant TypeCode.Object_Ptr :=
              Unwind_Typedefs (Get_Type_Obj (Src_C));
      TCK : constant TCKind := TypeCode.Kind (TC);

      Dst_TCK : constant TCKind :=
                  TypeCode.Kind (Unwind_Typedefs (Get_Type_Obj (Dst_C)));
   begin
      if Src_C'Address = Dst_C'Address then
         return;
      end if;

      if Dst_TCK /= TCK then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Dst_C.The_Value = null then
         Set_Value (Dst_C, Clone (Src_C.The_Value.all), Foreign => False);
      else
         case TCK is
            when Tk_Null | Tk_Void =>
               null;

            when Tk_Short =>
               Set_Any_Value (Short'(From_Any (Src_C)), Dst_C);

            when Tk_Long =>
               Set_Any_Value (Long'(From_Any (Src_C)), Dst_C);

            when Tk_Ushort =>
               Set_Any_Value (Unsigned_Short'(From_Any (Src_C)), Dst_C);

            when Tk_Ulong =>
               Set_Any_Value (Unsigned_Long'(From_Any (Src_C)), Dst_C);

            when Tk_Float =>
               Set_Any_Value (Types.Float'(From_Any (Src_C)), Dst_C);

            when Tk_Double =>
               Set_Any_Value (Double'(From_Any (Src_C)), Dst_C);

            when Tk_Boolean =>
               Set_Any_Value (Boolean'(From_Any (Src_C)), Dst_C);

            when Tk_Char =>
               Set_Any_Value (Char'(From_Any (Src_C)), Dst_C);

            when Tk_Octet =>
               Set_Any_Value (Octet'(From_Any (Src_C)), Dst_C);

            when Tk_Longlong =>
               Set_Any_Value (Long_Long'(From_Any (Src_C)), Dst_C);

            when Tk_Ulonglong =>
               Set_Any_Value (Unsigned_Long_Long'(From_Any (Src_C)), Dst_C);

            when Tk_Longdouble =>
               Set_Any_Value (Long_Double'(From_Any (Src_C)), Dst_C);

            when Tk_Widechar =>
               Set_Any_Value (Wchar'(From_Any (Src_C)), Dst_C);

            when Tk_String =>
               declare
                  Bound : constant Types.Unsigned_Long := TypeCode.Length (TC);
               begin
                  if Bound = 0 then
                     Set_Any_Value (Types.String'(From_Any (Src_C)), Dst_C);
                  else
                     Elementary_Any_Bounded_String.Set_Any_Value
                       (Elementary_Any_Bounded_String.From_Any (Src_C), Dst_C);
                  end if;
               end;

            when Tk_Wstring =>
               declare
                  Bound : constant Types.Unsigned_Long := TypeCode.Length (TC);
               begin
                  if Bound = 0 then
                     Set_Any_Value (Types.Wide_String'(From_Any (Src_C)),
                                    Dst_C);
                  else
                     Elementary_Any_Bounded_Wide_String.Set_Any_Value
                       (Elementary_Any_Bounded_Wide_String.From_Any (Src_C),
                        Dst_C);
                  end if;
               end;

            when Tk_Any =>
               Set_Any_Value (Any'(From_Any (Src_C)), Dst_C);

            when Tk_TypeCode =>
               Set_Any_Value (TypeCode.Local_Ref'(From_Any (Src_C)), Dst_C);

            when Tk_Objref =>
               declare
                  New_CC : constant Content_Ptr :=
                    Clone (CC   => Src_C.The_Value.all,
                           Into => Dst_C.The_Value);
               begin
                  if Dst_C.The_Value = null then
                     Set_Value (Dst_C, New_CC, Foreign => False);
                  else
                     pragma Assert (New_CC = Dst_C.The_Value);
                     null;
                  end if;
               end;

            when
              Tk_Struct   |
              Tk_Except   |
              Tk_Union    |
              Tk_Enum     |
              Tk_Sequence |
              Tk_Array    |
              Tk_Fixed    =>

               declare
                  El_TC : TypeCode.Object_Ptr;
                  Dst_ACC : Aggregate_Content'Class
                              renames Aggregate_Content'Class
                                (Dst_C.The_Value.all);
                  Src_ACC : Aggregate_Content'Class
                              renames Aggregate_Content'Class
                                (Src_C.The_Value.all);
                  Src_Count : constant Types.Unsigned_Long :=
                                Get_Aggregate_Count (Src_ACC);
               begin
                  Set_Aggregate_Count (Dst_ACC, Src_Count);

                  --  Set up El_TC for first element

                  case TCK is
                     when Tk_Enum | Tk_Sequence =>
                        El_TC := TypeCode.PTC_Unsigned_Long'Access;

                     when Tk_Union =>
                        El_TC := TypeCode.Discriminator_Type (TC);

                     when Tk_Array =>
                        El_TC := TypeCode.Content_Type (TC);

                     when Tk_Fixed =>
                        El_TC := TypeCode.PTC_Octet'Access;

                     when others =>
                        null;
                  end case;

                  for J in 0 .. Src_Count - 1 loop
                     if TCK = Tk_Struct or else TCK = Tk_Except then
                        El_TC := TypeCode.Member_Type (TC, J);
                     end if;

                     declare
                        Dst_El_C  : Any_Container;
                        Src_El_C  : Any_Container;

                        Dst_El_M  : aliased Mechanism := By_Reference;
                        Dst_El_CC : aliased Content'Class :=
                                      Get_Aggregate_Element
                                        (Dst_ACC'Access, El_TC, J,
                                         Dst_El_M'Access);

                        Src_El_M  : aliased Mechanism := By_Value;
                        Src_El_CC : aliased Content'Class :=
                                      Get_Aggregate_Element
                                        (Src_ACC'Access, El_TC, J,
                                         Src_El_M'Access);

                     begin
                        Set_Type (Src_El_C, El_TC);
                        Set_Value (Src_El_C,
                          Src_El_CC'Unchecked_Access, Foreign => True);

                        --  Case of an aggregate element that needs to be set
                        --  explicitly

                        if Dst_El_M = By_Value then
                           Set_Aggregate_Element (Dst_ACC, El_TC, J, Src_El_C);

                           --  This would be incorrect if Dst_ACC is a default
                           --  aggregate content, since in this case the call
                           --  will incorrectly steal the value from Src_El_C.

                           --  At least try to detect this fault case:

                           pragma Assert (not Is_Empty (Src_El_C));

                        --  Attempt in-place assignment

                        elsif Clone
                            (CC   => Src_El_CC,
                             Into => Dst_El_CC'Unchecked_Access) = null

                        --  Fall back to recursive element copy

                        then
                           Set_Type (Dst_El_C, El_TC);
                           Set_Value (Dst_El_C,
                             Dst_El_CC'Unchecked_Access, Foreign => True);

                           Copy_Any_Value (Dst_El_C, Src_El_C);
                        end if;

                        if J = 0 then
                           case TCK is
                              when Tk_Union =>
                                 El_TC :=
                                   TypeCode.Member_Type_With_Label
                                     (TC, Src_El_C);

                              when Tk_Sequence =>
                                 El_TC := TypeCode.Content_Type (TC);

                              when others =>
                                 null;
                           end case;
                        end if;
                     end;
                  end loop;
               end;

            when
              Tk_Value              |
              Tk_Valuebox           |
              Tk_Abstract_Interface |
              Tk_Local_Interface    |
              Tk_Component          |
              Tk_Home               |
              Tk_Event              |
              Tk_Principal          |
              Tk_Native             =>
               --  XXX : to be done
               pragma Debug (O ("Copy (" & Dst_TCK'Img & ": end"
                                & " NON IMPLEMENTED"));
               return;

            when Tk_Alias =>
               --  we should never be here, since the case statement uses the
               --  precise type of the anys, that is an unaliased type
               pragma Debug (O ("Equal (Any, Alias): end with exception"));
               raise Program_Error;

         end case;
      end if;
   end Copy_Any_Value;

   ---------------------
   -- Deep_Deallocate --
   ---------------------

   procedure Deep_Deallocate (Table : in out Content_Table) is
      use Content_Tables;
   begin
      pragma Debug (O ("Deep_Deallocate: enter"));

      if Initialized (Table) then
         for J in First (Table) .. Last (Table) loop

            --  If we are aborting during initialisation of the aggregate,
            --  not all elements might have been initialized at this point,
            --  so we need to test explicitly against null.

            if Table.Table (J) /= null then
               Smart_Pointers.Dec_Usage
                 (Smart_Pointers.Entity_Ptr (Table.Table (J)));
            end if;
         end loop;
      end if;

      Deallocate (Table);

      pragma Debug (O ("Deep_Deallocate: end"));
   end Deep_Deallocate;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Any_Container) is
   begin
      pragma Debug (O ("Finalizing Any_Container: enter"));

      if Self.Is_Finalized then
         return;
      end if;

      Self.Is_Finalized := True;
      Finalize_Value (Self);

      pragma Debug (O ("Finalizing Any_Container: leave"));
   end Finalize;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value (C : in out Any_Container'Class) is
   begin
      Set_Value (C, null, Foreign => False);
   end Finalize_Value;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value (CC : in out No_Content) is
   begin
      raise Program_Error;
   end Finalize_Value;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value (CC : in out Default_Aggregate_Content) is
   begin
      Deep_Deallocate (CC.V);
   end Finalize_Value;

   --------------
   -- From_Any --
   --------------

   function From_Any (C : Any_Container'Class) return Types.Octet
                      renames Elementary_Any_Octet.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Short
                      renames Elementary_Any_Short.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Long
                      renames Elementary_Any_Long.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Long_Long
                      renames Elementary_Any_Long_Long.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Short
                      renames Elementary_Any_UShort.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Long
                      renames Elementary_Any_ULong.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Long_Long
                      renames Elementary_Any_ULong_Long.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Boolean
                      renames Elementary_Any_Boolean.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Char
                      renames Elementary_Any_Char.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Wchar
                      renames Elementary_Any_Wchar.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Float
                      renames Elementary_Any_Float.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Double
                      renames Elementary_Any_Double.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Long_Double
                      renames Elementary_Any_Long_Double.From_Any;
   function From_Any (C : Any_Container'Class) return Types.String
                      renames Elementary_Any_String.From_Any;
   function From_Any (C : Any_Container'Class) return Types.Wide_String
                      renames Elementary_Any_Wide_String.From_Any;
   function From_Any (C : Any_Container'Class) return Any
                      renames Elementary_Any_Any.From_Any;
   function From_Any (C : Any_Container'Class) return TypeCode.Local_Ref
                      renames Elementary_Any_TypeCode.From_Any;

   function From_Any (A : Any) return Types.Octet
                      renames Elementary_Any_Octet.From_Any;
   function From_Any (A : Any) return Types.Short
                      renames Elementary_Any_Short.From_Any;
   function From_Any (A : Any) return Types.Long
                      renames Elementary_Any_Long.From_Any;
   function From_Any (A : Any) return Types.Long_Long
                      renames Elementary_Any_Long_Long.From_Any;
   function From_Any (A : Any) return Types.Unsigned_Short
                      renames Elementary_Any_UShort.From_Any;
   function From_Any (A : Any) return Types.Unsigned_Long
                      renames Elementary_Any_ULong.From_Any;
   function From_Any (A : Any) return Types.Unsigned_Long_Long
                      renames Elementary_Any_ULong_Long.From_Any;
   function From_Any (A : Any) return Types.Boolean
                      renames Elementary_Any_Boolean.From_Any;
   function From_Any (A : Any) return Types.Char
                      renames Elementary_Any_Char.From_Any;
   function From_Any (A : Any) return Types.Wchar
                      renames Elementary_Any_Wchar.From_Any;
   function From_Any (A : Any) return Types.Float
                      renames Elementary_Any_Float.From_Any;
   function From_Any (A : Any) return Types.Double
                      renames Elementary_Any_Double.From_Any;
   function From_Any (A : Any) return Types.Long_Double
                      renames Elementary_Any_Long_Double.From_Any;
   function From_Any (A : Any) return Types.String
                      renames Elementary_Any_String.From_Any;
   function From_Any (A : Any) return Types.Wide_String
                      renames Elementary_Any_Wide_String.From_Any;
   function From_Any (A : Any) return Any
                      renames Elementary_Any_Any.From_Any;
   function From_Any (A : Any) return TypeCode.Local_Ref
                      renames Elementary_Any_TypeCode.From_Any;

   ------------------------
   -- From_Any (strings) --
   ------------------------

   function From_Any (C : Any_Container'Class) return Standard.String is
      Bound : constant Types.Unsigned_Long :=
                TypeCode.Length (Unwind_Typedefs (Get_Type_Obj (C)));
   begin
      if Bound = 0 then

         --  Unbounded case
         --  Use unchecked access to underlying Types.String to avoid
         --  a costly Adjust.

         return To_Standard_String
           (Elementary_Any_String.Unchecked_Get_V
            (Elementary_Any_String.T_Content
             (C.The_Value.all)'Access).all);

      else

         --  Bounded case

         return Ada.Strings.Superbounded.Super_To_String
           (Elementary_Any_Bounded_String.From_Any (C));
      end if;
   end From_Any;

   function From_Any (C : Any_Container'Class) return Standard.Wide_String is
      Bound : constant Types.Unsigned_Long :=
                TypeCode.Length (Unwind_Typedefs (Get_Type_Obj (C)));
   begin
      if Bound = 0 then

         --  Unbounded case
         --  Use unchecked access to underlying Types.String to avoid
         --  a costly Adjust.

         return To_Wide_String
           (Elementary_Any_Wide_String.Unchecked_Get_V
            (Elementary_Any_Wide_String.T_Content
             (C.The_Value.all)'Access).all);

      else

         --  Bounded case

         return Ada.Strings.Wide_Superbounded.Super_To_String
           (Elementary_Any_Bounded_Wide_String.From_Any (C));
      end if;
   end From_Any;

   function String_From_Any is new From_Any_G (Standard.String, From_Any);
   function From_Any (A : Any) return Standard.String
                      renames String_From_Any;

   function Wide_String_From_Any is
     new From_Any_G (Standard.Wide_String, From_Any);
   function From_Any (A : Any) return Standard.Wide_String
                      renames Wide_String_From_Any;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count (Value : Any) return Unsigned_Long
   is
      CA_Ptr : constant Aggregate_Content_Ptr :=
                 Aggregate_Content_Ptr (Get_Value (Get_Container (Value).all));
   begin
      return Get_Aggregate_Count (CA_Ptr.all);
   end Get_Aggregate_Count;

   function Get_Aggregate_Count
     (ACC : Default_Aggregate_Content) return Unsigned_Long
   is
   begin
      return Unsigned_Long
        (Content_Tables.Last (ACC.V) - Content_Tables.First (ACC.V) + 1);
   end Get_Aggregate_Count;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (ACC   : not null access Aggregate_Content'Class;
      TC    : TypeCode.Local_Ref;
      Index : Unsigned_Long;
      Mech  : not null access Mechanism) return Content'Class
   is
   begin
      return Get_Aggregate_Element (ACC, TypeCode.Object_Of (TC), Index, Mech);
   end Get_Aggregate_Element;

   function Get_Aggregate_Element
     (ACC   : not null access Default_Aggregate_Content;
      TC    : TypeCode.Object_Ptr;
      Index : Unsigned_Long;
      Mech  : not null access Mechanism) return Content'Class
   is
      use PolyORB.Smart_Pointers;
      use Content_Tables;

      El_C_Ptr : Any_Container_Ptr renames
                   ACC.V.Table (First (ACC.V) + Natural (Index));
   begin
      pragma Debug (O ("Get_Aggregate_Element: enter"));

      pragma Debug (O ("Get_Aggregate_Element: Index = "
                       & Unsigned_Long'Image (Index)
                       & ", aggregate_count = "
                       & Unsigned_Long'Image (Get_Aggregate_Count (ACC.all))));

      if El_C_Ptr = null then

         --  Allocate new container and count one reference (the aggregate)

         El_C_Ptr := new Any_Container;
         Inc_Usage (Entity_Ptr (El_C_Ptr));

         El_C_Ptr.The_Type := TypeCode.To_Ref (TC);
      end if;

      if (El_C_Ptr.The_Value = null)
        or else (ACC.Kind = Tk_Union
                   and then
                 Index = 0
                   and then
                 Mech.all = By_Reference)
      then
         pragma Assert (Mech.all = By_Reference);

         --  When there is no current value for this aggregate element, or when
         --  getting the discriminant of an Union for update, set Mech to
         --  By_Value to force the caller to call Set_Aggregate_Element.

         Mech.all := By_Value;
         return No_Content'(null record);

      else
         Mech.all := By_Reference;
         return El_C_Ptr.The_Value.all;
      end if;
   end Get_Aggregate_Element;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (Value : Any;
      TC    : TypeCode.Local_Ref;
      Index : Unsigned_Long) return Any
   is
      CA_Ptr : constant Aggregate_Content_Ptr :=
                 Aggregate_Content_Ptr (Get_Container (Value).The_Value);
      A : Any;
      M : aliased Mechanism := By_Value;
      CC : constant Content'Class :=
             Get_Aggregate_Element (CA_Ptr,
               TypeCode.Object_Of (TC), Index, M'Access);
      New_CC : Content_Ptr;
      use PolyORB.Smart_Pointers;
   begin
      Set_Type (A, TC);

      New_CC := Clone (CC);

      Set_Value (Get_Container (A).all,  New_CC, Foreign => False);
      return A;
   end Get_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any;
      Index : Unsigned_Long) return Types.Unsigned_Long
     renames Elementary_Any_ULong.Get_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any_Container'Class;
      Index : Unsigned_Long) return Types.Unsigned_Long
     renames Elementary_Any_ULong.Get_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any;
      Index : Unsigned_Long) return Types.Octet
     renames Elementary_Any_Octet.Get_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any_Container'Class;
      Index : Unsigned_Long) return Types.Octet
     renames Elementary_Any_Octet.Get_Aggregate_Element;

   -------------------
   -- Get_Container --
   -------------------

   function Get_Container (A : Any) return Any_Container_Ptr is
   begin
      return Any_Container_Ptr (Entity_Of (A));
   end Get_Container;

   -------------------
   -- Get_Empty_Any --
   -------------------

   function Get_Empty_Any (Tc : TypeCode.Local_Ref) return Any is
      Result : Any;
   begin

      pragma Debug (O ("Get_Empty_Any: enter"));
      Set_Type (Result, Tc);
      pragma Debug (O ("Get_Empty_Any: type set"));

      return Result;
   end Get_Empty_Any;

   -----------------------------
   -- Get_Empty_Any_Aggregate --
   -----------------------------

   function Get_Empty_Any_Aggregate (TC : TypeCode.Local_Ref) return Any
   is
      A    : Any;
      Kind : constant TCKind := TypeCode.Kind (Unwind_Typedefs (TC));
   begin
      pragma Debug (O ("Get_Empty_Any_Aggregate: begin"));
      Set_Type (A, TC);

      if Kind in Aggregate_TCKind then
         Set_Value
           (Get_Container (A).all,
            Allocate_Default_Aggregate_Content (Kind), Foreign => False);
      end if;

      pragma Debug (O ("Get_Empty_Any_Aggregate: end"));
      return A;
   end Get_Empty_Any_Aggregate;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (A : Any) return TypeCode.Local_Ref is
   begin
      return TypeCode.To_Ref (Get_Type_Obj (A));
   end Get_Type;

   function Get_Type_Obj (A : Any) return TypeCode.Object_Ptr is
   begin
      return Get_Type_Obj (Get_Container (A).all);
   end Get_Type_Obj;

   function Get_Type (C : Any_Container'Class) return TypeCode.Local_Ref is
   begin
      return TypeCode.To_Ref (Get_Type_Obj (C));
   end Get_Type;

   function Get_Type_Obj
     (C : Any_Container'Class) return TypeCode.Object_Ptr
   is
   begin
      return TypeCode.Object_Of (C.The_Type);
   end Get_Type_Obj;

   ----------------------
   -- Get_Unwound_Type --
   ----------------------

   function Get_Unwound_Type (The_Any : Any) return TypeCode.Object_Ptr is
   begin
      return Unwind_Typedefs (Get_Type_Obj (The_Any));
   end Get_Unwound_Type;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (C : Any_Container'Class) return Content_Ptr is
   begin
      return C.The_Value;
   end Get_Value;

   -----------
   -- Image --
   -----------

   function Image (NV : NamedValue) return Standard.String is

      ---------------
      -- Flag_Name --
      ---------------

      function Flag_Name
        (F : Flags)
        return Standard.String;
      pragma Inline (Flag_Name);

      function Flag_Name
        (F : Flags)
        return Standard.String is
      begin
         case F is
            when ARG_IN =>
               return "in";
            when ARG_OUT =>
               return "out";
            when ARG_INOUT =>
               return "in out";
            when IN_COPY_VALUE =>
               return "in-copy";
            when others =>
               return "(invalid flag" & Flags'Image (F) & ")";
         end case;
      end Flag_Name;

   begin
      return Flag_Name (NV.Arg_Modes) & " "
        & To_Standard_String (NV.Name)
        & " = " & Image (NV.Argument);
   end Image;

   ----------------------
   -- Image (typecode) --
   ----------------------

   function Image (TC : TypeCode.Local_Ref) return Standard.String is
   begin
      return Image (TypeCode.Object_Of (TC));
   end Image;

   function Image (TC : TypeCode.Object_Ptr) return Standard.String is
      use TypeCode;

      Kind : constant TCKind := TypeCode.Kind (TC);
      Result : Types.String;
   begin
      case Kind is
         when
           Tk_Objref             |
           Tk_Struct             |
           Tk_Union              |
           Tk_Enum               |
           Tk_Alias              |
           Tk_Value              |
           Tk_Valuebox           |
           Tk_Native             |
           Tk_Abstract_Interface |
           Tk_Except             =>
            Result := To_PolyORB_String (TCKind'Image (Kind) & " ")
              & Types.String (Name (TC)) & " (" & Types.String (Id (TC)) & ")";

            --  Add a few information

            case Kind is
               when
                 Tk_Objref             |
                 Tk_Native             |
                 Tk_Abstract_Interface =>
                  return To_Standard_String (Result);

               when Tk_Alias =>
                  return To_Standard_String (Result)
                    & " <" & TCKind'Image (Kind) & ":"
                    & Image (Content_Type (TC)) & ">";

               when
                 Tk_Struct             |
                 Tk_Except             =>

                  Result := Result & " {";
                  for J in 0 .. Member_Count (TC) - 1 loop
                     Result := Result
                       & " "
                       & Image (Member_Type (TC, J))
                       & " "
                       & Types.String (Member_Name (TC, J))
                       & ";";
                  end loop;
                  Result := Result & " }";

                  return To_Standard_String (Result);

               when Tk_Union =>
                  Result := Result
                    & " ("
                    & Image (Discriminator_Type (TC))
                    & " :="
                    & Types.Long'Image (Default_Index (TC))
                    & ") {";

                  for J in 0 .. Member_Count (TC) - 1 loop
                     Result := Result &
                       " case " & Ada.Strings.Fixed.Trim
                                    (Image (Member_Label (TC, J)),
                                     Ada.Strings.Left)
                       & ": "
                       & Image (Member_Type (TC, J))
                       & " "
                       & Types.String (Member_Name (TC, J)) & ";";
                  end loop;
                  Result := Result & " }";

                  return To_Standard_String (Result);

               when others =>
                  return "<aggregate:" & TCKind'Image (Kind) & ">";
            end case;

         when Tk_Array | Tk_Sequence =>
            return TCKind'Image (Kind) & "<"
              & Image (Content_Type (TC)) & ","
              & Unsigned_Long'Image (Length (TC)) & " >";

         when Tk_String | Tk_Wstring =>
            declare
               function Tmpl return String;
               --  Return template type name, from typecode kind

               function Tmpl return String is
               begin
                  if Kind = Tk_Wstring then
                     return "wide_string";
                  else
                     return "string";
                  end if;
               end Tmpl;

               Bound : constant Types.Unsigned_Long := Length (TC);
               Bound_Img : constant String := Bound'Img;
            begin
               if Bound = 0 then
                  return Tmpl;
               else
                  return Tmpl & "<"
                    & Bound_Img (Bound_Img'First + 1 .. Bound_Img'Last) & ">";
               end if;
            end;

         when others =>
            return TCKind'Image (Kind);
      end case;
   end Image;

   -----------------
   -- Image (Any) --
   -----------------

   function Image (A : Any) return Standard.String is
   begin
      return Image (Get_Container (A).all);
   end Image;

   ---------------------------------
   -- Image (Any_Container'Class) --
   ---------------------------------

   function Image (C : Any_Container'Class) return Standard.String
   is
      TC   : constant TypeCode.Local_Ref := Unwind_Typedefs (Get_Type (C));
      Kind : constant TCKind := TypeCode.Kind (TC);
   begin
      if Is_Empty (C) then
         return "<empty>";
      end if;

      case Kind is
         when Tk_Short =>
            return Short'Image (From_Any (C));

         when Tk_Long =>
            return Long'Image (From_Any (C));

         when Tk_Ushort =>
            return Unsigned_Short'Image (From_Any (C));

         when Tk_Ulong =>
            return Unsigned_Long'Image (From_Any (C));

         when Tk_Float =>
            return Types.Float'Image (From_Any (C));

         when Tk_Double =>
            return Double'Image (From_Any (C));

         when Tk_Boolean =>
            return Boolean'Image (From_Any (C));

         when Tk_Char =>
            return Char'Image (From_Any (C));

         when Tk_Octet =>
            return Octet'Image (From_Any (C));

         when Tk_String =>
            return Standard.String'(From_Any (C));

         when Tk_Longlong =>
            return Long_Long'Image (From_Any (C));

         when Tk_Ulonglong =>
            return Unsigned_Long_Long'Image (From_Any (C));

         when Tk_Enum =>
            declare
               Index_C : Any_Container;
               Val_M   : aliased Mechanism := By_Value;
               CA_Ptr  : constant Aggregate_Content_Ptr :=
                           Aggregate_Content_Ptr (C.The_Value);

               Val_CC  : aliased Content'Class :=
                           Get_Aggregate_Element (CA_Ptr,
                             TypeCode.PTC_Unsigned_Long'Access, 0,
                             Val_M'Access);
            begin
               Set_Type  (Index_C, TC_Unsigned_Long);
               Set_Value (Index_C, Val_CC'Unchecked_Access, Foreign => True);
               return Types.To_Standard_String
                 (TypeCode.Enumerator_Name (TC, From_Any (Index_C)));
            end;

         when Tk_Value =>
            return "<Value:"
              & Image (Get_Type_Obj (C)) & ":"
              & System.Address_Image (Get_Value (C)'Address) & ">";

         when Tk_Any =>
            return "<Any:"
              & Image (Elementary_Any_Any.Unchecked_Get_V
                       (Elementary_Any_Any.T_Content
                        (Get_Value (C).all)'Access).all)
              & ">";

         when others =>
            return "<Any:" & Image (Get_Type_Obj (C)) & ">";
      end case;

   exception
      when others =>
         return "<Image raised an exception>";
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Any)
   is
      use type PolyORB.Smart_Pointers.Entity_Ptr;

      Container : constant Any_Container_Ptr := new Any_Container;
   begin
      pragma Debug (O ("Initializing Any: enter"));
      pragma Assert (Entity_Of (Self) = null);

      Set (Self, PolyORB.Smart_Pointers.Entity_Ptr (Container));
      pragma Debug (O ("Initializing Any: leave"));
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (A : Any) return Boolean is
   begin
      return Is_Empty (Get_Container (A).all);
   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (C : Any_Container'Class) return Boolean is
   begin
      return C.The_Value = null;
   end Is_Empty;

   --------------------
   -- Move_Any_Value --
   --------------------

   procedure Move_Any_Value (Dst_C, Src_C : in out Any_Container'Class) is
   begin
      if Src_C'Address = Dst_C'Address then
         return;
      end if;

      Set_Value (Dst_C, Src_C.The_Value, Src_C.Foreign);
      Src_C.The_Value := null;
      Src_C.Foreign   := False;
   end Move_Any_Value;

   --------------------
   -- Move_Any_Value --
   --------------------

   procedure Move_Any_Value (Dst : Any; Src : Any)
   is
      Src_C : constant Any_Container_Ptr := Get_Container (Src);
      Dst_C : constant Any_Container_Ptr := Get_Container (Dst);
   begin
      if TypeCode.Kind (Get_Unwound_Type (Dst))
         /= TypeCode.Kind (Get_Unwound_Type (Src))
      then
         pragma Debug (O ("Move_Any_Value from: "
                          & Image (Get_Unwound_Type (Src))));
         pragma Debug (O ("  to: " & Image (Get_Unwound_Type (Dst))));
         raise TypeCode.Bad_TypeCode;
      end if;
      Move_Any_Value (Dst_C.all, Src_C.all);
   end Move_Any_Value;

   -------------
   -- No_Wrap --
   -------------

   function No_Wrap (X : access T) return Content'Class is
   begin
      raise Program_Error;
      return No_Content'(null record);
   end No_Wrap;

   -------------------------
   -- Set_Aggregate_Count --
   -------------------------

   procedure Set_Aggregate_Count
     (ACC   : in out Default_Aggregate_Content;
      Count : Types.Unsigned_Long)
   is
   begin
      Content_Tables.Set_Last (ACC.V,
        Content_Tables.First (ACC.V) + Natural (Count) - 1);
   end Set_Aggregate_Count;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   procedure Set_Aggregate_Element
     (ACC    : in out Aggregate_Content'Class;
      TC     : TypeCode.Local_Ref;
      Index  : Unsigned_Long;
      From_C : in out Any_Container'Class)
   is
   begin
      Set_Aggregate_Element (ACC, TypeCode.Object_Of (TC), Index, From_C);
   end Set_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Aggregate_Content;
      TC     : TypeCode.Object_Ptr;
      Index  : Unsigned_Long;
      From_C : in out Any_Container'Class) is
   begin

      --  By default this is not implemented. This operation must be overridden
      --  for derived types of Aggregate_Content that may return No_Content
      --  in Get_Aggregate_Element.

      raise Program_Error;

   end Set_Aggregate_Element;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   procedure Set_Aggregate_Element
     (ACC    : in out Default_Aggregate_Content;
      TC     : TypeCode.Object_Ptr;
      Index  : Unsigned_Long;
      From_C : in out Any_Container'Class)
   is
      use Content_Tables;
      V_First : constant Natural := First (ACC.V);
      El_C : Any_Container'Class
               renames ACC.V.Table (V_First + Natural (Index)).all;
   begin
      if ACC.Kind = Tk_Union
        and then Index = 0
        and then not Is_Empty (El_C)
        and then not Is_Empty (ACC.V.Table (V_First + 1).all)
        and then El_C /= From_C
      then
         --  Changing the discriminant of a union: finalize previous member

         Finalize_Value (ACC.V.Table (V_First + 1).all);
      end if;
      Set_Type (El_C, TC);
      Move_Any_Value (Dst_C => El_C, Src_C => From_C);
   end Set_Aggregate_Element;

   -----------------------------
   -- Set_Any_Aggregate_Value --
   -----------------------------

   procedure Set_Any_Aggregate_Value (Agg_C : in out Any_Container'Class) is
      use TypeCode;
      Kind : constant TCKind :=
               TypeCode.Kind (Unwind_Typedefs (Get_Type_Obj (Agg_C)));
   begin
      pragma Debug (O ("Set_Any_Aggregate_Value: enter"));
      if Kind not in Aggregate_TCKind then
         raise TypeCode.Bad_TypeCode;
      end if;

      pragma Debug (O ("Set_Any_Aggregate_Value: typecode is correct"));

      if Agg_C.The_Value = null then
         Set_Value
           (Agg_C,
            Allocate_Default_Aggregate_Content (Kind), Foreign => False);
      end if;
   end Set_Any_Aggregate_Value;

   -------------------
   -- Set_Any_Value --
   -------------------

   procedure Set_Any_Value (X : Types.Short;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Short.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Long;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Long.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Long_Long;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Long_Long.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Unsigned_Short;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_UShort.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Unsigned_Long;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_ULong.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Unsigned_Long_Long;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_ULong_Long.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Float;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Float.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Double;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Double.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Long_Double;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Long_Double.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Boolean;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Boolean.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Char;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Char.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Wchar;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Wchar.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Octet;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Octet.Set_Any_Value;
   procedure Set_Any_Value (X : Any;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Any.Set_Any_Value;
   procedure Set_Any_Value (X : TypeCode.Local_Ref;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_TypeCode.Set_Any_Value;
   procedure Set_Any_Value (X : Types.String;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_String.Set_Any_Value;
   procedure Set_Any_Value (X : Types.Wide_String;
                            C : in out Any_Container'Class)
                            renames Elementary_Any_Wide_String.Set_Any_Value;

   procedure Set_Any_Value
     (X : Standard.String; C : in out Any_Container'Class)
   is
   begin
      Set_Any_Value (To_PolyORB_String (X), C);
   end Set_Any_Value;

   procedure Set_Any_Value (X : String; Bound : Positive;
                            C : in out Any_Container'Class) is
   begin
      Elementary_Any_Bounded_String.Set_Any_Value
        (Ada.Strings.Superbounded.To_Super_String
           (X, Max_Length => Bound), C);
   end Set_Any_Value;

   procedure Set_Any_Value (X : Wide_String; Bound : Positive;
                            C : in out Any_Container'Class) is
   begin
      Elementary_Any_Bounded_Wide_String.Set_Any_Value
        (Ada.Strings.Wide_Superbounded.To_Super_String
           (X, Max_Length => Bound), C);
   end Set_Any_Value;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type (A : in out Any; TC : TypeCode.Local_Ref) is
   begin
      Set_Type (A, TypeCode.Object_Of (TC));
   end Set_Type;

   procedure Set_Type (A : in out Any; TC : TypeCode.Object_Ptr) is
   begin
      Set_Type (Get_Container (A).all, TC);
   end Set_Type;

   procedure Set_Type
     (C  : in out Any_Container'Class;
      TC : TypeCode.Local_Ref)
   is
   begin
      Set_Type (C, TypeCode.Object_Of (TC));
   end Set_Type;

   procedure Set_Type
     (C  : in out Any_Container'Class;
      TC : TypeCode.Object_Ptr)
   is
   begin
      C.The_Type := TypeCode.To_Ref (TC);
   end Set_Type;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (C : in out Any_Container'Class; CC : Content_Ptr) is
   begin
      Set_Value (C, CC, Foreign => True);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (C : in out Any_Container'Class; CC : Content_Ptr; Foreign : Boolean)
   is
   begin
      if C.The_Value /= null and then not C.Foreign then
         Finalize_Value (C.The_Value.all);
         Free (C.The_Value);
      end if;

      C.The_Value := CC;
      C.Foreign   := Foreign;
   end Set_Value;

   ------------
   -- To_Any --
   ------------

   package To_Any_Instances is
      function To_Any is
        new To_Any_G
          (Types.Octet, TC_Octet, Elementary_Any_Octet.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Short, TC_Short, Elementary_Any_Short.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Long, TC_Long, Elementary_Any_Long.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Long_Long, TC_Long_Long,
           Elementary_Any_Long_Long.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Unsigned_Short, TC_Unsigned_Short,
           Elementary_Any_UShort.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Unsigned_Long, TC_Unsigned_Long,
           Elementary_Any_ULong.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Unsigned_Long_Long, TC_Unsigned_Long_Long,
           Elementary_Any_ULong_Long.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Boolean, TC_Boolean, Elementary_Any_Boolean.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Char, TC_Char, Elementary_Any_Char.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Wchar, TC_Wchar, Elementary_Any_Wchar.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Float, TC_Float, Elementary_Any_Float.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Double, TC_Double, Elementary_Any_Double.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Long_Double, TC_Long_Double,
           Elementary_Any_Long_Double.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.String, TC_String, Elementary_Any_String.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Types.Wide_String, TC_Wide_String,
           Elementary_Any_Wide_String.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (Any, TC_Any, Elementary_Any_Any.Set_Any_Value);

      function To_Any is
        new To_Any_G
          (TypeCode.Local_Ref, TC_TypeCode,
           Elementary_Any_TypeCode.Set_Any_Value);

   end To_Any_Instances;

   function To_Any (X : Types.Octet) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Short) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Long) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Long_Long) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Unsigned_Short) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Unsigned_Long) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Unsigned_Long_Long) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Boolean) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Char) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Wchar) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Float) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Double) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Long_Double) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.String) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Types.Wide_String) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : Any) return Any
                    renames To_Any_Instances.To_Any;
   function To_Any (X : TypeCode.Local_Ref) return Any
                    renames To_Any_Instances.To_Any;

   function To_Any (X : Standard.String) return Any is
   begin
      return To_Any (To_PolyORB_String (X));
   end To_Any;

   ---------------------
   -- Unwind_Typedefs --
   ---------------------

   function Unwind_Typedefs
     (TC : TypeCode.Local_Ref) return TypeCode.Local_Ref
   is
   begin
      return TypeCode.To_Ref (Unwind_Typedefs (TypeCode.Object_Of (TC)));
   end Unwind_Typedefs;

   function Unwind_Typedefs
     (TC : TypeCode.Object_Ptr) return TypeCode.Object_Ptr
   is
      Result : TypeCode.Object_Ptr := TC;
   begin
      while TypeCode.Kind (Result) = Tk_Alias loop
         Result := TypeCode.Content_Type (Result);
      end loop;

      return Result;
   end Unwind_Typedefs;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Types.Octet) return Content'Class
                    renames Elementary_Any_Octet.Wrap;
   function Wrap (X : access Types.Short) return Content'Class
                    renames Elementary_Any_Short.Wrap;
   function Wrap (X : access Types.Long) return Content'Class
                    renames Elementary_Any_Long.Wrap;
   function Wrap (X : access Types.Long_Long) return Content'Class
                    renames Elementary_Any_Long_Long.Wrap;
   function Wrap (X : access Types.Unsigned_Short) return Content'Class
                    renames Elementary_Any_UShort.Wrap;
   function Wrap (X : access Types.Unsigned_Long) return Content'Class
                    renames Elementary_Any_ULong.Wrap;
   function Wrap (X : access Types.Unsigned_Long_Long) return Content'Class
                    renames Elementary_Any_ULong_Long.Wrap;
   function Wrap (X : access Types.Boolean) return Content'Class
                    renames Elementary_Any_Boolean.Wrap;
   function Wrap (X : access Types.Char) return Content'Class
                    renames Elementary_Any_Char.Wrap;
   function Wrap (X : access Types.Wchar) return Content'Class
                    renames Elementary_Any_Wchar.Wrap;
   function Wrap (X : access Types.Float) return Content'Class
                    renames Elementary_Any_Float.Wrap;
   function Wrap (X : access Types.Double) return Content'Class
                    renames Elementary_Any_Double.Wrap;
   function Wrap (X : access Types.Long_Double) return Content'Class
                    renames Elementary_Any_Long_Double.Wrap;
   function Wrap (X : access Types.String) return Content'Class
                    renames Elementary_Any_String.Wrap;
   function Wrap (X : access Types.Wide_String) return Content'Class
                    renames Elementary_Any_Wide_String.Wrap;

   --  For compatibility with Ada 2005, we cannot use a renaming-as-body
   --  here, as the formal of the renamed subprogram would have to be
   --  explicitly null-excluding, which would make it illegal in Ada 95.

   function Wrap (X : access Any) return Content'Class is
   begin
      return Elementary_Any_Any.Wrap (X);
   end Wrap;

   function Wrap (X : access TypeCode.Local_Ref) return Content'Class
                    renames Elementary_Any_TypeCode.Wrap;
   function Wrap (X : access Ada.Strings.Superbounded.Super_String)
                    return Content'Class
                    renames Elementary_Any_Bounded_String.Wrap;
   function Wrap (X : access Ada.Strings.Wide_Superbounded.Super_String)
                    return Content'Class
                    renames Elementary_Any_Bounded_Wide_String.Wrap;

   --------------
   -- TypeCode --
   --------------

   package body TypeCode is

      --  Empty parameter list

      subtype Empty_Any_Array is Any_Array (1 .. 0);

      --  Default complex typecodes

      PTC_String      : TypeCode.Object_Ptr;
      PTC_Wide_String : TypeCode.Object_Ptr;

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
         pragma Debug (O ("Equal (TypeCode): enter"));

         if Kind (Left) /= Kind (Right) then
            pragma Debug (O ("Equal (TypeCode): end"));
            return False;
         end if;

         pragma Debug (O ("Equal (TypeCode): parameter number comparison"));

         Nb_Param := Parameter_Count (Right);

         if Nb_Param /= Parameter_Count (Left) then
            pragma Debug (O ("Equal (TypeCode): end"));
            return False;
         end if;

         if Nb_Param = 0 then
            pragma Debug (O ("Equal (TypeCode): end"));
            return True;
         end if;

         --  Recursive comparison

         pragma Debug (O ("Equal (TypeCode): recursive comparison"));

         for J in 0 .. Nb_Param - 1 loop
            if not "=" (Any_Container'Class'(Get_Parameter (Left, J).all),
                        Any_Container'Class'(Get_Parameter (Right, J).all))
            then
               pragma Debug (O ("Equal (TypeCode): end"));
               return False;
            end if;
         end loop;

         pragma Debug (O ("Equal (TypeCode): end"));
         return True;
      end Equal;

      -------------------
      -- Add_Parameter --
      -------------------

      procedure Add_Parameter (Self  : Local_Ref; Param : Any) is
         S_Parameters : Content_Ptr renames Object_Of (Self).Parameters;
      begin
         pragma Debug (O ("Add_Parameter: enter"));
         pragma Debug (O ("Add_Parameter: adding " & Image (Param)));

         if S_Parameters = null then
            S_Parameters := Allocate_Default_Aggregate_Content (Tk_TypeCode);
         end if;
         Add_Aggregate_Element
           (Default_Aggregate_Content (S_Parameters.all),
            Get_Container (Param));
         pragma Debug (O ("Add_Parameter: end"));
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

      --------------------------------
      -- Disable_Reference_Counting --
      --------------------------------

      procedure Disable_Reference_Counting (Self : in out Object) is
      begin
         Smart_Pointers.Disable_Reference_Counting (Self);
      end Disable_Reference_Counting;

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
                                     Member_Type (Right, J)) then
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
                 Member_Visibility (Right, J) then
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

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Self : in out Object) is
      begin
         pragma Debug (O ("Finalize (TypeCode.Object): enter"));

         if Self.Parameters /= null then
            Finalize_Value (Self.Parameters.all);
            Free (Self.Parameters);
         end if;

         pragma Debug (O ("Finalize (TypeCode.Object): leave"));
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
         TC_String, TC_Wide_String : Local_Ref;
      begin
         --  Do not ref count / garbage collect our library-level root TCs

         Smart_Pointers.Disable_Reference_Counting (PTC_Null);
         Smart_Pointers.Disable_Reference_Counting (PTC_Void);
         Smart_Pointers.Disable_Reference_Counting (PTC_Short);
         Smart_Pointers.Disable_Reference_Counting (PTC_Long);
         Smart_Pointers.Disable_Reference_Counting (PTC_Long_Long);
         Smart_Pointers.Disable_Reference_Counting (PTC_Unsigned_Short);
         Smart_Pointers.Disable_Reference_Counting (PTC_Unsigned_Long);
         Smart_Pointers.Disable_Reference_Counting (PTC_Unsigned_Long_Long);
         Smart_Pointers.Disable_Reference_Counting (PTC_Float);
         Smart_Pointers.Disable_Reference_Counting (PTC_Double);
         Smart_Pointers.Disable_Reference_Counting (PTC_Long_Double);
         Smart_Pointers.Disable_Reference_Counting (PTC_Boolean);
         Smart_Pointers.Disable_Reference_Counting (PTC_Char);
         Smart_Pointers.Disable_Reference_Counting (PTC_Wchar);
         Smart_Pointers.Disable_Reference_Counting (PTC_Octet);
         Smart_Pointers.Disable_Reference_Counting (PTC_Any);
         Smart_Pointers.Disable_Reference_Counting (PTC_TypeCode);

         TC_String      := Build_String_TC (0);
         TC_Wide_String := Build_Wstring_TC (0);

         PTC_String      := Object_Of (TC_String);
         PTC_Wide_String := Object_Of (TC_Wide_String);

         Smart_Pointers.Disable_Reference_Counting (PTC_String.all);
         Smart_Pointers.Disable_Reference_Counting (PTC_Wide_String.all);
      end Initialize;

      ------------
      -- Is_Nil --
      ------------

      function Is_Nil (Self : Local_Ref) return Boolean is
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
               pragma Debug (O ("Length: no such attribute for " & TK'Img));
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
        (Self : Local_Ref; Index : Unsigned_Long) return Any_Container_Ptr
      is
      begin
         return Member_Label (Object_Of (Self), Index);
      end Member_Label;

      function Member_Label
        (Self : Object_Ptr; Index : Unsigned_Long) return Any_Container_Ptr
      is
      begin
         return Get_Container (Member_Label (Self, Index));
      end Member_Label;

      ------------------
      -- Member_Label --
      ------------------

      function Member_Label
        (Self : Local_Ref; Index : Unsigned_Long) return Any
      is
      begin
         return Member_Label (Object_Of (Self), Index);
      end Member_Label;

      function Member_Label
        (Self : Object_Ptr; Index : Unsigned_Long) return Any
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
               return From_Any (Get_Parameter (Self, 3 * Index + 4).all);

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

            when Tk_Union =>
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

            when Tk_Value
              | Tk_Event =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 6).all);
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

      function Member_Type
        (Self : Object_Ptr; Index : Unsigned_Long) return Object_Ptr
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
         K        : constant TCKind := Kind (Self);
      begin
         pragma Debug (O ("Member_Type: enter, Kind is " & TCKind'Image (K)
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

            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               return Get_Parameter (Self, 3 * Index + 5);

            when Tk_Value
              | Tk_Event =>
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

      function Member_Type_With_Label
        (Self  : Object_Ptr;
         Label : Any) return Object_Ptr
      is
      begin
         return Member_Type_With_Label (Self, Get_Container (Label).all);
      end Member_Type_With_Label;

      function Member_Type_With_Label
        (Self  : Local_Ref;
         Label : Any_Container'Class) return Local_Ref
      is
      begin
         return To_Ref (Member_Type_With_Label (Object_Of (Self), Label));
      end Member_Type_With_Label;

      function Member_Type_With_Label
        (Self  : Object_Ptr;
         Label : Any_Container'Class) return Object_Ptr
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);

         Label_Found : Boolean := False;
         Member_Index : Long;

      begin
         pragma Debug (O ("Member_Type_With_Label: enter"));
         pragma Debug (O ("Member_Type_With_Label: Param_Nb = "
                          & Unsigned_Long'Image (Param_Nb)));

         --  See comments after the declaration of TypeCode.Object in the
         --  private part of PolyORB.Any.TypeCode to understand the magic
         --  numbers used here.

         if Kind (Self) /= Tk_Union then
            raise BadKind;
         end if;

         --  Look at the members until we got enough with the right label or we
         --  reach the end.

         pragma Debug (O ("Member_Type_With_Label: enter loop"));

         --  ??? This is horribly inefficient, we should have a fast lookup
         --  mechanism mapping a label value to the appropriate member index.

         Parameters :
         for Current_Member in 0 .. (Param_Nb - 4) / 3 - 1 loop

            --  We ignore the default member, as its placeholder label could
            --  interfere with a non-default label.

            if Long (Current_Member) /= Default_Index (Self)
              and then Member_Label (Self, Current_Member).all = Label
            then
               pragma Debug (O ("Member_Type_With_Label: matching label"));
               Label_Found := True;
               Member_Index := Long (Current_Member);
               exit Parameters;
            end if;
         end loop Parameters;

         if not Label_Found then
            Member_Index := Default_Index (Self);
            pragma Debug
              (O ("Member_Type_With_Label: using default member at index "
                  & Member_Index'Img));
         end if;

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
               declare
                  Res : PolyORB.Types.String;
               begin
                  Res := From_Any (Get_Parameter (Self, 0).all);
                  return Identifier (Res);
               end;

            when others =>
               raise BadKind;
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
         return To_Ref (PTC_String);
      end TC_String;

      --------------------
      -- TC_Wide_String --
      --------------------

      function TC_Wide_String return Local_Ref is
      begin
         return To_Ref (PTC_Wide_String);
      end TC_Wide_String;

      ------------------
      -- TC_Principal --
      ------------------

      function TC_Principal return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Principal, E);
      end TC_Principal;

      ---------------
      -- TC_Struct --
      ---------------

      function TC_Struct return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Struct, E);
      end TC_Struct;

      --------------
      -- TC_Union --
      --------------

      function TC_Union return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Union, E);
      end TC_Union;

      -------------
      -- TC_Enum --
      -------------

      function TC_Enum return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Enum, E);
      end TC_Enum;

      --------------
      -- TC_Alias --
      --------------

      function TC_Alias return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Alias, E);
      end TC_Alias;

      ---------------
      -- TC_Except --
      ---------------

      function TC_Except return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Except, E);
      end TC_Except;

      ---------------
      -- TC_Object --
      ---------------

      function TC_Object return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Objref, E);
      end TC_Object;

      --------------
      -- TC_Fixed --
      --------------

      function TC_Fixed return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Fixed, E);
      end TC_Fixed;

      -----------------
      -- TC_Sequence --
      -----------------

      function TC_Sequence return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Sequence, E);
      end TC_Sequence;

      --------------
      -- TC_Array --
      --------------

      function TC_Array return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Array, E);
      end TC_Array;

      --------------
      -- TC_Value --
      --------------

      function TC_Value return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Value, E);
      end TC_Value;

      -----------------
      -- TC_Valuebox --
      -----------------

      function TC_Valuebox return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Valuebox, E);
      end TC_Valuebox;

      ---------------
      -- TC_Native --
      ---------------

      function TC_Native return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Native, E);
      end TC_Native;

      ---------------------------
      -- TC_Abstract_Interface --
      ---------------------------

      function TC_Abstract_Interface return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Abstract_Interface, E);
      end TC_Abstract_Interface;

      ------------------------
      -- TC_Local_Interface --
      ------------------------

      function TC_Local_Interface return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Local_Interface, E);
      end TC_Local_Interface;

      ------------------
      -- TC_Component --
      ------------------

      function TC_Component return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Component, E);
      end TC_Component;

      -------------
      -- TC_Home --
      -------------

      function TC_Home return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Home, E);
      end TC_Home;

      --------------
      -- TC_Event --
      --------------

      function TC_Event return TypeCode.Local_Ref is
         E : Empty_Any_Array;
      begin
         return Build_Complex_TC (Tk_Event, E);
      end TC_Event;

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

end PolyORB.Any;
