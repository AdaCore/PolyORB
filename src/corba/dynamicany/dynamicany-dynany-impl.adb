------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               D Y N A M I C A N Y . D Y N A N Y . I M P L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Dynamic_Any;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

with DynamicAny.DynAny.Helper;

package body DynamicAny.DynAny.Impl is

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;
   use PolyORB.Types;
   use Internals;
   use type CORBA.Long;
   use type CORBA.Unsigned_Long;

   function Is_Ordinary_Aggregate (Kind : TCKind) return Boolean;
   --  Return True iff Kind is an ordinary aggregate (Tk_Struct,
   --  Tk_Union, Tk_Sequence, Tk_Array, Tk_Except).

   procedure Reset_Current_Position (Self : access Object'Class);
   --  Reset internal current position to initial value:
   --   0 - for Any's with not zero components
   --  -1 - for Any's without components or with zero components

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Self    : access Object;
      Dyn_Any : Local_Ref'Class)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Value : constant PolyORB.Any.Any
           := Object_Ptr (Entity_Of (Dyn_Any)).Value;

      begin
         if not Equivalent (Get_Type (Self.Value), Get_Type (Value)) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         Copy_Any_Value (Self.Value, Value);

         Reset_Current_Position (Self);
      end;
   end Assign;

   ---------------------
   -- Component_Count --
   ---------------------

   function Component_Count
     (Self : access Object)
      return CORBA.Unsigned_Long
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         T : constant PolyORB.Any.TypeCode.Object
           := Get_Unwound_Type (Self.Value);

      begin
         case Kind (T) is
            when Tk_Sequence =>
               return CORBA.Unsigned_Long (Get_Aggregate_Count (Self.Value));
               --  For sequences the operation returns the current number
               --  of elements.

            when Tk_Struct
              | Tk_Except
              | Tk_Value
            =>
               return CORBA.Unsigned_Long (Member_Count (T));
               --  For structures, exceptions and valuetypes the operation
               --  returns the number of members.

            when Tk_Array =>
               return CORBA.Unsigned_Long (Length (T));
               --  For arrays the operation returns the number of elements

            when Tk_Union =>
               raise Program_Error;
               --  For unions the operation returns 2 if the discriminator
               --  indicates that a named member is active; otherwise,
               --  it returns 1.

               --  XXX Not yet implemented. See CORBA 3.0 paragraph 9.2.2.9
               --  "Iterating through components of a DynAny"

            when others =>
               return 0;
               --  For all others types the operation always returns zero

         end case;
      end;
   end Component_Count;

   ----------
   -- Copy --
   ----------

   function Copy (Self : access Object) return Local_Ref'Class is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      return
        PolyORB.CORBA_P.Dynamic_Any.Create
        (CORBA.Internals.To_CORBA_Any (Self.Value), True, null);
   end Copy;

   -----------------------
   -- Current_Component --
   -----------------------

   function Current_Component (Self : access Object) return Local_Ref'Class is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      if not Is_Ordinary_Aggregate (Kind (Get_Unwound_Type (Self.Value)))
         or else Component_Count (Self) = 0
      then
         Helper.Raise_TypeMismatch
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      declare
         use Local_Ref_Lists;

         Null_Result : Local_Ref;
         Result      : Local_Ref;

      begin
         if Self.Current = -1 then
            return Result;
         end if;

         --  Check list of already created DynAny's and return cached value

         if Length (Self.Children) > Natural (Self.Current) then
            Result := Element (Self.Children, Natural (Self.Current)).all;

            if not Is_Nil (Result) then
               return Null_Result;
            end if;
         end if;

         --  Create new DynAny

         declare
            Elem : constant PolyORB.Any.Any
              := Get_Aggregate_Element
              (Self.Value, TypeCode.TC_Null, Unsigned_Long (Self.Current));

         begin
            Result :=
              PolyORB.CORBA_P.Dynamic_Any.Create
              (CORBA.Internals.To_CORBA_Any (Elem),
               True,
               Object_Ptr (Self));

            if Length (Self.Children) > Natural (Self.Current) then
               --  If corresponding item in list already present then
               --  set its value.

               Element (Self.Children, Natural (Self.Current)).all := Result;

            else
               --  Append empty items to list if needed

               for J
                 in Length (Self.Children) .. Integer (Self.Current) - 1
               loop
                  Append (Self.Children, Null_Result);
               end loop;

               --  Append created items

               Append (Self.Children, Result);
            end if;

            return Result;
         end;
      end;
   end Current_Component;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : access Object) is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      --  If DynAny is a top level object then mark it and all it's children
      --  are destroyed; otherwise do nothing.

      if Self.Parent = null then
         Mark_Destroyed (Self);
      end if;
   end Destroy;

   -----------
   -- Equal --
   -----------

   function Equal
     (Self    : access Object;
      Dyn_Any : Local_Ref'Class)
      return CORBA.Boolean
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      return Self.Value = Get_Value (Object_Ptr (Entity_Of (Dyn_Any)));
   end Equal;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Object) is
   begin
      --  Deallocate list of children

      Local_Ref_Lists.Deallocate (Self.Children);

      --  Finalize parent type

      CORBA.Local.Finalize (CORBA.Local.Object (Self));
   end Finalize;

   --------------
   -- From_Any --
   --------------

   procedure From_Any
     (Self  : access Object;
      Value : CORBA.Any)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Neutral : constant PolyORB.Any.Any
           := CORBA.Internals.To_PolyORB_Any (Value);

      begin
         if not Equivalent (Get_Type (Self.Value), Get_Type (Neutral)) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         Copy_Any_Value (Self.Value, Neutral);

         Reset_Current_Position (Self);
      end;
   end From_Any;

   ------------------
   -- Get_Abstract --
   ------------------

   function Get_Abstract (Self : access Object) return CORBA.AbstractBase.Ref
   is
      Result : CORBA.AbstractBase.Ref;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Abstract;

   -------------
   -- Get_Any --
   -------------

   function Get_Any (Self : access Object) return CORBA.Any is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Any then
            return
              CORBA.Internals.To_CORBA_Any
              (PolyORB.Any.Any'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Any, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Any then
                  return
                    CORBA.Internals.To_CORBA_Any
                    (PolyORB.Any.Any'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Any;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Self : access Object) return CORBA.Boolean is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Boolean then
            return From_Any (Self.Value);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Boolean,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Boolean then
                  return From_Any (Element);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Boolean;

   ---------------------
   -- Get_Boolean_Seq --
   ---------------------

   function Get_Boolean_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.BooleanSeq
   is
      Result : CORBA.IDL_SEQUENCES.BooleanSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Boolean_Seq;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Self : access Object) return CORBA.Char is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Char then
            return From_Any (Self.Value);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Char, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Char then
                  return From_Any (Element);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Char;

   ------------------
   -- Get_Char_Seq --
   ------------------

   function Get_Char_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.CharSeq
   is
      Result : CORBA.IDL_SEQUENCES.CharSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Char_Seq;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double (Self : access Object) return CORBA.Double is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Double then
            return CORBA.Double (PolyORB.Types.Double'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Double,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Double then
                  return
                    CORBA.Double (PolyORB.Types.Double'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Double;

   --------------------
   -- Get_Double_Seq --
   --------------------

   function Get_Double_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.DoubleSeq
   is
      Result : CORBA.IDL_SEQUENCES.DoubleSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Double_Seq;

   -----------------
   -- Get_Dyn_Any --
   -----------------

   function Get_Dyn_Any (Self : access Object) return Local_Ref'Class is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Any then
            declare
               use Local_Ref_Lists;

               Result : Local_Ref;

            begin
               --  In case of DynAny, contains another Any we use Children
               --  list for store only one reference to DynAny. Thus if
               --  list is empty we create new DynAny and if not empty
               --  return cached DynAny.

               if Is_Empty (Self.Children) then
                  Result :=
                    PolyORB.CORBA_P.Dynamic_Any.Create
                    (CORBA.Internals.To_CORBA_Any
                     (PolyORB.Any.Any'(From_Any (Self.Value))),
                     True,
                     Object_Ptr (Self));
                  Append (Self.Children, Result);

               else
                  Result := Element (Self.Children, 0).all;
               end if;

               return Result;
            end;

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Any, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Any then
                  --  In case of component of type Any we just return
                  --  the current component

                  return Current_Component (Self);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Dyn_Any;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Self : access Object) return CORBA.Float is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Float then
            return CORBA.Float (PolyORB.Types.Float'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Float, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Float then
                  return
                    CORBA.Float (PolyORB.Types.Float'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Float;

   -------------------
   -- Get_Float_Seq --
   -------------------

   function Get_Float_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.FloatSeq
   is
      Result : CORBA.IDL_SEQUENCES.FloatSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Float_Seq;

   --------------
   -- Get_Long --
   --------------

   function Get_Long (Self : access Object) return CORBA.Long is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Long then
            return CORBA.Long (PolyORB.Types.Long'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Long, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Long then
                  return CORBA.Long (PolyORB.Types.Long'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Long;

   ------------------
   -- Get_Long_Seq --
   ------------------

   function Get_Long_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongSeq
   is
      Result : CORBA.IDL_SEQUENCES.LongSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Long_Seq;

   --------------------
   -- Get_LongDouble --
   --------------------

   function Get_LongDouble (Self : access Object) return CORBA.Long_Double is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_LongDouble then
            return
              CORBA.Long_Double
              (PolyORB.Types.Long_Double'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Long_Double,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_LongDouble then
                  return
                    CORBA.Long_Double
                    (PolyORB.Types.Long_Double'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_LongDouble;

   ------------------------
   -- Get_LongDouble_Seq --
   ------------------------

   function Get_LongDouble_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongDoubleSeq
   is
      Result : CORBA.IDL_SEQUENCES.LongDoubleSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_LongDouble_Seq;

   ------------------
   -- Get_LongLong --
   ------------------

   function Get_LongLong (Self : access Object) return CORBA.Long_Long is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_LongLong then
            return
              CORBA.Long_Long
              (PolyORB.Types.Long_Long'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Long_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_LongLong then
                  return
                    CORBA.Long_Long
                    (PolyORB.Types.Long_Long'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_LongLong;

   ----------------------
   -- Get_LongLong_Seq --
   ----------------------

   function Get_LongLong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongLongSeq
   is
      Result : CORBA.IDL_SEQUENCES.LongLongSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_LongLong_Seq;

   ---------------
   -- Get_Octet --
   ---------------

   function Get_Octet (Self : access Object) return CORBA.Octet is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Octet then
            return CORBA.Octet (PolyORB.Types.Octet'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Octet, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Octet then
                  return
                    CORBA.Octet (PolyORB.Types.Octet'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Octet;

   -------------------
   -- Get_Octet_Seq --
   -------------------

   function Get_Octet_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.OctetSeq
   is
      Result : CORBA.IDL_SEQUENCES.OctetSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Octet_Seq;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (Self : access Object) return CORBA.Object.Ref is
      Result : CORBA.Object.Ref;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Reference;

   ---------------
   -- Get_Short --
   ---------------

   function Get_Short (Self : access Object) return CORBA.Short is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Short then
            return CORBA.Short (PolyORB.Types.Short'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Short, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Short then
                  return
                    CORBA.Short (PolyORB.Types.Short'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_Short;

   -------------------
   -- Get_Short_Seq --
   -------------------

   function Get_Short_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ShortSeq
   is
      Result : CORBA.IDL_SEQUENCES.ShortSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Short_Seq;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Self : access Object) return CORBA.String is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_String then
            return CORBA.String (PolyORB.Types.String'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_String,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_String then
                  return
                    CORBA.String (PolyORB.Types.String'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_String;

   ------------------
   -- Get_TypeCode --
   ------------------

   function Get_TypeCode (Self : access Object) return CORBA.TypeCode.Object is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_TypeCode then
            return
              CORBA.TypeCode.Internals.To_CORBA_Object
              (PolyORB.Any.TypeCode.Object'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Any, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Any then
                  return
                    CORBA.TypeCode.Internals.To_CORBA_Object
                    (PolyORB.Any.TypeCode.Object'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_TypeCode;

   ---------------
   -- Get_ULong --
   ---------------

   function Get_ULong (Self : access Object) return CORBA.Unsigned_Long is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ulong then
            return
              CORBA.Unsigned_Long
              (PolyORB.Types.Unsigned_Long'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ulong then
                  return
                    CORBA.Unsigned_Long
                    (PolyORB.Types.Unsigned_Long'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_ULong;

   -------------------
   -- Get_ULong_Seq --
   -------------------

   function Get_ULong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ULongSeq
   is
      Result : CORBA.IDL_SEQUENCES.ULongSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_ULong_Seq;

   -------------------
   -- Get_ULongLong --
   -------------------

   function Get_ULongLong
     (Self : access Object)
      return CORBA.Unsigned_Long_Long
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ulonglong then
            return
              CORBA.Unsigned_Long_Long
              (PolyORB.Types.Unsigned_Long_Long'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Long_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ulonglong then
                  return
                    CORBA.Unsigned_Long_Long
                    (PolyORB.Types.Unsigned_Long_Long'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_ULongLong;

   -----------------------
   -- Get_ULongLong_Seq --
   -----------------------

   function Get_ULongLong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ULongLongSeq
   is
      Result : CORBA.IDL_SEQUENCES.ULongLongSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_ULongLong_Seq;

   ----------------
   -- Get_UShort --
   ----------------

   function Get_UShort (Self : access Object) return CORBA.Unsigned_Short is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ushort then
            return
              CORBA.Unsigned_Short
              (PolyORB.Types.Unsigned_Short'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Short,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ushort then
                  return
                    CORBA.Unsigned_Short
                    (PolyORB.Types.Unsigned_Short'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_UShort;

   --------------------
   -- Get_UShort_Seq --
   --------------------

   function Get_UShort_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.UShortSeq
   is
      Result : CORBA.IDL_SEQUENCES.UShortSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_UShort_Seq;

   ---------------
   -- Get_WChar --
   ---------------

   function Get_WChar (Self : access Object) return CORBA.Wchar is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Widechar then
            return From_Any (Self.Value);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Wchar, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Widechar then
                  return From_Any (Element);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_WChar;

   -------------------
   -- Get_WChar_Seq --
   -------------------

   function Get_WChar_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.WCharSeq
   is
      Result : CORBA.IDL_SEQUENCES.WCharSeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_WChar_Seq;

   -----------------
   -- Get_WString --
   -----------------

   function Get_WString (Self : access Object) return CORBA.Wide_String is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Wstring then
            return
              CORBA.Wide_String
              (PolyORB.Types.Wide_String'(From_Any (Self.Value)));

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : constant PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Wide_String,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Wstring then
                  return
                    CORBA.Wide_String
                    (PolyORB.Types.Wide_String'(From_Any (Element)));

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Get_WString;

   --------------
   -- IDL_Type --
   --------------

   function IDL_Type (Self : access Object) return CORBA.TypeCode.Object is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      return CORBA.TypeCode.Internals.To_CORBA_Object (Get_Type (Self.Value));
   end IDL_Type;

   ---------------------
   -- Insert_Abstract --
   ---------------------

   procedure Insert_Abstract
     (Self  : access Object;
      Value : CORBA.AbstractBase.Ref)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Abstract;

   ----------------
   -- Insert_Any --
   ----------------

   procedure Insert_Any
     (Self  : access Object;
      Value : CORBA.Any)
   is

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Any then
            Set_Any_Value
              (CORBA.Internals.To_PolyORB_Any (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Any, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Any then
                  Set_Any_Value
                    (CORBA.Internals.To_PolyORB_Any (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Any;

   ---------------------
   -- Insert_Boolean  --
   ---------------------

   procedure Insert_Boolean
     (Self  : access Object;
      Value : CORBA.Boolean)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Boolean then
            Set_Any_Value (Value, Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Boolean,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Boolean then
                  Set_Any_Value (Value, Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Boolean;

   ------------------------
   -- Insert_Boolean_Seq --
   ------------------------

   procedure Insert_Boolean_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.BooleanSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Boolean_Seq;

   ------------------
   -- Insert_Char  --
   ------------------

   procedure Insert_Char
     (Self  : access Object;
      Value : CORBA.Char)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Char then
            Set_Any_Value (Value, Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Char, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Char then
                  Set_Any_Value (Value, Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Char;

   ---------------------
   -- Insert_Char_Seq --
   ---------------------

   procedure Insert_Char_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.CharSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Char_Seq;

   -------------------
   -- Insert_Double --
   -------------------

   procedure Insert_Double
     (Self  : access Object;
      Value : CORBA.Double)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Double then
            Set_Any_Value
              (PolyORB.Types.Double (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Double,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Double then
                  Set_Any_Value
                    (PolyORB.Types.Double (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Double;

   -----------------------
   -- Insert_Double_Seq --
   -----------------------

   procedure Insert_Double_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.DoubleSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Double_Seq;

   --------------------
   -- Insert_Dyn_Any --
   --------------------

   procedure Insert_Dyn_Any
     (Self  : access Object;
      Value : Local_Ref'Class)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Any then
            Set_Any_Value
              (Get_Value (Object_Ptr (Entity_Of (Value))),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Any, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Any then
                  Set_Any_Value
                    (Get_Value (Object_Ptr (Entity_Of (Value))),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Dyn_Any;

   -------------------
   -- Insert_Float  --
   -------------------

   procedure Insert_Float
     (Self  : access Object;
      Value : CORBA.Float)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Float then
            Set_Any_Value
              (PolyORB.Types.Float (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Float,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Float then
                  Set_Any_Value
                    (PolyORB.Types.Float (Value), Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Float;

   ----------------------
   -- Insert_Float_Seq --
   ----------------------

   procedure Insert_Float_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.FloatSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Float_Seq;

   ------------------
   -- Insert_Long  --
   ------------------

   procedure Insert_Long
     (Self  : access Object;
      Value : CORBA.Long)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Long then
            Set_Any_Value
              (PolyORB.Types.Long (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Long then
                  Set_Any_Value
                    (PolyORB.Types.Long (Value), Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Long;

   ---------------------
   -- Insert_Long_Seq --
   ---------------------

   procedure Insert_Long_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.LongSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Long_Seq;

   -----------------------
   -- Insert_LongDouble --
   -----------------------

   procedure Insert_LongDouble
     (Self  : access Object;
      Value : CORBA.Long_Double)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_LongDouble then
            Set_Any_Value
              (PolyORB.Types.Long_Double (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Long_Double,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_LongDouble then
                  Set_Any_Value
                    (PolyORB.Types.Long_Double (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_LongDouble;

   ---------------------------
   -- Insert_LongDouble_Seq --
   ---------------------------

   procedure Insert_LongDouble_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.LongDoubleSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_LongDouble_Seq;

   ----------------------
   -- Insert_LongLong  --
   ----------------------

   procedure Insert_LongLong
     (Self  : access Object;
      Value : CORBA.Long_Long)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_LongLong then
            Set_Any_Value
              (PolyORB.Types.Long_Long (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Long_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_LongLong then
                  Set_Any_Value
                    (PolyORB.Types.Long_Long (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_LongLong;

   -------------------------
   -- Insert_LongLong_Seq --
   -------------------------

   procedure Insert_LongLong_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.LongLongSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_LongLong_Seq;

   -------------------
   -- Insert_Octet  --
   -------------------

   procedure Insert_Octet
     (Self  : access Object;
      Value : CORBA.Octet)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Octet then
            Set_Any_Value
              (PolyORB.Types.Octet (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Octet,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Octet then
                  Set_Any_Value
                    (PolyORB.Types.Octet (Value), Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Octet;

   ----------------------
   -- Insert_Octet_Seq --
   ----------------------

   procedure Insert_Octet_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.OctetSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Octet_Seq;

   ----------------------
   -- Insert_Reference --
   ----------------------

   procedure Insert_Reference
     (Self  : access Object;
      Value : CORBA.Object.Ref)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Reference;

   -------------------
   -- Insert_Short  --
   -------------------

   procedure Insert_Short
     (Self  : access Object;
      Value : CORBA.Short)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Short then
            Set_Any_Value
              (PolyORB.Types.Short (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value, TypeCode.TC_Short, Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Short then
                  Set_Any_Value
                    (PolyORB.Types.Short (Value), Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_Short;

   ----------------------
   -- Insert_Short_Seq --
   ----------------------

   procedure Insert_Short_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.ShortSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_Short_Seq;

   -------------------
   -- Insert_String --
   -------------------

   procedure Insert_String
     (Self  : access Object;
      Value : CORBA.String)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_String then
            Set_Any_Value
              (PolyORB.Types.String (Value), Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_String,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_String then
                  Set_Any_Value
                    (PolyORB.Types.String (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_String;

   ---------------------
   -- Insert_Typecode --
   ---------------------

   procedure Insert_TypeCode
     (Self  : access Object;
      Value : CORBA.TypeCode.Object)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_TypeCode then
            Set_Any_Value
              (CORBA.TypeCode.Internals.To_PolyORB_Object (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_TypeCode,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_TypeCode then
                  Set_Any_Value
                    (CORBA.TypeCode.Internals.To_PolyORB_Object (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_TypeCode;

   -------------------
   -- Insert_ULong  --
   -------------------

   procedure Insert_ULong
     (Self  : access Object;
      Value : CORBA.Unsigned_Long)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ulong then
            Set_Any_Value
              (PolyORB.Types.Unsigned_Long (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ulong then
                  Set_Any_Value
                    (PolyORB.Types.Unsigned_Long (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_ULong;

   ----------------------
   -- Insert_ULong_Seq --
   ----------------------

   procedure Insert_ULong_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.ULongSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_ULong_Seq;

   ----------------------
   -- Insert_ULongLong --
   ----------------------

   procedure Insert_ULongLong
     (Self  : access Object;
      Value : CORBA.Unsigned_Long_Long)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ulonglong then
            Set_Any_Value
              (PolyORB.Types.Unsigned_Long_Long (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Long_Long,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ulonglong then
                  Set_Any_Value
                    (PolyORB.Types.Unsigned_Long_Long (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_ULongLong;

   --------------------------
   -- Insert_ULongLong_Seq --
   --------------------------

   procedure Insert_ULongLong_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.ULongLongSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_ULongLong_Seq;

   --------------------
   -- Insert_UShort  --
   --------------------

   procedure Insert_UShort
     (Self  : access Object;
      Value : CORBA.Unsigned_Short)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Ushort then
            Set_Any_Value
              (PolyORB.Types.Unsigned_Short (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Unsigned_Short,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Ushort then
                  Set_Any_Value
                    (PolyORB.Types.Unsigned_Short (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_UShort;

   -----------------------
   -- Insert_UShort_Seq --
   -----------------------

   procedure Insert_UShort_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.UShortSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_UShort_Seq;

   ------------------
   -- Insert_WChar --
   ------------------

   procedure Insert_WChar
     (Self  : access Object;
      Value : CORBA.Wchar)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Widechar then
            Set_Any_Value (Value, Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Wchar,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Widechar then
                  Set_Any_Value (Value, Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_WChar;

   ----------------------
   -- Insert_WChar_Seq --
   ----------------------

   procedure Insert_WChar_Seq
     (Self  : access Object;
      Value : CORBA.IDL_SEQUENCES.WCharSeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Insert_WChar_Seq;

   --------------------
   -- Insert_WString --
   --------------------

   procedure Insert_WString
     (Self  : access Object;
      Value : CORBA.Wide_String)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         K : constant TCKind := Kind (Get_Unwound_Type (Self.Value));

      begin
         if K = Tk_Wstring then
            Set_Any_Value
              (PolyORB.Types.Wide_String (Value),
               Get_Container (Self.Value).all);

         elsif not Is_Ordinary_Aggregate (K) then
            Helper.Raise_TypeMismatch
              ((CORBA.IDL_Exception_Members with null record));

         elsif Self.Current = -1 then
            Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));

         else
            declare
               Element : PolyORB.Any.Any
                 := Get_Aggregate_Element
                 (Self.Value,
                  TypeCode.TC_Wide_String,
                  Unsigned_Long (Self.Current));

            begin
               if Kind (Get_Unwound_Type (Element)) = Tk_Wstring then
                  Set_Any_Value
                    (PolyORB.Types.Wide_String (Value),
                     Get_Container (Element).all);

               else
                  Helper.Raise_TypeMismatch
                    ((CORBA.IDL_Exception_Members with null record));
               end if;
            end;
         end if;
      end;
   end Insert_WString;

   ---------------
   -- Internals --
   ---------------

   package body Internals is

      ------------
      -- Create --
      ------------

      function Create
        (Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr)
         return Local_Ref
      is
         Obj    : constant Object_Ptr := new Object;
         Result : Local_Ref;

      begin
         Initialize (Obj, Value, Parent);

         Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Obj));

         return Result;
      end Create;

      function Create
        (Value : PolyORB.Any.TypeCode.Object)
         return DynAny.Local_Ref
      is
         Obj    : constant Object_Ptr := new Object;
         Result : Local_Ref;

      begin
         Initialize (Obj, Value);

         Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Obj));

         return Result;
      end Create;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Self : access Object'Class) return PolyORB.Any.Any is
      begin
         return Self.Value;
      end Get_Value;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self     : access Object'Class;
         IDL_Type : PolyORB.Any.TypeCode.Object)
      is
         pragma Unreferenced (Self);
         pragma Unreferenced (IDL_Type);

      begin
         raise Program_Error;
      end Initialize;

      procedure Initialize
        (Self   : access Object'Class;
         Value  : PolyORB.Any.Any;
         Parent : Object_Ptr)
      is
      begin
         --  Copy Any value for top level object and store value otherwise

         if Parent = null then
            Self.Value := Get_Empty_Any (Get_Type (Value));
            Copy_Any_Value (Self.Value, Value);

         else
            Self.Value := Value;
         end if;

         --  Reset current position

         Reset_Current_Position (Self);

         --  Set up parent

         Self.Parent := Parent;
      end Initialize;

      ------------------
      -- Is_Destroyed --
      ------------------

      function Is_Destroyed (Self : access Object'Class) return Boolean is
      begin
         return Self.Is_Destroyed;
      end Is_Destroyed;

   end Internals;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return
        CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynAny.Repository_Id)
        or else
          CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ---------------------------
   -- Is_Ordinary_Aggregate --
   ---------------------------

   function Is_Ordinary_Aggregate (Kind : TCKind) return Boolean is
   begin
      case Kind is
         when Tk_Struct
           | Tk_Union
           | Tk_Sequence
           | Tk_Array
           | Tk_Except
           | Tk_Value
         =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Ordinary_Aggregate;

   --------------------
   -- Mark_Destroyed --
   --------------------

   procedure Mark_Destroyed (Self : access Object) is
      use Local_Ref_Lists;

      Iter : Iterator := First (Self.Children);

   begin
      Self.Is_Destroyed := True;

      while not Last (Iter) loop
         Mark_Destroyed (Object_Ptr (Entity_Of (Value (Iter).all)));
         Next (Iter);
      end loop;
   end Mark_Destroyed;

   ----------
   -- Next --
   ----------

   function Next (Self : access Object) return CORBA.Boolean is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      return Seek (Self, Self.Current + 1);
   end Next;

   ----------------------------
   -- Reset_Current_Position --
   ----------------------------

   procedure Reset_Current_Position (Self : access Object'Class) is
   begin
      if Is_Ordinary_Aggregate (Kind (Get_Unwound_Type (Self.Value)))
        and then Get_Aggregate_Count (Self.Value) /= 0
      then
         Self.Current := 0;

      else
         Self.Current := -1;
      end if;
   end Reset_Current_Position;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (Self : access Object) is
      Aux : Boolean;
      pragma Unreferenced (Aux);
      --  This variable used for temporary store return value of Seek
      --  operation. CORBA specification don't describe any exception
      --  for situation of Seek fault, so this value newer used.

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      Aux := Seek (Self, 0);
   end Rewind;

   ----------
   -- Seek --
   ----------

   function Seek
     (Self  : access Object;
      Index : CORBA.Long)
      return CORBA.Boolean
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      case Kind (Get_Unwound_Type (Self.Value)) is
         when Tk_Struct
           | Tk_Except
           | Tk_Sequence
           | Tk_Array
           | Tk_Value
           | Tk_Union
         =>
            if Index
              in 0 .. CORBA.Long (Get_Aggregate_Count (Self.Value) - 1)
            then
               Self.Current := Index;
               return True;

            else
               Self.Current := -1;
               return False;
            end if;

         when others =>
            return False;
      end case;
   end Seek;

   -------------
   -- To_Any  --
   -------------

   function To_Any (Self : access Object) return CORBA.Any is
      Result : PolyORB.Any.Any;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      Result := Get_Empty_Any (Get_Type (Self.Value));
      Copy_Any_Value (Result, Self.Value);

      return CORBA.Internals.To_CORBA_Any (Result);
   end To_Any;

end DynamicAny.DynAny.Impl;
