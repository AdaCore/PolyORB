------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Tags;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

with System.Address_Image;

package body PolyORB.Any is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("polyorb.any_refcnt");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;

   -----------------------------------------------
   -- Local declarations: Any contents wrappers --
   -----------------------------------------------

   --  'Octet' content

   type Content_Octet is new Content with record
      Value : Types.Octet_Ptr;
   end record;

   type Content_Octet_Ptr is access all Content_Octet;

   procedure Deallocate (Object : access Content_Octet);

   function Duplicate
     (Object : access Content_Octet)
     return Any_Content_Ptr;

   --  'Short' content

   type Content_Short is new Content with record
      Value : Types.Short_Ptr;
   end record;

   type Content_Short_Ptr is access all Content_Short;

   procedure Deallocate (Object : access Content_Short);

   function Duplicate
     (Object : access Content_Short)
     return Any_Content_Ptr;

   --  'Long' content

   type Content_Long is new Content with record
      Value : Types.Long_Ptr;
   end record;

   type Content_Long_Ptr is access all Content_Long;

   procedure Deallocate (Object : access Content_Long);

   function Duplicate
     (Object : access Content_Long)
     return Any_Content_Ptr;

   --  'Long_Long' content

   type Content_Long_Long is new Content with record
      Value : Types.Long_Long_Ptr;
   end record;

   type Content_Long_Long_Ptr is access all Content_Long_Long;

   procedure Deallocate (Object : access Content_Long_Long);

   function Duplicate
     (Object : access Content_Long_Long)
     return Any_Content_Ptr;

   --  'UShort' content

   type Content_UShort is new Content with record
      Value : Types.Unsigned_Short_Ptr;
   end record;

   type Content_UShort_Ptr is access all Content_UShort;

   procedure Deallocate (Object : access Content_UShort);

   function Duplicate
     (Object : access Content_UShort)
     return Any_Content_Ptr;

   --  'ULong' content

   type Content_ULong is new Content with record
      Value : Types.Unsigned_Long_Ptr;
   end record;

   type Content_ULong_Ptr is access all Content_ULong;

   procedure Deallocate (Object : access Content_ULong);

   function Duplicate
     (Object : access Content_ULong)
     return Any_Content_Ptr;

   --  'ULong_Long' content

   type Content_ULong_Long is new Content with record
      Value : Types.Unsigned_Long_Long_Ptr;
   end record;

   type Content_ULong_Long_Ptr is access all Content_ULong_Long;

   procedure Deallocate (Object : access Content_ULong_Long);

   function Duplicate
     (Object : access Content_ULong_Long)
     return Any_Content_Ptr;

   --  'Boolean' content

   type Content_Boolean is new Content with record
      Value : Types.Boolean_Ptr;
   end record;

   type Content_Boolean_Ptr is access all Content_Boolean;

   procedure Deallocate (Object : access Content_Boolean);

   function Duplicate
     (Object : access Content_Boolean)
     return Any_Content_Ptr;

   --  'Char' content

   type Content_Char is new Content with record
      Value : Types.Char_Ptr;
   end record;

   type Content_Char_Ptr is access all Content_Char;

   procedure Deallocate (Object : access Content_Char);

   function Duplicate
     (Object : access Content_Char)
     return Any_Content_Ptr;

   --  'WChar' content

   type Content_Wchar is new Content with record
      Value : Types.Wchar_Ptr;
   end record;

   type Content_Wchar_Ptr is access all Content_Wchar;

   procedure Deallocate (Object : access Content_Wchar);

   function Duplicate
     (Object : access Content_Wchar)
     return Any_Content_Ptr;

   --  'String' content

   type Content_String is new Content with record
      Value : Types.String_Ptr;
   end record;

   type Content_String_Ptr is access all Content_String;

   procedure Deallocate (Object : access Content_String);

   function Duplicate
     (Object : access Content_String)
     return Any_Content_Ptr;

   --  'Wide_String' content

   type Content_Wide_String is new Content with record
      Value : Types.Wide_String_Ptr;
   end record;

   type Content_Wide_String_Ptr is access all Content_Wide_String;

   procedure Deallocate (Object : access Content_Wide_String);

   function Duplicate
     (Object : access Content_Wide_String)
     return Any_Content_Ptr;

   --  'Float' content

   type Content_Float is new Content with record
      Value : Types.Float_Ptr;
   end record;

   type Content_Float_Ptr is access all Content_Float;

   procedure Deallocate (Object : access Content_Float);

   function Duplicate
     (Object : access Content_Float)
     return Any_Content_Ptr;

   --  'Double' content

   type Content_Double is new Content with record
      Value : Types.Double_Ptr;
   end record;

   type Content_Double_Ptr is access all Content_Double;

   procedure Deallocate (Object : access Content_Double);

   function Duplicate
     (Object : access Content_Double)
     return Any_Content_Ptr;

   --  'Long_Double' content

   type Content_Long_Double is new Content with record
      Value : Types.Long_Double_Ptr;
   end record;

   type Content_Long_Double_Ptr is access all Content_Long_Double;

   procedure Deallocate (Object : access Content_Long_Double);

   function Duplicate
     (Object : access Content_Long_Double)
     return Any_Content_Ptr;

   --  'TypeCode' content

   type Content_TypeCode is new Content with record
      Value : TypeCode.Object_Ptr;
   end record;

   type Content_TypeCode_Ptr is access all Content_TypeCode;

   procedure Deallocate (Object : access Content_TypeCode);

   function Duplicate
     (Object : access Content_TypeCode)
     return Any_Content_Ptr;

   --  'Any' content

   type Content_Any is new Content with record
      Value : Any_Ptr;
   end record;

   type Content_Any_Ptr is access all Content_Any;

   procedure Deallocate (Object : access Content_Any);

   function Duplicate
     (Object : access Content_Any)
     return Any_Content_Ptr;

   --  'Agregate' content

   --  While an aggregate is constructed, its contents are stored as a
   --  chained list.
   --  Once the construction is completed (i.e. the length of the list won't
   --  grow anymore), the list is converted to an array (to speed up access
   --  to random elements) and the aggegate is frozen (i.e. no elements can
   --  be added to it). Actually the freeze occurs the first time an element
   --  is retrieved through Get_Aggregate_Element.

   --  A list of Any contents (for construction of aggregates)

   package Content_Lists is new PolyORB.Utils.Chained_Lists (Any_Content_Ptr);
   subtype Content_List is Content_Lists.List;

   --  For complex types that could be defined in Idl a content_aggregate
   --  will be used.
   --
   --  Complex types include Struct, Union, Enum, Sequence,
   --  Array, Except, Fixed, Value, Valuebox, Abstract_Interface.
   --  Here is the way the content_list is used in each case
   --  (See CORBA V2.3 - 15.3) :
   --     - for Struct, Except : the elements are the values of each
   --  field in the order of the declaration
   --     - for Union : the value of the switch element comes
   --  first. Then come all the values of the corresponding fields
   --     - for Enum : an unsigned_long corresponding to the position
   --  of the value in the declaration is the only element
   --     - for Array : all the elements of the array, one by one.
   --     - for Sequence : the length first and then all the elements
   --  of the sequence, one by one.
   --     - for Fixed : XXX
   --     - for Value : XXX
   --     - for Valuebox : XXX
   --     - for Abstract_Interface : XXX

   type Content_Array is array (Natural range <>) of Any_Content_Ptr;
   type Content_Array_Access is access all Content_Array;

   function Duplicate (Contents : Content_Array_Access)
     return Content_Array_Access;

   procedure Deep_Deallocate (List : in out Content_List);
   procedure Deep_Deallocate (List : in out Content_Array_Access);

   type Aggregate_Value (Frozen : Boolean := False) is record
      case Frozen is
         when False =>
            Mutable_Value : Content_List := Content_Lists.Empty;
         when True =>
            Value : Content_Array_Access;
      end case;
   end record;
   procedure Freeze (Object : in out Aggregate_Value);

   type Content_Aggregate is new Content with record
      V : Aggregate_Value;
   end record;

   type Content_Aggregate_Ptr is access all Content_Aggregate;

   procedure Deallocate (Object : access Content_Aggregate);

   function Duplicate
     (Object : access Content_Aggregate)
     return Any_Content_Ptr;

   ---------------
   -- TypeCodes --
   ---------------

   package body TypeCode is

      ---------
      -- "=" --
      ---------

      function "="
        (Left, Right : in Object)
        return Boolean
      is
         Nb_Param : Unsigned_Long;
         Res : Boolean := True;
      begin
         pragma Debug (O ("Equal (TypeCode) : enter"));

         if Right.Kind /= Left.Kind then
            pragma Debug (O ("Equal (TypeCode) : end"));
            return False;
         end if;

         pragma Debug (O ("Equal (TypeCode) : parameter number comparison"));

         Nb_Param := Parameter_Count (Right);
         if Nb_Param /= Parameter_Count (Left) then
            pragma Debug (O ("Equal (TypeCode) : end"));
            return False;
         end if;

         if Nb_Param = 0 then
            pragma Debug (O ("Equal (TypeCode) : end"));
            return True;
         end if;

         --  recursive comparison
         pragma Debug (O ("Equal (TypeCode) : recursive comparison"));

         for J in 0 .. Nb_Param - 1 loop
            Res := Res and
              Equal (Get_Parameter (Left, J), Get_Parameter (Right, J));
            if Res = False then
               pragma Debug (O ("Equal (TypeCode) : end"));
               return False;
            end if;
         end loop;

         pragma Debug (O ("Equal (TypeCode) : end"));
         return Res;
      end "=";

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent
        (Left, Right : in Object)
        return Boolean
      is
         Nb_Param : Unsigned_Long := Member_Count (Left);
      begin
         --  comments are from the spec CORBA v2.3 - 10.7.1
         --  If the result of the kind operation on either TypeCode is
         --  tk_alias, recursively replace the TypeCode with the result
         --  of calling content_type, until the kind is no longer tk_alias.
         if Kind (Left) = Tk_Alias then
            return Equivalent (Content_Type (Left), Right);
         end if;
         if Kind (Right) = Tk_Alias then
            return Equivalent (Left, Content_Type (Right));
         end if;
         --  If results of the kind operation on each typecode differ,
         --  equivalent returns false.
         if Kind (Left) /= Kind (Right) then
            return False;
         end if;
         --  If the id operation is valid for the TypeCode kind, equivalent
         --  returns TRUE if the results of id for both TypeCodes are
         --  non-empty strings and both strings are equal. If both ids are
         --  non-empty but are not equal, then equivalent returns FALSE.
         case Kind (Left) is
            when Tk_Objref
              | Tk_Struct
              | Tk_Union
              | Tk_Enum
              | Tk_Value
              | Tk_Valuebox
              | Tk_Native
              | Tk_Abstract_Interface
              | Tk_Except =>
               declare
                  Id_Left  : constant RepositoryId := Id (Left);
                  Id_Right : constant RepositoryId := Id (Right);
                  Null_RepositoryId : constant RepositoryId
                    := RepositoryId'(To_PolyORB_String (""));
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
         --  If either or both id is an empty string, or the TypeCode kind
         --  does not support the id operation, equivalent will perform a
         --  structural comparison of the TypeCodes by comparing the results
         --  of the other TypeCode operations in the following bullet items
         --  (ignoring aliases as described in the first bullet.). The
         --  structural comparison only calls operations that are valid for
         --  the given TypeCode kind. If any of these operations do not return
         --  equal results, then equivalent returns FALSE. If all comparisons
         --  are equal, equivalent returns true.
         --    * The results of the name and member_name operations are ignored
         --  and not compared.
         --    * The results of the member_count operation are compared.
         case Kind (Left) is
            when Tk_Struct
              | Tk_Union
              | Tk_Enum
              | Tk_Value
              | Tk_Except =>
               if Member_Count (Left) /= Member_Count (Right) then
                  return False;
               end if;
               Nb_Param := Member_Count (Left);
            when others =>
               null;
         end case;
         --    * The results of the member_type operation for each member
         --  index are compared by recursively calling equivalent.
         case Kind (Left) is
            when Tk_Struct
              | Tk_Union
              | Tk_Value
              | Tk_Except =>
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
               then
                  if Member_Label (Left, J) /= Member_Label (Right, J) then
                     return False;
                  end if;
               end if;
            end loop;
         end if;
         --    * The results of the discriminator_type operation are compared
         --  by recursively calling equivalent.
         if Kind (Left) = Tk_Union and then
            not Equivalent (Discriminator_Type (Left),
                            Discriminator_Type (Right)) then
            return False;
         end if;
         --    * The results of the default_index operation are compared.
         if Kind (Left) = Tk_Union then
            if Default_Index (Left) > -1
              and then Default_Index (Right) > -1
            then
               if Default_Index (Left) /= Default_Index (Right) then
                  return False;
               end if;
            end if;
         end if;
         --    * The results of the length operation are compared.
         case Kind (Left) is
            when Tk_String
              | Tk_Sequence
              | Tk_Array =>
               if Length (Left) /= Length (Right) then
                  return False;
               end if;
            when others =>
               null;
         end case;
         --    * The results of the discriminator_type operation are compared
         --  by recursively calling equivalent.
         case Kind (Left) is
            when Tk_Sequence
              | Tk_Array
              | Tk_Valuebox =>
               if not Equivalent (Content_Type (Left),
                                  Content_Type (Right)) then
                  return False;
               end if;
            when others =>
               null;
         end case;
         --    * The results of the digits and scale operations are compared.
         if Kind (Left) = Tk_Fixed then
            if Fixed_Digits (Left) /= Fixed_Digits (Right) or
              Fixed_Scale (Left) /= Fixed_Scale (Right) then
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
                               Concrete_Base_Type (Right)) then
               return False;
            end if;
         end if;
         return True;
      end Equivalent;

      --------------------------
      -- Get_Compact_TypeCode --
      --------------------------

      function Get_Compact_TypeCode
        (Self : in Object)
        return Object is
      begin
         raise Not_Implemented;
         return Self;
      end Get_Compact_TypeCode;

      ----------
      -- Kind --
      ----------

      function Kind
        (Self : in Object)
        return TCKind is
      begin
         return Self.Kind;
      end Kind;

      --------
      -- Id --
      --------

      function Id
        (Self : in Object)
        return RepositoryId is
      begin
         case Kind (Self) is
            when Tk_Objref
              | Tk_Struct
              | Tk_Union
              | Tk_Enum
              | Tk_Alias
              | Tk_Value
              | Tk_Valuebox
              | Tk_Native
              | Tk_Abstract_Interface
              | Tk_Except =>
               declare
                  Res : PolyORB.Types.String;
               begin
                  Res := From_Any (Get_Parameter (Self, 1));
                  return RepositoryId (Res);
               end;

            when others =>
               raise BadKind;
         end case;
      end Id;

      ----------
      -- Name --
      ----------

      function Name
        (Self : in Object)
        return Identifier is
      begin
         case Kind (Self) is
            when Tk_Objref
              | Tk_Struct
              | Tk_Union
              | Tk_Enum
              | Tk_Alias
              | Tk_Value
              | Tk_Valuebox
              | Tk_Native
              | Tk_Abstract_Interface
              | Tk_Except =>
               declare
                  Res : PolyORB.Types.String;
               begin
                  Res := From_Any (Get_Parameter (Self, 0));
                  return Identifier (Res);
               end;

            when others =>
               raise BadKind;
         end case;
      end Name;

      ------------------
      -- Member_Count --
      ------------------

      function Member_Count
        (Self : in Object)
        return Unsigned_Long
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers returned here.
         case Kind (Self) is
            when Tk_Struct
              | Tk_Except =>
               return (Param_Nb / 2) - 1;

            when Tk_Union =>
               return (Param_Nb - 4) / 3;

            when Tk_Enum =>
               return Param_Nb - 2;

            when Tk_Value =>
               return (Param_Nb - 4) / 3;

            when others =>
               raise BadKind;
         end case;
      end Member_Count;

      -----------------
      -- Member_Name --
      -----------------

      function Member_Name
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Identifier
      is
         Param_Nb : constant Unsigned_Long
           := Parameter_Count (Self);
         Res      : PolyORB.Types.String;
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Struct
              | Tk_Except =>
               if Param_Nb < 2 * Index + 4 then
                  raise Bounds;
               end if;
               Res := From_Any (Get_Parameter (Self, 2 * Index + 3));
               return Identifier (Res);

            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 6));
               return Identifier (Res);

            when Tk_Enum =>
               if Param_Nb < Index + 3 then
                  raise Bounds;
               end if;
               Res := From_Any (Get_Parameter (Self, Index + 2));
               return Identifier (Res);

            when Tk_Value =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 6));
               return Identifier (Res);

            when others =>
               raise BadKind;
         end case;
      end Member_Name;

      -----------------
      -- Member_Type --
      -----------------

      function Member_Type
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Object
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
         K : constant TCKind := Kind (Self);
      begin
         pragma Debug (O ("member_type: enter, Kind is "
                          & TCKind'Image (K)));
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.

         case K is
            when Tk_Struct
              |  Tk_Except =>
               if Param_Nb < 2 * Index + 4 then
                  raise Bounds;
               end if;
               return From_Any (Get_Parameter (Self, 2 * Index + 2));

            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 5));

            when Tk_Value =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 5));

            when others =>
               raise BadKind;
         end case;
      end Member_Type;

      ------------------
      -- Member_Label --
      ------------------

      function Member_Label
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Any
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  raise Bounds;
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 4));

            when others =>
               raise BadKind;
         end case;
      end Member_Label;

      ---------------------
      -- Enumerator_Name --
      ---------------------

      function Enumerator_Name
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Types.Identifier
      is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      begin
         case Kind (Self) is
            when Tk_Enum =>
               if Param_Nb < Index + 3 then
                  raise Bounds;
               end if;
               return Types.Identifier
                 (Types.String'(From_Any (Get_Parameter (Self, Index + 2))));

            when others =>
               raise BadKind;
         end case;
      end Enumerator_Name;

      ------------------------
      -- Discriminator_Type --
      ------------------------

      function Discriminator_Type
        (Self : in Object)
        return Object is
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 2));

            when others =>
               raise BadKind;
         end case;
      end Discriminator_Type;

      -------------------
      -- Default_Index --
      -------------------

      function Default_Index
        (Self : in Object)
        return Types.Long is
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 3));

            when others =>
               raise BadKind;
         end case;
      end Default_Index;

      ------------
      -- Length --
      ------------

      function Length
        (Self : in Object)
        return Unsigned_Long is
      begin
         pragma Debug (O ("Length : enter & end"));
         case Kind (Self) is
            when Tk_String
              | Tk_Wstring
              | Tk_Sequence
              | Tk_Array =>
               return From_Any (Get_Parameter (Self, 0));
            when others =>
               raise BadKind;
         end case;
      end Length;

      ------------------
      -- Content_Type --
      ------------------

      function Content_Type
        (Self : in Object)
        return Object is
      begin
         case Kind (Self) is
            when Tk_Sequence
              | Tk_Array =>
               return From_Any (Get_Parameter (Self, 1));

            when Tk_Valuebox
              | Tk_Alias =>
               return From_Any (Get_Parameter (Self, 2));

            when others =>
               raise BadKind;
         end case;
      end Content_Type;

      ------------------
      -- Fixed_Digits --
      ------------------

      function Fixed_Digits
        (Self : in Object)
        return Unsigned_Short is
      begin
         case Kind (Self) is
            when Tk_Fixed =>
               return From_Any (Get_Parameter (Self, 0));

            when others =>
               raise BadKind;
         end case;
      end Fixed_Digits;

      -----------------
      -- Fixed_Scale --
      -----------------

      function Fixed_Scale
        (Self : in Object)
        return Short is
      begin
         case Kind (Self) is
            when Tk_Fixed =>
               return From_Any (Get_Parameter (Self, 1));

            when others =>
               raise BadKind;
         end case;
      end Fixed_Scale;

      -----------------------
      -- Member_Visibility --
      -----------------------

      function Member_Visibility
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Visibility is
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Value =>
               declare
                  Param_Nb : constant Unsigned_Long
                    := Parameter_Count (Self);
                  Res : Short;
               begin
                  if Param_Nb < 3 * Index + 7 then
                     raise Bounds;
                  end if;
                  Res := From_Any (Get_Parameter (Self, 3 * Index + 3));
                  return Visibility (Res);
               end;

            when others =>
               raise BadKind;
         end case;
      end Member_Visibility;

      -------------------
      -- Type_Modifier --
      -------------------

      function Type_Modifier
        (Self : in Object)
        return ValueModifier is
      begin
         case Kind (Self) is
            when Tk_Value =>
               declare
                  Res : Short;
               begin
                  Res := From_Any (Get_Parameter (Self, 2));
                  return ValueModifier (Res);
               end;

            when others =>
               raise BadKind;
         end case;
      end Type_Modifier;

      ------------------------
      -- Concrete_Base_Type --
      ------------------------

      function Concrete_Base_Type
        (Self : in Object)
        return Object is
      begin
         case Kind (Self) is
            when Tk_Value =>
               return From_Any (Get_Parameter (Self, 3));

            when others =>
               raise BadKind;
         end case;
      end Concrete_Base_Type;

      ----------------------------
      -- Member_Type_With_Label --
      ----------------------------

      function Member_Type_With_Label
        (Self  : in Object;
         Label : in Any;
         Index : in Unsigned_Long)
        return Object
      is
         Param_Nb : constant Unsigned_Long
           := Parameter_Count (Self);
         Current_Member : Unsigned_Long := 0;
         Default_Member : Unsigned_Long := -1;
         Member_Nb : Long := -1;
         Default_Nb : Long := -1;
      begin
         pragma Debug (O ("Member_Type_With_Label: enter"));
         pragma Debug (O ("Member_Type_With_Label: Param_Nb = "
                          & Unsigned_Long'Image (Param_Nb)
                          & ", Index = "
                          & Unsigned_Long'Image (Index)));

         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of CORBA.TypeCode
         --  to understand the magic numbers used here.

         if Kind (Self) = Tk_Union then
            --  Look at the members until we got enough with the
            --  right label or we reach the end.

            while Member_Nb < Long (Index)
              and then Param_Nb > 3 * Current_Member + 6 loop
               pragma Debug (O ("Member_Type_With_Label : enter loop"));
               --  if it is a default label, add one to the count
               if Default_Index (Self) = Long (Current_Member) then
                  pragma Debug (O ("Member_Type_With_Label : default label"));
                  Default_Nb := Default_Nb + 1;
                  --  else if it has the right label, add one to the count
               elsif Member_Label (Self, Current_Member) = Label then
                  pragma Debug (O ("Member_Type_With_Label : matching label"));
                  Member_Nb := Member_Nb + 1;
               end if;
               --  if we have enough default labels, keep the right
               --  parameter number in case we don't find any matching
               --  label
               if Default_Nb = Long (Index) then
                  pragma Debug (O ("Member_Type_With_Label : "
                                   & "default_member = "
                                   & Unsigned_Long'Image (Current_Member)));
                  Default_Member := Current_Member;
               end if;
               --  next member please
               Current_Member := Current_Member + 1;
            end loop;

            --  if we got enough member with the right label
            if Member_Nb = Long (Index) then
               pragma Debug (O ("Member_Type_With_Label : end"));
               return From_Any (Get_Parameter (Self, 3 * Current_Member + 2));
               --  else if we didn't got any matching label but
               --  we have enough default ones
            elsif Member_Nb = -1 and then Default_Nb >= Long (Index) then
               pragma Debug (O ("Member_Type_With_Label : default end"));
               return From_Any (Get_Parameter (Self, 3 * Default_Member + 5));
               --  else raise error
            else
               pragma Debug (O ("Member_Type_With_Label : "
                                & "end with exception"));
               raise Bounds;
            end if;
         else
            pragma Debug (O ("Member_Type_With_Label : "
                             & "end with exception"));
            raise BadKind;
         end if;
      end Member_Type_With_Label;

      -----------------------------
      -- Member_Count_With_Label --
      -----------------------------

      function Member_Count_With_Label
        (Self  : in Object;
         Label : in Any)
        return Unsigned_Long
      is
         Result : Unsigned_Long := 0;
         Default_Nb : Unsigned_Long := 0;
      begin
         --  See the big explanation after the declaration of
         --  TypeCode.Object in the private part of PolyORB.Any
         --  to understand the magic numbers used here.
         pragma Debug (O ("Member_Count_With_Label: enter"));
         if TypeCode.Kind (Self) = Tk_Union then
            pragma Debug (O ("Member_Count_With_Label: Member_Count = "
                             & Unsigned_Long'Image (Member_Count (Self))));
            for J in 0 .. Member_Count (Self) - 1 loop

               --  The label parameter for the default label is just a
               --  placeholder and must not be accounted for as a member
               --  for the label-specific count.

               if Default_Index (Self) = Long (J) then
                  pragma Debug
                    (O ("Member_Count_With_Label: member"
                        & Types.Unsigned_Long'Image (J)
                        & " is a default member."));
                  Default_Nb := Default_Nb + 1;
               elsif Member_Label (Self, J) = Label then
                  pragma Debug
                    (O ("Member_Count_With_Label: member"
                        & Types.Unsigned_Long'Image (J)
                        & " matches label."));
                  Result := Result + 1;
               end if;
            end loop;
            if Result = 0 then
               Result := Default_Nb;
            end if;
            pragma Debug (O ("Member_Count_With_Label: Result = "
                             & Unsigned_Long'Image (Result)));
            pragma Debug (O ("Member_Count_With_Label: end"));
            return Result;
         else
            pragma Debug (O ("Member_Count_With_Label: end "
                             & "with exception"));
            raise BadKind;
         end if;
      end Member_Count_With_Label;

      -------------------
      -- Get_Parameter --
      -------------------

      function Get_Parameter
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Any
      is
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         pragma Debug (O ("Get_Parameter : enter"));
         pragma Debug (O ("Get_Parameter : Index = " &
                          Unsigned_Long'Image (Index)));
         pragma Assert (Ptr /= null);
         pragma Debug (O ("Get_Parameter : assert OK"));
         if Index /= 0 then
            pragma Debug (O ("Get_Parameter : index /= 0"));
            for J in 0 .. Index - 1 loop
               Ptr := Ptr.Next;
               pragma Assert (Ptr /= null);
            end loop;
         end if;
         pragma Debug (O ("Get_Parameter : end"));
         return Ptr.Parameter;
      end Get_Parameter;

      -------------------
      -- Add_Parameter --
      -------------------

      procedure Add_Parameter
        (Self  : in out Object;
         Param : in     Any)
      is
         C_Ptr : Cell_Ptr := Self.Parameters;
      begin
         pragma Debug (O ("Add_Parameter: enter"));
         pragma Debug (O ("Add_Parameter: adding " & Image (Param)));

         if C_Ptr = null then
            Self.Parameters := new Cell'(Param, null);
         else
            while C_Ptr.Next /= null loop
               C_Ptr := C_Ptr.Next;
            end loop;
            C_Ptr.Next := new Cell'(Param, null);
         end if;
         pragma Debug (O ("Add_Parameter: end"));
      end Add_Parameter;

      ------------------
      -- Set_Volatile --
      ------------------

      procedure Set_Volatile
        (Self        : in out Object;
         Is_Volatile : in     Boolean) is
      begin
         Self.Is_Volatile := Is_Volatile;
      end Set_Volatile;

      ----------------------
      -- Destroy_TypeCode --
      ----------------------

      procedure Destroy_TypeCode
        (Self : in out Object)
      is

         procedure Free is new Ada.Unchecked_Deallocation
           (Cell, Cell_Ptr);

         C_Ptr : Cell_Ptr := Self.Parameters;

         procedure Deallocate_Cells (Self : in out Cell_Ptr);

         procedure Deallocate_Cells (Self : in out Cell_Ptr) is
         begin
            if Self = null then
               pragma Debug (O2 ("Deallocate_Cells: leave"));
               return;
            else
               pragma Debug (O2 ("Deallocate_Cells: iterate"));
               Deallocate_Cells (Self.Next);
               Free (Self);
            end if;
         end Deallocate_Cells;

      begin
         pragma Debug (O2 ("Destroy_TypeCode: enter"));

         if Self.Is_Destroyed then
            pragma Debug (O2 ("Destroy_TypeCode: already destroyed!"));
            return;
         end if;

         Self.Is_Destroyed := True;

         if Self.Is_Volatile then
            Deallocate_Cells (C_Ptr);
         else
            pragma Debug (O2 ("Destroy_TypeCode:"
                              & " no deallocating required"));
            null;
         end if;
         pragma Debug (O2 ("Destroy_TypeCode: leave"));

      end Destroy_TypeCode;

      --------------
      -- Set_Kind --
      --------------

      procedure Set_Kind
        (Self : out Object;
         Kind : in  TCKind) is
      begin
         Self.Kind := Kind;
         Self.Parameters := null;
      end Set_Kind;

      -------------
      -- TC_Null --
      -------------

      function TC_Null
        return TypeCode.Object is
      begin
         return PTC_Null;
      end TC_Null;

      -------------
      -- TC_Void --
      -------------

      function TC_Void
        return TypeCode.Object is
      begin
         return PTC_Void;
      end TC_Void;

      --------------
      -- TC_Short --
      --------------

      function TC_Short
        return TypeCode.Object is
      begin
         return PTC_Short;
      end TC_Short;

      -------------
      -- TC_Long --
      -------------

      function TC_Long
        return TypeCode.Object is
      begin
         return PTC_Long;
      end TC_Long;

      ------------------
      -- TC_Long_Long --
      ------------------

      function TC_Long_Long
        return TypeCode.Object is
      begin
         return PTC_Long_Long;
      end TC_Long_Long;

      -----------------------
      -- TC_Unsigned_Short --
      -----------------------

      function TC_Unsigned_Short
        return TypeCode.Object is
      begin
         return PTC_Unsigned_Short;
      end TC_Unsigned_Short;

      ----------------------
      -- TC_Unsigned_Long --
      ----------------------

      function TC_Unsigned_Long
        return TypeCode.Object is
      begin
         return PTC_Unsigned_Long;
      end TC_Unsigned_Long;

      ---------------------------
      -- TC_Unsigned_Long_Long --
      ---------------------------

      function TC_Unsigned_Long_Long
        return TypeCode.Object is
      begin
         return PTC_Unsigned_Long_Long;
      end TC_Unsigned_Long_Long;

      --------------
      -- TC_Float --
      --------------

      function TC_Float
        return TypeCode.Object is
      begin
         return PTC_Float;
      end TC_Float;

      ---------------
      -- TC_Double --
      ---------------

      function TC_Double
        return TypeCode.Object is
      begin
         return PTC_Double;
      end TC_Double;

      --------------------
      -- TC_Long_Double --
      --------------------

      function TC_Long_Double
        return TypeCode.Object is
      begin
         return PTC_Long_Double;
      end TC_Long_Double;

      ----------------
      -- TC_Boolean --
      ----------------

      function TC_Boolean
        return TypeCode.Object is
      begin
         return PTC_Boolean;
      end TC_Boolean;

      -------------
      -- TC_Char --
      -------------

      function TC_Char
        return TypeCode.Object is
      begin
         return PTC_Char;
      end TC_Char;

      --------------
      -- TC_WChar --
      --------------

      function TC_Wchar
        return TypeCode.Object is
      begin
         return PTC_Wchar;
      end TC_Wchar;

      --------------
      -- TC_Octet --
      --------------

      function TC_Octet
        return TypeCode.Object is
      begin
         return PTC_Octet;
      end TC_Octet;

      ------------
      -- TC_Any --
      ------------

      function TC_Any
        return TypeCode.Object is
      begin
         return PTC_Any;
      end TC_Any;

      -----------------
      -- TC_TypeCode --
      -----------------

      function TC_TypeCode
        return TypeCode.Object is
      begin
         return PTC_TypeCode;
      end TC_TypeCode;

      ---------------
      -- TC_String --
      ---------------

      function TC_String
        return TypeCode.Object
      is
         Result : TypeCode.Object := PTC_String;
      begin
         Add_Parameter (Result, To_Any (Unsigned_Long (0)));
         return Result;
      end TC_String;

      --------------------
      -- TC_Wide_String --
      --------------------

      function TC_Wide_String
        return TypeCode.Object is
      begin
         return PTC_Wide_String;
      end TC_Wide_String;

      ------------------
      -- TC_Principal --
      ------------------

      function TC_Principal
        return TypeCode.Object is
      begin
         return PTC_Principal;
      end TC_Principal;

      ---------------
      -- TC_Struct --
      ---------------

      function TC_Struct
        return TypeCode.Object is
      begin
         return PTC_Struct;
      end TC_Struct;

      --------------
      -- TC_Union --
      --------------

      function TC_Union
        return TypeCode.Object is
      begin
         return PTC_Union;
      end TC_Union;

      -------------
      -- TC_Enum --
      -------------

      function TC_Enum
        return TypeCode.Object is
      begin
         return PTC_Enum;
      end TC_Enum;

      --------------
      -- TC_Alias --
      --------------

      function TC_Alias
        return TypeCode.Object is
      begin
         return PTC_Alias;
      end TC_Alias;

      -----------------
      --  TC_Except  --
      -----------------

      function TC_Except
        return TypeCode.Object is
      begin
         return PTC_Except;
      end TC_Except;

      ---------------
      -- TC_Object --
      ---------------

      function TC_Object
        return TypeCode.Object is
      begin
         return PTC_Object;
      end TC_Object;

      --------------
      -- TC_Fixed --
      --------------

      function TC_Fixed
        return TypeCode.Object is
      begin
         return PTC_Fixed;
      end TC_Fixed;

      -----------------
      -- TC_Sequence --
      -----------------

      function TC_Sequence
        return TypeCode.Object is
      begin
         return PTC_Sequence;
      end TC_Sequence;

      --------------
      -- TC_Array --
      --------------

      function TC_Array
        return TypeCode.Object is
      begin
         return PTC_Array;
      end TC_Array;

      --------------
      -- TC_Value --
      --------------

      function TC_Value
        return TypeCode.Object is
      begin
         return PTC_Value;
      end TC_Value;

      -----------------
      -- TC_Valuebox --
      -----------------

      function TC_Valuebox
        return TypeCode.Object is
      begin
         return PTC_Valuebox;
      end TC_Valuebox;

      ---------------
      -- TC_Native --
      ---------------

      function TC_Native
        return TypeCode.Object is
      begin
         return PTC_Native;
      end TC_Native;

      ---------------------------
      -- TC_Abstract_Interface --
      ---------------------------

      function TC_Abstract_Interface
        return TypeCode.Object is
      begin
         return PTC_Abstract_Interface;
      end TC_Abstract_Interface;

      ---------------------
      -- Parameter_Count --
      ---------------------

      function Parameter_Count
        (Self : in Object)
        return Unsigned_Long
      is
         N : Unsigned_Long := 0;
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         while (Ptr /= null) loop
            N := N + 1;
            Ptr := Ptr.Next;
         end loop;

         return N;
      end Parameter_Count;

   end TypeCode;

   -----------
   --  Any  --
   -----------

   -----------
   -- Image --
   -----------

   function Image
     (TC : TypeCode.Object)
     return Standard.String
   is
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
              & Types.String'(From_Any (Get_Parameter (TC, 0)))
              & To_PolyORB_String (" (")
              & Types.String'(From_Any (Get_Parameter (TC, 1)))
              & To_PolyORB_String (")");

            --  Add a few information

            case Kind is
               when
                 Tk_Objref             |
                 Tk_Native             |
                 Tk_Abstract_Interface =>
                  return To_Standard_String (Result);

               when
                 Tk_Struct             |
                 Tk_Except             =>
                  Result := Result & To_PolyORB_String (" {");

                  declare
                     I : Types.Unsigned_Long := 2;
                     C : constant Types.Unsigned_Long
                       := Parameter_Count (TC);
                  begin
                     while I < C loop
                        Result := Result & To_PolyORB_String
                          (" " & Image
                           (TypeCode.Object'
                            (From_Any (Get_Parameter (TC, I))))
                           & " ")
                          & Types.String'
                          (From_Any (Get_Parameter (TC, I + 1)));
                        I := I + 2;
                     end loop;
                  end;
                  Result := Result & To_PolyORB_String (" }");

                  return To_Standard_String (Result);

               when others =>
                  return "<aggregate:" & TCKind'Image (Kind) & ">";
            end case;

         when others =>
            return TCKind'Image (Kind);
      end case;
   end Image;

   function Image
     (A : Any)
     return Standard.String
   is
      TC   : constant TypeCode.Object := Get_Unwound_Type (A);
      Kind : constant TCKind := TypeCode.Kind (TC);
   begin
      if Is_Empty (A) then
         return "<empty>";
      end if;

      case Kind is
         when Tk_Short =>
            return Short'Image (From_Any (A));

         when Tk_Long =>
            return Long'Image (From_Any (A));

         when Tk_Ushort =>
            return Unsigned_Short'Image (From_Any (A));

         when Tk_Ulong =>
            return Unsigned_Long'Image (From_Any (A));

         when Tk_Float =>
            return Types.Float'Image (From_Any (A));

         when Tk_Double =>
            return Double'Image (From_Any (A));

         when Tk_Boolean =>
            return Boolean'Image (From_Any (A));

         when Tk_Char =>
            return Char'Image (From_Any (A));

         when Tk_Octet =>
            return Octet'Image (From_Any (A));

         when Tk_String =>
            return To_Standard_String (From_Any (A));

         when Tk_Longlong =>
            return Long_Long'Image (From_Any (A));

         when Tk_Ulonglong =>
            return Unsigned_Long_Long'Image (From_Any (A));

         when Tk_Enum =>
            return Types.To_Standard_String
              (TypeCode.Enumerator_Name
               (TC, From_Any (Get_Aggregate_Element
                              (A, TC_Unsigned_Long, 0))));

         when Tk_Value =>
            return "<Value:"
              & Image (Get_Type (A)) & ":"
              & System.Address_Image (Get_Value (A)'Address) & ">";

         when Tk_Any =>
            return "<Any:"
              & Image (Content_Any_Ptr (Get_Value (A)).Value.all) & ">";

         when others =>
            return "<Any:" & Image (Get_Type (A)) & ">";
      end case;
   end Image;

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : in Any)
     return Boolean is
   begin
      pragma Debug (O ("Equal (Any): enter, "
                       & Image (Left) & " =? " & Image (Right)));
      if not TypeCode.Equal (Get_Type (Left), Get_Type (Right)) then
         pragma Debug (O ("Equal (Any) : end"));
         return False;
      end if;

      pragma Debug (O ("Equal (Any) : passed typecode test"));
      case TypeCode.Kind (Get_Unwound_Type (Left)) is
         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Equal (Any, Null or Void) : end"));
            return True;

         when Tk_Short =>
            declare
               L : constant Short := From_Any (Left);
               R : constant Short := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Short) : end"));
               return L = R;
            end;

         when Tk_Long =>
            declare
               L : constant Long := From_Any (Left);
               R : constant Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long) : end"));
               return L = R;
            end;

         when Tk_Ushort =>
            declare
               L : constant Unsigned_Short := From_Any (Left);
               R : constant Unsigned_Short := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Ushort) : end"));
               return L = R;
            end;

         when Tk_Ulong =>
            declare
               L : constant Unsigned_Long := From_Any (Left);
               R : constant Unsigned_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Ulong) : end"));
               return L = R;
            end;

         when Tk_Float =>
            declare
               L : constant Types.Float := From_Any (Left);
               R : constant Types.Float := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Float) : end"));
               return L = R;
            end;

         when Tk_Double =>
            declare
               L : constant Double := From_Any (Left);
               R : constant Double := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Double) : end"));
               return L = R;
            end;

         when Tk_Boolean =>
            declare
               L : constant Boolean := From_Any (Left);
               R : constant Boolean := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Boolean) : end"));
               return L = R;
            end;

         when Tk_Char =>
            declare
               L : constant Char := From_Any (Left);
               R : constant Char := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Char) : end"));
               return L = R;
            end;

         when Tk_Octet =>
            declare
               L : constant Octet := From_Any (Left);
               R : constant Octet := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Octet) : end"));
               return L = R;
            end;

         when Tk_Any =>
            declare
               L : constant Any := From_Any (Left);
               R : constant Any := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Any) : end"));
               return Equal (L, R);
            end;

         when Tk_TypeCode =>
            declare
               L : constant TypeCode.Object := From_Any (Left);
               R : constant TypeCode.Object := From_Any (Right);
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
                  pragma Debug (O ("Equal (Any, TypeCode) : end"));
                  return TypeCode.Equal (R, L);
               end if;
            end;

         when Tk_Principal =>
            --  XXX : to be done
            pragma Debug (O ("Equal (Any, Principal) : end"
                             & " NOT IMPLEMENTED -> TRUE"));
            return True;

         when Tk_Objref =>
            declare
--               L : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Left);
--               R : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, ObjRef) : end"
                                & " NOT IMPLEMENTED -> TRUE"));
               --  XXX : is_equivalent has to be implemented
               return True;
               --  return CORBA.Object.Is_Equivalent (L, R);
            end;

         when Tk_Struct
           | Tk_Except =>
            declare
               List_Type : constant TypeCode.Object
                 := Get_Unwound_Type (Left);
               Member_Type : TypeCode.Object;
            begin
               --  for each member in the aggregate, compare both values
               for J in 0 .. TypeCode.Member_Count (List_Type) - 1 loop
                  Member_Type := TypeCode.Member_Type (List_Type, J);
                  if not Equal (Get_Aggregate_Element (Left, Member_Type, J),
                                Get_Aggregate_Element (Right, Member_Type, J))
                  then
                     pragma Debug (O ("Equal (Any, Struct or Except) : end"));
                     return False;
                  end if;
               end loop;
               pragma Debug (O ("Equal (Any, Struct or Except) : end"));
               return True;
            end;

         when Tk_Union =>
            declare
               List_Type : constant TypeCode.Object
                 := Get_Unwound_Type (Left);
               Switch_Type : constant TypeCode.Object :=
                 TypeCode.Discriminator_Type (List_Type);
               Member_Type : TypeCode.Object;
            begin
               --  first compares the switch value
               if not Equal (Get_Aggregate_Element (Left, Switch_Type,
                                                    Unsigned_Long (0)),
                             Get_Aggregate_Element (Right, Switch_Type,
                                                    Unsigned_Long (0)))
               then
                  pragma Debug (O ("Equal (Any, Union) : end"));
                  return False;
               end if;

               declare
                  Switch_Label : Any
                    := Get_Aggregate_Element
                    (Left, Switch_Type, Unsigned_Long (0));
               begin
                  --  then, for each member in the aggregate,
                  --  compares both values
                  for J in 1 .. TypeCode.Member_Count_With_Label
                    (List_Type, Switch_Label) loop
                     Member_Type := TypeCode.Member_Type_With_Label
                       (List_Type, Switch_Label, J - 1);
                     if not Equal
                       (Get_Aggregate_Element (Left, Member_Type, J),
                        Get_Aggregate_Element (Right, Member_Type, J))
                     then
                        pragma Debug (O ("Equal (Any, Union) : end"));
                        return False;
                     end if;
                  end loop;
                  pragma Debug (O ("Equal (Any,Union) : end"));
                  return True;
               end;
            end;

         when Tk_Enum =>
            pragma Debug (O ("Equal (Any, Enum) : end"));
            --  compares the only element of both aggregate : an unsigned long
            return Equal
              (Get_Aggregate_Element (Left, TC_Unsigned_Long,
                                      Unsigned_Long (0)),
               Get_Aggregate_Element (Right, TC_Unsigned_Long,
                                      Unsigned_Long (0)));

         when Tk_Sequence
           | Tk_Array =>
            declare
               List_Type : constant TypeCode.Object
                 := Get_Unwound_Type (Left);
               Member_Type : constant TypeCode.Object
                 := TypeCode.Content_Type (List_Type);
            begin
               --  for each member in the aggregate, compare both values

               for J in 0 .. TypeCode.Length (List_Type) - 1 loop
                  if not Equal (Get_Aggregate_Element (Left, Member_Type, J),
                                Get_Aggregate_Element (Right, Member_Type, J))
                  then
                     pragma Debug (O ("Equal (Any, Sequence or Array) : end"));
                     return False;
                  end if;
               end loop;

               pragma Debug (O ("Equal (Any, Sequence or Array) : end"));
               return True;
            end;

         when Tk_Fixed
           | Tk_Value
           | Tk_Valuebox
           | Tk_Abstract_Interface =>
            --  XXX : to be done
            pragma Debug (O ("Equal (Any, Fixed or Value or ValueBox "
                             & "or Abstract_Interface) : end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;

         when Tk_String =>
            declare
               L : Types.String := From_Any (Left);
               R : Types.String := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, String) : end"));
               return L = R;
            end;

         when Tk_Alias =>
            --  we should never be here, since the case statement uses the
            --  precise type of the anys, that is an unaliased type
            pragma Debug (O ("Equal (Any, Alias) : end with exception"));
            raise Program_Error;

         when Tk_Longlong =>
            declare
               L : constant Long_Long := From_Any (Left);
               R : constant Long_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long_Long) : end"));
               return L = R;
            end;

         when Tk_Ulonglong =>
            declare
               L : constant Unsigned_Long_Long := From_Any (Left);
               R : constant Unsigned_Long_Long := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Unsigned_Long_Long) : end"));
               return L = R;
            end;

         when Tk_Longdouble =>
            declare
               L : constant Long_Double := From_Any (Left);
               R : constant Long_Double := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Long_Double) : end"));
               return L = R;
            end;

         when Tk_Widechar =>
            declare
               L : constant Wchar := From_Any (Left);
               R : constant Wchar := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Wchar) : end"));
               return L = R;
            end;

         when Tk_Wstring =>
            declare
               L : constant Types.Wide_String := From_Any (Left);
               R : constant Types.Wide_String := From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, Wide_String) : end"));
               return L = R;
            end;

         when Tk_Native =>
            --  XXX  to be done
            pragma Debug (O ("Equal (Any, Native) : end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;
      end case;
   end "=";

   --------------------------
   -- Compare_Any_Contents --
   --------------------------

   function Compare_Any_Contents
     (Left  : in Any;
      Right : in Any)
     return Boolean
   is
      C_Left, C_Right : Any_Content_Ptr_Ptr;
   begin
      C_Left := Get_Value_Ptr (Left);
      C_Right := Get_Value_Ptr (Right);

      pragma Debug (O ("Compare: "
                       & System.Address_Image (C_Left.all'Address)
                       & " = "
                       & System.Address_Image (C_Right.all'Address)));

      return C_Left = C_Right;
   end Compare_Any_Contents;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in Short)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Short'
                 (Value => new Short'(Item)));
      Set_Type (Result, TypeCode.TC_Short);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Long)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long'
                 (Value => new Long'(Item)));
      Set_Type (Result, TypeCode.TC_Long);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Long_Long)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Long'
                 (Value => new Long_Long'(Item)));
      Set_Type (Result, TypeCode.TC_Long_Long);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Unsigned_Short)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_UShort'
                 (Value => new Unsigned_Short'(Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Short);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Unsigned_Long)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_ULong'
                 (Value => new Unsigned_Long'(Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Long);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Unsigned_Long_Long)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_ULong_Long'
                 (Value => new Unsigned_Long_Long'(Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Long_Long);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Types.Float)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Float'
                 (Value => new Types.Float'(Item)));
      Set_Type (Result, TypeCode.TC_Float);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Double)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Double'
                 (Value => new Double'(Item)));
      Set_Type (Result, TypeCode.TC_Double);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Long_Double)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Double'
                 (Value => new Long_Double'(Item)));
      Set_Type (Result, TypeCode.TC_Long_Double);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Boolean)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Boolean'
                 (Value => new Boolean'(Item)));
      Set_Type (Result, TypeCode.TC_Boolean);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Char)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Char'
                 (Value => new Char'(Item)));
      Set_Type (Result, TypeCode.TC_Char);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Wchar)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Wchar'
                 (Value => new Wchar'(Item)));
      Set_Type (Result, TypeCode.TC_Wchar);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Octet)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_Octet'
                 (Value => new Octet'(Item)));
      Set_Type (Result, TypeCode.TC_Octet);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Any)
     return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Any'
                 (Value => new Any'(Item)));
      Set_Type (Result, TypeCode.TC_Any);

      return Result;
   end To_Any;

   function To_Any
     (Item : in TypeCode.Object)
     return Any
   is
      Result : Any;
   begin
      Set_Value (Result, new Content_TypeCode'
                 (Value => new TypeCode.Object'(Item)));
      Set_Type (Result, TypeCode.TC_TypeCode);

      return Result;
   end To_Any;

   function To_Any
     (Item : in Types.String)
     return Any
   is
      Result : Any;
      Tco : TypeCode.Object;
   begin
      pragma Debug (O ("To_Any (String) : enter"));

      TypeCode.Set_Kind (Tco, Tk_String);
      TypeCode.Add_Parameter (Tco, To_Any (Unsigned_Long (0)));
      TypeCode.Set_Volatile (Tco, True);
      --  the string is supposed to be unbounded

      Set_Value (Result, new Content_String'
                 (Value => new PolyORB.Types.String'(Item)));
      Set_Type (Result, Tco);
      pragma Debug (O ("To_Any (String) : end"));
      return Result;
   end To_Any;

   function To_Any
     (Item : in Types.Wide_String)
     return Any
   is
      Result : Any;
      Tco : TypeCode.Object;
   begin
      TypeCode.Set_Kind (Tco, Tk_Wstring);
      TypeCode.Add_Parameter (Tco, To_Any (Unsigned_Long (0)));
      TypeCode.Set_Volatile (Tco, True);
      --  the string is supposed to be unbounded

      Set_Value (Result, new Content_Wide_String'
                 (Value => new Types.Wide_String'(Item)));
      Set_Type (Result, Tco);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : in Any)
     return Short is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Short) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Short_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Long is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Long) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Long_Long is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Longlong) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Unsigned_Short is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ushort) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_UShort_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Unsigned_Long is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ulong then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_ULong_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Unsigned_Long_Long is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ulonglong then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_ULong_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Types.Float is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Float) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Float_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Double is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Double) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Double_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Long_Double is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Longdouble) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Double_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Boolean is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Boolean) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Boolean_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Char is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Char) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Char_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Wchar is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Widechar) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Wchar_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Octet is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Octet) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Octet_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Any is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Any) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Any_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return TypeCode.Object is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_TypeCode) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_TypeCode_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Types.String is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_String) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_String_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any
     (Item : in Any)
     return Types.Wide_String is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Wstring) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Wide_String_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (The_Any : in Any)
     return TypeCode.Object
   is
      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (The_Any));
   begin
      pragma Debug (O ("Get_Type : enter & end"));
      return Container.The_Type;
   end Get_Type;

   ---------------------
   -- Unwind_Typedefs --
   ---------------------

   function Unwind_Typedefs
     (TC : in TypeCode.Object)
     return TypeCode.Object
   is
      Result : TypeCode.Object := TC;
   begin
      while TypeCode.Kind (Result) = Tk_Alias loop
         Result := TypeCode.Content_Type (Result);
      end loop;

      return Result;
   end Unwind_Typedefs;

   ----------------------
   -- Get_Unwound_Type --
   ----------------------

   function Get_Unwound_Type
     (The_Any : in Any)
     return TypeCode.Object is
   begin
      return Unwind_Typedefs (Get_Type (The_Any));
   end Get_Unwound_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object)
   is
      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (The_Any));
   begin
      pragma Debug (O ("Set_Type: enter"));
      TypeCode.Destroy_TypeCode (Container.The_Type);
      Container.The_Type := The_Type;
      pragma Debug (O ("Set_Type: leave"));
   end Set_Type;

   -------------------------------
   -- Iterate_Over_Any_Elements --
   -------------------------------

   procedure Iterate_Over_Any_Elements
     (In_Any : in Any) is
   begin
      raise Not_Implemented;
   end Iterate_Over_Any_Elements;

   -------------------
   -- Get_Empty_Any --
   -------------------

   function Get_Empty_Any
     (Tc : TypeCode.Object)
     return Any
   is
      Result : Any;
   begin
      pragma Debug (O ("Get_Empty_Any : enter"));
      Set_Type (Result, Tc);
      pragma Debug (O ("Get_Empty_Any : type set"));

      return Result;
   end Get_Empty_Any;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Any_Value : in Any)
     return Boolean is
   begin
      pragma Debug (O ("Is_empty : enter & end"));
      return Get_Value (Any_Value) = null;
   end Is_Empty;

   -------------------
   -- Set_Any_Value --
   -------------------

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Octet)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Octet then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Octet_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Octet'(Value => new Octet'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Short)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Short then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Short_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Short'(Value => new Short'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Long)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Long then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Long_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Long'(Value => new Types.Long'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Long_Long)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Longlong then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Long_Long_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Long_Long'(Value => new Types.Long_Long'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Unsigned_Short)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Ushort then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_UShort_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_UShort'(Value => new Unsigned_Short'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Unsigned_Long)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Ulong then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_ULong_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_ULong'(Value => new Unsigned_Long'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Unsigned_Long_Long)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /=
        Tk_Ulonglong then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_ULong_Long_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_ULong_Long'(Value =>
                                     new Unsigned_Long_Long'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Boolean)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Boolean then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Boolean_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Boolean'(Value => new Boolean'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Char)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Char then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Char_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Char'(Value => new Char'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Wchar)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Widechar then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Wchar_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Wchar'(Value => new Wchar'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     PolyORB.Types.String)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_String then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_String_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_String'(Value => new PolyORB.Types.String'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Wide_String)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Wstring then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Wide_String_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Wide_String'(Value => new Types.Wide_String'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Float)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Float then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Float_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Float'(Value => new Types.Float'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Double)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Double then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Double_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Double'(Value => new Double'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Long_Double)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /=
        Tk_Longdouble then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Long_Double_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Long_Double'(Value => new Types.Long_Double'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     TypeCode.Object)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_TypeCode then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_TypeCode_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_TypeCode'(Value => new TypeCode.Object'(Value));
      end if;

   end Set_Any_Value;

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Any)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Any then
         raise TypeCode.Bad_TypeCode;
      end if;

      if Container.The_Value.all /= null then
         Content_Any_Ptr (Container.The_Value.all).Value.all := Value;
      else
         Container.The_Value.all :=
           new Content_Any'(Value => new Any'(Value));
      end if;

   end Set_Any_Value;

   -----------------------------
   -- Set_Any_Aggregate_Value --
   -----------------------------

   procedure Set_Any_Aggregate_Value
     (Any_Value : in out Any)
   is
      use TypeCode;

      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Any_Value));
   begin
      pragma Debug (O ("Set_Any_Aggregate_Value: enter"));
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Struct
        and then TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Union
        and then TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Enum
        and then TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Sequence
        and then TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Array
        and then TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Except
      then
         raise TypeCode.Bad_TypeCode;
      end if;

      pragma Debug (O ("Set_Any_Aggregate_Value: typecode is correct"));

      if Container.The_Value.all = null then
         Container.The_Value.all := new Content_Aggregate;
      end if;

   end Set_Any_Aggregate_Value;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count (Value : Any) return Unsigned_Long
   is
      CA_Ptr : constant Content_Aggregate_Ptr
        := Content_Aggregate_Ptr (Get_Value (Value));
   begin
      if CA_Ptr.V.Frozen then
         return CA_Ptr.V.Value'Length;
      else
         return Unsigned_Long
           (Content_Lists.Length (CA_Ptr.V.Mutable_Value));
      end if;
   end Get_Aggregate_Count;

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : in     Any)
   is
      Value_Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Value));

      Element_Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Element));

      CA_Ptr : constant Content_Aggregate_Ptr
        := Content_Aggregate_Ptr (Value_Container.The_Value.all);
   begin
      pragma Debug (O ("Add_Aggregate_Element : enter"));
      pragma Debug (O ("Add_Aggregate_Element : element kind is "
                       & TCKind'Image
                       (TypeCode.Kind
                        (Get_Type (Element)))));

      if CA_Ptr.V.Frozen then
         raise Program_Error;
      end if;

      Content_Lists.Append
        (CA_Ptr.V.Mutable_Value,
         Duplicate (Element_Container.The_Value.all));

      pragma Debug (O ("Add_Aggregate_Element : end"));
   end Add_Aggregate_Element;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : TypeCode.Object;
      Index : Unsigned_Long)
     return Any
   is
      Value_Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Value));
      CA_Ptr : constant Content_Aggregate_Ptr
        := Content_Aggregate_Ptr (Value_Container.The_Value.all);
      Result : Any;
   begin
      pragma Debug (O ("Get_Aggregate_Element : enter"));

      pragma Assert (Value_Container.The_Value /= null);
      if not CA_Ptr.V.Frozen then
         Freeze (CA_Ptr.V);
      end if;
      pragma Assert (CA_Ptr.V.Frozen);

      pragma Debug (O ("Get_Aggregate_Element : Index = "
                       & Unsigned_Long'Image (Index)
                       & ", aggregate_count = "
                       & Unsigned_Long'Image
                       (Get_Aggregate_Count (Value))));

      Set_Value (Result, Duplicate (CA_Ptr.V.Value (Integer (Index))));
      Set_Type (Result, Tc);

      pragma Debug (O ("Get_Aggregate_Element : end"));
      return Result;
   end Get_Aggregate_Element;

   -----------------------------
   -- Get_Empty_Any_Aggregate --
   -----------------------------

   function Get_Empty_Any_Aggregate
     (Tc : TypeCode.Object)
     return Any
   is
      Result : Any;
   begin
      pragma Debug (O ("Get_Empty_Any_Aggregate : begin"));
      Set_Value (Result, new Content_Aggregate);
      Set_Type (Result, Tc);

      pragma Debug (O ("Get_Empty_Any_Aggregate : end"));
      return Result;
   end Get_Empty_Any_Aggregate;

   --------------------
   -- Copy_Any_Value --
   --------------------

   procedure Copy_Any_Value (Dest : Any; Src : Any) is
   begin
      pragma Debug (O ("Copy_Any_Value: enter"));

      if TypeCode.Kind (Get_Unwound_Type (Dest))
        /= TypeCode.Kind (Get_Unwound_Type (Src))
      then
         pragma Debug (O ("Copy Any value from: "
                          & Image (Get_Unwound_Type (Src))));
         pragma Debug (O ("  to: " & Image (Get_Unwound_Type (Dest))));
         raise TypeCode.Bad_TypeCode;
      end if;

      if TypeCode.Kind (Get_Unwound_Type (Src)) /= Tk_Void
        and then not Compare_Any_Contents (Dest, Src)
      then

         --  For non-void, non-identical Any instances,
         --  deallocate old Dest contents and duplicate
         --  Src contents.

         Set_Value (Dest, Duplicate (Get_Value (Src)));

      end if;

      pragma Debug (O ("Copy_Any_Value: leave"));
   end Copy_Any_Value;

   ---------------------
   -- Deep_Deallocate --
   ---------------------

   procedure Deep_Deallocate (List : in out Content_List)
   is
      use Content_Lists;

      I : Iterator := First (List);
   begin
      pragma Debug (O2 ("Deep_Deallocate : enter"));

      while not Last (I) loop
         pragma Debug (O2 ("Deep_Deallocate: object type is "
                           & Ada.Tags.External_Tag
                           (Value (I).all'Tag)));
         Deallocate (Value (I).all);
         Next (I);
      end loop;

      Deallocate (List);

      pragma Debug (O2 ("Deep_Deallocate : end"));
   end Deep_Deallocate;

   procedure Deep_Deallocate (List : in out Content_Array_Access)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Content_Array, Content_Array_Access);
   begin
      pragma Debug (O2 ("Deep_Deallocate(A): enter"));

      for J in List'Range loop
         pragma Debug (O2 ("Deep_Deallocate: object type is "
                           & Ada.Tags.External_Tag (List (J)'Tag)));
         Deallocate (List (J));
      end loop;

      Deallocate (List);

      pragma Debug (O2 ("Deep_Deallocate(A): end"));
   end Deep_Deallocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Object : access Content)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Object);
      pragma Warnings (On);

   begin
      pragma Debug (O2 ("Deallocate (generic) : enter & end"));

      --  We should never be here since Any_Content_Ptr should
      --  never be the real type of a variable

      raise Program_Error;
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Octet)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Octet) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Octet) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Short)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Short) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Short) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Long)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Long_Long)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long_Long) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long_Long) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_UShort)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (UShort) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (UShort) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_ULong)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (ULong) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (ULong) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_ULong_Long)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (ULongLong) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (ULongLong) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Boolean)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Boolean) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Boolean) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Char)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Char) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Char) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Wchar)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Wchar) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Wchar) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_String)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (String) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (String) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Wide_String)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Wide_String) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Wide_String) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Float)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Float) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Float) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Double)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Double) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Double) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Long_Double)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long_Double) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long_Double) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_TypeCode)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (TypeCode) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (TypeCode) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Any)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Any) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Any) : end"));
   end Deallocate;

   procedure Deallocate
     (Object : access Content_Aggregate)
   is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Aggregate) : enter"));

      if Object.V.Frozen then
         Deep_Deallocate (Object.V.Value);
      else
         Deep_Deallocate (Object.V.Mutable_Value);
      end if;

      --  then deallocate the object itself
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Aggregate) : end"));
   end Deallocate;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (Contents : Content_Array_Access)
     return Content_Array_Access
   is
      Result : constant Content_Array_Access
        := new Content_Array (Contents'Range);
   begin
      pragma Debug (O ("Duplicate (Content_List): enter"));
      for J in Contents'Range loop
         Result (J) := Duplicate (Contents (J));
      end loop;
      pragma Debug (O ("Duplicate (Content_List): leave"));
      return Result;
   end Duplicate;

   function Duplicate
     (Object : access Content_Octet)
     return Any_Content_Ptr is
   begin
      return new Content_Octet'
        (Value => new Octet'(Content_Octet_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Short)
     return Any_Content_Ptr is
   begin
      return new Content_Short'
        (Value => new Short'(Content_Short_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Long)
     return Any_Content_Ptr is
   begin
      return new Content_Long'
        (Value => new Types.Long'(Content_Long_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Long_Long)
     return Any_Content_Ptr is
   begin
      return new Content_Long_Long'
        (Value => new Types.Long_Long'
         (Content_Long_Long_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_UShort)
     return Any_Content_Ptr is
   begin
      return new Content_UShort'
        (Value => new Unsigned_Short'
         (Content_UShort_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_ULong)
     return Any_Content_Ptr is
   begin
      return new Content_ULong'
        (Value => new Unsigned_Long'
         (Content_ULong_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_ULong_Long)
     return Any_Content_Ptr is
   begin
      return new Content_ULong_Long'
        (Value => new Unsigned_Long_Long'
         (Content_ULong_Long_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Boolean)
     return Any_Content_Ptr is
   begin
      return new Content_Boolean'
        (Value => new Boolean'
         (Content_Boolean_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Char)
     return Any_Content_Ptr is
   begin
      return new Content_Char'
        (Value => new Char'
         (Content_Char_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Wchar)
     return Any_Content_Ptr is
   begin
      return new Content_Wchar'
        (Value => new Wchar'
         (Content_Wchar_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_String)
     return Any_Content_Ptr is
   begin
      return new Content_String'
        (Value => new PolyORB.Types.String'
         (Content_String_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Wide_String)
     return Any_Content_Ptr is
   begin
      return new Content_Wide_String'
        (Value => new Types.Wide_String'
         (Content_Wide_String_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Float)
     return Any_Content_Ptr is
   begin
      return new Content_Float'
        (Value => new Types.Float'
         (Content_Float_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Double)
     return Any_Content_Ptr is
   begin
      return new Content_Double'
        (Value => new Double'
         (Content_Double_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Long_Double)
     return Any_Content_Ptr is
   begin
      return new Content_Long_Double'
        (Value => new Types.Long_Double'
         (Content_Long_Double_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_TypeCode)
     return Any_Content_Ptr is
   begin
      return new Content_TypeCode'
        (Value => new TypeCode.Object'
         (Content_TypeCode_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Any)
     return Any_Content_Ptr is
   begin
      return new Content_Any'
        (Value => new Any'
         (Content_Any_Ptr (Object).Value.all));
   end Duplicate;

   function Duplicate
     (Object : access Content_Aggregate)
     return Any_Content_Ptr
   is
   begin
      pragma Debug (O ("Duplicate (Content_Aggregate) : enter & end"));
      if not Object.V.Frozen then
         Freeze (Object.V);
      end if;
      return new Content_Aggregate'
        (V => (Frozen => True,
               Value => Duplicate (Object.V.Value)));
   end Duplicate;

   ------------------
   -- Set_Volatile --
   ------------------

   procedure Set_Volatile
     (Obj         : in out Any;
      Is_Volatile : in     Boolean)
   is

      Container : Any_Container_Ptr := Any_Container_Ptr (Entity_Of (Obj));
   begin
      TypeCode.Set_Volatile (Container.The_Type, Is_Volatile);
   end Set_Volatile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Any)
   is
      use type PolyORB.Smart_Pointers.Entity_Ptr;

      Container : Any_Container_Ptr := new Any_Container;
   begin
      pragma Debug (O ("Initializing Any: enter"));
      pragma Assert (Entity_Of (Self) = null);

      Container.The_Value := new Any_Content_Ptr'(null);
      Set (Self, PolyORB.Smart_Pointers.Entity_Ptr (Container));
      pragma Debug (O ("Initializing Any: leave"));
   end Initialize;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Obj       : in Any;
      The_Value : in Any_Content_Ptr)
   is
      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Obj));
   begin
      if Container.The_Value.all /= null then
         Deallocate (Container.The_Value.all);
      end if;

      Container.The_Value.all := The_Value;
   end Set_Value;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Object : in out Aggregate_Value)
   is
      pragma Assert (not Object.Frozen);
      use Content_Lists;

      Elements : constant Content_Array_Access
        := new Content_Array (0 .. Length (Object.Mutable_Value) - 1);
      J : Natural := Elements'First;
      It : Iterator := First (Object.Mutable_Value);
   begin
      while not Last (It) loop
         Elements (J) := Value (It).all;
         J := J + 1;
         Next (It);
      end loop;
      Deallocate (Object.Mutable_Value);
      Object := (Frozen => True, Value => Elements);
   end Freeze;

   -------------------
   -- Get_Value_Ptr --
   -------------------

   function Get_Value_Ptr
     (Obj : Any)
     return Any_Content_Ptr_Ptr
   is
      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Obj));
   begin
      return Container.The_Value;
   end Get_Value_Ptr;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Obj : Any)
     return Any_Content_Ptr
   is
      Container : constant Any_Container_Ptr
        := Any_Container_Ptr (Entity_Of (Obj));
   begin
      return Container.The_Value.all;
   end Get_Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self : in out Any_Container) is
   begin
      pragma Debug (O ("Finalizing Any_Container: enter"));

      if Self.Is_Finalized then
         return;
      end if;

      Self.Is_Finalized := True;

      TypeCode.Destroy_TypeCode (Self.The_Type);
      pragma Debug (O2 (" * typecode deallocated"));

      if Self.The_Value.all /= null then
         pragma Debug (O2 (" * deallocation of a "
                           & Ada.Tags.External_Tag
                           (Self.The_Value.all'Tag)));
         Deallocate (Self.The_Value.all);
      end if;
      pragma Debug (O2 (" * content released"));

      Deallocate_Any_Content_Ptr (Self.The_Value);
      pragma Debug (O2 (" * content_ptr released"));

      pragma Debug (O ("Finalizing Any_Container: leave"));
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image
     (NV : NamedValue)
     return Standard.String
   is

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      null;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"any",
       Conflicts => Empty,
       Depends   => +"smart_pointers",
       Provides  => Empty,
       Init      => Initialize'Access));

end PolyORB.Any;
