------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/polyorb-any.adb#30 $

with Ada.Exceptions;
with Ada.Tags;

with PolyORB.Locks;
with PolyORB.Log;

with System.Address_Image;

package body PolyORB.Any is

   use PolyORB.Locks;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("polyorb.any_refcnt");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;

   ---------------
   -- TypeCodes --
   ---------------

   package body TypeCode is

      -----------
      --  "="  --
      -----------
      function "=" (Left, Right : in Object) return Boolean is
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
         for I in 0 .. Nb_Param - 1 loop
            Res := Res and
              Equal (Get_Parameter (Left, I), Get_Parameter (Right, I));
            if Res = False then
               pragma Debug (O ("Equal (TypeCode) : end"));
               return False;
            end if;
         end loop;
         pragma Debug (O ("Equal (TypeCode) : end"));
         return Res;
      end "=";

      ------------------
      --  Equivalent  --
      ------------------
      function Equivalent (Left, Right : in Object)
                           return Boolean is
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
               for I in 0 .. Nb_Param - 1 loop
                  if not Equivalent (Member_Type (Left, I),
                                     Member_Type (Right, I)) then
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
            for I in 0 .. Nb_Param - 1 loop
               if Types.Long (I) /= Default_Index (Left)
                 and then Types.Long (I) /= Default_Index (Right)
               then
                  if Member_Label (Left, I) /= Member_Label (Right, I) then
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
            if Default_Index (Left) > -1 and Default_Index (Right) > -1 then
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
            for I in 0 .. Nb_Param - 1 loop
               if Member_Visibility (Left, I) /=
                 Member_Visibility (Right, I) then
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


      ----------------------------
      --  Get_Compact_TypeCode  --
      ----------------------------
      function Get_Compact_TypeCode (Self : in Object)
                                     return Object is
      begin
         return Self;
      end Get_Compact_TypeCode;

      ------------
      --  Kind  --
      ------------
      function Kind (Self : in Object)
                     return TCKind is
      begin
         return Self.Kind;
      end Kind;

      ----------
      --  Id  --
      ----------

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

      ------------
      --  Name  --
      ------------
      function Name (Self : in Object)
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

      --------------------
      --  Member_Count  --
      --------------------
      function Member_Count (Self : in Object)
                             return Unsigned_Long is
         Param_Nb : constant Unsigned_Long := Parameter_Count (Self);
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
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

      -------------------
      --  Member_Name  --
      -------------------
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
         --  typecode.object in the private part of corba.typecode
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
         --  typecode.object in the private part of corba.typecode
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
         --  typecode.object in the private part of corba.typecode
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

      --------------------------
      --  Discriminator_Type  --
      --------------------------
      function Discriminator_Type
        (Self : in Object)
        return Object is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 2));
            when others =>
               raise BadKind;
         end case;
      end Discriminator_Type;

      ---------------------
      --  Default_Index  --
      ---------------------
      function Default_Index
        (Self : in Object)
        return Types.Long is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 3));
            when others =>
               raise BadKind;
         end case;
      end Default_Index;

      --------------
      --  Length  --
      --------------
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

      --------------------
      --  Content_Type  --
      --------------------
      function Content_Type (Self : in Object) return Object is
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

      --------------------
      --  Fixed_Digits  --
      --------------------
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

      -------------------
      --  Fixed_Scale  --
      -------------------
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

      -------------------------
      --  Member_Visibility  --
      -------------------------
      function Member_Visibility
        (Self  : in Object;
         Index : in Unsigned_Long)
        return Visibility is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
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

      ---------------------
      --  Type_Modifier  --
      ---------------------
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

      --------------------------
      --  Concrete_Base_Type  --
      --------------------------
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
         --  TypeCode.Object in the private part of TypeCode
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
               elsif Get_Parameter (Self, 3 * Current_Member + 4) = Label then
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

      -------------------------------
      --  Member_Count_With_Label  --
      -------------------------------
      function Member_Count_With_Label
        (Self : in Object;
         Label : in Any)
        return Unsigned_Long
      is
         Result : Unsigned_Long := 0;
         Default_Nb : Unsigned_Long := 0;
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of PolyORB.Any
         --  to understand the magic numbers used here.
         pragma Debug (O ("Member_Count_With_Label : enter"));
         if TypeCode.Kind (Self) = Tk_Union then
            pragma Debug (O ("Member_Count_With_Label : Member_Count = "
                             & Unsigned_Long'Image (Member_Count (Self))));
            for I in 0 .. Member_Count (Self) - 1 loop
               if Get_Parameter (Self, 3 * I + 4) = Label then
                  Result := Result + 1;
               end if;
               if Default_Index (Self) = Long (I) then
                  Default_Nb := Default_Nb + 1;
               end if;
            end loop;
            if Result = 0 then
               Result := Default_Nb;
            end if;
            pragma Debug (O ("Member_Count_With_Label : Result = "
                             & Unsigned_Long'Image (Result)));
            pragma Debug (O ("Member_Count_With_Label : end"));
            return Result;
         else
            pragma Debug (O ("Member_Count_With_Label : end "
                             & "with exception"));
            raise BadKind;
         end if;
      end Member_Count_With_Label;

      ---------------------
      --  Get_Parameter  --
      ---------------------
      function Get_Parameter (Self : in Object;
                              Index : in Unsigned_Long)
                              return Any is
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         pragma Debug (O ("Get_Parameter : enter"));
         pragma Debug (O ("Get_Parameter : Index = " &
                          Unsigned_Long'Image (Index)));
         pragma Assert (Ptr /= null);
         pragma Debug (O ("Get_Parameter : assert OK"));
         if Index /= 0 then
            pragma Debug (O ("Get_Parameter : index /= 0"));
            for I in 0 .. Index - 1 loop
               Ptr := Ptr.Next;
               pragma Assert (Ptr /= null);
            end loop;
         end if;
         pragma Debug (O ("Get_Parameter : end"));
         return Ptr.Parameter;
      end Get_Parameter;

      ---------------------
      --  Add_Parameter  --
      ---------------------
      procedure Add_Parameter (Self  : in out Object;
                               Param : in Any) is
         C_Ptr : Cell_Ptr := Self.Parameters;
      begin
         if C_Ptr = null then
            Self.Parameters := new Cell' (Param, null);
         else
            while C_Ptr.Next /= null loop
               C_Ptr := C_Ptr.Next;
            end loop;
            C_Ptr.Next := new Cell' (Param, null);
         end if;
      end Add_Parameter;

      ----------------
      --  Set_Kind  --
      ----------------
      procedure Set_Kind (Self : out Object;
                          Kind : in TCKind) is
      begin
         Self.Kind := Kind;
         Self.Parameters := null;
      end Set_Kind;

      ---------------
      --  TC_Null  --
      ---------------
      function TC_Null return TypeCode.Object is
      begin
         return PTC_Null;
      end TC_Null;

      ---------------
      --  TC_Void  --
      ---------------
      function TC_Void return TypeCode.Object is
      begin
         return PTC_Void;
      end TC_Void;

      ----------------
      --  TC_Short  --
      ----------------
      function TC_Short return TypeCode.Object is
      begin
         return PTC_Short;
      end TC_Short;

      ---------------
      --  TC_Long  --
      ---------------
      function TC_Long return TypeCode.Object is
      begin
         return PTC_Long;
      end TC_Long;

      --------------------
      --  TC_Long_Long  --
      --------------------
      function TC_Long_Long return TypeCode.Object is
      begin
         return PTC_Long_Long;
      end TC_Long_Long;

      -------------------------
      --  TC_Unsigned_Short  --
      -------------------------
      function TC_Unsigned_Short return TypeCode.Object is
      begin
         return PTC_Unsigned_Short;
      end TC_Unsigned_Short;

      ------------------------
      --  TC_Unsigned_Long  --
      ------------------------
      function TC_Unsigned_Long return TypeCode.Object is
      begin
         return PTC_Unsigned_Long;
      end TC_Unsigned_Long;

      -----------------------------
      --  TC_Unsigned_Long_Long  --
      -----------------------------
      function TC_Unsigned_Long_Long return TypeCode.Object is
      begin
         return PTC_Unsigned_Long_Long;
      end TC_Unsigned_Long_Long;

      ----------------
      --  TC_Float  --
      ----------------
      function TC_Float return TypeCode.Object is
      begin
         return PTC_Float;
      end TC_Float;

      -----------------
      --  TC_Double  --
      -----------------
      function TC_Double return TypeCode.Object is
      begin
         return PTC_Double;
      end TC_Double;

      ----------------------
      --  TC_Long_Double  --
      ----------------------
      function TC_Long_Double return TypeCode.Object is
      begin
         return PTC_Long_Double;
      end TC_Long_Double;

      ------------------
      --  TC_Boolean  --
      ------------------
      function TC_Boolean return TypeCode.Object is
      begin
         return PTC_Boolean;
      end TC_Boolean;

      ---------------
      --  TC_Char  --
      ---------------
      function TC_Char return TypeCode.Object is
      begin
         return PTC_Char;
      end TC_Char;

      ----------------
      --  TC_WChar  --
      ----------------
      function TC_Wchar return TypeCode.Object is
      begin
         return PTC_Wchar;
      end TC_Wchar;

      ----------------
      --  TC_Octet  --
      ----------------
      function TC_Octet return TypeCode.Object is
      begin
         return PTC_Octet;
      end TC_Octet;

      --------------
      --  TC_Any  --
      --------------
      function TC_Any return TypeCode.Object is
      begin
         return PTC_Any;
      end TC_Any;

      -------------------
      --  TC_TypeCode  --
      -------------------
      function TC_TypeCode return TypeCode.Object is
      begin
         return PTC_TypeCode;
      end TC_TypeCode;

      -----------------
      --  TC_String  --
      -----------------
      function TC_String return TypeCode.Object is
         Result : TypeCode.Object := PTC_String;
      begin
         Add_Parameter (Result, To_Any (Unsigned_Long (0)));
         return Result;
      end TC_String;

      ----------------------
      --  TC_Wide_String  --
      ----------------------
      function TC_Wide_String return TypeCode.Object is
      begin
         return PTC_Wide_String;
      end TC_Wide_String;

      --------------------
      --  TC_Principal  --
      --------------------
      function TC_Principal return TypeCode.Object is
      begin
         return PTC_Principal;
      end TC_Principal;

      -----------------
      --  TC_Struct  --
      -----------------
      function TC_Struct return TypeCode.Object is
      begin
         return PTC_Struct;
      end TC_Struct;

      ----------------
      --  TC_Union  --
      ----------------
      function TC_Union return TypeCode.Object is
      begin
         return PTC_Union;
      end TC_Union;

      ---------------
      --  TC_Enum  --
      ---------------
      function TC_Enum return TypeCode.Object is
      begin
         return PTC_Enum;
      end TC_Enum;

      ----------------
      --  TC_Alias  --
      ----------------
      function TC_Alias return TypeCode.Object is
      begin
         return PTC_Alias;
      end TC_Alias;

      -----------------
      --  TC_Except  --
      -----------------
      function TC_Except return TypeCode.Object is
      begin
         return PTC_Except;
      end TC_Except;

      -----------------
      --  TC_Object  --
      -----------------
      function TC_Object return TypeCode.Object is
      begin
         return PTC_Object;
      end TC_Object;

      ----------------
      --  TC_Fixed  --
      ----------------
      function TC_Fixed return TypeCode.Object is
      begin
         return PTC_Fixed;
      end TC_Fixed;

      -------------------
      --  TC_Sequence  --
      -------------------
      function TC_Sequence return TypeCode.Object is
      begin
         return PTC_Sequence;
      end TC_Sequence;

      ----------------
      --  TC_Array  --
      ----------------
      function TC_Array return TypeCode.Object is
      begin
         return PTC_Array;
      end TC_Array;

      ----------------
      --  TC_Value  --
      ----------------
      function TC_Value return TypeCode.Object is
      begin
         return PTC_Value;
      end TC_Value;

      -------------------
      --  TC_Valuebox  --
      -------------------
      function TC_Valuebox return TypeCode.Object is
      begin
         return PTC_Valuebox;
      end TC_Valuebox;

      -----------------
      --  TC_Native  --
      -----------------
      function TC_Native return TypeCode.Object is
      begin
         return PTC_Native;
      end TC_Native;

      -----------------------------
      --  TC_Abstract_Interface  --
      -----------------------------
      function TC_Abstract_Interface return TypeCode.Object is
      begin
         return PTC_Abstract_Interface;
      end TC_Abstract_Interface;

      -----------------------
      --  Parameter_Count  --
      -----------------------
      function Parameter_Count (Self : in Object)
                                return Unsigned_Long is
         N : Unsigned_Long := 0;
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         while (Ptr /= null)
         loop
            N := N + 1;
            Ptr := Ptr.Next;
         end loop;
         return N;
      end Parameter_Count;

   end TypeCode;


   -----------
   --  Any  --
   -----------

   function Image (TC : TypeCode.Object) return Standard.String;
   function Image (TC : TypeCode.Object) return Standard.String is
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

            case Kind is
               when
                 Tk_Objref             |
                 Tk_Native             |
                 Tk_Abstract_Interface =>
                  return To_Standard_String (Result);
               when Tk_Struct | Tk_Except =>
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

   function Image (A : Any) return Standard.String is
      Kind : constant TCKind := TypeCode.Kind (A.The_Type);
   begin
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
         when others =>
            return "<Any:" & Image (A.The_Type) & ">";
      end case;
   end Image;

   -----------
   --  "="  --
   -----------
   function "=" (Left, Right : in Any) return Boolean is
   begin
      pragma Debug (O ("Equal (Any) : enter"));
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
               pragma Debug (O ("Equal (Any, TypeCode) : end"));
               return TypeCode.Equal (R, L);
            end;
         when Tk_Principal =>
            --  FIXME : to be done
            pragma Debug (O ("Equal (Any, Principal) : end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;
         when Tk_Objref =>
            declare
--               L : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Left);
--               R : CORBA.Object.Ref := CORBA.Object.Helper.From_Any (Right);
            begin
               pragma Debug (O ("Equal (Any, ObjRef) : end"));
               --  FIXME : is_equivalent has to be implemented
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
               for I in 0 .. TypeCode.Member_Count (List_Type) - 1 loop
                  Member_Type := TypeCode.Member_Type (List_Type, I);
                  if not Equal (Get_Aggregate_Element (Left, Member_Type, I),
                                Get_Aggregate_Element (Right, Member_Type, I))
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
                  for I in 1 .. TypeCode.Member_Count_With_Label
                    (List_Type, Switch_Label) loop
                     Member_Type := TypeCode.Member_Type_With_Label
                       (List_Type, Switch_Label, I - 1);
                     if not Equal
                       (Get_Aggregate_Element (Left, Member_Type, I),
                        Get_Aggregate_Element (Right, Member_Type, I))
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
               for I in 0 .. TypeCode.Length (List_Type) - 1 loop
                  if not Equal (Get_Aggregate_Element (Left, Member_Type, I),
                                Get_Aggregate_Element (Right, Member_Type, I))
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
            --  FIXME : to be done
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
            --  FIXME : to be done
            pragma Debug (O ("Equal (Any, Native) : end"
                             & " NON IMPLEMENTED -> TRUE"));
            return True;
      end case;
   end "=";

   -----------------------------------
   --  To_Any conversion functions  --
   -----------------------------------

   function To_Any (Item : in Short) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Short' (Value => new Short' (Item)));
      Set_Type (Result, TypeCode.TC_Short);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long' (Value => new Long' (Item)));
      Set_Type (Result, TypeCode.TC_Long);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Long'
                 (Value => new Long_Long' (Item)));
      Set_Type (Result, TypeCode.TC_Long_Long);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_UShort'
                 (Value => new Unsigned_Short' (Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Short);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
      Result : Any;
   begin
      pragma Debug (O ("To_Any (ULong) : enter"));
      Set_Value (Result, new Content_ULong'
                 (Value => new Unsigned_Long' (Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Long);
      pragma Debug (O ("To_Any (ULong) : end"));
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_ULong_Long'
                 (Value => new Unsigned_Long_Long' (Item)));
      Set_Type (Result, TypeCode.TC_Unsigned_Long_Long);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Types.Float) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Float'
                 (Value => new Types.Float' (Item)));
      Set_Type (Result, TypeCode.TC_Float);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Double) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Double'
                 (Value => new Double' (Item)));
      Set_Type (Result, TypeCode.TC_Double);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Double'
                 (Value => new Long_Double' (Item)));
      Set_Type (Result, TypeCode.TC_Long_Double);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Boolean'
                 (Value => new Boolean' (Item)));
      Set_Type (Result, TypeCode.TC_Boolean);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Char) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Char'
                 (Value => new Char' (Item)));
      Set_Type (Result, TypeCode.TC_Char);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Wchar'
                 (Value => new Wchar' (Item)));
      Set_Type (Result, TypeCode.TC_Wchar);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Octet) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Octet'
                 (Value => new Octet' (Item)));
      Set_Type (Result, TypeCode.TC_Octet);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Any) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Any'
                 (Value => new Any' (Item)));
      Set_Type (Result, TypeCode.TC_Any);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in TypeCode.Object) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_TypeCode'
                 (Value => new TypeCode.Object' (Item)));
      Set_Type (Result, TypeCode.TC_TypeCode);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Types.String) return Any is
      Result : Any;
      Tco : TypeCode.Object;
   begin
      pragma Debug (O ("To_Any (String) : enter"));
      TypeCode.Set_Kind (Tco, Tk_String);
      TypeCode.Add_Parameter (Tco, To_Any (Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      Set_Value (Result, new Content_String'
                 (Value => new PolyORB.Types.String' (Item)));
      Set_Type (Result, Tco);
      pragma Debug (O ("To_Any (String) : end"));
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Types.Wide_String) return Any is
      Result : Any;
      Tco : TypeCode.Object;
   begin
      TypeCode.Set_Kind (Tco, Tk_Wstring);
      TypeCode.Add_Parameter (Tco, To_Any (Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      Set_Value (Result, new Content_Wide_String'
                 (Value => new Types.Wide_String' (Item)));
      Set_Type (Result, Tco);
      --  Inc_Usage (Result);
      return Result;
   end To_Any;

   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (Item : in Any) return Short is
   begin
      pragma Debug (O ("From_Any (Short) : enter & end"));
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Short) then
         raise TypeCode.Bad_TypeCode;
      end if;
      pragma Debug (O ("From_Any (Short) : is_empty = "
                       & Boolean'Image (Is_Empty (Item))));
      pragma Debug (O ("From_Any (Short) : Item type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      pragma Debug (O ("From_Any (Short) : value is "
                       & Short'Image
                       (Content_Short_Ptr (Get_Value (Item)).Value.all)));
      return Content_Short_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      pragma Debug (O ("From_Any (Long) : enter & end"));
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Long) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Longlong) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ushort) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_UShort_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ulong then
         raise TypeCode.Bad_TypeCode;
      end if;
      pragma Assert (Get_Value (Item) /= null);
      pragma Debug (O ("any content type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      return Content_ULong_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Ulonglong then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_ULong_Long_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Types.Float is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Float) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Float_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Double) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Double_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Longdouble) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Long_Double_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Boolean) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Boolean_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Char) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Char_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Widechar) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Wchar_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Octet) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Octet_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Any is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Any) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Any_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return TypeCode.Object is
   begin
      pragma Debug (O ("From_Any (typeCode) : enter & end"));
      pragma Debug
        (O ("From_Any (typeCode) : Kind (Item) is "
            & TCKind'Image (TypeCode.Kind (Get_Unwound_Type (Item)))));
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_TypeCode) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_TypeCode_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Types.String is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_String) then
         pragma Debug (O ("From_Any (String) : type is supposed to be "
                          & TCKind'Image (TypeCode.Kind
                                          (Get_Unwound_Type (Item)))));
         pragma Debug (O ("From_Any (String) : actual type is "
                          & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
         raise TypeCode.Bad_TypeCode;
      end if;
      pragma Debug (O ("Container type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      return Content_String_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   function From_Any (Item : in Any) return Types.Wide_String is
   begin
      if (TypeCode.Kind (Get_Unwound_Type (Item)) /= Tk_Wstring) then
         raise TypeCode.Bad_TypeCode;
      end if;
      return Content_Wide_String_Ptr (Get_Value (Item)).Value.all;
   end From_Any;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (The_Any : in Any) return  TypeCode.Object is
   begin
      pragma Debug (O ("Get_Type : enter & end"));
      return The_Any.The_Type;
   end Get_Type;

   ------------------------
   --  Get_Unwound_Type  --
   ------------------------

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

   function Get_Unwound_Type (The_Any : in Any) return  TypeCode.Object is
   begin
      return Unwind_Typedefs (Get_Type (The_Any));
   end Get_Unwound_Type;

   ----------------
   --  Set_Type  --
   ----------------
   procedure Set_Type (The_Any : in out Any;
                       The_Type : in TypeCode.Object) is
   begin
      The_Any.The_Type := The_Type;
   end Set_Type;

   ---------------------------------
   --  Iterate_Over_Any_Elements  --
   ---------------------------------
   procedure Iterate_Over_Any_Elements (In_Any : in Any) is
   begin
      null;
   end Iterate_Over_Any_Elements;

   ---------------------
   --  Get_Empty_Any  --
   ---------------------
   function Get_Empty_Any (Tc : TypeCode.Object) return Any is
      Result : Any;
   begin
      pragma Debug (O ("Get_Empty_Any : enter"));
      Set_Type (Result, Tc);
      pragma Debug (O ("Get_Empty_Any : type set"));
      Inc_Usage (Result);
      pragma Debug (O ("Get_Empty_Any : usage incremented"));
      return Result;
   end Get_Empty_Any;

   -----------
   --  Any  --
   -----------

   ----------------
   --  Is_Empty  --
   ----------------
   function Is_Empty (Any_Value : in Any) return Boolean is
   begin
      pragma Debug (O ("Is_empty : enter & end"));
      return Get_Value (Any_Value) = null;
   end Is_Empty;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : Octet) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Octet then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Octet_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Octet'(Value => new Octet' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Short) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Short then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Short_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Short'(Value => new Short' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
      pragma Debug (O ("Set_Any_Value : the any value is "
                       & Short'Image
                       (Content_Short_Ptr
                        (Get_Value (Any_Value)).Value.all)));
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long) is
      use TypeCode;
      Kind : constant TCKind := TypeCode.Kind
        (Get_Unwound_Type (Any_Value));
   begin
      if Kind /= Tk_Long then
         pragma Debug (O ("Wrong TCKind: expect Tk_Long, found "
                          & TCKind'Image (Kind)));
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Long_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Long'(Value => new Types.Long' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long_Long) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Longlong then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Long_Long_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Long_Long'(Value => new Types.Long_Long' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Unsigned_Short) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Ushort then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_UShort_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_UShort'(Value => new Unsigned_Short' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Unsigned_Long) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Ulong then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_ULong_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_ULong'(Value => new Unsigned_Long' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Unsigned_Long_Long) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /=
        Tk_Ulonglong then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_ULong_Long_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_ULong_Long'(Value =>
                                     new Unsigned_Long_Long' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Boolean) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Boolean then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Boolean_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Boolean'(Value => new Boolean' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Char) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Char then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Char_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Char'(Value => new Char' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Wchar) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Widechar then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Wchar_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Wchar'(Value => new Wchar' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in PolyORB.Types.String) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_String then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_String_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_String'(Value => new PolyORB.Types.String' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Wide_String) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Wstring then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Wide_String_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Wide_String'(Value => new Types.Wide_String' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Float) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Float then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Float_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Float'(Value => new Types.Float' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Double) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Double then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Double_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Double'(Value => new Double' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long_Double) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /=
        Tk_Longdouble then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Long_Double_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Long_Double'(Value => new Types.Long_Double' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in TypeCode.Object) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_TypeCode then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_TypeCode_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_TypeCode'(Value => new TypeCode.Object' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Any) is
      use TypeCode;
   begin
      if TypeCode.Kind (Get_Unwound_Type (Any_Value)) /= Tk_Any then
         raise TypeCode.Bad_TypeCode;
      end if;
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all /= null then
         Content_Any_Ptr (Any_Value.The_Value.all).Value.all := Value;
      else
         Any_Value.The_Value.all :=
           new Content_Any'(Value => new Any' (Value));
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Value;

   -------------------------------
   --  Set_Any_Aggregate_Value  --
   -------------------------------
   procedure Set_Any_Aggregate_Value (Any_Value : in out Any) is
      use TypeCode;
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
      Lock_W (Any_Value.Any_Lock);
      if Any_Value.The_Value.all = null then
         Any_Value.The_Value.all :=
          new Content_Aggregate;
      end if;
      Unlock_W (Any_Value.Any_Lock);
   end Set_Any_Aggregate_Value;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count (Value : Any)
     return Unsigned_Long is
   begin
      return Get_Content_List_Length
        (Content_Aggregate_Ptr
         (Get_Value (Value)).Value);
   end Get_Aggregate_Count;

   ---------------------------
   -- Add_Aggregate_Element --
   ---------------------------

   procedure Add_Aggregate_Element
     (Value : in out Any;
      Element : in Any)
   is
      Cl : Content_List;
   begin
      pragma Debug (O ("Add_Aggregate_Element : enter"));
      Lock_W (Value.Any_Lock);
      Cl := Content_Aggregate_Ptr (Value.The_Value.all).Value;
      pragma Debug (O ("Add_Aggregate_Element : element kind is "
                       & TCKind'Image
                       (TypeCode.Kind
                        (Get_Type (Element)))));
      Content_Lists.Append
        (Content_Aggregate_Ptr (Value.The_Value.all).Value,
         Duplicate (Element.The_Value.all));
      Unlock_W (Value.Any_Lock);
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
      Result : Any;
      Ptr : Content_List;
   begin
      pragma Debug (O ("Get_Aggregate_Element : enter"));
      Lock_R (Value.Any_Lock);
      pragma Assert (Value.The_Value /= null);
      Ptr := Content_Aggregate_Ptr (Value.The_Value.all).Value;
      pragma Debug (O ("Get_Aggregate_Element : Index = "
                       & Unsigned_Long'Image (Index)
                       & ", aggregate_count = "
                       & Unsigned_Long'Image
                       (Get_Aggregate_Count (Value))));

      Result.The_Value.all
        := Duplicate
        (Content_Lists.Element (Ptr, Integer (Index)).all);
      Unlock_R (Value.Any_Lock);
      Inc_Usage (Result);
      Set_Type (Result, Tc);
      pragma Debug (O ("Get_Aggregate_Element : end"));
      return Result;
   end Get_Aggregate_Element;

   -----------------------------
   -- Get_Empty_Any_Aggregate --
   -----------------------------

   function Get_Empty_Any_Aggregate
     (Tc : TypeCode.Object)
     return Any is
      Result : Any;
   begin
      pragma Debug (O ("Get_Empty_Any_Aggregate : begin"));
      Set_Value (Result, new Content_Aggregate);
      Set_Type (Result, Tc);
      Inc_Usage (Result);
      pragma Debug (O ("Get_Empty_Any_Aggregate : end"));
      return Result;
   end Get_Empty_Any_Aggregate;

   procedure Copy_Any_Value (Dest : Any; Src : Any)
   is
   begin
      if TypeCode.Kind (Get_Unwound_Type (Dest))
        /= TypeCode.Kind (Get_Unwound_Type (Src))
      then
         pragma Debug (O ("Copy Any value from: "
                          & Image (Get_Unwound_Type (Src))));
         pragma Debug (O ("  to: " & Image (Get_Unwound_Type (Dest))));
         raise TypeCode.Bad_TypeCode;
      end if;

      Lock_W (Dest.Any_Lock);

      if TypeCode.Kind (Get_Unwound_Type (Src)) /= Tk_Void
        and then Dest.The_Value /= Src.The_Value
      then

         --  For non-void, non-identical Any instances,
         --  deallocate old Dest contents and duplicate
         --  Src contents.

         if Dest.The_Value.all /= null then
            Deallocate (Dest.The_Value.all);
            --  We can do a simple deallocate/replacement here
            --  because The_Value.all.all is not alised (ie
            --  it is pointed to only by the Any_Content_Ptr
            --  The_Value.all).
         end if;
         Dest.The_Value.all := Duplicate (Get_Value (Src));
      end if;
      Unlock_W (Dest.Any_Lock);
   end Copy_Any_Value;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (List : in Content_List) return Content_List is
      use Content_Lists;

      R : Content_List;
      I : Iterator := First (List);
   begin
      pragma Debug (O ("Duplicate (Content_List): enter"));
      while not Last (I) loop
         Append (R, Duplicate (Value (I).all));
         Next (I);
      end loop;
      pragma Debug (O ("Duplicate (Content_List): leave"));
      return R;
   end Duplicate;

   -----------------------
   --  Deep_Deallocate  --
   -----------------------
   procedure Deep_Deallocate (List : in out Content_List) is
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

   -----------------------------
   -- Get_Content_List_Length --
   -----------------------------

   function Get_Content_List_Length (List : in Content_List)
     return Unsigned_Long is
   begin
      return Unsigned_Long (Content_Lists.Length (List));
   end Get_Content_List_Length;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content) is
   begin
      pragma Debug (O2 ("Deallocate (generic) : enter & end"));
      pragma Warnings (Off);
      pragma Unreferenced (Object);
      pragma Warnings (On);
      --  we should never be here since Any_Content_Ptr should
      --  never be the real type of a variable
      raise Program_Error;
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------

   procedure Deallocate (Object : access Content_Octet) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Octet) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Octet) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Short) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Short) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Short) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Long) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Long_Long) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long_Long) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long_Long) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_UShort) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (UShort) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (UShort) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_ULong) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (ULong) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (ULong) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_ULong_Long) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (ULongLong) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (ULongLong) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Boolean) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Boolean) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Boolean) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Char) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Char) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Char) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Wchar) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Wchar) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Wchar) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_String) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (String) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (String) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Wide_String) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Wide_String) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Wide_String) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Float) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Float) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Float) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Double) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Double) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Double) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Long_Double) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Long_Double) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Long_Double) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_TypeCode) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (TypeCode) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (TypeCode) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Any) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Any) : enter"));
      Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Any) : end"));
   end Deallocate;

   ------------------
   --  Deallocate  --
   ------------------

   procedure Deallocate (Object : access Content_ObjRef) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (ObjRef) : enter"));
      PolyORB.References.Deallocate (Object.Value);
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (ObjRef) : end"));
   end Deallocate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Octet)
                       return Any_Content_Ptr is
   begin
      return new Content_Octet'
        (Value => new Octet' (Content_Octet_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Short)
                       return Any_Content_Ptr is
   begin
      return new Content_Short'
        (Value => new Short' (Content_Short_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long)
                       return Any_Content_Ptr is
   begin
      pragma Debug (O ("Duplicate (Long) : enter & end"));
      return new Content_Long'
        (Value => new Types.Long' (Content_Long_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long_Long)
                       return Any_Content_Ptr is
   begin
      return new Content_Long_Long'
        (Value => new Types.Long_Long'
         (Content_Long_Long_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_UShort)
                       return Any_Content_Ptr is
   begin
      return new Content_UShort'
        (Value => new Unsigned_Short'
         (Content_UShort_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_ULong)
                       return Any_Content_Ptr is
   begin
      pragma Debug (O ("Duplicate (ULong) : enter & end"));
      return new Content_ULong'
        (Value => new Unsigned_Long'
         (Content_ULong_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_ULong_Long)
                       return Any_Content_Ptr is
   begin
      return new Content_ULong_Long'
        (Value => new Unsigned_Long_Long'
         (Content_ULong_Long_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Boolean)
                       return Any_Content_Ptr is
   begin
      return new Content_Boolean'
        (Value => new Boolean'
         (Content_Boolean_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Char)
                       return Any_Content_Ptr is
   begin
      return new Content_Char'
        (Value => new Char'
         (Content_Char_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Wchar)
                       return Any_Content_Ptr is
   begin
      return new Content_Wchar'
        (Value => new Wchar'
         (Content_Wchar_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_String)
                       return Any_Content_Ptr is
   begin
      return new Content_String'
        (Value => new PolyORB.Types.String'
         (Content_String_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Wide_String)
                       return Any_Content_Ptr is
   begin
      return new Content_Wide_String'
        (Value => new Types.Wide_String'
         (Content_Wide_String_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Float)
                       return Any_Content_Ptr is
   begin
      return new Content_Float'
        (Value => new Types.Float'
         (Content_Float_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Double)
                       return Any_Content_Ptr is
   begin
      return new Content_Double'
        (Value => new Double'
         (Content_Double_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long_Double)
                       return Any_Content_Ptr is
   begin
      return new Content_Long_Double'
        (Value => new Types.Long_Double'
         (Content_Long_Double_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_TypeCode)
                       return Any_Content_Ptr is
   begin
      return new Content_TypeCode'
        (Value => new TypeCode.Object'
         (Content_TypeCode_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Any)
                       return Any_Content_Ptr is
   begin
      return new Content_Any'
        (Value => new Any'
         (Content_Any_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_ObjRef)
                       return Any_Content_Ptr is
   begin
      return new Content_ObjRef'
        (Value => new PolyORB.References.Ref'
         (Content_ObjRef_Ptr (Object).Value.all));
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Aggregate)
                       return Any_Content_Ptr is
   begin
      pragma Debug (O ("Duplicate (Content_Aggregate) : enter & end"));
      return new Content_Aggregate'
        (Value => Duplicate
         (Content_Aggregate_Ptr (Object).Value));
   end Duplicate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content_Aggregate) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      pragma Debug (O2 ("Deallocate (Aggregate) : enter"));
      --  first deallocate every element of the list of values
      Deep_Deallocate (Object.Value);
      --  then deallocate the object itself
      Deallocate_Any_Content (Obj);
      pragma Debug (O2 ("Deallocate (Aggregate) : end"));
   end Deallocate;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize (Object : in out Any) is
   begin
      pragma Debug (O2 ("Initialize: enter, Object = "
                        & System.Address_Image (Object'Address)));
      Object.Ref_Counter := new Natural'(1);
      PolyORB.Locks.Create (Object.Any_Lock);
      pragma Debug
        (O2 ("  Lck = "
             & System.Address_Image (Object.Any_Lock.all'Address)));
      Object.The_Value := new Any_Content_Ptr'(null);
   end Initialize;

   --------------
   --  Adjust  --
   --------------
   procedure Adjust (Object : in out Any) is
   begin
      pragma Debug (O2 ("Adjust : enter, Object = "
                        & System.Address_Image (Object'Address)));
      pragma Debug (O2 ("  Cnt = "
                         & Integer'Image (Get_Counter (Object))));
      pragma Debug (O2 ("  Lck = "
                        & System.Address_Image
                        (Object.Any_Lock.all'Address)));

      Inc_Usage (Object);
      pragma Debug (O2 ("Adjust : end"));
   end Adjust;

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (Object : in out Any) is
   begin
      pragma Debug (O2 ("Finalize: enter, Object = "
                        & System.Address_Image (Object'Address)));
      Dec_Usage (Object);
      pragma Debug (O2 ("Finalize: end"));
   exception
      when E : others =>
         pragma Debug (O2 ("Finalize: KABOOM!"));
         pragma Debug (O2 (Ada.Exceptions.Exception_Information (E)));
         raise;
   end Finalize;

   -----------------
   --  Set_Value  --
   -----------------
   procedure Set_Value (Obj : in out Any; The_Value : in Any_Content_Ptr) is
   begin
      Lock_W (Obj.Any_Lock);
      Obj.The_Value.all := The_Value;
      Unlock_W (Obj.Any_Lock);
   end Set_Value;

   ---------------------
   --  Get_Value_Ptr  --
   ---------------------
   function Get_Value_Ptr (Obj : Any) return Any_Content_Ptr_Ptr is
      Content_Ptr : Any_Content_Ptr_Ptr;
   begin
      Lock_R (Obj.Any_Lock);
      Content_Ptr := Obj.The_Value;
      Unlock_R (Obj.Any_Lock);
      return Content_Ptr;
   end Get_Value_Ptr;

   -----------------
   --  Get_Value  --
   -----------------
   function Get_Value (Obj : Any) return Any_Content_Ptr is
   begin
      return Get_Value_Ptr (Obj).all;
   end Get_Value;

   -----------------
   -- Get_Counter --
   -----------------

   function Get_Counter (Obj : Any) return Natural is
   begin
      Lock_R (Obj.Any_Lock);
      declare
         Counter : constant Natural := Obj.Ref_Counter.all;
      begin
         Unlock_R (Obj.Any_Lock);
         return Counter;
      end;
   end Get_Counter;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : in Any) is
   begin
      pragma Debug (O2 ("Inc_Usage : enter"));
      Lock_W (Obj.Any_Lock);
      Obj.Ref_Counter.all := Obj.Ref_Counter.all + 1;
      Unlock_W (Obj.Any_Lock);
      pragma Debug (O2 ("Inc_Usage : end"));
   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Any) is
   begin
      pragma Debug (O2 ("Dec_Usage: enter, Obj = "
                        & System.Address_Image (Obj'Address)));
      pragma Debug
        (O2 ("  Lck = "
             & System.Address_Image (Obj.Any_Lock.all'Address)));
      Lock_W (Obj.Any_Lock);
      pragma Debug (O2 ("Dec_Usage: lock placed, Cnt = "
                        & Integer'Image (Obj.Ref_Counter.all)));
      if Obj.Ref_Counter.all > 1 then
         Obj.Ref_Counter.all := Obj.Ref_Counter.all - 1;
         pragma Debug (O2 ("Dec_Usage: counter decremented"));
         Unlock_W (Obj.Any_Lock);
         pragma Debug (O2 ("Dec_Usage: lock released"));
      else
         pragma Debug (O2 ("Dec_Usage: about to release the any"));
         if Obj.The_Value.all /= null then
            pragma Debug (O2 ("Dec_Usage: deallocation of a "
                              & Ada.Tags.External_Tag
                              (Obj.The_Value.all'Tag)));
            Deallocate (Obj.The_Value.all);
         end if;
         pragma Debug (O2 ("Dec_Usage: content released"));
         Deallocate_Any_Content_Ptr (Obj.The_Value);
         pragma Debug (O2 ("Dec_Usage: content_Ptr released"));
         Deallocate (Obj.Ref_Counter);
         pragma Debug (O2 ("Dec_Usage: counter deallocated"));
         Unlock_W (Obj.Any_Lock);
         pragma Debug (O2 ("Dec_Usage: lock released, DESTROYING Lck = "
                           & System.Address_Image (Obj.Any_Lock.all'Address)));
         PolyORB.Locks.Destroy (Obj.Any_Lock);
      end if;
      pragma Debug (O2 ("Dec_Usage : end"));
   end Dec_Usage;

   function Image (NV : NamedValue) return Standard.String
   is
      function Flag_Name (F : Flags) return Standard.String;
      pragma Inline (Flag_Name);

      function Flag_Name (F : Flags) return Standard.String is
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

end PolyORB.Any;
