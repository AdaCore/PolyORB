------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Exceptions;
with Broca.Debug;

with Broca.Configuration;
pragma Elaborate (Broca.Configuration);
pragma Warnings (Off, Broca.Configuration);

with Ada.Tags;

package body CORBA is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("corba_reference_count");
   procedure O2 is new Broca.Debug.Output (Flag2);

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_CORBA_String (Source : Standard.String)
                             return CORBA.String is
   begin
      return CORBA.String
        (Ada.Strings.Unbounded.To_Unbounded_String (Source));
   end To_CORBA_String;

   function To_Standard_String (Source : CORBA.String)
                                return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (Source));
   end To_Standard_String;

   function To_CORBA_Wide_String (Source : Standard.Wide_String)
                                  return CORBA.Wide_String is
   begin
      return CORBA.Wide_String
        (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Source));
   end To_CORBA_Wide_String;

   function To_Standard_Wide_String (Source : CORBA.Wide_String)
                                     return Standard.Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Source));
   end To_Standard_Wide_String;


   ----------------------------------------
   --  Get_Members for system exceptions --
   ----------------------------------------
   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
     renames Broca.Exceptions.Get_Members;


   ----------------------
   -- other exceptions --
   ----------------------

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InvalidName_Members) is
   begin
      To := InvalidName_Members'(IDL_Exception_Members with null record);
   end Get_Members;

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InconsistentTypeCode_Members) is
   begin
      To := InconsistentTypeCode_Members'
        (IDL_Exception_Members with null record);
   end Get_Members;

   ------------------
   --  Get_Members --
   ------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PolicyError_Members)
   is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;


   -----------------
   --  TypeCodes  --
   -----------------

   package body TypeCode is

      ------------------
      --  Get_Members --
      ------------------
      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To   : out Bounds_Members) is
      begin
         Broca.Exceptions.User_Get_Members (From, To);
      end Get_Members;

      ------------------
      --  Get_Members --
      ------------------
      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To   : out BadKind_Members) is
      begin
         Broca.Exceptions.User_Get_Members (From, To);
      end Get_Members;

      -----------
      --  "="  --
      -----------
      function "=" (Left, Right : in Object) return Boolean is
         Nb_Param : CORBA.Unsigned_Long;
         Res : CORBA.Boolean := True;
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
         Nb_Param : CORBA.Unsigned_Long := Member_Count (Left);
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
                  Id_Left : RepositoryId := Id (Left);
                  Id_Right : RepositoryId := Id (Right);
               begin
                  if Id_Left /= Null_RepositoryId and
                    Id_Right /= Null_RepositoryId then
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
               if CORBA.Long (I) /= Default_Index (Left) and
                 CORBA.Long (I) /= Default_Index (Right) then
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
      function Id (Self : in Object)
                   return CORBA.RepositoryId is
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
                  Res : CORBA.String;
               begin
                  Res := From_Any (Get_Parameter (Self, 1));
                  return RepositoryId (Res);
               end;
            when others =>
               declare
                  Member : Bounds_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Id;

      ------------
      --  Name  --
      ------------
      function Name (Self : in Object)
                     return CORBA.Identifier is
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
                  Res : CORBA.String;
               begin
                  Res := From_Any (Get_Parameter (Self, 0));
                  return Identifier (Res);
               end;
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Name;

      --------------------
      --  Member_Count  --
      --------------------
      function Member_Count (Self : in Object)
                             return Unsigned_Long is
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
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
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Member_Count;

      -------------------
      --  Member_Name  --
      -------------------
      function Member_Name (Self  : in Object;
                            Index : in CORBA.Unsigned_Long)
                            return CORBA.Identifier is
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
         Res : CORBA.String;
         Member : Bounds_Members;
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Struct
              | Tk_Except =>
               if Param_Nb < 2 * Index + 4 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               Res := From_Any (Get_Parameter (Self, 2 * Index + 3));
               return Identifier (Res);
            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 6));
               return Identifier (Res);
            when Tk_Enum =>
               if Param_Nb < Index + 3 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               Res := From_Any (Get_Parameter (Self, Index + 2));
               return Identifier (Res);
            when Tk_Value =>
               if Param_Nb < 3 * Index + 7 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 6));
               return Identifier (Res);
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Member_Name;

      -------------------
      --  Member_Type  --
      -------------------
      function Member_Type
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Object is
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
         Member : Bounds_Members;
      begin
         pragma Debug (O ("member_type : enter"));
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Struct
              | Tk_Except =>
               pragma Debug (O ("member_type : dealing with a struct "
                                & "or an exception"));
               if Param_Nb < 2 * Index + 4 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 2 * Index + 2));
            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 5));
            when Tk_Value =>
               if Param_Nb < 3 * Index + 7 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 5));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Member_Type;

      --------------------
      --  Member_Label  --
      --------------------
      function Member_Label
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return CORBA.Any is
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
         Member : Bounds_Members;
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               if Param_Nb < 3 * Index + 7 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 4));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Member_Label;

      --------------------------
      --  Discriminator_Type  --
      --------------------------
      function Discriminator_Type (Self : in Object)
                                   return Object is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 2));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Discriminator_Type;

      ---------------------
      --  Default_Index  --
      ---------------------
      function Default_Index (Self : in Object)
                              return CORBA.Long is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               return From_Any (Get_Parameter (Self, 3));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Default_Index;

      --------------
      --  Length  --
      --------------
      function Length (Self : in Object)
                       return CORBA.Unsigned_Long is
      begin
         pragma Debug (O ("Length : enter & end"));
         case Kind (Self) is
            when Tk_String
              | Tk_Wstring
              | Tk_Sequence
              | Tk_Array =>
               return From_Any (Get_Parameter (Self, 0));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
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
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Content_Type;

      --------------------
      --  Fixed_Digits  --
      --------------------
      function Fixed_Digits (Self : in Object)
                             return CORBA.Unsigned_Short is
      begin
         case Kind (Self) is
            when Tk_Fixed =>
               return From_Any (Get_Parameter (Self, 0));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Fixed_Digits;

      -------------------
      --  Fixed_Scale  --
      -------------------
      function Fixed_Scale (Self : in Object)
                            return CORBA.Short is
      begin
         case Kind (Self) is
            when Tk_Fixed =>
               return From_Any (Get_Parameter (Self, 1));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Fixed_Scale;

      -------------------------
      --  Member_Visibility  --
      -------------------------
      function Member_Visibility
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Visibility is
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Value =>
               declare
                  Member : Bounds_Members;
                  Param_Nb : Unsigned_Long := Parameter_Count (Self);
                  Res : Short;
               begin
                  if Param_Nb < 3 * Index + 7 then
                     Broca.Exceptions.User_Raise_Exception
                       (Bounds'Identity, Member);
                  end if;
                  Res := From_Any (Get_Parameter (Self, 3 * Index + 3));
                  return Visibility (Res);
               end;
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Member_Visibility;

      ---------------------
      --  Type_Modifier  --
      ---------------------
      function Type_Modifier (Self : in Object)
                              return CORBA.ValueModifier is
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
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Type_Modifier;

      --------------------------
      --  Concrete_Base_Type  --
      --------------------------
      function Concrete_Base_Type (Self : in Object)
                                   return Object is
      begin
         case Kind (Self) is
            when Tk_Value =>
               return From_Any (Get_Parameter (Self, 3));
            when others =>
               declare
                  Member : BadKind_Members;
               begin
                  Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                         Member);
               end;
         end case;
      end Concrete_Base_Type;

      ------------------------------
      --  Member_Type_With_Label  --
      ------------------------------
      function Member_Type_With_Label
        (Self  : in Object;
         Label : in Any;
         Index : in CORBA.Unsigned_Long) return Object is
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
         Current_Member : Unsigned_Long := 0;
         Default_Member : Unsigned_Long := -1;
         Member_Nb : Long := -1;
         Default_Nb : Long := -1;
      begin
         pragma Debug (O ("Member_Type_With_Label : enter"));
         pragma Debug (O ("Member_Type_With_Label : Param_Nb = "
                          & Unsigned_Long'Image (Param_Nb)));
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         if Kind (Self) = Tk_Union then
            --  look at the member until we got enough with the
            --  good label or we reach the end
            while Member_Nb < Long (Index) and
              Param_Nb > 3 * Current_Member + 6 loop
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
            elsif Member_Nb = -1 and Default_Nb >= Long (Index) then
               pragma Debug (O ("Member_Type_With_Label : default end"));
               return From_Any (Get_Parameter (Self, 3 * Default_Member + 5));
               --  else raise error
            else
               declare
                  Member : Bounds_Members;
               begin
                  pragma Debug (O ("Member_Type_With_Label : "
                                   & "end with exception"));
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end;
            end if;
         else
            declare
               Member : BadKind_Members;
            begin
               pragma Debug (O ("Member_Type_With_Label : "
                                & "end with exception"));
               Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                      Member);
            end;
         end if;
      end Member_Type_With_Label;

      -------------------------------
      --  Member_Count_With_Label  --
      -------------------------------
      function Member_Count_With_Label
        (Self : in Object;
         Label : in Any)
         return Unsigned_Long is
         Result : Unsigned_Long := 0;
         Default_Nb : Unsigned_Long := 0;
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
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
            declare
               Member : BadKind_Members;
            begin
               pragma Debug (O ("Member_Count_With_Label : end "
                                & "with exception"));
               Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                      Member);
            end;
         end if;
      end Member_Count_With_Label;

      ---------------------
      --  Get_Parameter  --
      ---------------------
      function Get_Parameter (Self : in Object;
                              Index : in CORBA.Unsigned_Long)
                              return Any is
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         pragma Debug (O ("Get_Parameter : enter"));
         pragma Debug (O ("Get_Parameter : Index = " &
                          CORBA.Unsigned_Long'Image (Index)));
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
                               Param : in CORBA.Any) is
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
                          Kind : in CORBA.TCKind) is
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
         Add_Parameter (Result, To_Any (CORBA.Unsigned_Long (0)));
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
      --  TC_Objref  --
      -----------------
      function TC_Objref return TypeCode.Object is
      begin
         return PTC_Objref;
      end TC_Objref;

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

   -------------------
   --  Get_Members  --
   -------------------
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out UnknownUserException_Members) is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;

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
      case TypeCode.Kind (Get_Type (Left)) is
         when Tk_Null | Tk_Void =>
            pragma Debug (O ("Equal (Any) : end"));
            return True;
         when Tk_Short =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Short_Ptr (Get_Value (Left)).Value =
              Content_Short_Ptr (Get_Value (Right)).Value;
         when Tk_Long =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Long_Ptr (Get_Value (Left)).Value =
              Content_Long_Ptr (Get_Value (Right)).Value;
         when Tk_Ushort =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_UShort_Ptr (Get_Value (Left)).Value =
              Content_UShort_Ptr (Get_Value (Right)).Value;
         when Tk_Ulong =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_ULong_Ptr (Get_Value (Left)).Value =
              Content_ULong_Ptr (Get_Value (Right)).Value;
         when Tk_Float =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Float_Ptr (Get_Value (Left)).Value =
              Content_Float_Ptr (Get_Value (Right)).Value;
         when Tk_Double =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Double_Ptr (Get_Value (Left)).Value =
              Content_Double_Ptr (Get_Value (Right)).Value;
         when Tk_Boolean =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Boolean_Ptr (Get_Value (Left)).Value =
              Content_Boolean_Ptr (Get_Value (Right)).Value;
         when Tk_Char =>
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Char_Ptr (Get_Value (Left)).Value =
              Content_Char_Ptr (Get_Value (Right)).Value;
         when Tk_Octet =>
            pragma Debug (O ("Equal (Any) : comparing with a tk_octet"));
            pragma Debug (O ("Equal (Any) : end"));
            return Content_Octet_Ptr (Get_Value (Left)).Value =
              Content_Octet_Ptr (Get_Value (Right)).Value;
         when Tk_Any =>
            pragma Debug (O ("Equal (Any) : end"));
            return Equal (Content_Any_Ptr (Get_Value (Left)).Value,
                          Content_Any_Ptr (Get_Value (Right)).Value);
         when Tk_TypeCode =>
            pragma Debug (O ("Equal (Any) : end"));
            return TypeCode.Equal
              (Content_TypeCode_Ptr (Get_Value (Left)).Value,
               Content_TypeCode_Ptr (Get_Value (Right)).Value);

         when Tk_Principal =>
            pragma Debug (O ("Equal (Any) : end"));
            return True;

         when Tk_Objref =>
            pragma Debug (O ("Equal (Any) : end"));
            return True;
--          return Object.Is_Equivalent (Object.From_Any (Left),
--                                       Object.From_Any (Right));
         when Tk_Struct | Tk_Union =>
            pragma Debug (O ("Equal (Any) : end"));
            return True;
            --  agregate comparison (recursive)
--          declare
--             N : CORBA.Long;
--             Cl1, Cl2 : Content_List;
--          begin
--             Cl1 := Content_Agregat_Ptr (Get_Value (Left)).Value;
--             Cl2 := Content_Agregat_Ptr (Get_Value (Right)).Value;
--             N := Agregate_Count (Cl1);
--             if (N /= Agregate_Count (Cl2)) then
--                return False;
--             end if;
--                declare
--                   Res : CORBA.Boolean := True;
--                   Tc : TypeCode.Object := Get_Type (Left);
--                   Any_Member_Tc : TypeCode.Object;
--                begin
--                   for I in 0 .. N - 1 loop
--                      Any_Member_Tc := TypeCode.From_Any
--                        (TypeCode.Parameter
--                      (Tc,
--                       TypeCode.Member_Index
--                       (TypeCode.Kind (Get_Type (Left)), I)));
--                      Res := Res and
--                        Equal
--                        (Get_Any_Agregate_Member (Left, Any_Member_Tc, I),
--                         Get_Any_Agregate_Member (Right, Any_Member_Tc, I));
--                      if Res = False then
--                         return False;
--                      end if;
--                   end loop;
--                   return True;
--                end;
--             end;

         when others =>
            pragma Debug (O ("Equal (Any) : end"));
            --  unsupported type for comparison :
            --  tk_principal, tk_objref
            return False;
      end case;
   end "=";

   -----------------------------------
   --  To_Any conversion functions  --
   -----------------------------------

   function To_Any (Item : in Short) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Short' (Value => Item));
      Set_Type (Result, TypeCode.TC_Short);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long' (Value => Item));
      Set_Type (Result, TypeCode.TC_Long);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Long' (Value => Item));
      Set_Type (Result, TypeCode.TC_Long_Long);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_UShort' (Value => Item));
      Set_Type (Result, TypeCode.TC_Unsigned_Short);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
      Result : Any;
   begin
      pragma Debug (O ("To_Any (ULong) : enter"));
      Set_Value (Result, new Content_ULong' (Value => Item));
      Set_Type (Result, TypeCode.TC_Unsigned_Long);
      pragma Debug (O ("To_Any (ULong) : end"));
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_ULong_Long' (Value => Item));
      Set_Type (Result, TypeCode.TC_Unsigned_Long_Long);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Float) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Float' (Value => Item));
      Set_Type (Result, TypeCode.TC_Float);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Double) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Double' (Value => Item));
      Set_Type (Result, TypeCode.TC_Double);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Long_Double' (Value => Item));
      Set_Type (Result, TypeCode.TC_Long_Double);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Boolean' (Value => Item));
      Set_Type (Result, TypeCode.TC_Boolean);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Char) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Char' (Value => Item));
      Set_Type (Result, TypeCode.TC_Char);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Wchar' (Value => Item));
      Set_Type (Result, TypeCode.TC_Wchar);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Octet) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Octet' (Value => Item));
      Set_Type (Result, TypeCode.TC_Octet);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in Any) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_Any' (Value => Item));
      Set_Type (Result, TypeCode.TC_Any);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in TypeCode.Object) return Any is
      Result : Any;
   begin
      Set_Value (Result, new Content_TypeCode' (Value => Item));
      Set_Type (Result, TypeCode.TC_TypeCode);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in CORBA.String) return Any is
      Result : Any;
      Tco : CORBA.TypeCode.Object;
   begin
      pragma Debug (O ("To_Any (String) : enter"));
      CORBA.TypeCode.Set_Kind (Tco, Tk_String);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      Set_Value (Result, new Content_String' (Value => Item));
      Set_Type (Result, Tco);
      pragma Debug (O ("To_Any (String) : end"));
      Inc_Usage (Result);
      return Result;
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
      Result : Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set_Kind (Tco, Tk_Wstring);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      Set_Value (Result, new Content_Wide_String' (Value => Item));
      Set_Type (Result, Tco);
      Inc_Usage (Result);
      return Result;
   end To_Any;

   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (Item : in Any) return Short is
   begin
      pragma Debug (O ("From_Any (Short) : enter & end"));
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Short) then
         raise Bad_TypeCode;
      end if;
      pragma Debug (O ("From_Any (Short) : is_empty = "
                       & Boolean'Image (CORBA.Is_Empty (Item))));
      pragma Debug (O ("From_Any (Short) : Item type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      pragma Debug (O ("From_Any (Short) : value is "
                       & CORBA.Short'Image
                       (Content_Short_Ptr (Get_Value (Item)).Value)));
      return Content_Short_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      pragma Debug (O ("From_Any (Long) : enter & end"));
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Long) then
         raise Bad_TypeCode;
      end if;
      return Content_Long_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Longlong) then
         raise Bad_TypeCode;
      end if;
      return Content_Long_Long_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Ushort) then
         raise Bad_TypeCode;
      end if;
      return Content_UShort_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      if TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Ulong then
         raise Bad_TypeCode;
      end if;
      pragma Assert (Get_Value (Item) /= null);
      pragma Debug (O ("any content type is "
                       & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      return Content_ULong_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      if TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Ulonglong then
         raise Bad_TypeCode;
      end if;
      return Content_ULong_Long_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Float is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Float) then
         raise Bad_TypeCode;
      end if;
      return Content_Float_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Double) then
         raise Bad_TypeCode;
      end if;
      return Content_Double_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Longdouble) then
         raise Bad_TypeCode;
      end if;
      return Content_Long_Double_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Boolean) then
         raise Bad_TypeCode;
      end if;
      return Content_Boolean_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Char) then
         raise Bad_TypeCode;
      end if;
      return Content_Char_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Widechar) then
         raise Bad_TypeCode;
      end if;
      return Content_Wchar_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Octet) then
         raise Bad_TypeCode;
      end if;
      return Content_Octet_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return Any is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Any) then
         raise Bad_TypeCode;
      end if;
      return Content_Any_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return TypeCode.Object is
   begin
      pragma Debug (O ("From_Any (typeCode) : enter & end"));
      pragma Debug
        (O ("From_Any (typeCode) : Kind (Item) is "
            & CORBA.TCKind'Image (TypeCode.Kind (Get_Precise_Type (Item)))));
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_TypeCode) then
         raise Bad_TypeCode;
      end if;
      return Content_TypeCode_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.String is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_String) then
         pragma Debug (O ("From_Any (String) : type is supposed to be "
                          & TCKind'Image (TypeCode.Kind
                                          (Get_Precise_Type (Item)))));
         pragma Debug (O ("From_Any (String) : actual type is "
                          & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
         raise Bad_TypeCode;
      end if;
      return Content_String_Ptr (Get_Value (Item)).Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
   begin
      if (TypeCode.Kind (Get_Precise_Type (Item)) /= Tk_Wstring) then
         raise Bad_TypeCode;
      end if;
      return Content_Wide_String_Ptr (Get_Value (Item)).Value;
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
   --  Get_Precise_Type  --
   ------------------------
   function Get_Precise_Type (The_Any : in Any) return  TypeCode.Object is
      Result : TypeCode.Object := Get_Type (The_Any);
   begin
      while CORBA.TypeCode.Kind (Result) = Tk_Alias loop
         Result := CORBA.TypeCode.Content_Type (Result);
      end loop;
      return Result;
   end Get_Precise_Type;

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
      Set_Type (Result, Tc);
      Inc_Usage (Result);
      --  we put ref_count to 1 even if there is no pointer is
      --  because if there were one in the future, we would have
      --  a reference on it here.
      return Result;
   end Get_Empty_Any;

   -----------
   --  Any  --
   -----------

   ----------------
   --  Is_Empty  --
   ----------------
   function Is_Empty (Any_Value : in CORBA.Any) return Boolean is
   begin
      pragma Debug (O ("Is_empty : enter & end"));
      return Get_Value (Any_Value) = null;
   end Is_Empty;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : CORBA.Octet) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Octet then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Octet_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Octet'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Short) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Short then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Short_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Short'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
      pragma Debug (O ("Set_Any_Value : the any value is "
                       & CORBA.Short'Image
                       (Content_Short_Ptr
                        (Get_Value (Any_Value)).Value)));
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Long then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Long_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Long'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long_Long) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Longlong then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Long_Long_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Long_Long'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Short) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Ushort then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_UShort_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_UShort'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Long) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Ulong then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_ULong_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_ULong'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Long_Long) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Ulonglong then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_ULong_Long_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value :=
          new Content_ULong_Long'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Boolean) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Boolean then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Boolean_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Boolean'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Char) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Char then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Char_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Char'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Wchar) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Widechar then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Wchar_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Wchar'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.String) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_String then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_String_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_String'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Wide_String) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Wstring then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Wide_String_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Wide_String'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Float) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Float then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Float_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Float'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Double) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Double then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Double_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Double'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long_Double) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Longdouble then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Long_Double_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Long_Double'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.TypeCode.Object) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_TypeCode then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_TypeCode_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_TypeCode'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   ---------------------
   --  Set_Any_Value  --
   ---------------------
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Any) is
      use TypeCode;
   begin
      if CORBA.TypeCode.Kind (Get_Type (Any_Value)) /= Tk_Any then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value /= null then
         Content_Any_Ptr (Any_Value.The_Value).Value := Value;
      else
         Any_Value.The_Value := new Content_Any'(Value => Value);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Value;

   -------------------------------
   --  Set_Any_Aggregate_Value  --
   -------------------------------
   procedure Set_Any_Aggregate_Value (Any_Value : in out CORBA.Any) is
      use TypeCode;
   begin
      pragma Debug (O ("Set_Any_Aggregate_Value : enter"));
      if CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Struct
        and CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Union
        and CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Enum
        and CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Sequence
        and CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Array
        and CORBA.TypeCode.Kind (Get_Precise_Type (Any_Value)) /= Tk_Except
      then
         Broca.Exceptions.Raise_Bad_TypeCode;
      end if;
      pragma Debug (O ("Set_Any_Aggregate_Value : no exception raised"));
      Any_Value.Any_Lock.Lock_W;
      if Any_Value.The_Value = null then
         Any_Value.The_Value :=
          new Content_Aggregate'(Value => Null_Content_List);
      end if;
      Any_Value.Any_Lock.Unlock_W;
   end Set_Any_Aggregate_Value;

   ---------------------------
   --  Get_Aggregate_Count  --
   ---------------------------
   function Get_Aggregate_Count (Value : Any) return CORBA.Unsigned_Long is
      N : CORBA.Unsigned_Long := 0;
      Ptr : Content_List := Content_Aggregate_Ptr (Get_Value (Value)).Value;
   begin
      while Ptr /= null loop
         N := N + 1;
         Ptr := Ptr.Next;
      end loop;
      return N;
   end Get_Aggregate_Count;

   -----------------------------
   --  Add_Aggregate_Element  --
   -----------------------------
   procedure Add_Aggregate_Element (Value : in out Any;
                                    Element : in Any) is
      Cl : Content_List;
   begin
      pragma Debug (O ("Add_Aggregate_Element : enter"));
      Value.Any_Lock.Lock_W;
      Cl := Content_Aggregate_Ptr (Value.The_Value).Value;
      pragma Debug (O ("Add_Aggregate_Element : element kind is "
                       & CORBA.TCKind'Image
                       (CORBA.TypeCode.Kind
                        (CORBA.Get_Type (Element)))));
      if Cl = Null_Content_List then
         Content_Aggregate_Ptr (Value.The_Value).Value
           := new Content_Cell' (Duplicate (Element.The_Value),
                                 Null_Content_List);
      else
         while Cl.Next /= Null_Content_List loop
            Cl := Cl.Next;
         end loop;
         Cl.Next := new Content_Cell' (Duplicate (Element.The_Value),
                                       Null_Content_List);
      end if;
      Value.Any_Lock.Unlock_W;
      pragma Debug (O ("Add_Aggregate_Element : end"));
   end Add_Aggregate_Element;

   -----------------------------
   --  Get_Aggregate_Element  --
   -----------------------------
   function Get_Aggregate_Element (Value : Any;
                                   Tc : CORBA.TypeCode.Object;
                                   Index : CORBA.Unsigned_Long)
                                   return Any is
      Result : Any;
      Ptr : Content_List;
   begin
      pragma Debug (O ("Get_Aggregate_Element : enter"));
      Value.Any_Lock.Lock_R;
      Ptr := Content_Aggregate_Ptr (Value.The_Value).Value;
      pragma Debug (O ("Get_Aggregate_Element : Index = "
                       & CORBA.Unsigned_Long'Image (Index)
                       & ", aggregate_count = "
                       & CORBA.Unsigned_Long'Image
                       (Get_Aggregate_Count (Value))));
      pragma Assert (Get_Aggregate_Count (Value) > Index);
      if Index > 0 then
         for I in 0 .. Index - 1 loop
            Ptr := Ptr.Next;
         end loop;
      end if;
      pragma Assert (Ptr /= null);
      pragma Assert (Ptr.The_Value /= null);
      Result.The_Value := Duplicate (Ptr.The_Value);
      Value.Any_Lock.Unlock_R;
      Inc_Usage (Result);
      Set_Type (Result, Tc);
      pragma Debug (O ("Get_Aggregate_Element : end"));
      return Result;
   end Get_Aggregate_Element;

   -------------------------------
   --  Get_Empty_Any_Aggregate  --
   -------------------------------
   function Get_Empty_Any_Aggregate (Tc : CORBA.TypeCode.Object)
                                     return Any is
      Result : Any;
   begin
      Set_Value (Result,
                 new Content_Aggregate'(Value => Null_Content_List));
      Set_Type (Result, Tc);
      Inc_Usage (Result);
      return Result;
   end Get_Empty_Any_Aggregate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (List : in Content_List) return Content_List is
   begin
      pragma Debug (O ("Duplicate (Content_List) : enter & end"));
      if List /= null then
         return new Content_Cell'(The_Value => List.The_Value,
                                  Next => Duplicate (List.Next));
      else
         return null;
      end if;
   end Duplicate;

   -----------------------
   --  Deep_Deallocate  --
   -----------------------
   procedure Deep_Deallocate (List : in out Content_List) is
   begin
      if List /= null then
         Deep_Deallocate (List.Next);
         Deallocate (List.The_Value);
         Deallocate (List);
      end if;
   end Deep_Deallocate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content)
                       return Any_Content_Ptr is
   begin
      --  we should never be here since Any_Content_Ptr should
      --  never be the real type of a variable
      pragma Debug (O ("Duplicate (Any_Content_Ptr) : enter & end"));
      Broca.Exceptions.Raise_Internal;
      return null;
   end Duplicate;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Object : access Content) is
      Obj : Any_Content_Ptr := Any_Content_Ptr (Object);
   begin
      Deallocate_Any_Content (Obj);
   end Deallocate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Octet)
                       return Any_Content_Ptr is
   begin
      return new Content_Octet'
        (Value => Content_Octet_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Short)
                       return Any_Content_Ptr is
   begin
      return new Content_Short'
        (Value => Content_Short_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long)
                       return Any_Content_Ptr is
   begin
      pragma Debug (O ("Duplicate (Long) : enter & end"));
      return new Content_Long'
        (Value => Content_Long_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long_Long)
                       return Any_Content_Ptr is
   begin
      return new Content_Long_Long'
        (Value => Content_Long_Long_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_UShort)
                       return Any_Content_Ptr is
   begin
      return new Content_UShort'
        (Value => Content_UShort_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_ULong)
                       return Any_Content_Ptr is
   begin
      pragma Debug (O ("Duplicate (ULong) : enter & end"));
      return new Content_ULong'
        (Value => Content_ULong_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_ULong_Long)
                       return Any_Content_Ptr is
   begin
      return new Content_ULong_Long'
        (Value => Content_ULong_Long_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Boolean)
                       return Any_Content_Ptr is
   begin
      return new Content_Boolean'
        (Value => Content_Boolean_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Char)
                       return Any_Content_Ptr is
   begin
      return new Content_Char'
        (Value => Content_Char_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Wchar)
                       return Any_Content_Ptr is
   begin
      return new Content_Wchar'
        (Value => Content_Wchar_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_String)
                       return Any_Content_Ptr is
   begin
      return new Content_String'
        (Value => Content_String_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Wide_String)
                       return Any_Content_Ptr is
   begin
      return new Content_Wide_String'
        (Value => Content_Wide_String_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Float)
                       return Any_Content_Ptr is
   begin
      return new Content_Float'
        (Value => Content_Float_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Double)
                       return Any_Content_Ptr is
   begin
      return new Content_Double'
        (Value => Content_Double_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Long_Double)
                       return Any_Content_Ptr is
   begin
      return new Content_Long_Double'
        (Value => Content_Long_Double_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_TypeCode)
                       return Any_Content_Ptr is
   begin
      return new Content_TypeCode'
        (Value => Content_TypeCode_Ptr (Object).Value);
   end Duplicate;

   -----------------
   --  Duplicate  --
   -----------------
   function Duplicate (Object : access Content_Any)
                       return Any_Content_Ptr is
   begin
      return new Content_Any'
        (Value => Content_Any_Ptr (Object).Value);
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
   procedure Deallocate (Object : in out Content_Aggregate_Ptr) is
   begin
      --  first deallocate every alement of the list of values
      Deep_Deallocate (Object.Value);
      --  then deallocate the object itself
      Deallocate (Any_Content_Ptr (Object));
   end Deallocate;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize (Object : in out Any) is
   begin
      pragma Debug (O2 ("Initialize"));
      Object.Ref_Counter := new Natural'(0);
      Object.Any_Lock := new Broca.Locks.Rw_Lock_Type;
   end Initialize;

   --------------
   --  Adjust  --
   --------------
   procedure Adjust (Object : in out Any) is
   begin
      pragma Debug (O2 ("Adjust : enter"));
      if Object.As_Reference then
         Inc_Usage (Object);
      else
         if Get_Value (Object) /= Null_Content_Ptr then
            pragma Debug (O2 ("object type is "
                             & Ada.Tags.External_Tag
                             (Get_Value (Object).all'Tag)));
            pragma Assert (Object.Any_Lock /= null);
            Object.Any_Lock.Lock_R;
            Object.The_Value := Duplicate (Object.The_Value);
            Object.Any_Lock.Unlock_R;
            Object.Ref_Counter := new Natural'(1);
            Object.Any_Lock := new Broca.Locks.Rw_Lock_Type;
         end if;
      end if;
      pragma Debug (O2 ("Adjust : end"));
   end Adjust;

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (Object : in out Any) is
   begin
      pragma Debug (O2 ("Finalize : enter"));
      pragma Debug (O2 ("Finalize"));
      Dec_Usage (Object);
      pragma Debug (O2 ("Finalize : end"));
   end Finalize;

   -----------------
   --  Set_Value  --
   -----------------
   procedure Set_Value (Obj : in out Any; The_Value : in Any_Content_Ptr) is
   begin
      Obj.Any_Lock.Lock_W;
      Obj.The_Value := The_Value;
      Obj.Any_Lock.Unlock_W;
   end Set_Value;

   -------------------
   --  Set_Counter  --
   -------------------
   procedure Set_Counter (Obj : in out Any; The_Counter : in Natural_Ptr) is
   begin
      Obj.Any_Lock.Lock_W;
      Obj.Ref_Counter := The_Counter;
      Obj.Any_Lock.Unlock_W;
   end Set_Counter;

   -----------------
   --  Get_Value  --
   -----------------
   function Get_Value (Obj : Any) return Any_Content_Ptr is
      Content : Any_Content_Ptr;
   begin
      Obj.Any_Lock.Lock_R;
      Content := Obj.The_Value;
      Obj.Any_Lock.Unlock_R;
      return Content;
   end Get_Value;

   -------------------
   --  Get_Counter  --
   -------------------
   function Get_Counter (Obj : Any) return Natural_Ptr is
      Counter : Natural_Ptr;
   begin
      Obj.Any_Lock.Lock_R;
      Counter := Obj.Ref_Counter;
      Obj.Any_Lock.Unlock_R;
      return Counter;
   end Get_Counter;

   -----------------
   --  Inc_Usage  --
   -----------------
   procedure Inc_Usage (Obj : in Any) is
   begin
      pragma Debug (O2 ("Inc_Usage : enter"));
      Obj.Any_Lock.Lock_W;
      Obj.Ref_Counter.all := Obj.Ref_Counter.all + 1;
      Obj.Any_Lock.Unlock_W;
      pragma Debug (O2 ("Inc_Usage : end"));
   end Inc_Usage;

   -----------------
   --  Dec_Usage  --
   -----------------
   procedure Dec_Usage (Obj : in out Any) is
   begin
      pragma Debug (O2 ("Dec_Usage : enter"));
      pragma Debug (O2 ("Dec_Usage"));
      Obj.Any_Lock.Lock_W;
      pragma Debug (O2 ("Dec_Usage : lock placed"));
      if Obj.Ref_Counter.all > 1 then
         Obj.Ref_Counter.all := Obj.Ref_Counter.all - 1;
         pragma Debug (O2 ("Dec_Usage : counter decremented"));
         Obj.Any_Lock.Unlock_W;
         pragma Debug (O2 ("Dec_Usage : lock released"));
      else
         if Obj.The_Value /= Null_Content_Ptr then
            Deallocate (Obj.The_Value);
            Deallocate (Obj.Ref_Counter);
            pragma Debug (O2 ("Dec_Usage : counter deallocated"));
            Obj.Any_Lock.Unlock_W;
            pragma Debug (O2 ("Dec_Usage : lock released"));
            Deallocate (Obj.Any_Lock);
         else
            Obj.Any_Lock.Unlock_W;
            pragma Debug (O2 ("Dec_Usage : lock released"));
         end if;
      end if;
      pragma Debug (O2 ("Dec_Usage : end"));
   end Dec_Usage;

end CORBA;

