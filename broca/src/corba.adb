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

package body CORBA is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba");
   procedure O is new Broca.Debug.Output (Flag);

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
         if Right.Kind /= Left.Kind then
            return False;
         end if;
         --  recursive comparison
         Nb_Param := Member_Count (Right);
         if Nb_Param /= Member_Count (Left) then
            return False;
         end if;
         for I in 0 .. Nb_Param - 1 loop
            Res := Res and
              Equal (Get_Parameter (Left, I), Get_Parameter (Right, I));
            if Res = False then
               return False;
            end if;
         end loop;
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
               if Member_Label (Left, I) /= Member_Label (Right, I) then
                  return False;
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
         if Kind (Left) = Tk_Union and then
           Default_Index (Left) /= Default_Index (Right) then
            return False;
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
               return Param_Nb / 3 - 1;
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
               if Param_Nb < 3 * Index + 6 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               Res := From_Any (Get_Parameter (Self, 3 * Index + 5));
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
               return From_Any (Get_Parameter (Self, 2 * Index + 2));
            when Tk_Union =>
               if Param_Nb < 3 * Index + 6 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 4));
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
               if Param_Nb < 3 * Index + 6 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               return From_Any (Get_Parameter (Self, 3 * Index + 3));
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
         Param_Nb : Unsigned_Long := Parameter_Count (Self);
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         case Kind (Self) is
            when Tk_Union =>
               Param_Nb := Param_Nb / 3 - 1;
               for I in 1 .. Param_Nb loop
                  if Kind (Get_Type (Get_Parameter (Self, 3 * I)))
                    = Tk_Octet then
                     declare
                        Val : CORBA.Octet :=
                          From_Any (Get_Parameter (Self, 3 * I));
                     begin
                        if Val = 0 then
                           return CORBA.Long (I - 1);
                        end if;
                     end;
                  end if;
               end loop;
               return -1;
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
         Member_Index : Unsigned_Long := -1;
         Member : Bounds_Members;
      begin
         --  See the big explanation after the declaration of
         --  typecode.object in the private part of corba.typecode
         --  to understand the magic numbers used here.
         if Kind (Self) = Tk_Union then
            while Member_Index < Index loop
               if Param_Nb < 3 * Current_Member + 6 then
                  Broca.Exceptions.User_Raise_Exception (Bounds'Identity,
                                                         Member);
               end if;
               if Get_Parameter (Self, 3 * Current_Member + 6) = Label then
                  Member_Index := Member_Index + 1;
               end if;
               Current_Member := Current_Member + 1;
            end loop;
            return From_Any (Get_Parameter (Self, 3 * Current_Member + 1));
         else
            declare
               Member : BadKind_Members;
            begin
               Broca.Exceptions.User_Raise_Exception (BadKind'Identity,
                                                      Member);
            end;
         end if;
      end Member_Type_With_Label;

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
         if Index /= 0 then
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
      begin
         return PTC_String;
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

   -----------
   --  "="  --
   -----------
   function "=" (Left, Right : in Any) return Boolean is
   begin
      pragma Debug (O ("Equal : enter"));
      if not TypeCode.Equal (Get_Type (Left), Get_Type (Right)) then
         return False;
      end if;
      pragma Debug (O ("Equal : passed typecode test"));
      case TypeCode.Kind (Get_Type (Left)) is
         when Tk_Null | Tk_Void =>
            return True;
         when Tk_Short =>
            return Content_Short_Ptr (Left.The_Value).Value =
              Content_Short_Ptr (Right.The_Value).Value;
         when Tk_Long =>
            return Content_Long_Ptr (Left.The_Value).Value =
              Content_Long_Ptr (Right.The_Value).Value;
         when Tk_Ushort =>
            return Content_UShort_Ptr (Left.The_Value).Value =
              Content_UShort_Ptr (Right.The_Value).Value;
         when Tk_Ulong =>
            return Content_ULong_Ptr (Left.The_Value).Value =
              Content_ULong_Ptr (Right.The_Value).Value;
         when Tk_Float =>
            return Content_Float_Ptr (Left.The_Value).Value =
              Content_Float_Ptr (Right.The_Value).Value;
         when Tk_Double =>
            return Content_Double_Ptr (Left.The_Value).Value =
              Content_Double_Ptr (Right.The_Value).Value;
         when Tk_Boolean =>
            return Content_Boolean_Ptr (Left.The_Value).Value =
              Content_Boolean_Ptr (Right.The_Value).Value;
         when Tk_Char =>
            return Content_Char_Ptr (Left.The_Value).Value =
              Content_Char_Ptr (Right.The_Value).Value;
         when Tk_Octet =>
            pragma Debug (O ("comparing with a tk_octet"));
            return Content_Octet_Ptr (Left.The_Value).Value =
              Content_Octet_Ptr (Right.The_Value).Value;
         when Tk_Any =>
            return Equal (Content_Any_Ptr (Left.The_Value).Value,
                          Content_Any_Ptr (Right.The_Value).Value);
         when Tk_TypeCode =>
            return TypeCode.Equal
              (Content_TypeCode_Ptr (Left.The_Value).Value,
               Content_TypeCode_Ptr (Right.The_Value).Value);

         when Tk_Principal =>
            return True;

         when Tk_Objref =>
            return True;
--          return Object.Is_Equivalent (Object.From_Any (Left),
--                                       Object.From_Any (Right));
         when Tk_Struct | Tk_Union =>
            return True;
            --  agregate comparison (recursive)
--          declare
--             N : CORBA.Long;
--             Cl1, Cl2 : Content_List;
--          begin
--             Cl1 := Content_Agregat_Ptr (Left.The_Value).Value;
--             Cl2 := Content_Agregat_Ptr (Right.The_Value).Value;
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
            --  unsupported type for comparison :
            --  tk_principal, tk_objref
            return False;
      end case;
   end "=";

   -----------------------------------
   --  To_Any conversion functions  --
   -----------------------------------

   function To_Any (Item : in Short) return Any is
   begin
      return (new Content_Short' (Value => Item),
              TypeCode.TC_Short);
   end To_Any;

   function To_Any (Item : in Long) return Any is
   begin
      return (new Content_Long' (Value => Item),
              TypeCode.TC_Long);
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
   begin
      return (new Content_Long_Long' (Value => Item),
              TypeCode.TC_Long_Long);
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
   begin
      return (new Content_UShort' (Value => Item),
              TypeCode.TC_Unsigned_Short);
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
   begin
      return (new Content_ULong' (Value => Item),
              TypeCode.TC_Unsigned_Long);
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
   begin
      return (new Content_ULong_Long' (Value => Item),
              TypeCode.TC_Unsigned_Long_Long);
   end To_Any;

   function To_Any (Item : in Float) return Any is
   begin
      return (new Content_Float' (Value => Item),
              TypeCode.TC_Float);
   end To_Any;

   function To_Any (Item : in Double) return Any is
   begin
      return (new Content_Double' (Value => Item),
              TypeCode.TC_Double);
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
   begin
      return (new Content_Long_Double' (Value => Item),
              TypeCode.TC_Long_Double);
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
   begin
      return (new Content_Boolean' (Value => Item),
              TypeCode.TC_Boolean);
   end To_Any;

   function To_Any (Item : in Char) return Any is
   begin
      return (new Content_Char' (Value => Item),
              TypeCode.TC_Char);
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
   begin
      return (new Content_Wchar' (Value => Item),
              TypeCode.TC_Wchar);
   end To_Any;

   function To_Any (Item : in Octet) return Any is
   begin
      return (new Content_Octet' (Value => Item),
              TypeCode.TC_Octet);
   end To_Any;

   function To_Any (Item : in Any) return Any is
   begin
      return (new Content_Any' (Value => Item),
              TypeCode.TC_Any);
   end To_Any;

   function To_Any (Item : in TypeCode.Object) return Any is
   begin
      return (new Content_TypeCode' (Value => Item),
              TypeCode.TC_TypeCode);
   end To_Any;

   function To_Any (Item : in CORBA.String) return Any is
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set_Kind (Tco, Tk_String);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      return (new Content_String' (Value => Item), Tco);
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set_Kind (Tco, Tk_Wstring);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Unsigned_Long (0)));
      --  the string is supposed to be unbounded
      return (new Content_Wide_String' (Value => Item), Tco);
   end To_Any;

   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (Item : in Any) return Short is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Short) then
         raise Bad_Typecode;
      end if;
      return Content_Short_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Long) then
         raise Bad_Typecode;
      end if;
      return Content_Long_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Longlong) then
         raise Bad_Typecode;
      end if;
      return Content_Long_Long_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Ushort) then
         raise Bad_Typecode;
      end if;
      return Content_UShort_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
   begin
      if TypeCode.Kind (Item.The_Type) /= Tk_Ulong then
         raise Bad_Typecode;
      end if;
      return Content_ULong_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
   begin
      if TypeCode.Kind (Item.The_Type) /= Tk_Ulonglong then
         raise Bad_Typecode;
      end if;
      return Content_ULong_Long_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Float is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Float) then
         raise Bad_Typecode;
      end if;
      return Content_Float_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Double is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Double) then
         raise Bad_Typecode;
      end if;
      return Content_Double_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Longdouble) then
         raise Bad_Typecode;
      end if;
      return Content_Long_Double_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Boolean) then
         raise Bad_Typecode;
      end if;
      return Content_Boolean_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Char is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Char) then
         raise Bad_Typecode;
      end if;
      return Content_Char_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Widechar) then
         raise Bad_Typecode;
      end if;
      return Content_Wchar_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Octet is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Octet) then
         raise Bad_Typecode;
      end if;
      return Content_Octet_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return Any is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Any) then
         raise Bad_Typecode;
      end if;
      return Content_Any_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return TypeCode.Object is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_TypeCode) then
         raise Bad_Typecode;
      end if;
      return Content_TypeCode_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.String is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_String) then
         raise Bad_Typecode;
      end if;
      return Content_String_Ptr (Item.The_Value).Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Wstring) then
         raise Bad_Typecode;
      end if;
      return Content_Wide_String_Ptr (Item.The_Value).Value;
   end From_Any;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (The_Any : in Any) return  TypeCode.Object is
   begin
      return The_Any.The_Type;
   end Get_Type;

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
   begin
      return (The_Type => Tc, The_Value => Null_Content_Ptr);
   end Get_Empty_Any;

   -----------
   --  Any  --
   -----------

   ---------------------------
   --  Get_Aggregate_Count  --
   ---------------------------
   function Get_Aggregate_Count (Value : Any) return CORBA.Unsigned_Long is
      N : CORBA.Unsigned_Long := 0;
      Ptr : Content_List := Content_Aggregate_Ptr (Value.The_Value).Value;
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
      Cl : Content_List := Content_Aggregate_Ptr (Value.The_Value).Value;
   begin
      pragma Debug (O ("Add_Aggregate_Element : enter"));
      if Cl = Null_Content_List then
         Content_Aggregate_Ptr (Value.The_Value).Value
           := new Content_Cell' (Element.The_Value,
                                 Null_Content_List);
      else
         while Cl.Next /= null loop
            Cl := Cl.Next;
         end loop;
         Cl.Next := new Content_Cell' (Value.The_Value,
                                       Null_Content_List);
      end if;
      pragma Debug (O ("Add_Aggregate_Element : end"));
   end Add_Aggregate_Element;

   -----------------------------
   --  Get_Aggregate_Element  --
   -----------------------------
   function Get_Aggregate_Element (Value : Any;
                                   Tc : CORBA.TypeCode.Object;
                                   Index : CORBA.Unsigned_Long)
                                   return Any is
      Ptr : Content_List := Content_Aggregate_Ptr (Value.The_Value).Value;
   begin
      pragma Debug (O ("Get_Aggregate_Element : end"));
      pragma Assert (Get_Aggregate_Count (Value) > Index);
      if Index > 0 then
         for I in 0 .. Index - 1 loop
            Ptr := Ptr.Next;
         end loop;
      end if;
      pragma Debug (O ("Get_Aggregate_Element : end"));
      return (The_Value => Ptr.The_Value, The_Type => Tc);
   end Get_Aggregate_Element;

   -------------------------------
   --  Get_Empty_Any_Aggregate  --
   -------------------------------
   function Get_Empty_Any_Aggregate (Tc : CORBA.TypeCode.Object)
                                     return Any is
      Result : Any;
   begin
      Result.The_Value :=
       new Content_Aggregate'(Value => Null_Content_List);
      Result.The_Type := Tc;
      return Result;
   end Get_Empty_Any_Aggregate;

end CORBA;

