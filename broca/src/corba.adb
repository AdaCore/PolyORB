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
      begin
         return Equal (Left, Right);
      end Equivalent;


      ----------------------------
      --  Get_Compact_TypeCode  --
      ----------------------------
      function Get_Compact_TypeCode (Self : in Object)
                                     return Object is
      begin
         return TC_Null;
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
         return Null_RepositoryId;
      end Id;

      ------------
      --  Name  --
      ------------
      function Name (Self : in Object)
                     return CORBA.Identifier is
      begin
         return Null_Identifier;
      end Name;

      --------------------
      --  Member_Count  --
      --------------------
      function Member_Count (Self : in Object)
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
      end Member_Count;

      -------------------
      --  Member_Name  --
      -------------------
      function Member_Name (Self  : in Object;
                            Index : in CORBA.Unsigned_Long)
                            return CORBA.Identifier is
      begin
         return Null_Identifier;
      end Member_Name;

      -------------------
      --  Member_Type  --
      -------------------
      function Member_Type
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Object is
      begin
         return TC_Null;
      end Member_Type;

      --------------------
      --  Member_Label  --
      --------------------
      function Member_Label
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return CORBA.Any is
      begin
         return (The_Value => null, The_Type => TC_Null);
      end Member_Label;

      --------------------------
      --  Discriminator_Type  --
      --------------------------
      function Discriminator_Type (Self : in Object)
                                   return Object is
      begin
         return TC_Null;
      end Discriminator_Type;

      ---------------------
      --  Default_Index  --
      ---------------------
      function Default_Index (Self : in Object)
                              return CORBA.Long is
      begin
         return 0;
      end Default_Index;

      --------------
      --  Length  --
      --------------
      function Length (Self : in Object)
                       return CORBA.Unsigned_Long is
      begin
         return 0;
      end Length;

      --------------------
      --  Content_Type  --
      --------------------
      function Content_Type (Self : in Object) return Object is
      begin
         return TC_Null;
      end Content_Type;

      --------------------
      --  Fixed_Digits  --
      --------------------
      function Fixed_Digits (Self : in Object)
                             return CORBA.Unsigned_Short is
      begin
         return 0;
      end Fixed_Digits;

      -------------------
      --  Fixed_Scale  --
      -------------------
      function Fixed_Scale (Self : in Object)
                            return CORBA.Short is
      begin
         return 0;
      end Fixed_Scale;

      -------------------------
      --  Member_Visibility  --
      -------------------------
      function Member_Visibility
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Visibility is
      begin
         return PRIVATE_MEMBER;
      end Member_Visibility;

      ---------------------
      --  Type_Modifier  --
      ---------------------
      function Type_Modifier (Self : in Object)
                              return CORBA.ValueModifier is
      begin
         return VTM_NONE;
      end Type_Modifier;

      --------------------------
      --  Concrete_Base_Type  --
      --------------------------
      function Concrete_Base_Type (Self : in Object)
                                   return Object is
      begin
         return TC_Null;
      end Concrete_Base_Type;

      ---------------------
      --  Get_Parameter  --
      ---------------------
      function Get_Parameter (Self : in Object;
                              Index : in CORBA.Unsigned_Long)
                              return Any is
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         pragma Assert (Ptr /= null);
         for I in 0 .. Index - 1
         loop
            Ptr := Ptr.Next;
            pragma Assert (Ptr /= null);
         end loop;
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

--       ----------------------------
--       --  conversion functions  --
--       ----------------------------

--       function From_Any (From : in CORBA.Any)
--                          return CORBA.TypeCode.Object
--       is
--          Tmp : Content_TypeCode_Ptr;
--       begin
--          if (TypeCode.Kind (From.The_Type) /= Tk_TypeCode) then
--             raise Bad_TypeCode;
--          end if;
--          Tmp := Content_TypeCode_Ptr (From.The_Value);
--          return Tmp.Value;
--       end From_Any;

--       function To_Any (From : in CORBA.TypeCode.Object)
--                        return CORBA.Any is
--          The_Any : CORBA.Any;
--       begin
--          The_Any := (new Content_TypeCode' (Value => From),
--                      TypeCode.TC_TypeCode);
--          return The_Any;
--       end To_Any;

--       function  Member_Index
--         (Tck : in TCKind;
--          N   : in CORBA.Long)
--          return CORBA.Long
--       is
--          --  see the spec about the composition of the parameters list
--          --  for typecodes
--       begin
--          case Tck is
--             when Tk_Struct =>
--                return 2 * (N + 1);
--             when Tk_Union =>
--                return 3 * N + 4;
--             when others =>
--                return 0;
--          end case;
--       end Member_Index;

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
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Short' (Value => Item),
                  TypeCode.TC_Short);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Long) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Long' (Value => Item),
                  TypeCode.TC_Long);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Long_Long) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Long_Long' (Value => Item),
                  TypeCode.TC_Long_Long);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Unsigned_Short) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_UShort' (Value => Item),
                  TypeCode.TC_Unsigned_Short);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Unsigned_Long) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_ULong' (Value => Item),
                  TypeCode.TC_Unsigned_Long);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Unsigned_Long_Long) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_ULong_Long' (Value => Item),
                  TypeCode.TC_Unsigned_Long_Long);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Float) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Float' (Value => Item),
                  TypeCode.TC_Float);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Double) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Double' (Value => Item),
                  TypeCode.TC_Double);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Long_Double) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Long_Double' (Value => Item),
                  TypeCode.TC_Long_Double);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Boolean) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Boolean' (Value => Item),
                  TypeCode.TC_Boolean);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Char) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Char' (Value => Item),
                  TypeCode.TC_Char);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Wchar) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Wchar' (Value => Item),
                  TypeCode.TC_Wchar);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Octet) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Octet' (Value => Item),
                  TypeCode.TC_Octet);
      return The_Any;
   end To_Any;

   function To_Any (Item : in Any) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Any' (Value => Item),
                  TypeCode.TC_Any);
      return The_Any;
   end To_Any;

   function To_Any (Item : in TypeCode.Object) return Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_TypeCode' (Value => Item),
                  TypeCode.TC_TypeCode);
      return The_Any;
   end To_Any;

   function To_Any (Item : in CORBA.String) return Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set_Kind (Tco, Tk_String);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Long (0)));
      --  the string is supposed to be unbounded
      The_Any := (new Content_String' (Value => Item), Tco);
      return The_Any;
   end To_Any;

   function To_Any (Item : in CORBA.Wide_String) return Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set_Kind (Tco, Tk_Wstring);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Long (0)));
      --  the string is supposed to be unbounded
      The_Any := (new Content_Wide_String' (Value => Item), Tco);
      return The_Any;
   end To_Any;

   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (Item : in Any) return Short is
      Tmp : Content_Short_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Short) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Short_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Long is
      Tmp : Content_Long_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Long) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Long_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Long is
      Tmp : Content_Long_Long_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Longlong) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Long_Long_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Short is
      Tmp : Content_UShort_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Ushort) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_UShort_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long is
      Tmp : Content_ULong_Ptr;
   begin
      if TypeCode.Kind (Item.The_Type) /= Tk_Ulong then
         raise Bad_Typecode;
      end if;
      Tmp := Content_ULong_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Unsigned_Long_Long is
      Tmp : Content_ULong_Long_Ptr;
   begin
      if TypeCode.Kind (Item.The_Type) /= Tk_Ulonglong then
         raise Bad_Typecode;
      end if;
      Tmp := Content_ULong_Long_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Float is
      Tmp : Content_Float_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Float) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Float_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Double is
      Tmp : Content_Double_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Double) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Double_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Long_Double is
      Tmp : Content_Long_Double_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Longdouble) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Long_Double_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Boolean is
      Tmp : Content_Boolean_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Boolean) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Boolean_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Char is
      Tmp : Content_Char_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Char) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Char_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Wchar is
      Tmp : Content_Wchar_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Widechar) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Wchar_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Octet is
      Tmp : Content_Octet_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Octet) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Octet_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return Any is
      Tmp : Content_Any_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Any) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Any_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return TypeCode.Object is
      Tmp : Content_TypeCode_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_TypeCode) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_TypeCode_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.String is
      Tmp : Content_String_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_String) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_String_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (Item : in Any) return CORBA.Wide_String is
      Tmp : Content_Wide_String_Ptr;
   begin
      if (TypeCode.Kind (Item.The_Type) /= Tk_Wstring) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Wide_String_Ptr (Item.The_Value);
      return Tmp.Value;
   end From_Any;

   ----------------
   --  Get_Type  --
   ----------------
   function Get_Type (The_Any : in Any) return  TypeCode.Object is
   begin
      return The_Any.The_Type;
   end Get_Type;

   ---------------------------------
   --  Iterate_Over_Any_Elements  --
   ---------------------------------
   procedure Iterate_Over_Any_Elements (In_Any : in Any) is
   begin
      null;
   end Iterate_Over_Any_Elements;


--    --------------------------
--    --  Force_Any_TypeCode  --
--    --------------------------

--    procedure Force_Any_TypeCode
--      (A : in out CORBA.Any;
--       T : in     CORBA.TypeCode.Object)
--    is
--    begin
--       A.The_Type := T;
--    end Force_Any_TypeCode;


--    ------------------------------------
--    --  Prepare_Any_From_Agregate_Tc  --
--    ------------------------------------

--    function Prepare_Any_From_Agregate_Tc
--      (Tc : in TypeCode.Object)
--       return Any
--    is
--       A : Any;
--       Cl : Content_List;
--    begin
--       A.The_Type := Tc;
--       --  copy typecode
--       A.The_Value :=
--        new Content_Agregat' (Value => Cl);
--       --  this any will be an agregate
--       return A;
--    end Prepare_Any_From_Agregate_Tc;


--    -------------------------------
--    --  Add_Agregate_Any_Member  --
--    -------------------------------

--    procedure Add_Agregate_Any_Member
--      (A      : in out Any;
--       Member : in     Any)
--    is
--       Cl : Content_List;
--    begin
--       --  this append to a list a cell containing a pointer to the
--       --  value of the new member
--       Cl := Content_Agregat_Ptr (A.The_Value).Value;
--       if Cl = null then
--          Content_Agregat_Ptr (A.The_Value).Value
--            := new Content_Cell' (Member.The_Value, null);
--       else
--          while Cl.Next /= null loop
--             Cl := Cl.Next;
--          end loop;
--          Cl.Next := new Content_Cell' (Member.The_Value, null);
--       end if;
--    end Add_Agregate_Any_Member;


--    -------------------------------
--    --  Get_Any_Agregate_Member  --
--    -------------------------------

--    function Get_Any_Agregate_Member
--      (A      : in Any;
--       Tc     : in TypeCode.Object;
--       N      : in CORBA.Long)
--       return Any
--    is
--       Res : Any;
--       Ptr : Content_List := Content_Agregat_Ptr (A.The_Value).Value;
--    begin
--       pragma Debug (O ("entering Get_Any_Agregate_Member"));
--       for I in 0 .. N - 1 loop
--          if Ptr.Next = null then
--             raise Adabroker_DII_Any_Agregate_Error;
--          else
--             Ptr := Ptr.Next;
--          end if;
--       end loop;
--       pragma Debug (O ("before res update"));
--       Res := (Ptr.The_Value, Tc);
--       pragma Debug (O ("leaving Get_Any_Agregate_Member"));
--       return Res;
--    end Get_Any_Agregate_Member;



--    -------------------------
--    --  Any_Agregate_Size  --
--    -------------------------

--    function Any_Agregate_Size
--      (A : in Any)
--       return CORBA.Long
--    is
--       Cl : Content_List := Content_Agregat_Ptr (A.The_Value).Value;
--    begin
--       return Agregate_Count (Cl);
--    end Any_Agregate_Size;


   ----------------------
   --  Agregate_Count  --
   ----------------------
   function Agregate_Count
     (Cl : in Content_List)
      return CORBA.Long
   is
      N : CORBA.Long := 0;
      Ptr : Content_List := Cl;
   begin
      while Ptr /= null loop
         N := N + 1;
         Ptr := Ptr.Next;
      end loop;
      return N;
   end Agregate_Count;


end CORBA;

