------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.27 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with AdaBroker.Exceptions; use AdaBroker.Exceptions;
--  with CORBA.Object;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

package body CORBA is

   Flag : constant Natural
     := AdaBroker.Debug.Is_Active ("corba");
   procedure O is new AdaBroker.Debug.Output (Flag);

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members)
   is
      Result : IDL_Exception_Members'Class
        := AdaBroker.Exceptions.Get_Members (From);
   begin
      if Result in System_Exception_Members'Class then
         To := System_Exception_Members (Result);
      else
         pragma Debug
           (O ("cannot cast IDL_Exception_Members" &
               " into System_Exception_Members"));
         null;
      end if;
   end Get_Members;

   ---------------------
   -- To_CORBA_String --
   ---------------------

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String is
   begin
      return CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (S));
   end To_CORBA_String;

   ------------------------
   -- To_Standard_String --
   ------------------------

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (S));
   end To_Standard_String;

   package body TypeCode is

      procedure Set (O : out Object;
                     K : in CORBA.TCKind) is
      begin
         O.Kind := K;
         O.Parameters := null;
      end Set;

      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To   : out Bounds_Members) is
      begin
         To := Get_Members (From);
      end Get_Members;

      function Get_Members
        (X : Ada.Exceptions.Exception_Occurrence)
        return Bounds_Members
      is
         Result : IDL_Exception_Members'Class
           := AdaBroker.Exceptions.Get_Members (X);
      begin
         if Result not in Bounds_Members'Class then
            pragma Debug
              (O ("cannot cast IDL_Exception_Members into Bounds_Members"));
            raise Constraint_Error;
         end if;
         return Bounds_Members (Result);
      end Get_Members;

      function Equal (Self : in Object;
                      TC   : in Object)
                      return CORBA.Boolean
      is
         Nb_Param : CORBA.Long;
         Res : CORBA.Boolean := True;
      begin
         if Self.Kind /= TC.Kind then
            return False;
         end if;
         --  recursive comparison
         Nb_Param := Param_Count (Self);
         if Nb_Param /= Param_Count (TC) then
            return False;
         end if;
         for I in 0 .. Nb_Param - 1 loop
            Res := Res and
              Any_Equal (Parameter (TC, I), Parameter (Self, I));
            if Res = False then
               return False;
            end if;
         end loop;
         return Res;
      end Equal;

      function Kind (Self : in Object)
                     return TCKind is
      begin
         return Self.Kind;
      end Kind;

      function Param_Count (Self : in Object)
                            return CORBA.Long is
         N : Long := 0;
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         while (Ptr /= null)
         loop
            N := N + 1;
            Ptr := Ptr.Next;
         end loop;
         return N;
      end Param_Count;

      function Parameter (Self : in Object;
                          Index : in CORBA.Long)
                          return Any is
         --  assumes that the list has been built so that the first
         --  be the first reached
         Ptr : Cell_Ptr := Self.Parameters;
      begin
         if (Ptr = null) then
            raise Out_Of_Bounds_Index;
         end if;
         for I in 0 .. Index - 1
         loop
            Ptr := Ptr.Next;
            if (Ptr = null) then
               raise  Out_Of_Bounds_Index;
            end if;
         end loop;
         return Ptr.Parameter;
      end Parameter;

      --  implementation dependant ones :
      procedure Add_Parameter
        (Self  : in out Object;
         Param : in     CORBA.Any)
      is
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

      function From_Any (From : in CORBA.Any)
                         return CORBA.TypeCode.Object
      is
         Tmp : Content_TypeCode_Ptr;
      begin
         if (TypeCode.Kind (From.The_Type) /= Tk_TypeCode) then
            raise Bad_Typecode;
         end if;
         Tmp := Content_TypeCode_Ptr (From.The_Value);
         return Tmp.Value;
      end From_Any;

      function To_Any (From : in CORBA.TypeCode.Object)
                       return CORBA.Any is
         The_Any : CORBA.Any;
      begin
         The_Any := (new Content_TypeCode' (Value => From),
                     TypeCode.TC_TypeCode);
         return The_Any;
      end To_Any;

      function  Member_Index
        (Tck : in TCKind;
         N   : in CORBA.Long)
         return CORBA.Long
      is
      begin
         case Tck is
            when Tk_Struct =>
               return 2 * (N + 1);
            when Tk_Union =>
               return 3 * N + 4;
            when others =>
               return 0;
         end case;
      end Member_Index;

   end TypeCode;

   function Get_Type (The_Any : in CORBA.Any)
                      return  CORBA.TypeCode.Object is
   begin
      return The_Any.The_Type;
   end Get_Type;


   -----------------------------------
   --  To_Any conversion functions  --
   -----------------------------------

   function To_Any (From : in CORBA.Octet)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Octet' (Value => From), TypeCode.TC_Octet);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Short)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Short' (Value => From), TypeCode.TC_Short);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Long)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Long' (Value => From), TypeCode.TC_Long);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Unsigned_Short)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_UShort' (Value => From), TypeCode.TC_Ushort);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Unsigned_Long)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_ULong' (Value => From), TypeCode.TC_Ulong);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Boolean)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Boolean' (Value => From), TypeCode.TC_Boolean);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Char)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Char' (Value => From), TypeCode.TC_Char);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.String)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_String);
      CORBA.TypeCode.Add_Parameter (Tco, To_Any (CORBA.Long (0)));
      --  the string is supposed to be unbounded
      The_Any := (new Content_String' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Float)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Float' (Value => From), TypeCode.TC_Float);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Double)
                    return CORBA.Any is
      The_Any : CORBA.Any;
   begin
      The_Any := (new Content_Double' (Value => From), TypeCode.TC_Double);
      return The_Any;
   end To_Any;

   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (From : in CORBA.Any)
                      return CORBA.Octet is
      Tmp : Content_Octet_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Octet) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Octet_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Short is
      Tmp : Content_Short_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Short) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Short_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Long is
      Tmp : Content_Long_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Long) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Long_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Unsigned_Short is
      Tmp : Content_UShort_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Ushort) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_UShort_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Unsigned_Long is
      Tmp : Content_ULong_Ptr;
   begin
      if TypeCode.Kind (From.The_Type) /= Tk_Ulong
        and TypeCode.Kind (From.The_Type) /= Tk_Enum then
         --  enum's any also carry an unsigned long
         --  so we use the same from_any function
         raise Bad_Typecode;
      end if;
      Tmp := Content_ULong_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Boolean is
      Tmp : Content_Boolean_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Boolean) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Boolean_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Char is
      Tmp : Content_Char_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Char) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Char_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.String is
      Tmp : Content_String_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_String) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_String_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Float is
      Tmp : Content_Float_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Float) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Float_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Double is
      Tmp : Content_Double_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Double) then
         raise Bad_Typecode;
      end if;
      Tmp := Content_Double_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   procedure Force_Any_TypeCode
     (A : in out CORBA.Any;
      T : in     CORBA.TypeCode.Object)
   is
   begin
      A.The_Type := T;
   end Force_Any_TypeCode;


   function Prepare_Any_From_Agregate_Tc
     (Tc : in TypeCode.Object)
      return Any
   is
      A : Any;
      Cl : Content_List;
   begin
      A.The_Type := Tc;
      --  copy typecode
      A.The_Value :=
       new Content_Agregat' (Value => Cl);
      --  this any will be an agregate
      return A;
   end Prepare_Any_From_Agregate_Tc;


   procedure Add_Agregate_Any_Member
     (A      : in out Any;
      Member : in     Any)
   is
      Cl : Content_List;
   begin
      --  this append to a list a cell containing a pointer to the
      --  value of the new member
      Cl := Content_Agregat_Ptr (A.The_Value).Value;
      if Cl = null then
         Content_Agregat_Ptr (A.The_Value).Value
           := new Content_Cell' (Member.The_Value, null);
      else
         while Cl.Next /= null loop
            Cl := Cl.Next;
         end loop;
         Cl.Next := new Content_Cell' (Member.The_Value, null);
      end if;
   end Add_Agregate_Any_Member;

   function Get_Any_Agregate_Member
     (A      : in Any;
      Tc     : in TypeCode.Object;
      N      : in CORBA.Long)
      return Any
   is
      Res : Any;
      Ptr : Content_List := Content_Agregat_Ptr (A.The_Value).Value;
   begin
      pragma Debug (O ("entering Get_Any_Agregate_Member"));
      for I in 0 .. N - 1 loop
         if Ptr.Next = null then
            raise Adabroker_DII_Any_Agregate_Error;
         else
            Ptr := Ptr.Next;
         end if;
      end loop;
      pragma Debug (O ("before res update"));
      Res := (Ptr.The_Value, Tc);
      pragma Debug (O ("leaving Get_Any_Agregate_Member"));
      return Res;
   end Get_Any_Agregate_Member;

   function Simple_Any_Equal
     (A1 : in Any;
      A2 : in Any)
      return CORBA.Boolean
   is
   begin
      if TypeCode.Kind (Get_Type (A1)) /= TypeCode.Kind (Get_Type (A2)) then
         return False;
      end if;
      case TypeCode.Kind (Get_Type (A1)) is
         when Tk_Null =>
            return True;
         when Tk_Void =>
            return True;
         when Tk_Float =>
            return Content_Float_Ptr (A1.The_Value).Value =
              Content_Float_Ptr (A2.The_Value).Value;
         when Tk_Double =>
            return Content_Double_Ptr (A1.The_Value).Value =
              Content_Double_Ptr (A2.The_Value).Value;
         when Tk_Long =>
            return Content_Long_Ptr (A1.The_Value).Value =
              Content_Long_Ptr (A2.The_Value).Value;
         when Tk_Ulong =>
            return Content_ULong_Ptr (A1.The_Value).Value =
              Content_ULong_Ptr (A2.The_Value).Value;
         when Tk_Short =>
            return Content_Short_Ptr (A1.The_Value).Value =
              Content_Short_Ptr (A2.The_Value).Value;
         when Tk_Ushort =>
            return Content_UShort_Ptr (A1.The_Value).Value =
              Content_UShort_Ptr (A2.The_Value).Value;
         when Tk_Boolean =>
            return Content_Boolean_Ptr (A1.The_Value).Value =
              Content_Boolean_Ptr (A2.The_Value).Value;
         when Tk_Char =>
            return Content_Char_Ptr (A1.The_Value).Value =
              Content_Char_Ptr (A2.The_Value).Value;
         when Tk_Octet =>
            return Content_Octet_Ptr (A1.The_Value).Value =
              Content_Octet_Ptr (A2.The_Value).Value;
         when others =>
            --  unsupported type for comparison
            return False;
      end case;
   end Simple_Any_Equal;

   function Any_Equal
     (A1 : in Any;
      A2 : in Any)
      return CORBA.Boolean
   is
      Tck : TCKind;
   begin
      pragma Debug (O ("entering any_equal"));
      if not TypeCode.Equal (Get_Type (A1), Get_Type (A2)) then
         return False;
      end if;
      pragma Debug (O ("passed typecode test"));
      Tck := TypeCode.Kind (Get_Type (A1));
      case TypeCode.Kind (Get_Type (A1)) is
         when Tk_Null | Tk_Void =>
            return True;
         when Tk_Float =>
            return Content_Float_Ptr (A1.The_Value).Value =
              Content_Float_Ptr (A2.The_Value).Value;
         when Tk_Double =>
            return Content_Double_Ptr (A1.The_Value).Value =
              Content_Double_Ptr (A2.The_Value).Value;
         when Tk_Long =>
            return Content_Long_Ptr (A1.The_Value).Value =
              Content_Long_Ptr (A2.The_Value).Value;
         when Tk_Ulong =>
            return Content_ULong_Ptr (A1.The_Value).Value =
              Content_ULong_Ptr (A2.The_Value).Value;
         when Tk_Short =>
            return Content_Short_Ptr (A1.The_Value).Value =
              Content_Short_Ptr (A2.The_Value).Value;
         when Tk_Ushort =>
            return Content_UShort_Ptr (A1.The_Value).Value =
              Content_UShort_Ptr (A2.The_Value).Value;
         when Tk_Boolean =>
            return Content_Boolean_Ptr (A1.The_Value).Value =
              Content_Boolean_Ptr (A2.The_Value).Value;
         when Tk_Char =>
            return Content_Char_Ptr (A1.The_Value).Value =
              Content_Char_Ptr (A2.The_Value).Value;
         when Tk_Octet =>
            pragma Debug (O ("comparing with a tk_octet"));
            return Content_Octet_Ptr (A1.The_Value).Value =
              Content_Octet_Ptr (A2.The_Value).Value;
         when Tk_Any =>
            return Any_Equal (Content_Any_Ptr (A1.The_Value).Value,
                              Content_Any_Ptr (A2.The_Value).Value);
         when Tk_TypeCode =>
            return TypeCode.Equal (Content_TypeCode_Ptr (A1.The_Value).Value,
                                   Content_TypeCode_Ptr (A2.The_Value).Value);
--         when Tk_Objref =>
--            return Object.Is_Equivalent (Object.From_Any (A1),
--                                         Object.From_Any (A2));
         when Tk_Struct | Tk_Union =>
            --  agregate comparison (recursive)
            declare
               N : CORBA.Long;
               Cl1, Cl2 : Content_List;
            begin
               Cl1 := Content_Agregat_Ptr (A1.The_Value).Value;
               Cl2 := Content_Agregat_Ptr (A2.The_Value).Value;
               N := Agregate_Count (Cl1);
               if (N /= Agregate_Count (Cl2)) then
                  return False;
               end if;
               declare
                  Res : CORBA.Boolean := True;
                  Tc : TypeCode.Object := Get_Type (A1);
                  Any_Member_Tc : TypeCode.Object;
               begin
                  for I in 0 .. N - 1 loop
                     Any_Member_Tc :=
                       TypeCode.From_Any
                       (TypeCode.Parameter (Tc,
                                            TypeCode.Member_Index (Tck, I)));
                     Res := Res and
                       Any_Equal
                       (Get_Any_Agregate_Member (A1, Any_Member_Tc, I),
                        Get_Any_Agregate_Member (A2, Any_Member_Tc, I));
                     if Res = False then
                        return False;
                     end if;
                  end loop;
                  return True;
               end;
            end;

         when others =>
            --  unsupported type for comparison :
            --  tk_principal, tk_objref
            return False;
      end case;
   end Any_Equal;

   function Any_Agregate_Size
     (A : in Any)
      return CORBA.Long
   is
      Cl : Content_List := Content_Agregat_Ptr (A.The_Value).Value;
   begin
      return Agregate_Count (Cl);
   end Any_Agregate_Size;

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

