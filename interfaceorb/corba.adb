------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.23 $
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
                      return CORBA.Boolean is
      begin
         --  does shallow comparison, should be deep ??? see later
         return ((Self.Kind = TC.Kind)
                 and (Self.Parameters = TC.Parameters));
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
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Octet);
      The_Any := (new C_Octet' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Short)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Short);
      The_Any := (new C_Short' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Long)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Long);
      The_Any := (new C_Long' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Unsigned_Short)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Ushort);
      The_Any := (new C_UShort' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Unsigned_Long)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Ulong);
      The_Any := (new C_ULong' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Boolean)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Boolean);
      The_Any := (new C_Boolean' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.Char)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Char);
      The_Any := (new C_Char' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function To_Any (From : in CORBA.String)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_String);
      The_Any := (new C_String' (Value => From), Tco);
      return The_Any;
   end To_Any;


   -------------------------------------
   --  From_Any conversion functions  --
   -------------------------------------

   function From_Any (From : in CORBA.Any)
                      return CORBA.Octet is
      Tmp : C_Octet_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Octet) then
         raise Bad_Typecode;
      end if;
      Tmp := C_Octet_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Short is
      Tmp : C_Short_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Short) then
         raise Bad_Typecode;
      end if;
      Tmp := C_Short_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Unsigned_Short is
      Tmp : C_UShort_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Ushort) then
         raise Bad_Typecode;
      end if;
      Tmp := C_UShort_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Unsigned_Long is
      Tmp : C_ULong_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Ulong) then
         raise Bad_Typecode;
      end if;
      Tmp := C_ULong_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Boolean is
      Tmp : C_Boolean_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Boolean) then
         raise Bad_Typecode;
      end if;
      Tmp := C_Boolean_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.Char is
      Tmp : C_Char_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Char) then
         raise Bad_Typecode;
      end if;
      Tmp := C_Char_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   function From_Any (From : in CORBA.Any)
                      return CORBA.String is
      Tmp : C_String_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_String) then
         raise Bad_Typecode;
      end if;
      Tmp := C_String_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

end CORBA;
