------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . S O A P _ P . P A R A M E T E R S             --
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

with PolyORB.Types;
with PolyORB.SOAP_P.Types;

package body PolyORB.SOAP_P.Parameters is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Any.NVList.Internals;
   use PolyORB.Any.NVList.Internals.NV_Lists;

   function Argument_Count (P : List) return Natural is
   begin
      return Natural (Get_Count (Ref (P)));
   end Argument_Count;

   function Argument (P : List; Name : String)
     return NamedValue
   is
      use PolyORB.Types;

      Arg_Id : constant Identifier := To_PolyORB_String (Name);
      It : Iterator := First (List_Of (Ref (P)).all);
   begin
      while not Last (It) loop
         if Value (It).Name = Arg_Id then
            return Value (It).all;
         end if;
         Next (It);
      end loop;

      raise SOAP_P.Types.Data_Error;
   end Argument;

   function Argument (P : List; N : Positive)
     return NamedValue is
   begin
      return Element (List_Of (Ref (P)).all, N - 1).all;
   exception
      when others =>
         raise SOAP_P.Types.Data_Error;
   end Argument;

   function Exist (P : List; Name : String)
     return Boolean is
   begin
      declare
         NV : constant NamedValue := Argument (P, Name);
      begin
         pragma Warnings (Off, NV);
         --  Not referenced. We are only interested in knowing
         --  whether the call to Argument raised an exception.
         return True;
      end;
   exception
      when SOAP_P.Types.Data_Error =>
         return False;

   end Exist;

   function Get (P : List; Name : String) return Integer is
   begin
      return Integer
        (PolyORB.Types.Long'(From_Any (Argument (P, Name).Argument)));
   end Get;

   function Get (P : List; Name : String) return Long_Float is
   begin
      return Long_Float
        (PolyORB.Types.Double'(From_Any (Argument (P, Name).Argument)));
   end Get;

   function Get (P : List; Name : String) return String is
   begin
      return PolyORB.Types.To_Standard_String
        (From_Any (Argument (P, Name).Argument));
   end Get;

   function Get (P : List; Name : String) return Boolean is
   begin
      return From_Any (Argument (P, Name).Argument);
   end Get;

--    function Get (P : List; Name : String) return Types.SOAP_Record;
--    --  Returns parameter named Name in P as a SOAP Struct value. Raises
--    --  Types.Data_Error if this parameter does not exist or is not a SOAP
--    --  Struct.

--    function Get (P : List; Name : String) return Types.SOAP_Array;
--    --  Returns parameter named Name in P as a SOAP Array value. Raises
--    --  Types.Data_Error if this parameter does not exist or is not a SOAP
--    --  Array.

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : List; O : NamedValue) return List
   is
      Res : constant List := P;
   begin
      Append (List_Of (Ref (Res)).all, O);
      return Res;
   end "&";

   function "+" (O : NamedValue) return List is
      Res : Ref;
   begin
      Create (Res);
      Add_Item (Res, O);
      return List'(Res with null record);
   end "+";

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : List; N : Natural) is
   begin
      if Argument_Count (P) /= N then
         raise SOAP_P.Types.Data_Error;
      end if;
   end Check;

   procedure Check_Typecode_Kind
     (P : List;
      Name : String;
      Tk : PolyORB.Any.TCKind);

   procedure Check_Typecode_Kind
     (P : List;
      Name : String;
      Tk : PolyORB.Any.TCKind)
   is
   begin
      if PolyORB.Any.TypeCode.Kind
        (Get_Type (Argument (P, Name).Argument)) /= Tk then
         raise SOAP_P.Types.Data_Error;
      end if;
   end Check_Typecode_Kind;

   procedure Check_Integer (P : List; Name : String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Long);
   end Check_Integer;

   procedure Check_Float (P : List; Name : String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Double);
   end Check_Float;

   procedure Check_Boolean (P : List; Name : String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Boolean);
   end Check_Boolean;

   procedure Check_Time_Instant (P : List; Name : String) is
   begin
      --  XXX ???
      raise SOAP_P.Types.Data_Error;
   end Check_Time_Instant;

   procedure Check_Base64 (P : List; Name : String) is
   begin
      --  XXX ???
      raise SOAP_P.Types.Data_Error;
   end Check_Base64;

   procedure Check_Null (P : List; Name : String) is
   begin
      --  XXX ???
      raise SOAP_P.Types.Data_Error;
   end Check_Null;

   procedure Check_Record (P : List; Name : String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Struct);
   end Check_Record;

   procedure Check_Array (P : List; Name : String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Array);
   end Check_Array;

end PolyORB.SOAP_P.Parameters;
