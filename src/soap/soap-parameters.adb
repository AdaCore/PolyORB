------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      S O A P . P A R A M E T E R S                       --
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

--  $Id$

with PolyORB.Types;
with SOAP.Types;

package body SOAP.Parameters is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Any.NVList.Internals;
   use PolyORB.Any.NVList.Internals.NV_Sequence;

   function Argument_Count (P : in List) return Natural is
   begin
      return Natural (Get_Count (Ref (P)));
   end Argument_Count;

   function Argument (P : in List; Name : in String)
     return NamedValue
   is
      use PolyORB.Types;

      Arg_Id : constant Identifier := To_PolyORB_String (Name);
      Args : constant Element_Array := To_Element_Array
        (List_Of (Ref (P)).all);
   begin
      for I in Args'Range loop
         if Args (I).Name = Arg_Id then
            return Args (I);
         end if;
      end loop;

      raise SOAP.Types.Data_Error;
   end Argument;

   function Argument (P : in List; N : in Positive)
     return NamedValue is
   begin
      return Element_Of (List_Of (Ref (P)).all, N);
   exception
      when others =>
         raise SOAP.Types.Data_Error;
   end Argument;

   function Exist (P : in List; Name : in String)
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
      when SOAP.Types.Data_Error =>
         return False;
      when others =>
         raise;
   end Exist;

   function Get (P : in List; Name : in String) return Integer is
   begin
      return Integer
        (PolyORB.Types.Long'(From_Any (Argument (P, Name).Argument)));
   end Get;

   function Get (P : in List; Name : in String) return Long_Float is
   begin
      return Long_Float
        (PolyORB.Types.Double'(From_Any (Argument (P, Name).Argument)));
   end Get;

   function Get (P : in List; Name : in String) return String is
   begin
      return PolyORB.Types.To_Standard_String
        (From_Any (Argument (P, Name).Argument));
   end Get;

   function Get (P : in List; Name : in String) return Boolean is
   begin
      return From_Any (Argument (P, Name).Argument);
   end Get;

--    function Get (P : in List; Name : in String) return Types.SOAP_Record;
--    --  Returns parameter named Name in P as a SOAP Struct value. Raises
--    --  Types.Data_Error if this parameter does not exist or is not a SOAP
--    --  Struct.

--    function Get (P : in List; Name : in String) return Types.SOAP_Array;
--    --  Returns parameter named Name in P as a SOAP Array value. Raises
--    --  Types.Data_Error if this parameter does not exist or is not a SOAP
--    --  Array.

   ------------------
   -- Constructors --
   ------------------

   procedure Create (L : out List) is
      Res : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Res);
      L := List'(Res with null record);
   end Create;

   function "&" (P : in List; O : in NamedValue) return List
   is
      Res : List := P;
   begin
      Append (List_Of (Ref (Res)).all, O);
      return Res;
   end "&";

   function "+" (O : in NamedValue) return List is
      Res : Ref;
   begin
      Create (Res);
      Add_Item (Res, O);
      return List'(Res with null record);
   end "+";

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : in List; N : in Natural) is
   begin
      if Argument_Count (P) /= N then
         raise SOAP.Types.Data_Error;
      end if;
   end Check;

   procedure Check_Typecode_Kind
     (P : in List;
      Name : in String;
      Tk : in PolyORB.Any.TCKind);

   procedure Check_Typecode_Kind
     (P : in List;
      Name : in String;
      Tk : in PolyORB.Any.TCKind)
   is
   begin
      if PolyORB.Any.TypeCode.Kind
        (Get_Type (Argument (P, Name).Argument)) /= Tk then
         raise SOAP.Types.Data_Error;
      end if;
   end Check_Typecode_Kind;

   procedure Check_Integer (P : in List; Name : in String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Long);
   end Check_Integer;

   procedure Check_Float (P : in List; Name : in String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Double);
   end Check_Float;

   procedure Check_Boolean (P : in List; Name : in String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Boolean);
   end Check_Boolean;

   procedure Check_Time_Instant (P : in List; Name : in String) is
   begin
      --  XXX ???
      raise SOAP.Types.Data_Error;
   end Check_Time_Instant;

   procedure Check_Base64 (P : in List; Name : in String) is
   begin
      --  XXX ???
      raise SOAP.Types.Data_Error;
   end Check_Base64;

   procedure Check_Null (P : in List; Name : in String) is
   begin
      --  XXX ???
      raise SOAP.Types.Data_Error;
   end Check_Null;

   procedure Check_Record (P : in List; Name : in String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Struct);
   end Check_Record;

   procedure Check_Array (P : in List; Name : in String) is
   begin
      Check_Typecode_Kind (P, Name, Tk_Array);
   end Check_Array;

end SOAP.Parameters;
