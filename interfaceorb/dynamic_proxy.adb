------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D Y N A M I C . P R O X Y                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
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
--                  (email: adabroker-devel@ada.eu.org)                     --
--                                                                          --
------------------------------------------------------------------------------


with CORBA;
with CORBA.NVList;
with AdaBroker.NetBufferedStream;
with AdaBroker.GIOP_C;
use CORBA;
with CORBA.Object;
with CORBA.Object.OmniORB;

with AdaBroker.Debug;
pragma Elaborate_All (Adabroker.Debug);


package body Dynamic_Proxy is

   Flag : constant Natural
      := AdaBroker.Debug.Is_Active ("dynamic_proxy");
   procedure O is new AdaBroker.Debug.Output (Flag);

   ------------
   --  Init  --
   ------------

   procedure Init
     (Self : in out Operation_Proxy;
      Op   : in     Identifier;
      Args : in     NVList.Object;
      Res  : in     NamedValue;
      Ot   : in     Operation_Type)
   is
   begin
      Self.Op_Name := Op;
      Self.Op_Type := Ot;
      Self.Args := Args;
      NVList.Revert (Self.Args);
      Self.Private_Result := Res;

      --  to fix
      Set_User_Exceptions (Self, False);

   end Init;


   ------------------
   --  Get_Result  --
   ------------------

   function Get_Function_Result
     (Self : in Operation_Proxy)
      return NamedValue
   is
   begin
      return Self.Private_Result;
   end Get_Function_Result;

   function Get_Procedure_Result
     (Self : in Operation_Proxy)
      return NVList.Object
   is
   begin
      return Self.Args;
   end Get_Procedure_Result;


   -----------------
   --  Operation  --
   -----------------

   function Operation
    (Self : in Operation_Proxy)
     return CORBA.String is
   begin
      return CORBA.String (Self.Op_Name);
   end Operation;


   ------------------
   --  Align_Size  --
   ------------------



   function Align_Size
     (Self    : in Operation_Proxy;
      Size_In : in Unsigned_Long)
      return Unsigned_Long
   is
      S_Tmp : Unsigned_Long := Size_In;
      It    : NVList.Iterator;
      Nv    : NamedValue;
   begin
      pragma Debug (O ("entering Align_Size with " & S_Tmp'Img));
      NVList.Start (It, Self.Args);
      while not NVList.Done (It) loop
         Nv := NVList.Get (It);
         --  we are only interested in IN/INOUT args
         if (Nv.Arg_Modes = CORBA.ARG_IN
             or Nv.Arg_Modes = CORBA.ARG_INOUT) then
            pragma Debug (O ("aligning size for "
                             & CORBA.To_Standard_String (Nv.Name)));
            S_Tmp := Align_From_Any (Nv.Argument, S_Tmp);
         end if;
         NVList.Next (It);
      end loop;
      return S_Tmp;
   end Align_Size;


   ------------------------
   --  Marshal_Argument  --
   ------------------------

   procedure Marshal_Arguments
     (Self        : in     Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object)
   is
      It  : NVList.Iterator;
      Nv  : NamedValue;
   begin
      pragma Debug (O ("entering Marshal_Arguments"));
      --  assumes that the arguments are in a correct order in the list
      NVList.Start (It, Self.Args);
      while not NVList.Done (It) loop
         Nv := NVList.Get (It);
         --  we are only interested in IN/INOUT args
         if (Nv.Arg_Modes = CORBA.ARG_IN
             or Nv.Arg_Modes = CORBA.ARG_INOUT) then
            pragma Debug (O ("marshalling "
                             & CORBA.To_Standard_String (Nv.Name)));
            Marshall_From_Any (Nv.Argument, GIOP_Client);
         end if;
         NVList.Next (It);
      end loop;
   end Marshal_Arguments;


   ---------------------------------
   --  Unmarshal_Returned_Values  --
   ---------------------------------

   procedure Unmarshal_Returned_Values
     (Self        : in out Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object)
   is
      A : Any;
   begin
      pragma Debug (O ("entering Unmarshal_Arguments"));
      case Self.Op_Type is
         when Operation_Function =>
            pragma Debug (O ("operation is a function"));
            Unmarshall_To_Any (GIOP_Client,
                               Self.Private_Result.Argument);

            if (Self.Op_Name = To_CORBA_String ("inverseStruct")) then
               declare
                  Ma : Any := Self.Private_Result.Argument;
                  Tc : TypeCode.Object := Get_Type (Ma);
                  Mb1 : Any := TypeCode.Parameter (Tc, 0);
                  Mb2 : Any := TypeCode.Parameter (Tc, 1);
                  A : CORBA.Boolean := From_Any (Mb1);
                  B : CORBA.Long := From_Any (Mb2);
               begin
                  null;
                  if (TypeCode.Kind (Tc) /= Tk_Struct) then
                     pragma Debug (O ("pb on tk_struct"));
                     null;
                  end if;
                  pragma Debug (O (" i read : "
                                   & A'Img & " " & B'Img));
               end;
            end if;
         when Operation_Procedure =>
            pragma Debug (O ("operation is a procedure"));
            declare
               It : CORBA.NVList.Iterator;
               Nv : CORBA.NamedValue;
            begin
               CORBA.NVList.Start (It, Self.Args);
               while not CORBA.NVList.Done (It) loop
                  --  we are only interested in OUT/INOUT args
                  Nv := CORBA.NVList.Get (It);
                  if (Nv.Arg_Modes = CORBA.ARG_OUT
                      or Nv.Arg_Modes = CORBA.ARG_INOUT) then
                     pragma Debug
                       (O ("unmarshalling "
                           & CORBA.To_Standard_String (Nv.Name)));
                     Unmarshall_To_Any (GIOP_Client,
                                        Nv.Argument);
                     CORBA.NVList.Set_Argument (It, A);
                  end if;
                  CORBA.NVList.Next (It);
               end loop;
            end;
      end case;
   end Unmarshal_Returned_Values;


   ----------------------
   --  Align_From_Any  --
   ----------------------

   function Align_From_Any
     (A       : in Any;
      Size_In : in Unsigned_Long)
      return Unsigned_Long
   is
      S_Tmp : Unsigned_Long := Size_In;
      Tc    : TypeCode.Object := Get_Type (A);
      Tck   : TCKind := TypeCode.Kind (Tc);
   begin
      pragma Debug (O ("entering Align_From_Any"));
      case Tck is
         when Tk_Boolean =>
            declare
               Tmp : CORBA.Boolean := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Char =>
            declare
               Tmp : CORBA.Char := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Short =>
            declare
               Tmp : CORBA.Short := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Ushort =>
            declare
               Tmp : CORBA.Unsigned_Short := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Long =>
            declare
               Tmp : CORBA.Long := From_Any (A);
            begin
               pragma Debug (O ("align size for tk_long"));
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Ulong =>
            declare
               Tmp : CORBA.Unsigned_Long := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Float =>
            declare
               Tmp : CORBA.Float := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Double =>
            declare
               Tmp : CORBA.Double := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Octet =>
            declare
               Tmp : CORBA.Octet := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_String =>
            declare
               Tmp : CORBA.String := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Union =>
            --  the type code contains a list of 2 anys
            --  the first one acontains the discrimant
            --  the second contains the value
            declare
               Discriminant : Any := TypeCode.Parameter (Tc, 0);
               Actual_Value : Any := TypeCode.Parameter (Tc, 1);
            begin
               S_Tmp := Align_From_Any (Discriminant, S_Tmp);
               S_Tmp := Align_From_Any (Actual_Value, S_Tmp);
            end;
         when Tk_Enum =>
            declare
               Actual_Value : Any := TypeCode.Parameter (Tc, 0);
            begin
               S_Tmp := Align_From_Any (Actual_Value, S_Tmp);
            end;
         when Tk_Struct =>
            --  the type code contains a list of any
            --  which contain the members of the struct
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               pragma Debug (O ("align size for tk_struct"));
               for I in 0 .. Nb_Members - 1 loop
                  S_Tmp := Align_From_Any (TypeCode.Parameter (Tc, I), S_Tmp);
               end loop;
            end;
         when Tk_Sequence =>
            --  same for bounded or unbounded sequences
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               pragma Debug (O ("align size for tk_sequence"));
               --  the first element is actually the number of real elements
               for I in 0 .. Nb_Members - 1 loop
                  S_Tmp := Align_From_Any (TypeCode.Parameter (Tc, I), S_Tmp);
               end loop;
            end;
         when Tk_Array =>
            --  the typecode contains a list of any
            --  which  contain the values in the array
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               pragma Debug (O ("align size for tk_array; nbm = "
                                & Nb_Members'Img));
               for I in 0 .. Nb_Members - 1 loop
                  S_Tmp := Align_From_Any (TypeCode.Parameter (Tc, I), S_Tmp);
               end loop;
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref := CORBA.Object.From_Any (A);
            begin
               pragma Debug (O ("align size for tk_objref"));
               S_Tmp := CORBA.Object.OmniORB.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Any =>
            null;
            --  ?
         when others =>
            --  should never be reached ?
            null;
      end case;
      return S_Tmp;
   end Align_From_Any;


   -------------------------
   --  Marshall_From_Any  --
   -------------------------

   procedure Marshall_From_Any
     (A           : in      Any;
      GIOP_Client : in out AdaBroker.GIOP_C.Object)
   is
      Tc    : TypeCode.Object := Get_Type (A);
      Tck   : TCKind := TypeCode.Kind (Tc);
   begin
      case Tck is
         when Tk_Boolean =>
            declare
               Tmp : CORBA.Boolean := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Char =>
            declare
               Tmp : CORBA.Char := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Short =>
            declare
               Tmp : CORBA.Short := From_Any (A);
            begin
               pragma Debug (O ("marshalling a Short"));
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Ushort =>
            declare
               Tmp : CORBA.Unsigned_Short := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Long =>
            declare
               Tmp : CORBA.Long := From_Any (A);
            begin
               pragma Debug (O ("marshalling a Long"));
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Ulong =>
            declare
               Tmp : CORBA.Unsigned_Long := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Float =>
            declare
               Tmp : CORBA.Float := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Double =>
            declare
               Tmp : CORBA.Double := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Octet =>
            declare
               Tmp : CORBA.Octet := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_String =>
            declare
               Tmp : CORBA.String := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Union =>
            --  the type code contains a list of 2 anys
            --  the first one acontains the discriminant
            --  the second contains the value
            declare
               Actual_Value : CORBA.Any := TypeCode.Parameter (Tc, 1);
               Discriminant : CORBA.Any := TypeCode.Parameter (Tc, 0);
            begin
               pragma Debug (O ("marshalling tk_union, tc -> " &
                                TypeCode.Param_Count (Tc)'Img &
                                " members"));
               Marshall_From_Any (Discriminant, GIOP_Client);
               Marshall_From_Any (Actual_Value, GIOP_Client);
            end;
         when Tk_Enum =>
            declare
               Actual_Value : CORBA.Any := TypeCode.Parameter (Tc, 0);
            begin
               Marshall_From_Any (Actual_Value, GIOP_Client);
            end;
         when Tk_Struct =>
            --  see Align_From_Any for comments
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               for I in 0 .. Nb_Members - 1 loop
                  Marshall_From_Any (CORBA.TypeCode.Parameter (Tc, I),
                                     GIOP_Client);
               end loop;
            end;
         when Tk_Sequence =>
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               pragma Debug (O ("marshalling sequence with "
                                & Nb_Members'Img
                                & " elements"));
               for I in 0 .. Nb_Members - 1 loop
                  Marshall_From_Any (CORBA.TypeCode.Parameter (Tc, I),
                                     GIOP_Client);
               end loop;
            end;
         when Tk_Array =>
            --  see Align_From_Any for comments
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
            begin
               pragma Debug (O ("marshalling an array of "
                                & Nb_Members'Img));
               for I in 0 .. Nb_Members - 1 loop
                  Marshall_From_Any (CORBA.TypeCode.Parameter (Tc, I),
                                     GIOP_Client);
               end loop;
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref := CORBA.Object.From_Any (A);
            begin
               pragma Debug (O ("marshalling an objref"));
               CORBA.Object.OmniORB.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Any =>
            null;
            --  ?
         when others =>
            --  should never be reached ?
            null;
      end case;
   end Marshall_From_Any;


   -------------------------
   --  Unmarshall_To_Any  --
   -------------------------

   procedure Unmarshall_To_Any
     (GIOP_Client : in out AdaBroker.GIOP_C.Object;
      A           : in out CORBA.Any)
   is
      Tc  : CORBA.TypeCode.Object := CORBA.Get_Type (A);
      Tck : CORBA.TCKind := CORBA.TypeCode.Kind (Tc);
   begin
      case Tck is
         when Tk_Boolean =>
            declare
               Tmp : CORBA.Boolean;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               pragma Debug (O ("unmarshalling a boolean -> " & Tmp'Img));
               A := To_Any (Tmp);
            end;
         when Tk_Char =>
            declare
               Tmp : CORBA.Char;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Short =>
            declare
               Tmp : CORBA.Short;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Ushort =>
            declare
               Tmp : CORBA.Unsigned_Short;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Long =>
            declare
               Tmp : CORBA.Long;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               pragma Debug (O ("unmarshalling a long -> " & Tmp'Img));
               A := To_Any (Tmp);
            end;
         when Tk_Ulong =>
            declare
               Tmp : CORBA.Unsigned_Long;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Float =>
            declare
               Tmp : CORBA.Float;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Double =>
            declare
               Tmp : CORBA.Double;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Octet =>
            declare
               Tmp : CORBA.Octet;
               begin
                  AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
                  A := To_Any (Tmp);
               end;
         when Tk_String =>
            declare
               Tmp : CORBA.String;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
            end;
         when Tk_Union =>
            declare
               Discrimant : Any := TypeCode.Parameter (Tc, 0);
               --  the discriminant is really useless since the
               --  client must already now how the returned union is used
               Actual_Value : Any := TypeCode.Parameter (Tc, 1);
               Tc2 : TypeCode.Object;
            begin
               pragma Debug (O ("unmarshalling an union"));
               TypeCode.Set (Tc2, Tk_Union);
               Unmarshall_To_Any (GIOP_Client, Discrimant);
               Unmarshall_To_Any (GIOP_Client, Actual_Value);
               TypeCode.Add_Parameter (Tc2, Actual_Value);
               TypeCode.Add_Parameter (Tc2, Discrimant);
               SetAny (A, Tc2);
            end;
         when Tk_Enum =>
            declare
               Actual_Value : Any := TypeCode.Parameter (Tc, 0);
               Tc2 : TypeCode.Object;
            begin
               pragma Debug (O ("unmarshalling an enum"));
               TypeCode.Set (Tc2, Tk_Enum);
               Unmarshall_To_Any (GIOP_Client, Actual_Value);
               TypeCode.Add_Parameter (Tc2, Actual_Value);
               SetAny (A, Tc2);
            end;
         when Tk_Struct =>
            --  see Align_From_Any for comments
            pragma Debug (O ("unmarshalling a struct"));
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
               Member       : CORBA.Any;
               Tc2          : CORBA.TypeCode.Object;
            begin
               TypeCode.Set (Tc2, Tk_Struct);
               for I in 0 .. Nb_Members - 1 loop
                  Member := CORBA.TypeCode.Parameter (Tc, I);
                  Unmarshall_To_Any (GIOP_Client, Member);
                  TypeCode.Add_Parameter (Tc2, Member);
               end loop;
               TypeCode.Reverse_Parameters (Tc2);
               SetAny (A, Tc2);
            end;
         when Tk_Sequence =>
            pragma Debug (O ("unmarshalling a sequence"));
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
               Member       : CORBA.Any;
               Tc2          : CORBA.TypeCode.Object;
            begin
               TypeCode.Set (Tc2, Tk_Sequence);
               for I in 0 .. Nb_Members - 1 loop
                  Member := CORBA.TypeCode.Parameter (Tc, I);
                  Unmarshall_To_Any (GIOP_Client, Member);
                  TypeCode.Add_Parameter (Tc2, Member);
               end loop;
               TypeCode.Reverse_Parameters (Tc2);
               SetAny (A, Tc2);
            end;
         when Tk_Array =>
            --  see Align_From_Any for comments
            pragma Debug (O ("unmarshalling an array"));
            declare
               Nb_Members   : CORBA.Long := CORBA.TypeCode.Param_Count (Tc);
               Member       : CORBA.Any;
               Tc2          : CORBA.TypeCode.Object;
            begin
               TypeCode.Set (Tc2, Tk_Array);
               for I in 0 .. Nb_Members - 1 loop
                  Member := CORBA.TypeCode.Parameter (Tc, I);
                  Unmarshall_To_Any (GIOP_Client, Member);
                  TypeCode.Add_Parameter (Tc2, Member);
               end loop;
               TypeCode.Reverse_Parameters (Tc2);
               SetAny (A, Tc2);
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref;
               begin
                  pragma Debug (O ("unmarshalling an objref"));
                  CORBA.Object.OmniORB.Unmarshall (Tmp, GIOP_Client);
                  A := CORBA.Object.To_Any (Tmp);
               end;


         when Tk_Any =>
            null;
            --  ?
            when others =>
               --  should never be reached ?
               null;
      end case;
   end Unmarshall_To_Any;


end Dynamic_Proxy;
