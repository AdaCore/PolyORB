------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D Y N A M I C . P R O X Y                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
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
                               Self.Private_Result.Argument,
                               TypeCode.Kind
                               (Get_Type
                                (Self.Private_Result.Argument)));
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
                                        A,
                                        TypeCode.Kind
                                        (Get_Type (Nv.Argument)));
                     CORBA.NVList.Set_Argument (It, A);
                  end if;
                  CORBA.NVList.Next (It);
               end loop;
            end;
      end case;
   end Unmarshal_Returned_Values;


   function Align_From_Any
     (A       : in Any;
      Size_In : in Unsigned_Long)
      return Unsigned_Long
   is
      S_Tmp : Unsigned_Long := Size_In;
      Tc    : TypeCode.Object := Get_Type (A);
      Tck   : TCKind := TypeCode.Kind (Tc);
   begin
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
            --  the first one acontains the value
            --  the second contains the discriminant
            declare
               Actual_Value : CORBA.Any := TypeCode.Parameter (Tc, 0);
               Discriminant : CORBA.Any := TypeCode.Parameter (Tc, 1);
            begin
               S_Tmp := Align_From_Any (Discriminant, S_Tmp);
               S_Tmp := Align_From_Any (Actual_Value, S_Tmp);
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
            --  the first one acontains the value
            --  the second contains the discriminant
            declare
               Actual_Value : CORBA.Any := TypeCode.Parameter (Tc, 0);
               Discriminant : CORBA.Any := TypeCode.Parameter (Tc, 1);
            begin
               Marshall_From_Any (Discriminant, GIOP_Client);
               Marshall_From_Any (Actual_Value, GIOP_Client);
            end;
         when Tk_Any =>
            null;
            --  ?
         when others =>
            --  should never be reached ?
            null;
      end case;
   end Marshall_From_Any;

   procedure Unmarshall_To_Any
     (GIOP_Client : in out AdaBroker.GIOP_C.Object;
      A           :    out Any;
      Tck         : TCKind)
   is
   begin
      case Tck is
         when Tk_Boolean =>
            declare
               Tmp : CORBA.Boolean;
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
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
--            Discrimant : Any :=
            null;
         when Tk_Any =>
            null;
            --  ?
            when others =>
               --  should never be reached ?
               null;
      end case;
   end Unmarshall_To_Any;


end Dynamic_Proxy;
