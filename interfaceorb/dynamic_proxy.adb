------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D Y N A M I C . P R O X Y                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

--
--  First see comments at the beginning of dynamic-proxy.ads
--

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
      Self.Private_Result := Res;

      --  to fix : exceptions are currently not supported in DII
      Set_User_Exceptions (Self, False);

   end Init;


   ---------------------------
   --  Get_Function_Result  --
   ---------------------------

   function Get_Function_Result
     (Self : in Operation_Proxy)
      return NamedValue
   is
   begin
      return Self.Private_Result;
   end Get_Function_Result;


   ----------------------------
   --  Get_Procedure_Result  --
   ----------------------------

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
         pragma Debug (O ("reading an element in args NVList for align"));
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
      --  (the spec allows to suppose that the client has built a list
      --  of args in the right order)
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
      Tc : TypeCode.Object;
   begin
      pragma Debug (O ("entering Unmarshal_Arguments"));
      case Self.Op_Type is
         when Operation_Function =>
            --  only one element to unmarshall, save in Private_Result
            Tc := Get_Type (Self.Private_Result.Argument);
            Unmarshall_To_Any (GIOP_Client, Self.Private_Result.Argument, Tc);
         when Operation_Procedure =>
            --  a list of elements to unmarshall, saved in the args NVList
            declare
               A  : Any;
               It : CORBA.NVList.Iterator;
               Nv : CORBA.NamedValue;
            begin
               CORBA.NVList.Start (It, Self.Args);
               while not CORBA.NVList.Done (It) loop
                  --  we are only interested in OUT/INOUT args
                  Nv := CORBA.NVList.Get (It);
                  pragma Debug (O ("param name is : " &
                                   CORBA.To_Standard_String (Nv.Name)));
                  if (Nv.Arg_Modes = CORBA.ARG_OUT
                      or Nv.Arg_Modes = CORBA.ARG_INOUT) then
                     pragma Debug
                       (O ("unmarshalling "
                           & CORBA.To_Standard_String (Nv.Name)));
                     Tc := Get_Type (Nv.Argument);
                     Unmarshall_To_Any (GIOP_Client, A, Tc);
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
         --  we call AdaBroker.NetBufferedStream.Align_Size which is
         --  able to compute alignement for all basic types (overriden)
         when Tk_Boolean =>
            --  for basic types (that have a well-known size), we dont need
            --  to look at their actual value
            declare
               Tmp : CORBA.Boolean := True;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Char =>
            declare
               Tmp : CORBA.Char := 'A';
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Short =>
            declare
               Tmp : CORBA.Short := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Ushort =>
            declare
               Tmp : CORBA.Unsigned_Short := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Long =>
            declare
               Tmp : CORBA.Long := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Ulong =>
            declare
               Tmp : CORBA.Unsigned_Long := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Float =>
            declare
               Tmp : CORBA.Float := 0.0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Double =>
            declare
               Tmp : CORBA.Double := 0.0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Octet =>
            declare
               Tmp : CORBA.Octet := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_String =>
            --  this type has an unknown size, so we look at its actual
            --  value
            declare
               Tmp : CORBA.String := From_Any (A);
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Struct =>
            --  to be able to use recursion, we make an any from each
            --  member (which may be of a complex type as well) and we
            --  call recursively this function with the anyfied member
            --  as the argument
            declare
               Nb_Members   : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 1) / 2;
               Tc_Mb : TypeCode.Object;
            begin
               pragma Debug (O ("align size for tk_struct"));
               for I in 0 .. Nb_Members - 1 loop
                  declare
                     Member : Any;
                  begin
                     Tc_Mb := TypeCode.From_Any
                       (TypeCode.Parameter (Tc, 2 * (I + 1)));
                     Member :=
                       Get_Any_Agregate_Member (A, Tc_Mb, I);
                     S_Tmp := Align_From_Any (Member, S_Tmp);
                  end;
               end loop;
            end;
         when Tk_Union =>
            --  first get and align discriminant
            --  then find out in which case we are to find the type of
            --  the actual value, then align it recusively
            pragma Debug (O ("aligning size for an union"));
            declare
               --  nb_member gives the number of cases in the union
               Nb_Members : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 2) / 3;
               Tc_Discriminant, Tc_Member : TypeCode.Object;
               Discriminant, Member, Label : Any;
               Cpt : CORBA.Long := 0;  --  counter
               Tc_Other_Cpt : CORBA.Long := -1;  -- counter memory
               --  others_any is a special value for the switch in the
               --  union that corresponds to the 'others' case
               --  see CORBA 2.2 spec chap 8.7.1
               Others_Any : Any := To_Any (CORBA.Octet (0));
            begin
               --  get the type of the discriminant : we don't check if
               --  it is an authorized one
               Tc_Discriminant :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 1));
               --  get the value of the discriminant
               Discriminant :=
                 Get_Any_Agregate_Member (A, Tc_Discriminant, 0);
               --  align the discriminant (recursively, also this should
               --  not be necessary if it is of an authorized type)
               S_Tmp := Align_From_Any (Discriminant, S_Tmp);
               --  find out the type of the actual value, by scanning
               --  all the defined label values
               while Cpt < Nb_Members loop
                  --  get label value
                  Label := TypeCode.Parameter (Tc, 3 * Cpt + 2);
                  --  compare it to the discriminant
                  if Any_Equal (Label, Discriminant) then
                     --  if equal, exit the loop, cpt will tell us where
                     --  to find the corresponding typecode for the actual
                     --  value carried in the union
                     exit;
                  end if;
                  --  save 'others' case position (in case we get no match)
                  if Any_Equal (Label, Others_Any) then
                     Tc_Other_Cpt := Cpt;  -- positive or 0
                  end if;
                  Cpt := Cpt + 1;
                  --  keep on looping...
               end loop;
               if Cpt = Nb_Members then
                  --  it means that we got no match, so we check if there was
                  --  was an 'others' label found
                  if Tc_Other_Cpt >= 0 then --  if there was one
                     pragma Debug (O ("use of 'others' case"));
                     Cpt := Tc_Other_Cpt; --  then we use it
                  else  --  else we have a problem
                     --  it means that we found no match for the dicriminant
                     --  value, and the union has no 'others' case defined
                     raise CORBA.Adabroker_DII_Union_Interpretation_Error;
                  end if;
               end if;
               --  get the type code of the actual value
               Tc_Member := TypeCode.From_Any
                 (TypeCode.Parameter (Tc, 3 * Cpt + 4));
               --  get and align the actual value recursively
               Member :=
                 Get_Any_Agregate_Member (A, Tc_Member, 1);
               S_Tmp := Align_From_Any (Member, S_Tmp);
            end;
         when Tk_Enum =>
            pragma Debug (O ("aligning size for an enum"));
            declare
               --  enums are coded as unsigned long in any
               Tmp : CORBA.Unsigned_Long := 0;
            begin
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Tmp, S_Tmp);
            end;
         when Tk_Sequence =>
            pragma Debug (O ("aligning size for a sequence"));
            declare
               --  get the type of the elements of the sequence
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               --  get the number of elements in the sequence (stored in A)
               N : CORBA.Long := Any_Agregate_Size (A);
               Elt : Any;
            begin
               --  first the size (that will be sent too)
               S_Tmp :=
                 AdaBroker.NetBufferedStream.Align_Size (Unsigned_Long (N),
                                                         S_Tmp);
               --  then the elements
               --  BEWARE : although all the elements are of the same type,
               --  we have to align all of them one by one (and not just align
               --  N times one of them to save time), because elements of same
               --  type can have different alignement (think of unions)
               for I in 0 .. N - 1 loop
                  --  get into an any each element (possibly complex)
                  Elt := Get_Any_Agregate_Member (A, Elts_Type, I);
                  --  align it recursively
                  S_Tmp := Align_From_Any (Elt, S_Tmp);
               end loop;
            end;
         when Tk_Array =>
            pragma Debug (O ("aligning size for an array"));
            declare
               --  elements type
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               --  number of elements
               N : CORBA.Unsigned_Long :=
                 From_Any (TypeCode.Parameter (Tc, 1));
               Elt : Any;
            begin
               for I in 0 .. N - 1 loop
                  --  get and align each element
                  Elt :=
                    Get_Any_Agregate_Member (A, Elts_Type, CORBA.Long (I));
                  S_Tmp := Align_From_Any (Elt, S_Tmp);
               end loop;
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref := CORBA.Object.From_Any (A);
            begin
               pragma Debug (O ("aligning size for tk_objref"));
               S_Tmp := CORBA.Object.OmniORB.Align_Size (Tmp, S_Tmp);
            end;
         when others =>
            --  unsupported : typecode, principal
            --  not in CORBA 2.0 : tk_except, tk_alias
            raise CORBA.AdaBroker_DII_Unsupported_Type;
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
      --  basically the same structure than align_from_any
      --  so redundant comments were not reproduced here
      case Tck is
         when Tk_Boolean =>
            --  contrary to align_size, we obviously need to get the
            --  actual value for basic types to marshall it
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
         when Tk_Struct =>
            declare
               Nb_Members   : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 1) / 2;
               Tc_Mb : TypeCode.Object;
            begin
               pragma Debug (O ("marshalling for tk_struct"));
               for I in 0 .. Nb_Members - 1 loop
                  declare
                     Member : Any;
                  begin
                     Tc_Mb := TypeCode.From_Any
                       (TypeCode.Parameter (Tc, 2 * (I + 1)));
                     Member :=
                       Get_Any_Agregate_Member (A, Tc_Mb, I);
                     Marshall_From_Any (Member, GIOP_Client);
                  end;
               end loop;
            end;
         when Tk_Union =>
            --  see tk_union for align_size_from_any
            pragma Debug (O ("marshalling for an union"));
            declare
               Nb_Members : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 2) / 3;
               Tc_Discriminant, Tc_Member : TypeCode.Object;
               Discriminant, Member, Label : Any;
               Cpt : CORBA.Long := 0;
               Tc_Other_Cpt : CORBA.Long := -1;
               Others_Any : Any := To_Any (CORBA.Octet (0));
            begin
               Tc_Discriminant :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 1));
               Discriminant :=
                 Get_Any_Agregate_Member (A, Tc_Discriminant, 0);
               Marshall_From_Any (Discriminant, GIOP_Client);
               while Cpt < Nb_Members loop
                  Label := TypeCode.Parameter (Tc, 3 * Cpt + 2);
                  if Any_Equal (Label, Discriminant) then
                     exit;
                  end if;
                  if Any_Equal (Label, Others_Any) then
                     Tc_Other_Cpt := Cpt;
                     pragma Debug (O ("found 'others' position in "
                                      & Cpt'Img));
                  end if;
                  Cpt := Cpt + 1;
               end loop;
               if Cpt = Nb_Members then
                  if Tc_Other_Cpt >= 0 then
                     pragma Debug (O ("use of 'others' case"));
                     Cpt := Tc_Other_Cpt;
                  else
                     --  see align_size_from_any for comments
                     raise CORBA.Adabroker_DII_Union_Interpretation_Error;
                  end if;
               end if;
               Tc_Member := TypeCode.From_Any
                 (TypeCode.Parameter (Tc, 3 * Cpt + 4));
               Member :=
                 Get_Any_Agregate_Member (A, Tc_Member, 1);
               Marshall_From_Any (Member, GIOP_Client);
            end;
         when Tk_Enum =>
            pragma Debug (O ("marshalling from any an enum"));
            declare
               Tmp : CORBA.Unsigned_Long := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Marshall (Tmp, GIOP_Client);
            end;
         when Tk_Sequence =>
            pragma Debug (O ("marshalling from any a sequence"));
            declare
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               N : CORBA.Long := Any_Agregate_Size (A);
               Elt : Any;
            begin
               --  first marshall length
               AdaBroker.NetBufferedStream.Marshall (Unsigned_Long (N),
                                                     GIOP_Client);
               --  then marshall elements
               for I in 0 .. N - 1 loop
                  Elt := Get_Any_Agregate_Member (A, Elts_Type, I);
                  Marshall_From_Any (Elt, GIOP_Client);
               end loop;
            end;
         when Tk_Array =>
            pragma Debug (O ("marshalling from any an array"));
            declare
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               N : CORBA.Unsigned_Long :=
                 From_Any (TypeCode.Parameter (Tc, 1));
               Elt : Any;
            begin
               for I in 0 .. N - 1 loop
                  Elt :=
                    Get_Any_Agregate_Member (A, Elts_Type, CORBA.Long (I));
                  Marshall_From_Any (Elt, GIOP_Client);
               end loop;
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref := CORBA.Object.From_Any (A);
            begin
               pragma Debug (O ("marshalling from any an objref"));
               CORBA.Object.OmniORB.Marshall (Tmp, GIOP_Client);
            end;
         when others =>
            --  unsupported : typecode, principal
            --  not in CORBA 2.0 : tk_except, tk_alias
            raise CORBA.AdaBroker_DII_Unsupported_Type;
      end case;
   end Marshall_From_Any;


   -------------------------
   --  Unmarshall_To_Any  --
   -------------------------

   procedure Unmarshall_To_Any
     (GIOP_Client : in out AdaBroker.GIOP_C.Object;
      A           :    out Any;
      Tc          : in TypeCode.Object)
   is
      Tck : CORBA.TCKind := CORBA.TypeCode.Kind (Tc);
   begin
      --  basically the same structure than align_from_any
      --  so redundant comments were not reproduced here
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
               pragma Debug (O ("unmarshalled to any a string : "
                                & To_Standard_String (Tmp)));
               A := To_Any (Tmp);
            end;
         when Tk_Struct =>
            pragma Debug (O ("unmarshalling a struct"));
            declare
               Nb_Members : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 1) / 2;
            begin
               A := Prepare_Any_From_Agregate_Tc (Tc);
               --  build a shallow any with one level of recursion
               --  set its type code
               for I in 0 .. Nb_Members - 1 loop
                  declare
                     Member : Any;
                     Tc_Mb : TypeCode.Object :=
                       TypeCode.From_Any
                       (TypeCode.Parameter (Tc, 2 * (I + 1)));
                  begin
                     Unmarshall_To_Any (GIOP_Client,
                                        Member,
                                        Tc_Mb);
                     Add_Agregate_Any_Member (A, Member);
                  end;
               end loop;
               Force_Any_TypeCode (A, Tc);
            end;
         when Tk_Union =>
            pragma Debug (O ("unmarshalling an union"));
            declare
               Nb_Members : CORBA.Long :=
                 (CORBA.TypeCode.Param_Count (Tc) - 2) / 3;
               Tc_Discriminant, Tc_Member : TypeCode.Object;
               Discriminant, Member, Label : Any;
               Cpt : CORBA.Long := 0;
               Tc_Other_Cpt : CORBA.Long := -1;
               Others_Any : Any := To_Any (CORBA.Octet (0));
            begin
               A := Prepare_Any_From_Agregate_Tc (Tc);
               --  get and set discriminant
               Tc_Discriminant :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 1));
               Unmarshall_To_Any (GIOP_Client,
                                  Discriminant,
                                  Tc_Discriminant);
               Add_Agregate_Any_Member (A, Discriminant);
               --  find out the type of the actual value
               while Cpt < Nb_Members loop
                  --  label value
                  Label := TypeCode.Parameter (Tc, 3 * Cpt + 2);
                  if Any_Equal (Label, Discriminant) then
                     exit;
                  end if;
                  --  save 'others' case position
                  if Any_Equal (Label, Others_Any) then
                     Tc_Other_Cpt := Cpt;  -- positive or 0
                  end if;
                  Cpt := Cpt + 1;
               end loop;
               if Cpt = Nb_Members then
                  --  check if not 'others' case
                  if Tc_Other_Cpt >= 0 then --  if there was one
                     Cpt := Tc_Other_Cpt; --  then we use it
                  else  --  else we have a problem
                     raise CORBA.Adabroker_DII_Union_Interpretation_Error;
                  end if;
               end if;
               --  get the type code of the actual value
               Tc_Member := TypeCode.From_Any
                 (TypeCode.Parameter (Tc, 3 * Cpt + 4));
               --  get and set the actual value
               Unmarshall_To_Any (GIOP_Client,
                                  Member,
                                  Tc_Member);
               Add_Agregate_Any_Member (A, Member);
               Force_Any_TypeCode (A, Tc);
            end;
         when Tk_Enum =>
            pragma Debug (O ("unmarshalling to any an enum"));
            declare
               Tmp : CORBA.Unsigned_Long := From_Any (A);
            begin
               AdaBroker.NetBufferedStream.Unmarshall (Tmp, GIOP_Client);
               A := To_Any (Tmp);
               Force_Any_TypeCode (A, Tc);
            end;
         when Tk_Sequence =>
            pragma Debug (O ("unmarshalling to any a sequence"));
            declare
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               Elt : Any;
               N : CORBA.Unsigned_Long;
            begin
               A := Prepare_Any_From_Agregate_Tc (Tc);
               --  first unmarshall length
               AdaBroker.NetBufferedStream.Unmarshall (N, GIOP_Client);
               --  then unmarshall elements
               for I in 0 .. N - 1 loop
                  Unmarshall_To_Any (GIOP_Client, Elt, Elts_Type);
                  Add_Agregate_Any_Member (A, Elt);
               end loop;
               Force_Any_TypeCode (A, Tc);
            end;
         when Tk_Array =>
            declare
               Elts_Type : TypeCode.Object :=
                 TypeCode.From_Any (TypeCode.Parameter (Tc, 0));
               N : CORBA.Unsigned_Long :=
                 From_Any (TypeCode.Parameter (Tc, 1));
               Elt : Any;
            begin
               pragma Debug (O ("unmarshalling to any an array; n = "
                                & N'Img));
               A := Prepare_Any_From_Agregate_Tc (Tc);
               for I in 0 .. N - 1 loop
                  Unmarshall_To_Any (GIOP_Client, Elt, Elts_Type);
                  Add_Agregate_Any_Member (A, Elt);
               end loop;
               Force_Any_TypeCode (A, Tc);
            end;
         when Tk_Objref =>
            declare
               Tmp : CORBA.Object.Ref;
            begin
               pragma Debug (O ("unmarshalling to any an objref"));
                  CORBA.Object.OmniORB.Unmarshall (Tmp, GIOP_Client);
                  A := CORBA.Object.To_Any (Tmp);
            end;
         when others =>
            --  unsupported : typecode, principal
            --  not in CORBA 2.0 : tk_except, tk_alias
            raise CORBA.AdaBroker_DII_Unsupported_Type;
      end case;
   end Unmarshall_To_Any;

end Dynamic_Proxy;
