with Helper;
with CORBA; use CORBA;

with Report;    use Report;

--  To understand better the To_Any and From_Any functions, the reader
--  might want to read the CORBA spec 2.2 chapter 8.7.1 which explains
--  how TypeCode are specified.

package body Helper is


   function TC_String return TypeCode.Object is
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_String);
      TypeCode.Add_Parameter (Tc, To_Any (Unsigned_Long (0)));
      return Tc;
   end;


   ---------------
   --  Example  --
   ---------------

   function TC_Example return TypeCode.Object is
      Tc : TypeCode.Object;
      Any_D_Tc, Name,
        Any_Lab1, Any_Lab2, Any_Lab3,
        Mb1_Tc, Mb2_Tc, Mb3_Tc,
        Mb1_Name, Mb2_Name, Mb3_Name : Any;
      S : CORBA.String;
   begin
      TypeCode.Set (Tc, Tk_Union);  --  set the kind
      S := To_CORBA_String ("Example");  --  first parameter of typecode
      Name := To_Any (S);

      Any_D_Tc :=
        TypeCode.To_Any (TypeCode.TC_Long); --  2nd parameter (discrimant tc)
      Any_Lab1 := To_Any (CORBA.Long (1)); --  3rd param (label 1)
      S := To_CORBA_String ("Counter"); --  4th param (name member 1)
      Mb1_Name := To_Any (S);
      Mb1_Tc :=
        TypeCode.To_Any (TypeCode.TC_Long);  -- 5th param (typecode member 1)

      Any_Lab2 := To_Any (CORBA.Long (2)); --  6th param (label 2)
      S := To_CORBA_String ("Flags"); --  7th param (name member 2)
      Mb2_Name := To_Any (S);
      Mb2_Tc :=
        TypeCode.To_Any (TypeCode.TC_Boolean); -- 8th param (typecode member 2)

      Any_Lab3 := To_Any (CORBA.Octet (0)); --  9th param (label default)
      S := To_CORBA_String ("Unknown"); --  10th param (name member default)
      Mb3_Name := To_Any (S);
      Mb3_Tc :=
        TypeCode.To_Any (TypeCode.TC_Long);  -- 11th param (tc member default)

      TypeCode.Add_Parameter (Tc, Name);
      TypeCode.Add_Parameter (Tc, Any_D_Tc);
      TypeCode.Add_Parameter (Tc, Any_Lab1);
      TypeCode.Add_Parameter (Tc, Mb1_Name);
      TypeCode.Add_Parameter (Tc, Mb1_Tc);
      TypeCode.Add_Parameter (Tc, Any_Lab2);
      TypeCode.Add_Parameter (Tc, Mb2_Name);
      TypeCode.Add_Parameter (Tc, Mb2_Tc);
      TypeCode.Add_Parameter (Tc, Any_Lab3);
      TypeCode.Add_Parameter (Tc, Mb3_Name);
      TypeCode.Add_Parameter (Tc, Mb3_Tc);
      return Tc;
   end TC_Example;

   function To_Any (From : in Example) return Any is
      Arg_Any,
        Mb1_Any, Mb2_Any : Any;
   begin
      --  build the any argument with the right typecodede
      Arg_Any := Prepare_Any_From_Agregate_Tc (TC_Example);
      Mb1_Any := To_Any (CORBA.Long (From.Switch));
      case From.Switch is
         when 1 =>
            Mb2_Any := To_Any (From.Counter);
         when 2 =>
            Mb2_Any := To_Any (From.Flags);
         when others =>
            Mb2_Any := To_Any (From.Unknown);
      end case;
      Add_Agregate_Any_Member (Arg_Any, Mb1_Any);
      Add_Agregate_Any_Member (Arg_Any, Mb2_Any);
      return Arg_Any;
   end To_Any;

   function From_Any (From : in Any) return Example is
      Tc_Res : TypeCode.Object := Get_Type (From);
      Val_Any, Sw_Any : Any;
      Sw : CORBA.Long;
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Union) then
         raise Bad_Typecode;
      end if;
      --  get the switch
      Sw_Any := Get_Any_Agregate_Member (From, TypeCode.TC_Long, 0);
      Sw := From_Any (Sw_Any);
      begin
         case Sw is
            when 1 =>
               declare
                  Res : Example (1);
               begin
                  Val_Any :=
                    Get_Any_Agregate_Member (From, TypeCode.TC_Long, 1);
                  Res.Counter := From_Any (Val_Any);
                  return Res;
               end;
            when 2 =>
               declare
                  Res : Example (2);
               begin
                  Val_Any :=
                    Get_Any_Agregate_Member (From, TypeCode.TC_Boolean, 1);
                  Res.Flags := From_Any (Val_Any);
                  return Res;
               end;
            when others =>
               declare
                  Res : Example (Sw);
               begin
                  Val_Any :=
                    Get_Any_Agregate_Member (From, TypeCode.TC_Long, 1);
                  Res.Unknown := From_Any (Val_Any);
                  return Res;
               end;
         end case;
      end;
   end From_Any;

   --------------------
   --  simple array  --
   --------------------

   function TC_Simple_Array return TypeCode.Object is
      Tc : TypeCode.Object;
      P1, P2 : Any;
   begin
      P1 := TypeCode.To_Any (TypeCode.TC_Long);  --  type of the elements
      P2 := To_Any (CORBA.Unsigned_Long (10));  --  nb of elements
      TypeCode.Set (Tc, Tk_Array);
      TypeCode.Add_Parameter (Tc, P1);
      TypeCode.Add_Parameter (Tc, P2);
      return Tc;
   end TC_Simple_Array;

   function To_Any (From : in Simple_Array) return Any is
      A : Any := Prepare_Any_From_Agregate_Tc (Tc_Simple_Array);
   begin
      for I in 0 .. 9 loop
         Add_Agregate_Any_Member (A, To_Any (From (I)));
      end loop;
      return A;
   end To_Any;

   function From_Any (From : in Any) return Simple_Array is
      X : Simple_Array;
   begin
      for I in 0 .. 9 loop
         X (I) :=
           From_Any (Get_Any_Agregate_Member (From,
                                              TypeCode.TC_Long,
                                              CORBA.Long (I)));
      end loop;
      return X;
   end From_Any;

   ---------------------
   --  Simple_Struct  --
   ---------------------

   function TC_A_Array return TypeCode.Object is
      Tc : TypeCode.Object;
      P1, P2 : Any;
   begin
      P1 := TypeCode.To_Any (TypeCode.TC_Long);  --  type of the elements
      P2 := To_Any (CORBA.Unsigned_Long (10));  --  nb of elements
      TypeCode.Set (Tc, Tk_Array);
      TypeCode.Add_Parameter (Tc, P1);
      TypeCode.Add_Parameter (Tc, P2);
      return Tc;
   end TC_A_Array;

   function To_Any (From : A_Array) return Any is
      A : Any := Prepare_Any_From_Agregate_Tc (Tc_A_Array);
   begin
      for I in 0 .. 9 loop
         Add_Agregate_Any_Member (A, To_Any (From (I)));
      end loop;
      return A;
   end To_Any;

   function From_Any (From : in Any) return A_Array is
      X : A_Array;
   begin
      --  no typecode checking (I trust my test case)
      for I in 0 .. 9 loop
         X (I) :=
           From_Any (Get_Any_Agregate_Member (From,
                                              TypeCode.TC_Long,
                                              CORBA.Long (I)));
      end loop;
      return X;
   end From_Any;

   function TC_Simple_Struct return TypeCode.Object is
      Tc : TypeCode.Object;
      S : CORBA.String;
      Name,
        Mb1_Name, Mb1_Tc,
        Mb2_Name, Mb2_Tc : Any;
  begin
     TypeCode.Set (Tc, Tk_Struct);  --  set the kind
     S := To_CORBA_String ("simple_struct");  --  1st param of typecode
     Name := To_Any (S);                      --  is the name of the structure
     S := To_CORBA_String ("A");  --  2nd parameter is the name of 1st member
     Mb1_Name := To_Any (S);
     Mb1_Tc   := TypeCode.To_Any (TC_A_Array); --  3rd param : tc of 1st mb
     S := To_CORBA_String ("B");  --  4th param is the name of 2nd member
     Mb2_Name := To_Any (S);
     Mb2_Tc   :=
       TypeCode.To_Any (TypeCode.TC_Long); --  5th param is the tc of 2nd mb
     TypeCode.Add_Parameter (Tc, Name);  -- build parameters list
     TypeCode.Add_Parameter (Tc, Mb1_Name);
     TypeCode.Add_Parameter (Tc, Mb1_Tc);
     TypeCode.Add_Parameter (Tc, Mb2_Name);
     TypeCode.Add_Parameter (Tc, Mb2_Tc);
      return Tc;
   end TC_Simple_Struct;

   function To_Any (From : in Simple_Struct) return Any is
      A, Mb1_Any, Mb2_Any : Any;
   begin
      --  build the any with this Tc typeCode
      A := Prepare_Any_From_Agregate_Tc
        (TC_Simple_Struct);  -- this is a complex type
      Mb1_Any := To_Any (From.A);  --  the 1st mb of the struct
      Mb2_Any := To_Any (From.B);  --  the 2nd one
      Add_Agregate_Any_Member (A, Mb1_Any);  --  store them in the any
      Add_Agregate_Any_Member (A, Mb2_Any);
      --  done !
      return A;
   end To_Any;

   function From_Any (From : in Any) return Simple_Struct is
      X : Simple_Struct;
      Mb_A : Any :=
        Get_Any_Agregate_Member (From, TC_A_Array, 0);
   begin
      --  I don't do any checking on typecodes (too lazy)
      X.A := From_Any (Mb_A);
      X.B :=
        From_Any (Get_Any_Agregate_Member (From, TypeCode.TC_Long, 1));
      return X;
   end From_Any;

   -------------
   --  Color  --
   -------------

   function TC_Color return TypeCode.Object is
      Tc : TypeCode.Object;
      S : CORBA.String;
   begin
      TypeCode.Set (Tc, Tk_Enum);
      S := To_CORBA_String ("Color");
      TypeCode.Add_Parameter (Tc,
                              To_Any (S));  --  1st param is the name of enum
      S := To_CORBA_String ("Red");
      TypeCode.Add_Parameter (Tc,
                              To_Any (S));  --  then add names of enum tokens
      S := To_CORBA_String ("Green");
      TypeCode.Add_Parameter (Tc, To_Any (S));
      S := To_CORBA_String ("Blue");
      TypeCode.Add_Parameter (Tc, To_Any (S));
      return Tc;
   end TC_Color;

   function To_Any (From : in Color) return Any is
      A : Any;
      L : CORBA.Unsigned_Long;
   begin
      --  set the value
      L := CORBA.Unsigned_Long (Color'Pos (From));
      A := To_Any (L);
      Force_Any_TypeCode (A, TC_Color);
      return A;
   end To_Any;

   function From_Any (From : in Any) return Color is
      X : Color;
      Index : CORBA.Unsigned_Long := From_Any (From);
   begin
      X := Color'Val(Index);
      return X;
   end From_Any;


   ------------
   --  Line  --
   ------------

   function TC_Line return TypeCode.Object is
      Tc : TypeCode.Object;
      P1, P2 : Any;
   begin
      Typecode.Set (Tc, Tk_Array);
      P1 := TypeCode.To_Any (TC_Example);  -- 1st param is type of elements
      P2 := To_Any (CORBA.Unsigned_Long (3));  -- 2nd is nb of elts
      TypeCode.Add_Parameter (Tc, P1);
      TypeCode.Add_Parameter (Tc, P2);
      return Tc;
   end TC_Line;

   function To_Any (From : in Line) return Any is
      A : Any := Prepare_Any_From_Agregate_Tc (TC_Line);
   begin
      for I in 0 .. 2 loop
         Add_Agregate_Any_Member (A, To_Any (From (I)));
      end loop;
      return A;
   end To_Any;

   function From_Any (From : in Any) return Line is
      X : Line;
   begin
      for I in 0 .. 2 loop
         X (I) :=
           From_Any (Get_Any_Agregate_Member (From,
                                              TC_Example,
                                              CORBA.Long (I)));
      end loop;
      return X;
   end From_Any;


   --------------
   --  Square  --
   --------------

   function TC_Square return TypeCode.Object is
      Tc : TypeCode.Object;
      P1, P2 : Any;
   begin
      Typecode.Set (Tc, Tk_Array);
      P1 :=
        TypeCode.To_Any (TC_Simple_Struct);  -- 1st param is type of elements
      P2 := To_Any (CORBA.Unsigned_Long (2 * 2));  -- 2nd is nb of elts
      TypeCode.Add_Parameter (Tc, P1);
         TypeCode.Add_Parameter (Tc, P2);
      return Tc;
   end TC_Square;

   function To_Any (From : in Square) return Any is
      A : Any := Prepare_Any_From_Agregate_Tc (TC_Square);
   begin
      for I in 0  .. 1 loop
         for J in 0 .. 1 loop
            Add_Agregate_Any_Member (A, To_Any (From (I, J)));
         end loop;
      end loop;
      return A;
   end To_Any;

   function From_Any (From : in Any) return Square is
      X : Square;
      Cpt : CORBA.Long := 0;
   begin
      for I in 0 .. 1 loop
         for J in 0 .. 1 loop
            X (I, J) :=
              From_Any (Get_Any_Agregate_Member (From,
                                                 TC_Example,
                                                 CORBA.Long (Cpt)));
            Cpt := Cpt + 1;
         end loop;
      end loop;
      return X;
   end From_Any;


   ------------
   --  Cube  --
   ------------

   function TC_Cube return TypeCode.Object is
      Tc : TypeCode.Object;
      P1, P2 : Any;
   begin
      Typecode.Set (Tc, Tk_Array);
      P1 := TypeCode.To_Any (TC_String);  -- 1st param is type of elements
      P2 := To_Any (CORBA.Unsigned_Long (2 * 2 * 2));  -- 2nd is nb of elts
      TypeCode.Add_Parameter (Tc, P1);
      TypeCode.Add_Parameter (Tc, P2);
      return Tc;
   end TC_Cube;

   function To_Any (From : in Cube) return Any is
     A : Any :=  Prepare_Any_From_Agregate_Tc (TC_Cube);
   begin
      for I in 0  .. 1 loop
         for J in 0 .. 1 loop
            for K in 0 .. 1 loop
               Add_Agregate_Any_Member (A, To_Any (From (I, J, K)));
            end loop;
         end loop;
      end loop;
      return A;
   end To_Any;

   function From_Any (From : in Any) return Cube is
      X : Cube;
      Cpt : CORBA.Long := 0;
   begin
      for I in 0 .. 1 loop
         for J in 0 .. 1 loop
            for K in 0 .. 1 loop
               X (I, J, K) :=
                 From_Any (Get_Any_Agregate_Member (From,
                                                    TC_String,
                                                    CORBA.Long (Cpt)));
               Cpt := Cpt + 1;
            end loop;
         end loop;
      end loop;
      return X;
   end From_Any;

end Helper;

