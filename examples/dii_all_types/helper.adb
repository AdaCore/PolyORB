with Helper;
with CORBA; use CORBA;

package body Helper is

   --  Example  --

   function To_Any (From : in Example) return Any is
      A : Any;
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_Union);
      case From.Switch is
         when 1 =>
            TypeCode.Add_Parameter (Tc, To_Any (From.Counter));
         when 2 =>
            TypeCode.Add_Parameter (Tc, To_Any (From.Flags));
         when others =>
            TypeCode.Add_Parameter (Tc, To_Any (From.Unknown));
      end case;
      TypeCode.Add_Parameter (Tc, To_Any (From.Switch));
      SetAny (A, Tc);
      return A;
   end To_Any;

   function From_Any (From : in Any) return Example is
      Tc_Res : TypeCode.Object := Get_Type (From);
      Av_Res : Any;
      Sw : CORBA.Long;
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Union) then
         raise Bad_Typecode;
      end if;
      Sw := From_Any (TypeCode.Parameter (Tc_Res, 0));
      declare
         X : Example (Sw);
      begin
         case Sw is
            when 1 =>
               X.Counter := From_Any (TypeCode.Parameter (Tc_Res, 1));
            when 2 =>
               X.Flags := From_Any (TypeCode.Parameter (Tc_Res, 1));
            when others =>
               X.Unknown := From_Any (TypeCode.Parameter (Tc_Res, 1));
         end case;
         return X;
      end;
   end From_Any;


   --  Simple_Struct  --

   function To_Any (From : in Simple_Struct) return Any is      
      The_Any : Any;
      Tc, Tc_A : TypeCode.Object;
      Mb_A : Any;
   begin
      --     A member
      TypeCode.Set (Tc_A, Tk_Array);
      for I in reverse 0 .. 9 loop
         TypeCode.Add_Parameter (Tc_A, To_Any (From.A (I)));
      end loop;
      SetAny (Mb_A, Tc_A);
      --     full arg
      TypeCode.Set (Tc, Tk_Struct);
      TypeCode.Add_Parameter (Tc, To_Any (From.B));
      TypeCode.Add_Parameter (Tc, Mb_A);
      SetAny (The_Any, Tc);
      return The_Any;
   end To_Any;
   
   function From_Any (From : in Any) return Simple_Struct is
      X : Simple_Struct; 
      Tc_Res : TypeCode.Object := Get_Type (From);
      Any_A : Any := TypeCode.Parameter (Tc_Res, 0);
      Tc_Res_A : TypeCode.Object := Get_Type (Any_A);
      Any_B : Any := TypeCode.Parameter (Tc_Res, 1);
   begin
      for I in 0 .. 9 loop
	 X.A (I) := From_Any (TypeCode.Parameter (Tc_Res_A,
						 CORBA.Long (I)));
      end loop;
      X.B := From_Any (Any_B);
      return X;
   end From_Any;


   --  Color  --

   function To_Any (From : in Color) return Any is
      A : Any;
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_Enum);
      TypeCode.Add_Parameter 
	(Tc, To_Any (Unsigned_Long (Color'Pos (From))));
      SetAny (A, Tc);
      return A;
   end To_Any;

   function From_Any (From : in Any) return Color is
      X : Color;
      Tc_Res : TypeCode.Object := Get_Type (From);
      Index : Unsigned_Long;
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Enum) then
         raise Bad_Typecode;
      end if;
      Index := From_Any (TypeCode.Parameter (Tc_Res, 0));
      X := Color'Val (Index);
      return X;
   end From_Any;


   --  Line  --

   function To_Any (From : in Line) return Any is
      A : Any;
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_Array);
      for I in reverse 0 .. 2 loop
	 TypeCode.Add_Parameter (Tc, To_Any (From (I)));
      end loop;
      SetAny (A, Tc);
      return A;
   end To_Any;
   
   function From_Any (From : in Any) return Line is
      X : Line;
      Tc_Res : TypeCode.Object := Get_Type (From);
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Array) then
         raise Bad_Typecode;
      end if;
      for I in 0 .. 2 loop
	 X (I) := From_Any (TypeCode.Parameter (Tc_Res, CORBA.Long (I)));
      end loop;
      return X;
   end From_Any;
   

   --  Square  --

   function To_Any (From : in Square) return Any is
      A : Any;
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_Array);
      for I in reverse 0 .. 1 loop
	 for J in reverse 0 .. 1 loop
	    TypeCode.Add_Parameter (Tc, To_Any (From (I, J)));
	 end loop;
      end loop;
      SetAny (A, Tc);
      return A;
   end To_Any;
   
   function From_Any (From : in Any) return Square is
      X : Square;
      C : CORBA.Long;
      Tc_Res : TypeCode.Object := Get_Type (From);
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Array) then
         raise Bad_Typecode;
      end if;
      C := 0;
      for I in 0 .. 1 loop
	 for J in 0 ..1 loop
	    X (I, J) := From_Any (TypeCode.Parameter (Tc_Res, C));
	    C := C + 1;
	 end loop;
      end loop;
      return X;
   end From_Any;

   --  Cube  --

   function To_Any (From : in Cube) return Any is
      A : Any;
      Tc : TypeCode.Object;
   begin
      TypeCode.Set (Tc, Tk_Array);
      for I in reverse 0 .. 1 loop
	 for J in reverse 0 .. 1 loop
	    for K in reverse 0 .. 1 loop
	       TypeCode.Add_Parameter (Tc, To_Any (From (I, J, K)));
	    end loop;
	 end loop;
      end loop;
      SetAny (A, Tc);
      return A;
   end To_Any;
   
   function From_Any (From : in Any) return Cube is
      X : Cube;
      C : CORBA.Long;
      Tc_Res : TypeCode.Object := Get_Type (From);
   begin
      if (TypeCode.Kind (Tc_Res) /= Tk_Array) then
         raise Bad_Typecode;
      end if;
      C := 0;
      for I in 0 .. 1 loop
	 for J in 0 ..1 loop
	    for K in 0 ..1 loop
	       X (I, J, K) := From_Any (TypeCode.Parameter (Tc_Res, C));
	       C := C + 1;
	    end loop;
	 end loop;
      end loop;
      return X;
   end From_Any;

end Helper;





