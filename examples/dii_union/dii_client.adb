with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object.OmniORB;

with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.Object;

procedure Dii_Client is
   Repository_Id : CORBA.String;
   IOR      : CORBA.String;
   Modifier : Object.Ref;
   Ctx      : CORBA.Context.Object;
   Nvl      : CORBA.NVList.Object := CORBA.NVList.Null_Object;
   Result   : CORBA.NamedValue;
   Returns  : CORBA.Status;
   Rq       : CORBA.Request.Object;
   Argument : CORBA.NamedValue;

   Name, Any_D_Tc,
     Any_Lab1, Mb1_Name, Mb1_Tc,
     Any_Lab2, Mb2_Name, Mb2_Tc,
     Any_Lab3, Mb3_Name, Mb3_Tc,

     Sw_Any, Val_Any,

     Arg_Any, Res_Any,
     Mb1_Any, Mb2_Any : Any;

   Tc : TypeCode.Object;
   S : CORBA.String;
   L, Sw : CORBA.Long;
   B : CORBA.Boolean;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:Modifier:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, Modifier);

   if Object.Is_Nil (Modifier) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;




   Ada.Text_IO.Put_Line ("Client building request");


   --  build the type code for simple struct (since we don't have an IR)
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
   S := To_CORBA_String ("Flag"); --  7th param (name member 2)
   Mb2_Name := To_Any (S);
   Mb2_Tc :=
     TypeCode.To_Any (TypeCode.TC_Boolean); -- 8th param (typecode member 2)

   Any_Lab3 := To_Any (CORBA.Octet (0)); --  9th param (label default)
   S := To_CORBA_String ("Unknown"); --  10th param (name member default)
   Mb3_Name := To_Any (S);
   Mb3_Tc :=
     TypeCode.To_Any (TypeCode.TC_Long);  -- 11th param (typecode member def)

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

   --  build the any argument with this type code
   Arg_Any := Prepare_Any_From_Agregate_Tc (Tc);
   Mb1_Any := To_Any (CORBA.Long (2));
   Mb2_Any := To_Any (CORBA.Boolean (False));
   Add_Agregate_Any_Member (Arg_Any, Mb1_Any);
   Add_Agregate_Any_Member (Arg_Any, Mb2_Any);

   --  build the named value argument
   Argument := (To_CORBA_String ("arg"),
                Arg_Any, 0, CORBA.ARG_IN);


   --  build the return value
   Res_Any :=  Prepare_Any_From_Agregate_Tc (Tc);
   Result  := (To_CORBA_String ("res"),
               Res_Any, 0, CORBA.ARG_OUT);


   --  build the request
   Object.Create_Request (Modifier,
                          Ctx,
                          CORBA.To_CORBA_String ("modify"),
                          Nvl,
                          Result,
                          Rq,
                          0,
                          Returns);


   Ada.Text_IO.Put_Line ("Client adding arguments");

   --  add argument
   CORBA.Request.Add_Arg (Rq, Argument);

   --  invoke request
   Ada.Text_IO.Put_Line ("Client invoking request");
   CORBA.Request.Invoke (Rq, 0);

   --  get result
   Ada.Text_IO.Put_Line ("Client extracting result");
   Result := CORBA.Request.Return_Value (Rq);

   --  interpret the named value
   Res_Any := Result.Argument;
   Sw_Any := Get_Any_Agregate_Member (Res_Any, TypeCode.TC_Long, 0);
   Sw := From_Any (Sw_Any);
   case Sw is
      when 1 =>
         Val_Any := Get_Any_Agregate_Member (Res_Any, TypeCode.TC_Long, 1);
         L :=  From_Any (Val_Any);
         Ada.Text_IO.Put_Line ("<< sw = 1, counter = " & L'Img);
      when 2 =>
         Val_Any := Get_Any_Agregate_Member (Res_Any, TypeCode.TC_Boolean, 1);
         B := From_Any (Val_Any);
         Ada.Text_IO.Put_Line ("<< sw = 2, flag = " & B'Img);
      when others =>
         Val_Any := Get_Any_Agregate_Member (Res_Any, TypeCode.TC_Long, 1);
         L :=  From_Any (Val_Any);
         Ada.Text_IO.Put_Line ("<< sw = others, unknown = " & L'Img);
   end case;


end Dii_Client;
