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
   Mystruct   : Object.Ref;
   Ctx      : CORBA.Context.Object;
   Nvl      : CORBA.NVList.Object := CORBA.NVList.Null_Object;
   Result   : CORBA.NamedValue;
   Returns  : CORBA.Status;
   Rq       : CORBA.Request.Object;
   Argument : CORBA.NamedValue;
   Name, Mb1_Name, Mb1_Tc, Mb2_Name, Mb2_Tc, Arg_Any, Res_Any,
     Mb1_Any, Mb2_Any : Any;
   Tc, Tc1, Tc2 : TypeCode.Object;
   S : CORBA.String;
   L : CORBA.Long;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:Mystruct:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, Mystruct);

   if Object.Is_Nil (Mystruct) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;




   Ada.Text_IO.Put_Line ("Client building request");


   --  build the type code for simple struct (since we don't have an IR)
   TypeCode.Set (Tc, Tk_Struct);  --  set the kind
   S := To_CORBA_String ("simple_struct");
   Name := To_Any (S);
   S := To_CORBA_String ("a");
   Mb1_Name := To_Any (S);
   TypeCode.Set (Tc1, Tk_String);
   Mb1_Tc   := TypeCode.To_Any (Tc1);
   S := To_CORBA_String ("b");
   Mb2_Name := To_Any (S);
   TypeCode.Set  (Tc2, Tk_Long);
   Mb2_Tc   := TypeCode.To_Any (Tc2);

   TypeCode.Add_Parameter (Tc, Name);
   TypeCode.Add_Parameter (Tc, Mb1_Name);
   TypeCode.Add_Parameter (Tc, Mb1_Tc);
   TypeCode.Add_Parameter (Tc, Mb2_Name);
   TypeCode.Add_Parameter (Tc, Mb2_Tc);

   --  build the any argument with this type code
   Arg_Any := Prepare_Any_From_Agregate_Tc (Tc);
   S := To_CORBA_String ("hello string");
   Mb1_Any := To_Any (S);
   Mb2_Any := To_Any (CORBA.Long (2706));
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
   Object.Create_Request (Mystruct,
                          Ctx,
                          CORBA.To_CORBA_String ("echoStruct"),
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
   Mb1_Any := Get_Any_Agregate_Member (Res_Any, Tc1, 0);
   S := From_Any (Mb1_Any);
   Mb2_Any := Get_Any_Agregate_Member (Res_Any, Tc2, 1);
   L := From_Any (Mb2_Any);

   --  display results
   Ada.Text_IO.Put_Line (">> struct = "
                         & "hello string ; 2706");
   Ada.Text_IO.Put_Line ("<< struct = "
                         & To_Standard_String (S) & " ; "
                         & L'Img);

end Dii_Client;
