----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA.ORB;
with CORBA.NVList;
with CORBA.Request;
with CORBA.Context;
with CORBA.Object;
with PolyORB.CORBA_P.Exceptions;
with CORBA; use CORBA;
pragma Elaborate_All (CORBA);

package body Echo is

   function echoString
     (Self : Ref;
      Mesg : in CORBA.String)
     return CORBA.String
   is

      Operation_Name_Ü : constant CORBA.Identifier
        := CORBA.To_CORBA_String ("echoString");
      Self_Ref_Ü      : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Arg_Name_Ü_Mesg : CORBA.Identifier := To_CORBA_String ("Mesg");
      Request_Ü       : CORBA.Request.Object;
      Ctx_Ü           : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Argument_Ü_Mesg : CORBA.Any := CORBA.To_Any (Mesg);
      Arg_List_Ü      : CORBA.NVList.Ref;
      Result_Ü        : CORBA.NamedValue;
      Result_Name_Ü   : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         PolyORB.CORBA_P.Exceptions.Raise_Inv_Objref;
      end if;

      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List_Ü);

      --  Create argument list

      CORBA.NVList.Add_Item (Arg_List_Ü,
                             Arg_Name_Ü_Mesg,
                             Argument_Ü_Mesg,
                             CORBA.ARG_IN);
      --  setting the result type
      Result_Ü := (Name => Identifier (Result_Name_Ü),
            Argument => Get_Empty_Any (CORBA.TC_String),
            Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Self_Ref_Ü,
                                   Ctx_Ü,
                                   Operation_Name_Ü,
                                   Arg_List_Ü,
                                   Result_Ü,
                                   Request_Ü,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request_Ü, 0);

      --  Unmarshall return value.
      return CORBA.From_Any (Result_Ü.Argument);
   end echoString;

end Echo;
