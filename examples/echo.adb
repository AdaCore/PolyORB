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

      Operation_Name_� : constant CORBA.Identifier
        := CORBA.To_CORBA_String ("echoString");
      Self_Ref_�      : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Arg_Name_�_Mesg : CORBA.Identifier := To_CORBA_String ("Mesg");
      Request_�       : CORBA.Request.Object;
      Ctx_�           : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Argument_�_Mesg : CORBA.Any := CORBA.To_Any (Mesg);
      Arg_List_�      : CORBA.NVList.Ref;
      Result_�        : CORBA.NamedValue;
      Result_Name_�   : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         PolyORB.CORBA_P.Exceptions.Raise_Inv_Objref;
      end if;

      --  creating the argument list
      CORBA.ORB.Create_List (0, Arg_List_�);

      --  Create argument list

      CORBA.NVList.Add_Item (Arg_List_�,
                             Arg_Name_�_Mesg,
                             Argument_�_Mesg,
                             CORBA.ARG_IN);
      --  setting the result type
      Result_� := (Name => Identifier (Result_Name_�),
            Argument => Get_Empty_Any (CORBA.TC_String),
            Arg_Modes => 0);
      --  creating a request
      CORBA.Object.Create_Request (Self_Ref_�,
                                   Ctx_�,
                                   Operation_Name_�,
                                   Arg_List_�,
                                   Result_�,
                                   Request_�,
                                   0);
      --  sending message
      CORBA.Request.Invoke (Request_�, 0);

      --  Unmarshall return value.
      return CORBA.From_Any (Result_�.Argument);
   end echoString;

end Echo;
