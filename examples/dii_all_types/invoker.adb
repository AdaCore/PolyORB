with Invoker;
with CORBA;
with CORBA.Object;
with CORBA.Context;
with CORBA.NVList;
with CORBA.Request;

package body Invoker is

   procedure Run (Op_Name : in String;
                  Server  : in CORBA.Object.Ref;
                  Ctx     : in CORBA.Context.Object;
                  X : in CORBA.Any; Y : in out CORBA.Any)
   is
      Rq : CORBA.Request.Object;
      Argument, Result : CORBA. NamedValue;
      Returns : CORBA.Status;
   begin
      Argument := (CORBA.To_CORBA_String ("arg"), X, 0, CORBA.ARG_IN);
      Result   := (CORBA.To_CORBA_String ("res"), Y, 0, CORBA.ARG_OUT);
      CORBA.Object.Create_Request (Server,
                                   Ctx,
                                   CORBA.To_CORBA_String (Op_Name),
                                   CORBA.NVList.Null_Object,
                                   Result,
                                   Rq,
                                   0,
                                   Returns);
      CORBA.Request.Add_Arg (Rq, Argument);
      CORBA.Request.Invoke (Rq, 0);
      Result :=  CORBA.Request.Return_Value (Rq);
      Y := Result.Argument;
   end Run;

end Invoker;
