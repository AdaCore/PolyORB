with CORBA;
with CORBA.Object;
with CORBA.Context;

package Invoker is

   procedure Run (Op_Name : in String;
                  Server : CORBA.Object.Ref;
                  Ctx     : in CORBA.Context.Object;
                  X : in CORBA.Any; Y : in out CORBA.Any);

end Invoker;
