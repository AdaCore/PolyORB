----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_clt.cc in omniORB      ----
----                                                                    ----
----                package body echo                                   ----
----                                                                    ----
----------------------------------------------------------------------------


package body Echo is


   function To_Ref(From: in Corba.Object.Ref'Class) return Ref is
   begin
      -- cf to_ref.txt
   end ;

   function EchoString(Self: in Ref, message: in Corba.String) return Corba.String is

      Opcd : OmniProxyCallDesc_Echo ;

   begin
      OmniProxyCallDesc.Init(Opcd, "echoString", Message) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return Result(Opcd) ;
   end ;

End Echo ;
