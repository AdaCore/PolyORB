----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package body echo                                   ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----                                                                    ----
----------------------------------------------------------------------------

with Ada.Tags, Ada.exceptions ;
with Omniproxycallwrapper ;
with Echo.Proxies ;
with Corba.Object ; use Corba.Object ;

package body Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dynamic_Ref : Corba.Object.Ref'Class
        := Get_Dynamic_Ref(The_Ref) ;
      Result : Ref ;
   begin
      AdaBroker_Cast_To_Parent(Dynamic_Ref,Result) ;
      return Result ;
   end ;


   -- EchoString
   -------------
   function EchoString(Self: in Ref ;
                       Message: in Corba.String)
                       return Corba.String is

      Opcd : Echo.Proxies.EchoString_Proxy ;
      Result : Corba.String ;
   begin
      Assert_Ref_Not_Nil(Self) ;
      Opcd := Echo.Proxies.Create(Message) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      Result := Echo.Proxies.Get_Result(Opcd) ;
      Echo.Proxies.Free(Opcd) ;
      return Result ;
   end ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                      Result: out Corba.Object.Ref'Class) is
   begin
      -- I am the result !
      if Result in Ref then
         declare
            Tmp_Result : Corba.Object.Ref'Class := Real_Ref ;
         begin
            Result := Tmp_Result ;
            return ;
         end ;
      end if ;

      --try my first parent
      declare
         Tmp_Result : Corba.Object.Ref ;
      begin
         Tmp_Result := Corba.Object.Ref(Real_Ref) ;
         Corba.Object.AdaBroker_Cast_To_Parent(Tmp_Result, Result) ;
         return ;
      exception
         when Constraint_Error => null ;
      end ;

      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Echo.To_Ref :"
                                     & Corba.CRLF
                                     & "  Cannot cast Echo.Ref"
                                     & Corba.CRLF
                                     & "  into "
                                     & Ada.Tags.External_Tag(Result'tag)) ;
   end ;


End Echo ;




