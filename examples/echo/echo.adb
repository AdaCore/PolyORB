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

with BufferedStream ;
with Omniproxycallwrapper ;
with Omni, Omniorb, omniropeandkey ;
with Giop_S ;

with Echo.Proxies ;

package body Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- EchoString
   -------------
   function EchoString(Self: in Ref ;
                       Message: in Corba.String)
                       return Corba.String is

      Opcd : Echo.Proxies.EchoString_Proxy :=
        Echo.Proxies.Create("echoString", Message) ;
   begin
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return Echo.Proxies.Result(Opcd) ;
   end ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) is
      Tricky_Result : Corba.Object.Ref'Class := Real_Object ;
   begin
      if Result in Ref then
         Result := Tricky_Result ;
      else
         raise Constraint_Error ;
      end if;
   end ;


   --------------------------------------------------
   ----        server-side subprograms           ----
   --------------------------------------------------
   -- These subprograms are only used on the server side
   -- the are here because they must not be in echo-impl.adb
   -- otherwise the user would be bothered with code he does
   -- not need to see.
   -- All these subprograms will be inherited by the Echo.Impl.Object
   -- which inherits Echo.Ref

   -- Init
   -------
   procedure Adabroker_Init (Self : in out Ref ;
                             K : in OmniORB.ObjectKey) is
      L : OmniRopeAndKey.Object;
   begin
      raise Constraint_Error ;
      -- Init(L,Rope.Null_Rope,K,...);
      -- PROBLEME sur K : le type n'est pas le bon. En C, on trouve ici
      -- un cast plus que sauvage...
      -- To Be continued
   end;

   -- Dipatch
   ----------
   function AdaBroker_Dispatch (Self : in Ref ;
                                Orls : in Giop_S.Object ;
                                Orl_Op : in Corba.String ;
                                Orl_Response_Expected : Corba.Boolean)
                                return Corba.Boolean is
      Operation_Name : Standard.String := Corba.To_Standard_String(Orl_Op) ;
   begin
      if Operation_Name = "echoString" then
         declare
            Mesg : Corba.String ;
            Result : Corba.String ;
         begin
            -- unmarshaling the arguments
            Mesg := Giop_S.Unmarshal(Orls) ;

            -- change state
            Giop_S.Request_Received(Orls) ;

            -- call the implementation
            Result := EchoString(Self, Mesg) ;

            -- marshaling the result
            -- to be completed

            -- exiting, all is ok
            return True ;
         end;
      end if ;

      return False ;

   end ;


End Echo ;




