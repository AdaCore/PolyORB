----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_skeletons                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Omniropeandkey ;

package body Echo.Skeleton is


   -- Init
   -------
   procedure Adabroker_Init (Self : in out Echo.Impl.Object ;
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
   procedure AdaBroker_Dispatch (Self : in out Echo.Impl.Object ;
                                 Orls : in Giop_S.Object ;
                                 Orl_Op : in Corba.String ;
                                 Orl_Response_Expected : in Corba.Boolean ;
                                 Returns : out Corba.Boolean ) is

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
            Result := Echo.Impl.EchoString(Self'access, Mesg) ;

            -- marshaling the result
            -- to be completed

            -- exiting, all is ok
            Returns := True ;
            return ;
         end;
      end if ;

      Returns := False ;
      return ;

   end ;

end Echo.Skeleton ;
