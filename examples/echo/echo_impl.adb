----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_impl                                   ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------


package Echo.impl body is


   procedure Init (Self : in out Object, K : in OmniORB.ObjectKey) is
      L : OmniRopeAndKey.Object;
   begin
      Init(L,Rope.Null_Rope,K,...);
      -- PROBLEME sur K : le type n'est pas le bon. En C, on trouve ici
      -- un cast plus que sauvage...
      .... To Be continued
   end


private


End Echo_impl ;



