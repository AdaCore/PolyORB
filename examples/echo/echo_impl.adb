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


package body Echo.impl is


   procedure Init (Self : in out Object; K : in OmniORB.ObjectKey) is
      L : OmniRopeAndKey.Object;
   begin
      -- Init(L,Rope.Null_Rope,K,...);
      -- PROBLEME sur K : le type n'est pas le bon. En C, on trouve ici
      -- un cast plus que sauvage...
      -- To Be continued
   end;

   function Dispatch (Self : in Object;
                      Orls : in out Giop_S;
                      Orl_Op : in Corba.String;
                      Orl_Response_Expected : Corba.Boolean)
                      return Corba.Boolean is
   begin
      case Orl_Op is
         when "echoString" =>
            begin

            end;
         when others =>
            begin
            end;
      end case;
   end


End Echo.Impl ;



