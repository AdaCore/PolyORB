----                                                                    ----
----     This in an example which is hand-written                       ----
----                                                                    ----
----             procedure client                                       ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Ada.Command_Line ;
with Corba.Object ; use Corba.Object ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Weapon ;
with Vehicle ;
with Tank ;

with Text_IO ; use Text_IO ;


procedure Client is

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   T : Tank.Ref ;

   Ior : Corba.String ;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;
   Corba.Orb.String_To_Object(IOR, T) ;

   Tank.Shoot(T) ;

end Client ;



