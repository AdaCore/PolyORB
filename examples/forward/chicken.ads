----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------




with Corba.Object ;
with Egg_Forward ;
with Chicken_Forward ;

package Chicken is

   type Ref is new Corba.Object.Ref with null record ;

   function Lay( Self : in Ref) return Egg_Forward.Ref;

   package Convert is new Chicken_Forward.Convert(Ref) ;

end Chicken ;
