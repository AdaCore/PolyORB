----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba.Object ;
with Chicken_Forward ;
with Egg_Forward ;

package Egg is

   type Ref is new Corba.Object.Ref with null record ;
   function Hatch( Self : in Ref) return Chicken_Forward.Ref ;
   package Convert is new Egg_Forward.Convert(Ref) ;

end Egg ;


