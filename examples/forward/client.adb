----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------


with Chicken, Chicken_Forward ;
with Egg, Egg_Forward ;

procedure Client is
   My_Egg : Egg.Ref ;
   My_Chicken : Chicken.Ref ;
begin
   My_Chicken := Chicken.Convert.From_Forward(Egg.Hatch(My_Egg)) ;
   My_Egg := Egg.Convert.From_Forward(Chicken.Lay(My_Chicken)) ;
end ;
