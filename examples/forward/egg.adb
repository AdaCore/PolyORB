----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------


package body Egg is

   function Hatch(Self : in Ref) return Chicken_Forward.Ref is
      Result : Chicken_Forward.Ref ;
   begin
      return Result ;
   end ;

end Egg ;
