----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------


package body Chicken is

   function Lay(Self : in Ref) return Egg_Forward.Ref is
      Result : Egg_Forward.Ref ;
   begin
      return Result ;
   end ;

end Chicken ;
