-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package rope                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Rope is

  type Object is limited private ;

  function Null_Rope return Object ;

private

   type Object is null record ;

   Null_Rope_Internal : Object ;

end Rope ;
