-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package MemBufferedStream                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with BufferedStream ;

package MemBufferedStream is

   type Object is abstract new BufferedStream.Object with null record;


private



end MemBufferedStream ;

