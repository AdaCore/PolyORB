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


--with Interfaces.CPP ;

package MemBufferedStream is

   type Object is tagged null record;
--      Table : Interfaces.CPP.Vtable_Ptr ;
--   end record ;

--   pragma CPP_Class (Object);
--   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Ada_memBufferedStream
   -- (see Ada_memBufferedStream.hh)

end MemBufferedStream ;

