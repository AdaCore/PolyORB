-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package BufferedStream                       ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba ;

package BufferedStream is

   type Object is abstract tagged limited private ;


   procedure Marshall (Len : in CORBA.Unsigned_Long ;
                       Giop_Client : in out Object'Class) is abstract;
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects
   -- corresponds to operator>>=
   -- in bufferedStream.h L 203

   procedure Marshall (obj : in CORBA.Char ;
                       Giop_Client : in out Object'Class) is abstract;




   function UnMarshall (Giop_Client : in Object'Class)
                        return CORBA.Char is abstract;
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects


private

   type Object is abstract tagged limited null record ;

end BufferedStream ;

