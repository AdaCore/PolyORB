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



package BufferedStream is

   type Object is abstract tagged limited private ;


   procedure Marshall (CORBA.Unsigned_Long : in Len,
                         Object'Class : in Giop_Client) is abstract;
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects
   -- corresponds to operator>>=
   -- in bufferedStream.h L 203

   procedure Marshall (CORBA.Char : in Len,
                         Object'Class : in Giop_Client) is abstract;




   function UnMarshall (Object'Class : in Giop_Client)
                        return CORBA.Char is abstract;
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects


private



end BufferedStream ;

