-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package giop_c                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Giop_C is

   type Object is limited private ;

   procedure Put_Char_Array (Self : in out Object'class
                               ...
                            );
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b, int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154


private



end Giop_C ;

