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
with Omni ;

package BufferedStream is

   type Object is abstract tagged limited private ;


   --------------------------------------------------------
   ----   marshalling objects into buffered streams    ----
   --------------------------------------------------------

   procedure Marshall (Len : in CORBA.Unsigned_Long ;
                       Giop_Client : in out Object'Class);
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects
   -- corresponds to operator>>=
   -- in bufferedStream.h L 203

   procedure Marshall (obj : in CORBA.Char ;
                       Giop_Client : in out Object'Class) ;




   --------------------------------------------------------
   ---- unmarshalling objects from buffered streams    ----
   --------------------------------------------------------

   function UnMarshall (Giop_Client : in Object'Class)
                        return CORBA.Char ;
   -- marshalling Objects into BufferedStream
   -- has to be defined for all CORBA Objects


   -------------------------------------------------------
   ----           others                              ----
   -------------------------------------------------------

   procedure Put_Char_Array (Self: in Object ;
                               B: in Corba.String ;
                               Size: in Integer ;
                               Align: in Omni.Alignment_T := Omni.ALIGN_1 ;
                               StartMTU: in Corba.Boolean := False ;
                               At_Most_One: in Corba.Boolean := False ) ;
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b,
   --                             int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154

   procedure Get_Char_Array (Self : in Object ;
                               B : in Corba.String ;
                               Size : in Integer ;
                               Align : in Omni.Alignment_T := Omni.ALIGN_1 ;
                               StartMTU : in Corba.Boolean := False) ;
   -- wrapper around void NetBufferedStream::get_char_array(CORBA::Char* b,
   --                                               int size,
   --                                               omni::alignment_t align,
   --                                               CORBA::Boolean startMTU)
   -- in nbufferedStream.cc L 121

   private

   type Object is abstract tagged limited null record ;

end BufferedStream ;







