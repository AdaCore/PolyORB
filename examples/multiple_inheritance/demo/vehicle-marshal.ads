with NetbufferedStream ;
with MembufferedStream ;
with Giop_C ;
with Corba ;
use NetbufferedStream ;
use MembufferedStream ;
use type Corba.Unsigned_Long; 
package vehicle.Marshal is

   procedure Marshall (A : in dist1 ;
                       S : in out Netbufferedstream.Object'Class) ;

   procedure UnMarshall (A : out dist1 ;
                         S : in out Netbufferedstream.Object'Class) ;

   function Align_Size (A : in dist1 ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long ;


   procedure Marshall (A : in model ;
                       S : in out Netbufferedstream.Object'Class) ;

   procedure UnMarshall (A : out model ;
                       S : in out Netbufferedstream.Object'Class) ;

   function Align_Size (A : in model ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long ;


end vehicle.Marshal ;
