with Corba.Object ;
use Corba.Object ;
package body vehicle.Marshal is

   procedure Marshall (A : in model ;
                       S : in out Netbufferedstream.Object'Class) is
   begin
      Marshall (Corba.String(A), S) ;
   end Marshall ;

   procedure UnMarshall (A : out model ;
                         S : in out Netbufferedstream.Object'Class) is 

   begin
      UnMarshall (Corba.String(A), S) ;
   end UnMarshall ;

   function Align_Size (A : in model ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is 
   begin
      return Align_Size (Corba.String(A), Initial_Offset, N) ;
   end Align_Size ;


end vehicle.Marshal ;
