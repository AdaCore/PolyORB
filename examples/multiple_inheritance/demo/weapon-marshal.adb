with Corba.Object ;
use Corba.Object ;
package body weapon.Marshal is

   procedure Marshall (A : in name ;
                       S : in out Netbufferedstream.Object'Class) is
   begin
      Marshall (Corba.String(A), S) ;
   end Marshall ;

   procedure UnMarshall (A : out name ;
                         S : in out Netbufferedstream.Object'Class) is 

   begin
      UnMarshall (Corba.String(A), S) ;
   end UnMarshall ;

   function Align_Size (A : in name ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is 
   begin
      return Align_Size (Corba.String(A), Initial_Offset, N) ;
   end Align_Size ;


end weapon.Marshal ;
