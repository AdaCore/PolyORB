with Corba.Object ;
with NetbufferedStream ;
with MembufferedStream ;
use Corba.Object ;
use NetbufferedStream ;
use MembufferedStream ;
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


   procedure Marshall (A : in longueur_Array ;
                       S : in out Netbufferedstream.Object'Class) is
   begin
      for I1 in A'range(1) loop 
         Marshall (A(I1), S) ; 
      end loop ;
   end Marshall ;

   procedure UnMarshall (A : out longueur_Array ;
                         S : in out Netbufferedstream.Object'Class) is
   begin
      for I1 in A'range(1) loop 
         UnMarshall (A(I1), S) ; 
      end loop ;
   end UnMarshall ;

   function Align_Size (A : in longueur_Array ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_long := Initial_Offset ;
   begin
      Tmp := Align_Size (A(1), Initial_Offset, N * 3) ;
      return Tmp ;
   end Align_Size ;


   procedure Marshall(A : in dist ;
                      S : in out Netbufferedstream.Object'Class) is
   begin
      Marshall(A.longueur,S) ;
      Marshall(A.largeur,S) ;
   end Marshall;

   procedure UnMarshall(A : out dist ;
                        S : in out Netbufferedstream.Object'Class) is
   begin
      UnMarshall(A.longueur,S) ;
      UnMarshall(A.largeur,S) ;
   end Unmarshall;

   function Align_Size (A : in dist ;
                        Initial_Offset : in Corba.Unsigned_Long ;
                        N : in Corba.Unsigned_Long := 1)
                        return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long := Initial_Offset ;
   begin
      for I in 1..N loop
         Tmp := Align_Size(A.longueur, Tmp) ;
         Tmp := Align_Size(A.largeur, Tmp) ;
      end loop ;
      return Tmp ;
   end Align_Size;


end weapon.Marshal ;
