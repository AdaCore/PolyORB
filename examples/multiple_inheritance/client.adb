----                                                                    ----
----     This in an example which is hand-written                       ----
----                                                                    ----
----             procedure client                                       ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba.Object ; use Corba.Object ;
with Weapon ;
with Vehicle ;
with Tank ;

use Tank ;

with Text_IO ; use Text_IO ;


procedure Client is


   -----------------------------------------------
   ----        To test everything             ----
   -----------------------------------------------
   procedure Global_Test is
      W : Weapon.Ref ;
      V, vv : Vehicle.Ref ;
      T : Tank.Ref ;
      O : Corba.Object.Ref ;
   begin

      Put_Line(">>>>>>>> Test program for To_Ref <<<<<<<<<<<<") ;
      Put_Line("") ;

      Put_Line("--- Trying to cast Weapon.Ref into Corba.Object.Ref") ;
      O := To_Ref(W) ;
      Put_Line(">>> cast succeeded") ;
      Put_Line("") ;

      Put_Line("--- Trying to cast Tank.Ref into Corba.Object.Ref") ;
      O := To_Ref(T) ;
      Put_Line(">>> cast succeeded") ;
      Put_Line("") ;

      Put_Line("--- Trying to cast Tank.Ref into Vehicle.Ref") ;
      V := Vehicle.To_Ref(T) ;
      Put_Line(">>> cast succeeded") ;
      Put_Line("") ;

      Put_Line("--- Trying to cast Tank.Ref into Weapon.Ref") ;
      W := Weapon.To_Ref(T) ;
      Put_Line(">>> cast succeeded") ;
      Put_Line("") ;

      Put_Line("--- Trying to cast Vehicle.Ref (which is a Tank.Ref) into Weapon.Ref") ;
      W := Weapon.To_Ref(V) ;
      Put_Line(">>> cast succeeded") ;
      Put_Line("") ;

      begin
      Put_Line("--- Trying to cast Vehicle.Ref into Weapon.Ref") ;
      W := Weapon.To_Ref(Vv) ;
      Put_Line("**** cast succeeded : PROBLEM !!!!!") ;
      Put_Line("") ;
      exception
         when Constraint_Error =>
           Put_Line(">>> Cast did not succeed : ok !") ;
           Put_Line("") ;
           Put_Line("Everything works all right !!") ;
      end ;


   end Global_Test ;


begin
   -- Simple_Test ;
   Global_Test ;
end Client ;



