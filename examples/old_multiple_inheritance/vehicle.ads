----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package Vehicle                                     ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba, Corba.Object ;

package Vehicle is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record;
   type Ref_Ptr is access all Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   -- procedure Set_Mark (Self: in Ref ;
   --                    To: in Corba.String) ;

   -- function Get_Mark (Self: in Ref) return Corba.String ;

   -- procedure Drive (Self: in Ref; Vehicle_Mark: in Corba.String) ;



   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;



End Vehicle ;




