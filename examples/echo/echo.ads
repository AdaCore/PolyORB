----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo                                        ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba, Corba.Object ;

package Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;


   function EchoString(Self: in Ref ;
                       Message: in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;


   procedure Assert_Ref_Not_Nil(Self : in Echo.Ref) ;

   Interface_Repository_Id : constant String := "IDL:Echo:1.0" ;

private

   Nil_Ref : constant Ref := (Corba.Object.Nil_Ref with null record) ;

End Echo ;




