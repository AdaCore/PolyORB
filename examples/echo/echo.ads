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

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package Echo is

   Debug : constant Boolean := Adabroker_Debug.Is_Active("echo") ;

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;
   type Ref_Ptr is access all Ref ;

   Nil_Ref : aliased constant Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;


   function EchoString(Self: in Ref ;
                       Message: in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------
   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:Echo:1.0")  ;
   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function Is_A(The_Ref: in Ref; Repo_Id: in Corba.String) return Corba.Boolean ;
   function Is_A(Repo_Id: in Corba.String) return Corba.Boolean ;

   function Get_Nil_Ref(Self: in Ref) return Ref ;

private

   Nil_Ref : aliased constant Ref := (Corba.Object.Nil_Ref with null record) ;

End Echo;




