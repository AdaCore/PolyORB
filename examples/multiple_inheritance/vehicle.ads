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
   type Ref_Ptr is access all Ref'Class ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;



   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:Vehicle:1.0") ;
   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function Is_A(The_Ref: in Ref; Repo_Id: in Corba.String) return Corba.Boolean ;
   function Is_A(Repo_Id: in Corba.String) return Corba.Boolean ;

private

   procedure Initialize (Self: in out Ref);

   Nil_Ref : aliased Ref := (Corba.Object.Nil_Ref with null record) ;

End Vehicle ;




