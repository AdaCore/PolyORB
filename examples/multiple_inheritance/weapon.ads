----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Weapon object                                          ----
----                                                                    ----
----                package Weapon                                      ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba.Object ;


package Weapon is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;
   type Ref_Ptr is access all Ref ;

   Nil_Ref : aliased constant Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;


   -------------------------------------------------
   ----         IDL  definitions                ----
   -------------------------------------------------
   type Name is new Corba.String ;

   procedure Shoot(Self : in Ref) ;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:Weapon:1.0")  ;
   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function Is_A(The_Ref: in Ref; Repo_Id: in Corba.String) return Corba.Boolean ;
   function Is_A(Repo_Id: in Corba.String) return Corba.Boolean ;

   function Get_Nil_Ref(Self: in Ref) return Ref ;

private

   Nil_Ref : aliased constant Ref := (Corba.Object.Nil_Ref with null record) ;

End Weapon;


