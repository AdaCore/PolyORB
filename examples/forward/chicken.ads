----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------



with Ada.Unchecked_Deallocation ;
with Corba.Object ;
with Egg_Forward ;
with Chicken_Forward ;

package Chicken is

   type Ref is new Corba.Object.Ref with null record ;
   type Ref_Ptr is access all Ref'Class ;
   type Most_Derived_Ref_Ptr is access Ref ;


   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   Nil_Ref : aliased constant Ref ;

   function Lay( Self : in Ref) return Egg_Forward.Ref;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------
   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:Chicken:1.0")  ;
   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function Is_A(The_Ref: in Ref; Repo_Id: in Corba.String) return Corba.Boolean ;
   function Is_A(Repo_Id: in Corba.String) return Corba.Boolean ;

   function Get_Nil_Ref(Self: in Ref) return Ref ;

   -----------------------------------------------
   package Convert_Forward is new Chicken_Forward.Convert(Ref) ;
   procedure Free is new Ada.Unchecked_Deallocation(Ref, Most_Derived_Ref_Ptr) ;

private

   Nil_Ref : aliased constant Ref := (Corba.Object.Nil_Ref with null record) ;

end Chicken ;
