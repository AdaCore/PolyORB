with Corba.Object ;
with Corba ;
with AdaBroker ;
with Ada.Unchecked_Deallocation ;
package weapon is 

   -----------------------------
   --         The Spec        --
   -----------------------------

   type Ref is new Corba.Object.Ref with null record;
   type Ref_Ptr is access all Ref ;

   Nil_Ref : aliased constant Ref ;
   function To_Ref(The_Ref : in Corba.Object.Ref'Class) return Ref ;


  --------------------------------
  --   IDL declarations         --
  --------------------------------

   type name is new Corba.String ;
   type name_Ptr is access name ;

   procedure Free is new Ada.Unchecked_Deallocation(name, name_Ptr) ;


   type longueur_Array is array ( 0..2 ) of Corba.Long ;
   type longueur_Array_Ptr is access longueur_Array ;

   procedure Free is new Ada.Unchecked_Deallocation(longueur_Array, longueur_Array_Ptr) ;


   type dist is record
      longueur : longueur_Array;
      largeur : Corba.Long;
   end record ;
   type dist_Ptr is access dist ;

   procedure Free is new Ada.Unchecked_Deallocation(dist, dist_Ptr) ;


   procedure shoot(Self : in Ref; ranges : in dist ) ;



   -----------------------------
   --       Not in Spec       --
   -----------------------------

   Repository_Id : constant Corba.String := Corba.To_Corba_String("IDL:weapon:1.0") ;

   function Get_Repository_Id(Self : in Ref)
                              return Corba.String ;

   function Is_A(The_Ref : in Ref ;
                 Repo_Id : in Corba.String)
                 return Corba.Boolean ;

   function Is_A(Repo_Id : in Corba.String)
                 return Corba.Boolean ;

   function Get_Nil_Ref(Self : in Ref)
                        return Ref ;

   procedure Free is new Ada.Unchecked_Deallocation(Ref, Ref_Ptr) ;


private

   Nil_Ref : aliased constant Ref := ( Corba.Object.Nil_Ref with null record) ;
end weapon ;
