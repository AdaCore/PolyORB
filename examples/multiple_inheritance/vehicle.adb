----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package body Vehicle                                ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Text_IO ;

with Ada.Tags, Ada.exceptions ;

with Corba.Object ; use Corba.Object ;
use type Corba.String ;

package body Vehicle is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dynamic_Type : Corba.Object.Ref'Class
        := Get_Dynamic_Type(The_Ref) ;
      Result : Ref ;
      Repo_id : Corba.String := Get_Repository_Id(Result) ;
   begin
      if Is_A(Dynamic_Type, Repo_Id) then
         return  (Corba.Object.Ref(The_Ref) with null record)  ;
      end if ;

   Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                  "  Cannot cast "
                        & Corba.To_Standard_String(Get_Repository_Id(The_Ref))
                                  & Corba.CRLF
                                  & "  into "
                                  & Corba.To_Standard_String(Repo_Id)) ;
   end ;



   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref) return Corba.String is
   begin
      return  Corba.To_Corba_String("IDL:Vehicle:1.0") ;
   end ;


   -- Is_A
   -------
   function Is_A(The_Ref: in Ref;
                 Repo_Id: in Corba.String )
                 return Corba.Boolean is
      Parent1 : Corba.Object.Ref ;
   begin
      return (Get_Repository_Id(The_Ref) = Repo_Id
              or Get_Repository_Id(Parent1) = Repo_Id) ;
   end ;

   --------------------------------------------------
   ----                 private                  ----
   --------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Ref) is
   Nil_Ref_Ptr : Corba.Object.Ref_Ptr := Corba.Object.Ref(Nil_Ref)'Access ;
   begin
      Corba.Object.AdaBroker_Set_Dynamic_Type(Self,Nil_Ref_Ptr) ;
   end ;



End Vehicle ;

