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

with Ada.exceptions ;

with Corba.Object ; use Corba.Object ;
use type Corba.String ;

package body Vehicle is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dynamic_Type : Corba.Object.Ref'Class := Get_Dynamic_Type(The_Ref) ;
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
   ----      IDL   description                   ----
   --------------------------------------------------
   function Can_Drive(Self : in Ref ;
                       Age : in Corba.Unsigned_Short) return Corba.Boolean is
      Opcd : Vehicle.Proxies.Can_Drive_Proxy ;
   begin
      Vehicle.Proxies.Init(Opcd, Age) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return Vehicle.Proxies.Get_Result(Opcd) ;
   end ;





   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- Get_Nil_Ref
   --------------
   function Get_Nil_Ref(Self : in Ref) return Ref is
   begin
      return Nil_Ref ;
   end ;

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref) return Corba.String is
   begin
      return  Repository_Id ;
   end ;


   -- Is_A
   -------
   function Is_A(The_Ref: in Ref;
                 Repo_Id: in Corba.String )
                 return Corba.Boolean is
   begin
      return Is_A(Repo_Id) ;
   end ;

   -- Is_A
   -------
   function Is_A(Repo_Id: in Corba.String )
                 return Corba.Boolean is
   begin
      return (Repository_Id = Repo_Id
              or Corba.Object.Is_A(Repo_Id) ) ;
   end ;


begin

   Corba.Object.Register(Repository_Id, Nil_Ref'Access) ;
   Corba.Object.Create_Proxy_Object_Factory(Repository_Id) ;

End Vehicle ;

