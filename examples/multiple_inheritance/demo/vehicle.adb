with vehicle.Proxies ;
with Ada.Exceptions ;
with Corba.Object ;
with OmniProxyCallWrapper ;
with Corba ;
use Corba.Object ;
use type Corba.String ;
package body vehicle is 


   -----------------------------
   --         The Spec        --
   -----------------------------

   function To_Ref(The_Ref : in Corba.Object.ref'Class)
                   return Ref is
      Dynamic_Type : Corba.Object.Ref'Class := Get_Dynamic_Type(The_Ref) ;
      Result : Ref ;
      Repo_Id : Corba.String := Get_Repository_Id(Result) ;
   begin
      if Is_A(Dynamic_Type, Repo_Id) then
         corba.Object.Internal_Copy(The_Ref, Result) ;
         return Result ;
      end if ;

      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Cannot cast "
                                     & Corba.To_Standard_String(Get_Repository_Id(The_Ref))
                                     & Corba.CRLF
                                     & Corba.To_Standard_String(Repo_Id)) ;
   end ;


   --------------------------------------------------
   --          IDL declarations                    --
   --------------------------------------------------

   function Get_mark(Self : in Ref)
                     return Corba.String is
      Opcd : vehicle.Proxies.Get_mark_Proxy ;
   begin 
      vehicle.Proxies.Init(Opcd) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return vehicle.Proxies.Get_Result(Opcd) ;
   end ;


   procedure Set_mark(Self : in Ref ;
                      To : in Corba.String) is 
      Opcd : vehicle.Proxies.Set_mark_Proxy ;
   begin 
      vehicle.Proxies.Init(Opcd, To) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
   end ;


   function can_drive(Self : in Ref;
                       age : in Corba.Unsigned_Short)
                      return Corba.Boolean is
      Opcd : vehicle.Proxies.can_drive_Proxy ;
   begin 
      vehicle.Proxies.Init(Opcd, age) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return vehicle.Proxies.Get_Result(Opcd) ;
   end ;

   -----------------------------
   --       Not in Spec       --
   -----------------------------

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Ref)
                              return Corba.String is
   begin
      return Repository_Id ;
   end ;


   -- Is_A
   -------
   function Is_A(The_Ref : in Ref ;
                 Repo_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return Is_A(Repo_Id) ;
   end ;


   -- Is_A
   -------
   function Is_A(Repo_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return (Repository_Id = Repo_Id
              or Corba.Object.Is_A(Repo_Id));
   end ;


   -- Get_Nil_Ref
   --------------
   function Get_Nil_Ref(Self : in Ref)
                        return Ref is
   begin
      return Nil_Ref ;
   end ;


begin
   Corba.Object.Register(Repository_Id, Nil_Ref'Access) ;
   Corba.Object.Create_Proxy_Object_Factory(Repository_Id) ;
end vehicle ;
