with tank.Proxies ;
with Ada.Exceptions ;
with Corba.Object ;
with weapon.Proxies ;
with OmniProxyCallWrapper ;
with Corba ;
use Corba.Object ;
use type Corba.String ;
package body tank is 


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


   --------------------------------------
   -- inheritance from weapon
   --------------------------------------

   procedure shoot(Self : in Ref;
                   ranges : in Corba.Long) is
      Opcd : weapon.Proxies.shoot_Proxy ;
   begin 
      weapon.Proxies.Init(Opcd, ranges) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
   end ;

   --------------------------------------------------
   --          IDL declarations                    --
   --------------------------------------------------

   function move(Self : in Ref;
                  fast : in Corba.String)
                 return Corba.String is
      Opcd : tank.Proxies.move_Proxy ;
   begin 
      tank.Proxies.Init(Opcd, fast) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return tank.Proxies.Get_Result(Opcd) ;
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
              or vehicle.Is_A(Repo_Id)
              or weapon.Is_A(Repo_Id));
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
end tank ;
