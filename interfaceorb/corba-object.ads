-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Object                         ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package Corba.Object is

   type Ref is tagged private;

   --I boolean is_nil();
   function Is_Nil(Self: in Ref) return Boolean;
   function Is_Null(Self: in Ref) return Boolean renames Is_Nil;

   --I void release();
   procedure Release (Self : in out Ref'class);
   -- Not implemented in omniORB
   -- neither here

   --I Object duplicate();
   -- use assignment


   --------------------------------------------------
   ---        omniORB specific                    ---
   --------------------------------------------------

   procedure PR_Setobj(The_Object : in OmniObject.Object) ;
   -- wrapper around void CORBA::Object::PR_setobj(omniObject *obj)
   -- in corbaObject.cc L121

   function PR_Getobj return OmniObject.Object ;
   -- wrapper around omniObject* CORBA::Object::PR_getobj()
   -- in corbaObject.cc L128

   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------
   function Get_Dynamic_Object return Ref'Class ;


private

   type Ref is tagged record
      Dynamic_Object : access Ref'Class := Ref'Access ;
      end record ;

end Corba.Object ;
