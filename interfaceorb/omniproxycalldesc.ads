-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniProxyCallDesc                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba, Giop_C ;

package omniProxyCallDesc is

   type Object is abstract tagged limited private;

   -- all the following funcitions correspond
   -- to omniORB's OmniProxyCallDesc
   -- In proxyCall.h L33
   procedure Init (Self : in out Object ;
                   Operator : in String ;
                   Has_Exceptions : Corba.boolean);

   procedure Aligned_Size(Self : in Object ;
                          Size_In: in out Corba.Unsigned_Long) is abstract ;
   -- used to be a function as in omniORB
   -- taking Size_In as a parameter and
   -- returning the same parameter modified
   -- (Fabien, 02/11/99)

   procedure Marshal_Arguments (Self : in Object ;
                                Giop_Client: in out Giop_C.Object ) is abstract ;

   procedure Unmarshal_Returned_Values (Self : in Object ;
                                        Giop_Client: in out Giop_C.Object ) is abstract ;

   procedure User_Exception (Self : in Object ;
                             Giop_Client : in Giop_C.Object ;
                             RepoId : in CORBA.String) ;

   function Has_User_Exceptions (Self : in Object)
                                 return CORBA.Boolean ;

   function Operation_Len (Self : in Object)
                           return Integer ;

   function Operation (Self : in Object)
                       return CORBA.String;

private

   type Object(Op_Size : Natural :=0 ) is abstract tagged limited record
      Pd_Has_User_Exception : Corba.Boolean ;
      Operation_Name :  Standard.String(1..Op_Size) ;
   end record ;

end omniproxyCallDesc ;

