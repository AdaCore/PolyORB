-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniProxyCallDesc                    ----
----                                                               ----
----      This is a root class. For each subprogram of an IDL      ----
----    interface, a descendant of this class has to be provided.  ----
----    It contains al the information to make the remote call :   ----
----    arguments, results, exceptions, and how to send them on/   ----
----    reveive them from a giop.                                  ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba, Giop_C ;

package omniProxyCallDesc is

   type Object is abstract tagged private;

   -- all the following funcitions correspond
   -- to omniORB's OmniProxyCallDesc
   -- In proxyCall.h L33
   procedure Init (Self : in out Object ;
                   Has_Exceptions : Corba.Boolean := False ) ;


   function Operation (Self : in Object)
                       return CORBA.String is abstract ;

   procedure Free(Self : in out Object) is abstract ;

   function Aligned_Size(Self : in Object ;
                          Size_In: in Corba.Unsigned_Long )
     return Corba.Unsigned_Long is abstract ;
   -- used to be a function as in omniORB
   -- taking Size_In as a parameter and
   -- returning the same parameter modified
   -- (Fabien, 02/11/99)

   procedure Marshal_Arguments (Self : in Object ;
                                Giop_Client: in out Giop_C.Object ) is abstract ;

   procedure Unmarshal_Returned_Values (Self : in out Object ;
                                        Giop_Client: in Giop_C.Object ) is abstract ;

   procedure User_Exception (Self : in Object ;
                             Giop_Client : in Giop_C.Object ;
                             RepoId : in CORBA.String) ;
   -- must be overloaded by call descs which have exceptions

   function Has_User_Exceptions (Self : in Object)
                                 return CORBA.Boolean ;
   -- returns Pd_Has_User_Exception


private

   type Object is abstract tagged record
      Pd_Has_User_Exception : Corba.Boolean ;
   end record ;

end omniproxyCallDesc ;

