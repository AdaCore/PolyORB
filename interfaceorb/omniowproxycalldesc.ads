-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniOWProxyCallDesc                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package omniOWProxyCallDesc is

   type Object is abstract tagged limited private;

   -- all the following funcitions correspond
   -- to omniORB's OmniProxyCallDesc
   -- In proxyCall.h L68
   procedure Init (Self : in out Object,
                     Op : in CORBA::String,
                     Op_Len : in Size_T);

   function AlignedSize(Size_In: in Corba.Unsigned_Long)
     return Corba.Unsigned_Long is abstract ;

   procedure MarshalArguments (Giop_Client: in out Giop_C) is abstract ;

   function Operation_Len (Self : in Object) return Size_T;

   function Operation (Self : in Object) return CORBA::String;

private


end omniOWproxyCallDesc ;

