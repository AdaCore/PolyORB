with Omniobject ;
with Giop_S ;
package tank.Skeleton is

   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Dispatch_Returns : out Corba.Boolean) ;

end tank.Skeleton  ;
