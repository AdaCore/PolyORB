with Omniobject ;
with Giop_S ;
package Echo.Skeleton is

   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean) ;

end Echo.Skeleton  ;
