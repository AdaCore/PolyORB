with Giop_C ;
with Omniproxycalldesc ;
with Rope ;
with Iop ;
with Corba ;
package vehicle.Proxies is 
   -----------------------------------------------------------
   ---               Get_mark
   -----------------------------------------------------------

   type Get_mark_Proxy is new OmniProxyCallDesc.Object with private;

   procedure Init(Self : in out Get_mark_Proxy) ;

   function Operation(Self : in Get_mark_Proxy)
                      return Corba.String ;

   procedure Unmarshal_Returned_Values(Self : in out Get_mark_Proxy ;
                                       Giop_Client : in out Giop_C.Object);

   function Get_Result (Self : in Get_mark_Proxy )
                        return Corba.String; 


   -----------------------------------------------------------
   ---               Set_mark
   -----------------------------------------------------------

   type Set_mark_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out Set_mark_Proxy ;
                  Arg : in Corba.String) ;

   function Operation(Self : in Set_mark_Proxy)
                      return Corba.String ;

   function Align_Size(Self : in Set_mark_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self : in Set_mark_Proxy ;
                               Giop_Client : in out Giop_C.Object);

   -----------------------------------------------------------
   ---               can_drive
   -----------------------------------------------------------

   type can_drive_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out can_drive_Proxy ;
                  age : in Corba.Unsigned_Short) ;

   function Operation(Self : in can_drive_Proxy )
                      return Corba.String ;

   function Align_Size(Self : in can_drive_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self : in can_drive_Proxy ;
                               Giop_Client : in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self : in out can_drive_Proxy ;
                                       Giop_Client : in out Giop_C.Object) ;

   function Get_Result (Self : in can_drive_Proxy)
                        return Corba.Boolean; 


private 
   type Get_mark_Proxy is new OmniProxyCallDesc.Object with record 
      Private_Result : Corba.String_Ptr := null;
   end record ;
   procedure Finalize (Self : in out Get_mark_Proxy) ;

   type Set_mark_Proxy is new OmniProxyCallDesc.Object with record 
      Arg : Corba.String_Ptr := null;
   end record ;
   procedure Finalize (Self : in out Set_mark_Proxy) ;

   type can_drive_Proxy is new OmniProxyCallDesc.Object with record 
      age : Corba.Unsigned_Short_Ptr := null ;
      Private_Result : Corba.Boolean_Ptr := null;
   end record; 
   procedure Finalize(Self : in out can_drive_Proxy) ;

end vehicle.Proxies ;
