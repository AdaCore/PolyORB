with Netbufferedstream ;
with Membufferedstream ;
with Corba ;
with Corba.Object ;
with Echo.marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Corba ;
use Corba.Object ;
use Echo.marshal ;
package body Echo.Proxies is 
   -----------------------------------------------------------
   ---               echoString
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out echoString_Proxy ;
                  mesg : in Corba.String) is
   begin
      Set_User_Exceptions(Self, False ) ;
      Self.mesg := new Corba.String'(mesg) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in echoString_Proxy )
                      return Corba.String is
   begin
      return Corba.To_Corba_String("echoString") ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in echoString_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long := Size_In ;
   begin
      Tmp := Align_size(Self.mesg.all, Tmp) ;
      return Tmp ;
   end ;


   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self : in echoString_Proxy ;
                               Giop_Client : in out Giop_C.Object) is
   begin
      Marshall(Self.mesg.all,Giop_Client) ;
   end ;


   -- Unmarshal_Returned_Values
   ----------------------------
   procedure Unmarshal_Returned_Values(Self : in out echoString_Proxy ;
                                       Giop_Client : in out Giop_C.Object) is
      Result : Corba.String ;
   begin
      Unmarshall(Result, Giop_client) ;
      Self.Private_Result := new Corba.String'(Result) ;
   end ;


   -- Get_Result
   -------------
   function Get_Result (Self : in echoString_Proxy )
                        return Corba.String is
   begin
      return Self.Private_Result.all ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out echoString_Proxy) is
   begin
      Free(Self.mesg) ;
      Free(Self.Private_Result) ;
   end ;


end Echo.Proxies ;
