----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_proxies                                ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ;
with Omni ;
with Netbufferedstream ; use Netbufferedstream ;

use type Corba.Unsigned_Long ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Echo.Proxies is

   --------------------------------------------------
   ----        function EchoString               ----
   --------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out EchoString_Proxy ;
                  Arg : Corba.String) is
   begin
      Set_User_Exceptions(Self, False) ;
      Self.Arg_Msg := new Corba.String'(Arg) ;
   end ;


   -- Operation
   ------------
   function Operation (Self : in EchoString_Proxy)
                       return CORBA.String is
   begin
      return Corba.To_Corba_String("echoString") ;
   end ;



   -- Aligned_Size
   ---------------
   function Align_Size(Self: in EchoString_Proxy;
                       Size_In: in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
   begin
      return Netbufferedstream.Align_size(Self.Arg_Msg.all, Size_In);
   end;

   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self: in EchoString_Proxy ;
                               Giop_Client: in out Giop_C.Object ) is
   begin
      Marshall(Self.Arg_Msg.all,Giop_Client);
   end;

   -- UnMarshal_Return_Values
   --------------------------
   procedure Unmarshal_Returned_Values(Self: in out EchoString_proxy ;
                                       Giop_Client: in out Giop_C.Object) is
      Result : Corba.String ;
   begin
      Unmarshall(Result, Giop_Client) ;
      Self.Private_Result := new Corba.String'(Result) ;
   end ;


   -- Result
   ---------
   function Get_Result (Self : in EchoString_Proxy) return CORBA.String is
   begin
      return Self.Private_Result.all ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out EchoString_Proxy) is
   begin
      pragma Debug(Output(Echo_Proxies,"Finalizing EchoString_Proxy")) ;
      Corba.Free(Self.Arg_Msg) ;
      pragma Debug(Output(Echo_Proxies,"Deleted the argument")) ;
      Corba.Free(Self.Private_Result) ;
      pragma Debug(Output(Echo_Proxies,"Deleted the result")) ;
   end ;



end Echo.Proxies ;



