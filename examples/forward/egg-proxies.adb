with Corba ;
with Corba.Object ;
with Omni ;
with Netbufferedstream ; use Netbufferedstream ;

use type Corba.Unsigned_Long ;
with Adabroker_Debug ; use Adabroker_Debug ;
with Chicken ;

package body Egg.Proxies is

   -- Init
   -------
   procedure Init(Self : in out Hatch_Proxy) is
   begin
      Set_User_Exceptions(Self, False) ;
   end ;


   -- Operation
   ------------
   function Operation (Self : in Hatch_Proxy)
                       return CORBA.String is
   begin
      return Corba.To_Corba_String("hatch") ;
   end ;



   -- Aligned_Size
   ---------------
   function Align_Size(Self: in Hatch_Proxy;
                       Size_In: in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
   begin
      return Size_In;
   end;

   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self: in Hatch_Proxy ;
                               Giop_Client: in out Giop_C.Object ) is
   begin
      null ;
   end;

   -- UnMarshal_Return_Values
   --------------------------
   procedure Unmarshal_Returned_Values(Self: in out hatch_proxy ;
                                       Giop_Client: in out Giop_C.Object) is
      Result : Chicken.Ref ;
   begin
      Output(True,"Egg.Proxies.Unmarshal_Returned_Values : start") ;
      Corba.Object.Unmarshall(Result, Giop_Client) ;
      Output(True,"Egg.Proxies.Unmarshal_Returned_Values : chicken unmarshalled") ;
      Self.Private_Result := new Chicken.Ref'(Result) ;
      Output(True,"Egg.Proxies.Unmarshal_Returned_Values : Exiting procedure : OK") ;
   end ;


   -- Result
   ---------
   function Get_Result (Self : in Hatch_Proxy) return Chicken_Forward.Ref is
   begin
      Output(True,"Egg.Proxies.Get_Result") ;
      return Chicken.Convert_Forward.To_Forward(Self.Private_Result.all) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Hatch_Proxy) is
   begin
      Output(True,"Egg.Proxies.Finalize : start") ;
      Chicken.Free(Self.Private_Result) ;
      Output(True,"Egg.Proxies.Finalize : done") ;
   end ;



end Egg.Proxies ;

