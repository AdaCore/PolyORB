with Corba ;
with Corba.Object ;
with Omni ;
with Netbufferedstream ; use Netbufferedstream ;

use type Corba.Unsigned_Long ;

with Chicken ;

package body Chicken.Proxies is

   -- Init
   -------
   procedure Init(Self : in out Lay_Proxy) is
   begin
      Set_User_Exceptions(Self, False) ;
   end ;


   -- Operation
   ------------
   function Operation (Self : in Lay_Proxy)
                       return CORBA.String is
   begin
      return Corba.To_Corba_String("lay") ;
   end ;



   -- Aligned_Size
   ---------------
   function Align_Size(Self: in Lay_Proxy;
                       Size_In: in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
   begin
      return Size_In;
   end;

   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self: in Lay_Proxy ;
                               Giop_Client: in out Giop_C.Object ) is
   begin
      null ;
   end;

   -- UnMarshal_Return_Values
   --------------------------
   procedure Unmarshal_Returned_Values(Self: in out Lay_Proxy ;
                                       Giop_Client: in out Giop_C.Object) is
      Result : Egg.Ref ;
   begin
      Corba.Object.Unmarshall(Result, Giop_Client) ;
      Self.Private_Result := new Egg.Ref'(Result) ;
   end ;


   -- Result
   ---------
   function Get_Result (Self : in Lay_Proxy) return Egg_Forward.Ref is
   begin
      return Egg.Convert_Forward.To_Forward(Self.Private_Result.all) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Lay_Proxy) is
   begin
      Egg.Free(Self.Private_Result) ;
   end ;



end Chicken.Proxies ;




