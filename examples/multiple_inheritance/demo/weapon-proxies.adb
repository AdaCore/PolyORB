with Netbufferedstream ;
with Membufferedstream ;
with Corba ;
with Corba.Object ;
with weapon.marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Corba ;
use Corba.Object ;
use weapon.marshal ;
package body weapon.Proxies is 
   -----------------------------------------------------------
   ---               shoot
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out shoot_Proxy ;
                  ranges : in dist) is
   begin
      Set_User_Exceptions(Self, False ) ;
      Self.ranges := new dist'(ranges) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in shoot_Proxy )
                      return Corba.String is
   begin
      return Corba.To_Corba_String("shoot") ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in shoot_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long := Size_In ;
   begin
      Tmp := Align_size(Self.ranges.all, Tmp) ;
      return Tmp ;
   end ;


   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self : in shoot_Proxy ;
                               Giop_Client : in out Giop_C.Object) is
   begin
      Marshall(Self.ranges.all,Giop_Client) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out shoot_Proxy) is
   begin
      Free(Self.ranges) ;
   end ;


end weapon.Proxies ;
