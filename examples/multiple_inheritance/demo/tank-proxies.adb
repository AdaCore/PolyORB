with Netbufferedstream ;
with Membufferedstream ;
with Corba ;
with Corba.Object ;
with tank.marshal ;
with weapon ;
with weapon.marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Corba ;
use Corba.Object ;
use tank.marshal ;
use weapon ;
use weapon.marshal ;
package body tank.Proxies is 
   -----------------------------------------------------------
   ---               move
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out move_Proxy ;
                  wide : in weapon.dist) is
   begin
      Set_User_Exceptions(Self, False ) ;
      Self.wide := new weapon.dist'(wide) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in move_Proxy )
                      return Corba.String is
   begin
      return Corba.To_Corba_String("move") ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in move_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long := Size_In ;
   begin
      Tmp := Align_size(Self.wide.all, Tmp) ;
      return Tmp ;
   end ;


   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self : in move_Proxy ;
                               Giop_Client : in out Giop_C.Object) is
   begin
      Marshall(Self.wide.all,Giop_Client) ;
   end ;


   -- Unmarshal_Returned_Values
   ----------------------------
   procedure Unmarshal_Returned_Values(Self : in out move_Proxy ;
                                       Giop_Client : in out Giop_C.Object) is
      Returns : Corba.String ;
   begin
      Unmarshall(Returns, Giop_client) ;
      Self.Private_Result := new Corba.String'(Returns) ;
   end ;


   -- Get_Result
   -------------
   function Get_Result (Self : in move_Proxy )
                        return Corba.String is
   begin
      return Self.Private_Result.all ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out move_Proxy) is
   begin
      Free(Self.wide) ;
      Free(Self.Private_Result) ;
   end ;


   -----------------------------
   -- inheritance from weapon
   -----------------------------

   -----------------------------------------------------------
   ---               shoot
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out shoot_Proxy ;
                  ranges : in weapon.dist) is
   begin
      Set_User_Exceptions(Self, False ) ;
      Self.ranges := new weapon.dist'(ranges) ;
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




end tank.Proxies ;
