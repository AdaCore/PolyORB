with Netbufferedstream ;
with Membufferedstream ;
with Corba ;
with Corba.Object ;
with vehicle.marshal ;
use Netbufferedstream ;
use Membufferedstream ;
use Corba ;
use Corba.Object ;
use vehicle.marshal ;
package body vehicle.Proxies is 
   -----------------------------------------------------------
   ---               Get_mark
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out Get_mark_Proxy) is
   begin
      Set_User_Exceptions(Self, False) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in Get_mark_Proxy)
                      return Corba.String is
   begin
      return Corba.To_Corba_String("_get_mark") ;
   end ;


   -- Unmarshal_Returned_Values
   ----------------------------
   procedure Unmarshal_Returned_Values(Self : in out Get_mark_Proxy ;
                                       Giop_Client : in out Giop_C.Object) is
      Result : Corba.String ;
   begin
      Unmarshall(Result, Giop_client) ;
      Self.Private_Result := new Corba.String'(Result) ;
   end ;


   -- Get_Result
   -------------
   function Get_Result (Self : in Get_mark_Proxy )
                        return Corba.String is
   begin
      return Self.Private_Result.all ;
   end ;


   -- Finalize
   -----------
   procedure Finalize (Self : in out Get_mark_Proxy) is
   begin
      Free(Self.Private_Result) ;
   end ;


   -----------------------------------------------------------
   ---               Set_mark
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out Set_mark_Proxy ;
                  Arg : in Corba.String) is
   begin
      Set_User_Exceptions(Self, False) ;
      Self.Arg := new Corba.String'(Arg) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in Set_mark_Proxy)
                      return Corba.String is
   begin
      return Corba.To_Corba_String("_set_mark") ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in Set_mark_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
   begin
      return Align_Size(Self.Arg.all, Size_In) ;
   end ;


   -- Marshall_Arguments
   ---------------------
   procedure Marshal_Arguments(Self : in Set_mark_Proxy ;
                               Giop_Client : in out Giop_C.Object) is
   begin
      Marshall(Self.Arg.all, Giop_client) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize (Self : in out Set_mark_Proxy) is
   begin
      Free(Self.Arg) ;
   end ;


   -----------------------------------------------------------
   ---               can_drive
   -----------------------------------------------------------

   -- Init
   -------
   procedure Init(Self : in out can_drive_Proxy ;
                  age : in Corba.Unsigned_Short) is
   begin
      Set_User_Exceptions(Self, False ) ;
      Self.age := new Corba.Unsigned_Short'(age) ;
   end ;


   -- Operation
   ------------
   function Operation(Self : in can_drive_Proxy )
                      return Corba.String is
   begin
      return Corba.To_Corba_String("can_drive") ;
   end ;


   -- Align_Size
   -------------
   function Align_Size(Self : in can_drive_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long is
      Tmp : Corba.Unsigned_Long := Size_In ;
   begin
      Tmp := Align_size(Self.age.all, Tmp) ;
      return Tmp ;
   end ;


   -- Marshal_Arguments
   --------------------
   procedure Marshal_Arguments(Self : in can_drive_Proxy ;
                               Giop_Client : in out Giop_C.Object) is
   begin
      Marshall(Self.age.all,Giop_Client) ;
   end ;


   -- Unmarshal_Returned_Values
   ----------------------------
   procedure Unmarshal_Returned_Values(Self : in out can_drive_Proxy ;
                                       Giop_Client : in out Giop_C.Object) is
      Returns : Corba.Boolean ;
   begin
      Unmarshall(Returns, Giop_client) ;
      Self.Private_Result := new Corba.Boolean'(Returns) ;
   end ;


   -- Get_Result
   -------------
   function Get_Result (Self : in can_drive_Proxy )
                        return Corba.Boolean is
   begin
      return Self.Private_Result.all ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out can_drive_Proxy) is
   begin
      Free(Self.age) ;
      Free(Self.Private_Result) ;
   end ;


end vehicle.Proxies ;
