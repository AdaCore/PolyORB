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

with Ada.Characters.Latin_1 ;

with Corba ;
use type Corba.Unsigned_Long ;

with Omni ;


package body Echo.Proxies is

   -- Create
   ---------
   function Create(Arg : Corba.String) return EchoString_Proxy is
      Result : EchoString_Proxy ;
   begin
      Init(Result) ;
      Result.Arg_Msg := new Corba.String'(Arg) ;
      return Result ;
   end ;

   -- Free
   ----------
   procedure Free(Self : in out EchoString_Proxy) is
   begin
      Corba.Free(Self.Arg_Msg) ;
      Corba.Free(Self.Private_Result) ;
   end ;

   -- Aligned_Size
   --------------
   function Aligned_Size(Self: in EchoString_Proxy;
                         Size_In: in Corba.Unsigned_Long)
                         return Corba.Unsigned_Long is
      Msg_Size : Corba.Unsigned_Long ;
   begin
      Msg_Size := Omni.Align_To(Size_In,Omni.ALIGN_4)
        + Corba.Unsigned_Long(5) + Corba.Length(Self.Arg_Msg.all);
      return Msg_Size ;
   end;

   -- Marshal_Arguments
   -------------------
   procedure Marshal_Arguments(Self: in EchoString_Proxy ;
                              Giop_Client: in out Giop_C.Object ) is
      Len : CORBA.Unsigned_Long;
   begin
      Len := Corba.Length(Self.Arg_Msg.all) + 1;
      Giop_C.Marshal(Len,Giop_Client);
      if (Len > Corba.Unsigned_Long(1)) then
         Giop_C.Put_Char_Array (Giop_Client,Self.Arg_Msg.all,Len);
      else
         Giop_c.Marshal(Ada.Characters.Latin_1.Nul,Giop_Client);
      end if;
   end;

   -- UnMarshal_Return_Values
   ------------------------
   procedure Unmarshal_Returned_Values(Self: in out EchoString_proxy ;
                                       Giop_Client: in Giop_C.Object) is
      Result : Corba.String ;
   begin
      Result := Giop_C.UnMarshal(Giop_Client) ;
      Self.Private_Result := new Corba.String'(Result) ;
   end ;


   -- Result
   ---------
   function Get_Result (Self : in EchoString_Proxy) return CORBA.String is
   begin
      return Self.Private_Result.all ;
   end ;


end Echo.Proxies ;
