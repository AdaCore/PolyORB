-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----             package body omniProxyCallWrapper                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/10/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

package body omniProxyCallWrapper is

   procedure Invoke (O : in OmniObject.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) is
      Retries : Corba.Unsigned_Long := 0 ;
      Rope_And_Key : Omniropeandkey.Object ;
      Fwd : Corba.Boolean ;
      Reuse : Corba.Boolean := False ;
      Giop_Client : Giop_C.Object ;
      Message_Size : Corba.Unsigned_Long ;
   begin

      -- start infinite loop

      Assert_Object_Existent(O) ;

      Fwd := Get_Rope_And_Key(O, Rope_And_Key) ;

      -- Get a GIOP driven strand
      Giop_C.Init(Giop_Client, Omniropeandkey.Rope(Rope_And_Key)) ;
      Reuse := NetBufferedStream.Is_Reusing_Existing_Connection ;

      -- Calculate the size of the message
      Message_Size :=
        Giop_C.Request_Header_Size(Omniropeandkey.Key_Size(Rope_And_Key),
                                   Omniproxycalldesc.Operation_Len(Call_Desc)) ;


   end;


   procedure One_Way(O: in OmniObject.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object) ;
   -- reimplemented in Ada to call the C++ ORB
   -- see proxyCall.cc L181



private


end omniproxyCallWrapper ;
