----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package body echo                                   ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----                                                                    ----
----------------------------------------------------------------------------


package body Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
   begin
      -- cf to_ref.txt
   end ;

   function EchoString(Self: in Ref, message: in Corba.String) return Corba.String is

      Opcd : OmniProxyCallDesc_Echo ;
   begin
      OmniProxyCallDesc.Init(Opcd, "echoString", Message) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return Result(Opcd) ;
   end ;


   --------------------------------------------------
   ----              not in  spec                ----
   --------------------------------------------------

   function AlignedSize(Self: in OmniProxyCallDesc_Echo,
                          MsgSize: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
   begin
      MsgSize := Omni.Align_To(MsgSize,Omni.ALIGN_4) + 5 + Arg'Length;
      return MsgSize;
   end;


   procedure MarshalArguments(Self: in OmniProxyCallDesc_Echo,
                                Giop_Client: in out Giop_C) is
      Len : CORBA:Unsigned_Long;
   begin
      Len := Arg'Length + 1;

   end

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Echo,
                                       Giop_Client: in out Giop_C) ;



End Echo ;


