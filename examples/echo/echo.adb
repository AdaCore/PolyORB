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

   -- To_Ref
   ---------
   function To_Ref(The_Ref : in Corba.Object.Ref'Class) return Ref is
      Real_Object : Corba.Object'Class ;
      Result : Ref ;
   begin
      Real_Object := Get_Dynamic_Object(The_Ref) ;
      AdaBroker_Cast_To_Parent(Real_Object,Result) ;
      return Result;
   end ;

   -- EchoString
   -------------
   function EchoString(Self: in Ref; message: in Corba.String)
                       return Corba.String is

      Opcd : OmniProxyCallDesc_Echo ;
   begin
      OmniProxyCallDesc.Init(Opcd, "echoString", Message) ;
      OmniProxyCallWrapper.Invoke(Self, Opcd) ;
      return Echo.Result(Opcd) ;
   end ;


   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   -- AlignedSize
   --------------
   function AlignedSize(Self: in OmniProxyCallDesc_Echo;
                          MsgSize: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
   begin
      MsgSize := Omni.Align_To(MsgSize,Omni.ALIGN_4) + 5 + Arg'Length;
      return MsgSize;
   end;

   -- MarshalArguments
   -------------------
   procedure MarshalArguments(Self: in OmniProxyCallDesc_Echo;
                                Giop_Client: in out Giop_C) is
      Len : CORBA.Unsigned_Long;
   begin
      Len := Arg'Length + 1;
      Marshall(Len,Giop_Client);
      if (Len > 1) then
         Put_Char_Array (Giop_Client,Arg,Len);
      else
         Marshall(Nul,Giop_Client);
      end if;
   end;

   -- UnMarshalReturnValues
   ------------------------
   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Echo;
                                     Giop_Client: in out Giop_C) is
   begin
      Result :=  UnMarshall(Giop_Client);
   end ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object'Class) is
      Data_Conversion_Error : Corba.Data_Conversion ;
   begin
      if Real_Object'Tag = Result'Tag then
         Result := Real_Object ;
      else
         raise Data_Conversion_Error;
      end if ;
   end ;


End Echo ;


