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

with Corba, Corba.Object, BufferedStream ;
with Omniproxycalldesc, Omniproxycallwrapper ;

package body Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- To_Ref
   ---------
   function To_Ref(The_Ref : in Corba.Object.Ref'Class) return Ref is
      Real_Object : Corba.Object.Ref'Class :=
        Corba.Object.Get_Dynamic_Object(The_Ref) ;
      Result : Ref ;
   begin
      AdaBroker_Cast_To_Parent(Echo.Ref(Real_Object),Result) ;
      return Result;
   end ;

   -- EchoString
   -------------
   function EchoString(Self: in Ref ;
                       Message: in Corba.String)
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
                        Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
   begin
      MsgSize := Omni.Align_To(MsgSize,Omni.ALIGN_4) + 5 + Arg'Length;
      return Size_In;
   end;

   -- MarshalArguments
   -------------------
   procedure MarshalArguments(Self: in OmniProxyCallDesc_Echo ;
                              Giop_Client: in out Giop_C.Object ) is
      Len : CORBA.Unsigned_Long;
   begin
      Len := Arg'Length + 1;
      BufferedStream.Marshall(Len,Giop_Client);
      if (Len > 1) then
         BufferedStream.Put_Char_Array (Giop_Client,Arg,Len);
      else
         BufferedStream.Marshall(Nul,Giop_Client);
      end if;
   end;

   -- UnMarshalReturnValues
   ------------------------
   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Echo ;
                                     Giop_Client: in out Giop_C.Object) is
   begin
      Result :=  BufferedStream.UnMarshall(Giop_Client);
   end ;


   -- Result
   ---------
   function Result (Self : in Ref) return CORBA.String is
   begin
      return Private_Result ;
   end ;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object'Class) is
      Impossible_Cast : Corba.Constraint_Error ;
   begin
      if Real_Object'Tag = Result'Tag then
         Result := Real_Object ;
      else
         raise Impossible_Cast;
      end if ;
   end ;


End Echo ;


