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

with BufferedStream ;
with Omniproxycallwrapper ;
with Omni ;

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

      Opcd : OmniProxyCallDesc_EchoString ;
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
   function AlignedSize(Self: in OmniProxyCallDesc_EchoString;
                        Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
      Msg_Size : Corba.Unsigned_Long ;
   begin
      Msg_Size := Omni.Align_To(Size_In,Omni.ALIGN_4)
        + 5 + Corba.Length(Self.Arg);
      return Msg_Size ;
   end;

   -- MarshalArguments
   -------------------
   procedure MarshalArguments(Self: in OmniProxyCallDesc_EchoString ;
                              Giop_Client: in out Giop_C.Object ) is
      Len : CORBA.Unsigned_Long;
   begin
      Len := Self.Arg'Length + 1;
      BufferedStream.Marshall(Len,Giop_Client);
      if (Len > 1) then
         Giop_C.Put_Char_Array (Giop_Client,Self.Arg,Len);
      else
         BufferedStream.Marshall(Nul,Giop_Client);
      end if;
   end;

   -- UnMarshalReturnValues
   ------------------------
   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Echo ;
                                     Giop_Client: in out Giop_C.Object) is
   begin
      Self.Private_Result :=  BufferedStream.UnMarshall(Giop_Client);
   end ;


   -- Result
   ---------
   function Result (Self : in Ref) return CORBA.String is
   begin
      return Self.Private_Result ;
   end ;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   -- AdaBroker_Cast_To_Parent
   ---------------------------
   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object'Class) is
   begin
      if Real_Object'Tag = Result'Tag then
         Result := Real_Object ;
      else
         raise Constraint_Error ;
      end if ;
   end ;


   --------------------------------------------------
   ----        server-side subprograms           ----
   --------------------------------------------------
   -- These subprograms are only used on the server side
   -- the are here because they must not be in echo-impl.adb
   -- otherwise the user would be bothered with code he does
   -- not need to see.
   -- All these subprograms will be inherited by the Echo.Impl.object
   -- which inherits Echo.Ref

   -- Init
   -------
   procedure Adabroker_Init (Self : in out Ref ;
                             K : in OmniORB.ObjectKey) is
      L : OmniRopeAndKey.Object;
   begin
      -- Init(L,Rope.Null_Rope,K,...);
      -- PROBLEME sur K : le type n'est pas le bon. En C, on trouve ici
      -- un cast plus que sauvage...
      -- To Be continued
   end;

   -- Dipatch
   ----------
   function AdaBroker_Dispatch (Self : in Ref ;
                                Orls : in out Giop_S ;
                                Orl_Op : in Corba.String ;
                                Orl_Response_Expected : Corba.Boolean)
                                return Corba.Boolean is
   begin
      case To_Lower(Orl_Op) is
         when "echostring" =>
            declare
               Mesg : Corba.String ;
               Result : Corba.String ;
            begin
               -- unmarshaling the arguments
               Mesg := Unmarshal(Orls) ;

               -- change state
               Request_Received(Orls) ;

               -- call the implementation
               Result := EchoString(Ref, Mesg) ;

               -- marshaling the result
               -- to be completed

               -- exiting, all is ok
               return True ;

            end;
         when others => return False ;
      end case;
   end


End Echo ;


