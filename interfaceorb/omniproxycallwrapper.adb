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

with Ada.Exceptions ;

with Corba, Corba.Object ;
with Giop, Giop_C ;
with Omniropeandkey, Omniproxycalldesc ;
with BufferedStream, Netbufferedstream ;
use Omniproxycalldesc ;



package body omniProxyCallWrapper is

   -- Invoke
   ---------
   ----------------
   -- This is a traduction into Ada of the C++ function
   -- invoke in proxyCall.cc L 46
   --
   -- Does not take into account :
   --   - omniORB's tracelevel
   --
   -- Remember to implement the exception handlers
   -- hand the infinite loop
   --
   -- Remember to put the use clauses and suppress the dotted notation
   -- for the inheritance to be taken into account
   -- (Fabien)
   --
   -- pb with Giop_C.Request_Completed
   -- 0 or 1 parameter
   ----------------
   procedure Invoke (The_Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) is
      The_Parameter_Obj : Corba.Object.Ref'Class := The_Obj ;
      Retries : Corba.Unsigned_Long := 0 ;
      Rope_And_Key : Omniropeandkey.Object ;
      Is_Fwd : Corba.Boolean ;
      Reuse : Corba.Boolean := False ;
      Giop_Client : Giop_C.Object ;
      Message_Size : Corba.Unsigned_Long ;
   begin

      -- sould start infinite loop
      -- to be done when exceptions are handled

      Corba.Object.Assert_Object_Existent(The_Parameter_Obj) ;

      Corba.Object.Get_Rope_And_Key(The_Parameter_Obj, Rope_And_Key, Is_Fwd) ;

      -- Get a GIOP driven strand
      Giop_C.Init(Giop_Client, Omniropeandkey.Rope(Rope_And_Key)) ;
      Reuse := NetBufferedStream.Is_Reusing_Existing_Connection(Giop_Client) ;

      -- Calculate the size of the message
      Message_Size :=
        Giop_C.Request_Header_Size(Omniropeandkey.Key_Size(Rope_And_Key),
                                   Omniproxycalldesc.Operation_Len(Call_Desc)) ;

      Aligned_Size(Call_Desc,Message_Size) ;

      Giop_C.Initialize_Request(Giop_Client,
                                Omniropeandkey.Key(Rope_And_Key),
                                Omniropeandkey.Key_Size(Rope_And_Key),
                                OmniProxycalldesc.Operation(Call_Desc),
                                Message_Size,
                                False);

      -- Marshal the arguments to the operation
      Marshal_Arguments(Call_Desc, Giop_Client) ;

      -- wait for the reply
      case Giop_C.Receive_Reply(Giop_Client) is
         when Giop.NO_EXCEPTION =>
            -- unmarshal the results and out/inout arguments
            Unmarshal_Returned_Values(Call_Desc, Giop_Client) ;
            Giop_C.Request_Completed(Giop_Client) ;
            return ;
         when Giop.USER_EXCEPTION =>
            -- check if the proxy was allowed to raise an exception
            if not Omniproxycalldesc.Has_User_Exceptions(Call_Desc) then
               declare
                  Excpt_Members : Corba.Unknown_Members ;
               begin
                  Giop_C.Request_Completed(Giop_Client, True) ;
                  Excpt_Members := (0, Corba.Completed_Maybe) ;
                  Corba.Raise_Corba_Exception(Corba.Unknown'Identity,
                                              Excpt_Members) ;
               end ;

            end if ;

            -- retrieve the interface repository ID of the exception
            declare
               RepoID_Len : Corba.Unsigned_Long ;
               RepoID : Corba.String ;
            begin
               RepoID_Len := BufferedStream.UnMarshal(Giop_Client) ;
               BufferedStream.Get_Char_Array(Giop_Client, RepoID, RepoID_Len) ;
               -- may be simplified,
               -- it was done like this in C++ for memory allocation
               Omniproxycalldesc.User_Exception(Call_Desc, Giop_Client, RepoID) ;

               -- never reach this point,
               -- the preceding operations must raise either
               -- a user exception or Corba.Marshal
               Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                              "Should never reach this point,"
                                              & Corba.CRLF
                                              & "omniproxycallwrapper.adb"
                                              & Corba.CRLF
                                              & "procedure invoke"
                                              & Corba.CRLF
                                              & "when Giop.USER_EXCEPTION") ;
            end ;

         when Giop.SYSTEM_EXCEPTION =>
            Giop_C.Request_Completed(Giop_Client, True) ;
            Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                           "Giop.SYSTEM_EXCEPTION should not be returned"
                                           & Corba.CRLF
                                           & "by Giop_C.Receive_Reply"
                                           & Corba.CRLF
                                           & "in omniproxycallwrapper.adb"
                                           & Corba.CRLF
                                           & "procedure invoke, when giop.SYSTEM_EXCEPTION") ;

         when Giop.LOCATION_FORWARD =>
            declare
               Obj : Corba.Object.Ref'Class :=
                 Corba.Object.UnMarshal_Obj_Ref(Giop_Client) ;
               Excpt_Members : Corba.Comm_Failure_Members ;
               R : Omniropeandkey.Object ;
               Unneeded_Result : Corba.Boolean ;
            begin
               Giop_C.Request_Completed(Giop_Client) ;
               if Corba.Object.Is_Nil(Obj) then
                  -- omniORB's log info : omitted
                  Excpt_Members := (0, Corba.Completed_No) ;
                  Corba.Raise_Corba_Exception(Corba.Comm_Failure'Identity,
                                              Excpt_Members) ;
               end if;
               Corba.Object.Get_Rope_And_Key(Obj, R, Unneeded_result) ;
               Corba.Object.Set_Rope_And_Key(The_Parameter_Obj, R) ;
               -- omniORB's log info : omitted

               -- we go back to the beginning of the infinite loop
               -- with a modified Omniobject
            end ;
      end case ;

      -- Exception handlers
      -- are not imlpemented yet
      -- they should be the same as in proxyCall.cc L 46
   exception
      when Ex : Corba.Comm_Failure =>
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "in omniproxycallwrapper.adb, procedure invoke"
                                        & Corba.CRLF
                                        & "caught exception Corba.Comm_Failure"
                                        & Corba.CRLF
                                        & "handler not implemented yet");
      when Ex : Corba.Transient =>
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "in omniproxycallwrapper.adb, procedure invoke"
                                        & Corba.CRLF
                                        & "caught exception Corba.Transient"
                                        & Corba.CRLF
                                        & "handler not implemented yet");
      when Ex : Corba.Object_Not_Exist =>
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "in omniproxycallwrapper.adb, procedure invoke"
                                        & Corba.CRLF
                                        & "caught exception Corba.Object_Not_Exist"
                                        & Corba.CRLF
                                        & "handler not implemented yet");
      when Ex : others =>
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "in omniproxycallwrapper.adb, procedure invoke"
                                        & Corba.CRLF
                                        & "caught other exception "
                                        & Ada.Exceptions.Exception_Name(Ex)
                                        & Corba.CRLF
                                        & "handler not implemented yet");

         -- should end infinite loop
         -- to be done when exceptions are handled

   end;


   -- One_Way
   ----------
   procedure One_Way(The_Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object) is
   begin
      return ;
   end ;





end omniproxyCallWrapper ;
