-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Giop                            ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----   For each function defined in the IDL file, a descendant     ----
----   of Omniproxycalldesc is created. It is the object in        ----
----   charge of storing the arguments of the function,            ----
----   marshalling them into a bufferedstream, call the remote     ----
----   object, and unmarshall the result.                          ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

with Ada.Exceptions ;

with Corba, Corba.Object ;
with Giop ;
with Omniropeandkey ;
with Giop_C ; use Giop_C ;
with Netbufferedstream ; use Netbufferedstream ;
with Omniproxycalldesc ; use Omniproxycalldesc ;



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
   -- and the infinite loop
   --
   ----------------
   procedure Invoke (Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) is
      OmniObj_Ptr : Omniobject.Object_Ptr := OmniObject.GetOmniObject (Obj);
--      Retries : Corba.Unsigned_Long := 0 ;
      Rope_And_Key : Omniropeandkey.Object ;
      Is_Fwd : Corba.Boolean ;
      Reuse : Corba.Boolean := False ;
      Giop_Client : Giop_C.Object ;
--      Message_Size : Corba.Unsigned_Long ;
   begin
      loop
         -- verify that the underlying omniobject is not null
         if OmniObj_Ptr = null then
            Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                            "null omniobject_ptr found in method invoke (omniProxyCallWrapper L 86)") ;
         end if ;

         -- verify that the object exists
         Omniobject.Assert_Object_Existent(OmniObj_Ptr.all) ;

         -- get the current values of the rope and the key
         Omniobject.Get_Rope_And_Key(OmniObj_Ptr.all,Rope_And_Key,Is_Fwd) ;

         -- Get a GIOP driven strand
         Giop_C.Init (Giop_Client, Omniropeandkey.Get_Rope(Rope_And_Key)) ;

         -- do the giop_client reuse an existing connection ?
         Reuse := Netbufferedstream.Is_Reusing_Existing_Connection(Giop_Client) ;

         -- Calculates the size of the message
         -- first the size of the header
         Message_Size :=
           Giop_C.Request_Header_Size(Omniropeandkey.Key_Size(Rope_And_Key),
                                      Corba.Length(Operation(Call_Desc))) ;
         -- and then the size of the message itself
         Message_Size := Omniproxycalldesc.Aligned_Size (Call_Desc,
                                                         Message_Size) ;

         -- Initialise the request
         Giop_C.Initialize_Request(Giop_Client,
                                   Omniropeandkey.Get_Key(Rope_And_Key),
                                   Omniropeandkey.Key_Size(Rope_And_Key),
                                   OmniProxycalldesc.Operation(Call_Desc),
                                   Message_Size,
                                   False);

         -- Marshal the arguments to the operation
         Omniproxycalldesc.Marshal_Arguments (Call_Desc, Giop_Client) ;

         -- wait for the reply
         case Giop_C.Receive_Reply(Giop_Client) is

            when Giop.NO_EXCEPTION =>
               -- unmarshal the results and out/inout arguments
               Omniproxycalldesc.Unmarshal_Returned_Values(Call_Desc,
                                                           Giop_Client) ;
               -- inform the ORB that the request was completed
               Giop_C.Request_Completed(Giop_Client) ;
               return ;

            when Giop.USER_EXCEPTION =>
               -- check if the exception is due to the proxycalldesc
               if not Omniproxycalldesc.Has_User_Exceptions(Call_Desc) then
                  declare
                     Excpt_Members : Corba.Unknown_Members ;
                  begin
                     -- inform the ORB that the message was skiped
                     Giop_C.Request_Completed(Giop_Client,True) ;
                     -- raise an Unknown exception
                     Excpt_Members := (0, Corba.Completed_Maybe) ;
                     Corba.Raise_Corba_Exception(Corba.Unknown'Identity,
                                                 Excpt_Members) ;
                  end ;
               end if ;

            -- retrieve the interface repository ID of the exception
            declare
               RepoID : Corba.String ;
            begin
               -- UnMarshalls the RepoID
               Netbufferedstream.UnMarshal(RepoID,Giop_Client) ;

               -- may be simplified,
               -- it was done like this in C++ for memory allocation
               Omniproxycalldesc.User_Exception(Call_Desc,
                                                Giop_Client,
                                                RepoID) ;

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
               -- inform the ORB that the message was skiped
               Giop_C.Request_Completed(Giop_Client, True) ;
               -- and raise a fatal exception
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
                  Obj_Ref : Corba.Object.Ref'Class ;
                  Omniobj_Ptr2 : Omniobject.Object_Ptr ;
                  R : Omniropeandkey.Object ;
                  Unneeded_Result : Corba.Boolean ;
               begin
                  -- unmarshall the object
                  Corba.Object.UnMarshal (Obj_Ref,Giop_Client) ;
                  -- inform the ORB that the request was completed
                  Giop_C.Request_Completed(Giop_Client) ;
                  -- verify that the object is not null
                  if Corba.Object.Is_Nil(Obj_Ref) then
                     declare
                        Excpt_Members : Corba.Comm_Failure_Members ;
                        begin
                           -- raise a Comm_Failure exception
                           Excpt_Members := (0, Corba.Completed_No) ;
                           Corba.Raise_Corba_Exception (Corba.Comm_Failure'Identity,
                                                        Excpt_Members) ;
                        end ;
                  end if;
                  -- get the underlying omniobject object
                  Omniobj_Ptr2 := Corba.Object.Get_OmniObject_Ptr(Obj_Ref) ;
                  -- verify it is not null
                  if OmniObj_Ptr2 = null then
                     Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                                     "null omniobject_ptr found in method invoke (omniProxyCallWrapper L 216)") ;
                  end if ;
                  -- get the rope and the key of the object
                  Corba.Object.Get_Rope_And_Key (Omniobj2_Ptr.all,
                                                 R,
                                                 Unneeded_result) ;
                  -- and set these rope and key to Omniobj
                  Corba.Object.Set_Rope_And_Key(Omniobj.all, R) ;
                  return ;

               end ;
         end case ;

      exception

         when

               ----------------------------------------------------
               ----------------------------------------------------
               ------------------- J'EN SUIS LA -------------------
               ----------------------------------------------------
               ----------------------------------------------------





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

      end loop ;
   end;


   -- One_Way
   ----------
   procedure One_Way(The_Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class) is
   begin
      return ;
   end ;





end omniproxyCallWrapper ;
