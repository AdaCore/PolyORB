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
with Ada.Unchecked_Conversion ;
with Interfaces.C ;
with System ;

with Corba, Corba.Object, Corba.Exceptions ;
with Giop ;
with Omniropeandkey ;
with Giop_C ;
with Netbufferedstream ;
with Omniproxycalldesc ;
with Omniobject ; use type Omniobject.Object_Ptr ;
with Sys_Dep ;


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
   ----------------
   procedure Invoke (Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) is
      OmniObj_Ptr : Omniobject.Object_Ptr := Corba.Object.Get_OmniObject_Ptr (Obj);
      -- pointer on the underlying omniobject

      Retries : Corba.Unsigned_Long := 0 ;
      -- current number of retries

      Rope_And_Key : Omniropeandkey.Object ;
      -- rope and key of the omniobject

      Is_Fwd : Corba.Boolean ;
      -- False if Rope_And_Key corresponds to the rope and key stored in
      -- the IOP profile. True otherwise.

      Reuse : Corba.Boolean := False ;
      -- true if this is not the first Sync object instantiated to
      -- use the Strand. False otherwise.

      Giop_Client : Giop_C.Object ;
      -- Giop_c object used forcommunications with the ORB

      Message_Size : Corba.Unsigned_Long ;
      -- size of the message to pass to the giop_c object
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
           Giop_C.Request_Header_Size
           (Omniropeandkey.Key_Size(Rope_And_Key),
            Corba.Length(Omniproxycalldesc.Operation(Call_Desc))) ;
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
               Netbufferedstream.UnMarshall (RepoID,Giop_Client) ;

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
                  Obj_Ref : Corba.Object.Ref ;
                  Omniobj_Ptr2 : Omniobject.Object_Ptr ;
                  R : Omniropeandkey.Object ;
                  Unneeded_Result : Corba.Boolean ;
               begin
                  -- unmarshall the object
                  Corba.Object.UnMarshall (Obj_Ref,Giop_Client) ;
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
                  Omniobject.Get_Rope_And_Key (Omniobj_Ptr2.all,
                                               R,
                                               Unneeded_result) ;
                  -- and set these rope and key to Omniobj
                  Omniobject.Set_Rope_And_Key(Omniobj_Ptr.all, R) ;
                  return ;

               end ;
         end case ;
      end loop ;

   exception

      when E : Corba.Comm_Failure =>
         declare
            Member : Corba.Comm_Failure_Members ;
         begin
            Corba.Get_Members (E, Member) ;
            if Reuse or Is_Fwd then
               if Is_Fwd then
                  Omniobject.Reset_Rope_And_Key (OmniObj_Ptr.all) ;
               end if ;
               if Omni_Call_Transient_Exception_Handler
                 (OmniObj_Ptr.all,Retries,Member.Minor, Member.Completed) then
                  declare
                    Member2 : Corba.Transient_Members := (Member.Minor,
                                                          Member.Completed) ;
                  begin
                     Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                                  Member2) ;
                  end ;
               end if ;
            else
               if Omni_Comm_Failure_Exception_Handler
                 (OmniObj_Ptr.all,Retries,Member.Minor,Member.Completed) then
                  Corba.Raise_Corba_Exception (Corba.Comm_Failure'Identity,
                                               Member) ;
               end if ;
            end if ;
         end ;

      when E : Corba.Transient =>
         declare
            Member : Corba.Transient_Members ;
         begin
            if Omni_Call_Transient_Exception_Handler
              (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
               Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                            Member) ;
            end if ;
         end ;

      when E : Corba.Object_Not_Exist =>
         declare
            Member : Corba.Object_Not_Exist_Members ;
         begin
            if Is_Fwd then
               -- if Is_Fwd = True, we have to reset the rope and the key
               -- of the object according to IOP profile.
               Omniobject.Reset_Rope_And_Key (OmniObj_Ptr.all) ;
               if Omni_Call_Transient_Exception_Handler
                 (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
                  declare
                     Member2 : Corba.Transient_Members := (Member.Minor,
                                                           Member.Completed) ;
                  begin
                     Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                                  Member2) ;
                  end ;
               end if ;
            else
               if Omni_System_Exception_Handler
                 (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
                  Corba.Raise_Corba_Exception (Corba.Object_Not_Exist'Identity,
                                               Member) ;
               end if ;
            end if ;
         end ;

      when E : Corba.Unknown |
        Corba.Bad_Param |
        Corba.No_Memory |
        Corba.Imp_Limit |
        Corba.Inv_Objref |
        Corba.No_Permission |
        Corba.Internal |
        Corba.Marshal |
        Corba.Initialization_Failure |
        Corba.No_Implement |
        Corba.Bad_Typecode |
        Corba.Bad_Operation |
        Corba.No_Resources |
        Corba.No_Response |
        Corba.Persist_Store |
        Corba.Bad_Inv_Order |
        Corba.Free_Mem |
        Corba.Inv_Ident |
        Corba.Inv_Flag |
        Corba.Intf_Repos |
        Corba.Bad_Context |
        Corba.Obj_Adapter |
        Corba.Data_Conversion =>
         declare
            Member : Corba.Inv_Objref_Members ;
         begin
            if Omni_System_Exception_Handler
              (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
               Corba.Raise_Corba_Exception (Ada.Exceptions.Exception_Identity (E),
                                            Member) ;
            end if ;
         end;

   end;


   -- One_Way
   ----------
   procedure One_Way(Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class) is
      OmniObj_Ptr : Omniobject.Object_Ptr := Corba.Object.Get_OmniObject_Ptr (Obj);
      -- pointer on the underlying omniobject

      Retries : Corba.Unsigned_Long := 0 ;
      -- current number of retries

      Rope_And_Key : Omniropeandkey.Object ;
      -- rope and key of the omniobject

      Is_Fwd : Corba.Boolean ;
      -- False if Rope_And_Key corresponds to the rope and key stored in
      -- the IOP profile. True otherwise.

      Reuse : Corba.Boolean := False ;
      -- true if this is not the first Sync object instantiated to
      -- use the Strand. False otherwise.

      Giop_Client : Giop_C.Object ;
      -- Giop_c object used forcommunications with the ORB

      Message_Size : Corba.Unsigned_Long ;
      -- size of the message to pass to the giop_c object
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
           Giop_C.Request_Header_Size
           (Omniropeandkey.Key_Size(Rope_And_Key),
            Corba.Length(Omniproxycalldesc.Operation(Call_Desc))) ;
         -- and then the size of the message itself
         Message_Size := Omniproxycalldesc.Aligned_Size (Call_Desc,
                                                         Message_Size) ;

         -- Initialise the request
         Giop_C.Initialize_Request(Giop_Client,
                                   Omniropeandkey.Get_Key(Rope_And_Key),
                                   Omniropeandkey.Key_Size(Rope_And_Key),
                                   OmniProxycalldesc.Operation(Call_Desc),
                                   Message_Size,
                                   True);

         -- Marshal the arguments to the operation
         Omniproxycalldesc.Marshal_Arguments (Call_Desc, Giop_Client) ;

         -- wait for the reply
         case Giop_C.Receive_Reply(Giop_Client) is

            when Giop.NO_EXCEPTION =>
               -- inform the ORB that the request was completed
               Giop_C.Request_Completed(Giop_Client) ;

            when Giop.USER_EXCEPTION |
              Giop.SYSTEM_EXCEPTION |
              Giop.LOCATION_FORWARD =>
               Giop_C.Request_Completed(Giop_Client,True) ;
               Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                               "GIOP_C::ReceiveReply() returned unexpected code on oneway" &
                                               Corba.CRLF & "in method One_Way" &
                                               Corba.CRLF & "(see omniproxycallwrapper L 422") ;
         end case ;

      end loop ;

   exception

      when E : Corba.Comm_Failure =>
         declare
            Member : Corba.Comm_Failure_Members ;
         begin
            Corba.Get_Members (E, Member) ;
            if Reuse or Is_Fwd then
               if Is_Fwd then
                  Omniobject.Reset_Rope_And_Key (OmniObj_Ptr.all) ;
               end if ;
               if Omni_Call_Transient_Exception_Handler
                 (OmniObj_Ptr.all,Retries,Member.Minor, Member.Completed) then
                  declare
                    Member2 : Corba.Transient_Members := (Member.Minor,
                                                          Member.Completed) ;
                  begin
                     Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                                  Member2) ;
                  end ;
               end if ;
            else
               if Omni_Comm_Failure_Exception_Handler
                 (OmniObj_Ptr.all,Retries,Member.Minor,Member.Completed) then
                  Corba.Raise_Corba_Exception (Corba.Comm_Failure'Identity,
                                               Member) ;
               end if ;
            end if ;
         end ;

      when E : Corba.Transient =>
         declare
            Member : Corba.Transient_Members ;
         begin
            if Omni_Call_Transient_Exception_Handler
              (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
               Corba.Raise_Corba_Exception (Corba.Transient'Identity,
                                            Member) ;
            end if ;
         end ;

      when E : Corba.Unknown |
        Corba.Bad_Param |
        Corba.No_Memory |
        Corba.Imp_Limit |
        Corba.Object_Not_Exist |
        Corba.Inv_Objref |
        Corba.No_Permission |
        Corba.Internal |
        Corba.Marshal |
        Corba.Initialization_Failure |
        Corba.No_Implement |
        Corba.Bad_Typecode |
        Corba.Bad_Operation |
        Corba.No_Resources |
        Corba.No_Response |
        Corba.Persist_Store |
        Corba.Bad_Inv_Order |
        Corba.Free_Mem |
        Corba.Inv_Ident |
        Corba.Inv_Flag |
        Corba.Intf_Repos |
        Corba.Bad_Context |
        Corba.Obj_Adapter |
        Corba.Data_Conversion =>
         declare
            Member : Corba.Inv_Objref_Members ;
         begin
            if Omni_System_Exception_Handler
              (OmniObj_Ptr.all, Retries, Member.Minor, Member.Completed) then
               Corba.Raise_Corba_Exception (Ada.Exceptions.Exception_Identity (E),
                                            Member) ;
            end if ;
         end;

   end ;



   -------------------------------------------------
   --              exception handling             --
   -------------------------------------------------


   -- Completion_Status_To_C_Int
   -----------------------------
   function Completion_Status_To_C_Int (Status : in Corba.Completion_Status)
                                        return Interfaces.C.Int is
   begin
      case Status is
         when Corba.Completed_Yes =>
            return Interfaces.C.Int (0) ;
         when Corba.Completed_No =>
            return Interfaces.C.Int (1) ;
         when Corba.Completed_Maybe =>
            return Interfaces.C.Int (2) ;
      end case ;
   end;


   -- C_Int_To_Completion_Status
   -----------------------------
   function C_Int_To_Completion_Status (N : in Interfaces.C.Int)
                                        return Corba.Completion_Status is
   begin
      case N is
         when 1 =>
            return Corba.Completed_Yes ;
         when 2 =>
            return Corba.Completed_No ;
         when 3 =>
            return Corba.Completed_Maybe ;
         when others =>
            Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity,
                                            "Expected Completion_Status in C_Int_To_Completion_Status" & Corba.CRLF &
                                            "Int out of range" & Corba.CRLF &
                                            "(see corba_exceptions.adb L210)");
      end case ;
   end ;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- C_Omni_Call_Transient_Exeption_Handler
   -----------------------------------------
   function C_Omni_Call_Transient_Exeption_Handler
     (Obj : in System.Address ;
      Retries : in Interfaces.C.Unsigned_Long ;
      Minor : in Interfaces.C.Unsigned_Long ;
      Status : in Interfaces.C.Int)
      return Sys_Dep.C_Boolean ;
   pragma Import (CPP,
                  C_Omni_Call_Transient_Exeption_Handler,
                  "_omni_callTransientExceptionHandler__FP10omniObjectUlUlQ25CORBA16CompletionStatus") ;


   -- Omni_Call_Transient_Exception_Handler
   ----------------------------------------
   function Omni_Call_Transient_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean is
      C_Obj : System.Address ;
      C_Retries : Interfaces.C.Unsigned_Long ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_Obj := Obj'Address ;
      C_Retries := Ada_To_C_Unsigned_Long (Retries) ;
      C_Minor := Ada_To_C_Unsigned_Long (Minor) ;
      C_Status := Completion_Status_To_C_Int (Status) ;
      -- ... calls the C function ...
      C_Result := C_Omni_Call_Transient_Exeption_Handler (C_Obj,
                                                          C_Retries,
                                                          C_Minor,
                                                          C_Status) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;


   -- C_Omni_Comm_Failure_Exception_Handler
   ----------------------------------------
   function C_Omni_Comm_Failure_Exception_Handler
     (Obj : in System.Address ;
      Retries : in Interfaces.C.Unsigned_Long ;
      Minor : in Interfaces.C.Unsigned_Long ;
      Status : in Interfaces.C.Int)
      return Sys_Dep.C_Boolean ;
   pragma Import (CPP,
                  C_Omni_Comm_Failure_Exception_Handler,
                  "_omni_callCommFailureExceptionHandler__FP10omniObjectUlUlQ25CORBA16CompletionStatus") ;


   -- Omni_Comm_Failure_Exception_Handler
   --------------------------------------
   function Omni_Comm_Failure_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean is
      C_Obj : System.Address ;
      C_Retries : Interfaces.C.Unsigned_Long ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_Obj := Obj'Address ;
      C_Retries := Ada_To_C_Unsigned_Long (Retries) ;
      C_Minor := Ada_To_C_Unsigned_Long (Minor) ;
      C_Status := Completion_Status_To_C_Int (Status) ;
      -- ... and calls the C function
      C_Result := C_Omni_Comm_Failure_Exception_Handler (C_Obj,
                                                         C_Retries,
                                                         C_Minor,
                                                         C_Status) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;


   -- C_Omni_System_Exception_Handler
   ----------------------------------
   function C_Omni_System_Exception_Handler
     (Obj : in System.Address ;
      Retries : in Interfaces.C.Unsigned_Long ;
      Minor : in Interfaces.C.Unsigned_Long ;
      Status : in Interfaces.C.Int)
      return Sys_Dep.C_Boolean ;
   pragma Import (CPP,
                  C_Omni_System_Exception_Handler,
                  "_omni_callSystemExceptionHandler__FP10omniObjectUlUlQ25CORBA16CompletionStatus") ;


   -- Omni_System_Exception_Handler
   --------------------------------
   function Omni_System_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean is
      C_Obj : System.Address ;
      C_Retries : Interfaces.C.Unsigned_Long ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_Obj := Obj'Address ;
      C_Retries := Ada_To_C_Unsigned_Long (Retries) ;
      C_Minor := Ada_To_C_Unsigned_Long (Minor) ;
      C_Status := Completion_Status_To_C_Int (Status) ;
      -- ... and calls the C function
      C_Result := C_Omni_System_Exception_Handler (C_Obj,
                                                   C_Retries,
                                                   C_Minor,
                                                   C_Status) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;


   -- C_Omni_Object_Not_Exist_Exception_Handler
   --------------------------------------------
   function C_Omni_Object_Not_Exist_Exception_Handler
     (Obj : in System.Address ;
      Retries : in Interfaces.C.Unsigned_Long ;
      Minor : in Interfaces.C.Unsigned_Long ;
      Status : in Interfaces.C.Int)
      return Sys_Dep.C_Boolean ;
   pragma Import (CPP,
                  C_Omni_Object_Not_Exist_Exception_Handler,
                  "_omni_callObjectNotExistExceptionHandler__FP10omniObjectUlUlQ25CORBA16CompletionStatus") ;


   -- Omni_Object_Not_Exist_Exception_Handler
   ------------------------------------------
   function Omni_Object_Not_Exist_Exception_Handler
     (Obj : in Omniobject.Object'Class ;
      Retries : in Corba.Unsigned_Long ;
      Minor : in Corba.Unsigned_Long ;
      Status : in Corba.Completion_Status)
      return Corba.Boolean is
      C_Obj : System.Address ;
      C_Retries : Interfaces.C.Unsigned_Long ;
      C_Minor : Interfaces.C.Unsigned_Long ;
      C_Status : Interfaces.C.Int ;
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_Obj := Obj'Address ;
      C_Retries := Ada_To_C_Unsigned_Long (Retries) ;
      C_Minor := Ada_To_C_Unsigned_Long (Minor) ;
      C_Status := Completion_Status_To_C_Int (Status) ;
      -- ... and calls the C function
      C_Result := C_Omni_Object_Not_Exist_Exception_Handler (C_Obj,
                                                             C_Retries,
                                                             C_Minor,
                                                             C_Status) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;



end omniproxyCallWrapper ;
