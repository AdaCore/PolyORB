
--  For each function defined in the IDL file, a descendant of
--  Omniproxycalldesc is created. It is the object in charge of storing the
--  arguments of the function, marshalling them into a bufferedstream, call
--  the remote object, and unmarshall the result.

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Interfaces.C;

with System;

with CORBA;
with CORBA.Object;
with CORBA.Object.OmniORB;

with AdaBroker; use AdaBroker;
with AdaBroker.GIOP;
with AdaBroker.GIOP_C;
with AdaBroker.NetBufferedStream;
with AdaBroker.OmniORB;
with AdaBroker.OmniRopeAndKey;
with AdaBroker.OmniProxyCallDesc;
with AdaBroker.Sysdep;
with AdaBroker.Rope;

with AdaBroker.Debug;
pragma Elaborate_All (Adabroker.Debug);

package body AdaBroker.OmniProxyCallWrapper is

   Flag : constant Natural
      := AdaBroker.Debug.Is_Active ("omniproxycallwrapper");
   procedure O is new AdaBroker.Debug.Output (Flag);

   use type CORBA.Unsigned_Long;
   use type AdaBroker.OmniORB.OmniObject;
   use type AdaBroker.OmniORB.OmniObject_Ptr;

   function Completion_Status_To_C_Int
     (Status : in CORBA.Completion_Status)
      return Interfaces.C.int;

   function C_Int_To_Completion_Status
     (N : in Interfaces.C.int)
      return CORBA.Completion_Status;

   procedure Handle_CORBA_Exception
     (OmniObj_Ptr   : in OmniORB.OmniObject_Ptr;
      Ex_Member     : in out CORBA.Ex_Body'Class;
      Ex_Occurrence : in Ada.Exceptions.Exception_Occurrence;
      Retries       : in out CORBA.Unsigned_Long);

   ----------------------------
   -- Handle_CORBA_Exception --
   ----------------------------

   procedure Handle_CORBA_Exception
     (OmniObj_Ptr   : in OmniORB.OmniObject_Ptr;
      Ex_Member     : in out CORBA.Ex_Body'Class;
      Ex_Occurrence : in Ada.Exceptions.Exception_Occurrence;
      Retries       : in out CORBA.Unsigned_Long)
   is
   begin
      CORBA.Get_Members (Ex_Occurrence, Ex_Member);
      Retries := Retries + 1;
      if not Omni_System_Exception_Handler
        (OmniObj_Ptr.all,
         Retries,
         Ex_Member.Minor,
         Ex_Member.Completed)
      then
         CORBA.Raise_CORBA_Exception
           (Ada.Exceptions.Exception_Identity (Ex_Occurrence), Ex_Member);
      end if;
   end Handle_CORBA_Exception;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class)
   is
      --  This is a traduction into Ada of the C++ function invoke in
      --  proxyCall.cc L 46
      --
      --  Does not take into account : - omniORB's tracelevel

      OmniObj_Ptr : OmniORB.OmniObject_Ptr
        := CORBA.Object.Get_Implementation (Obj);
      --  Pointer on the underlying omniobject

      Retries : CORBA.Unsigned_Long := 0;
      --  Current number of retries

      Rope_And_Key : OmniRopeAndKey.Controlled_Wrapper;
      --  Rope and key of the omniobject

      The_Rope : Rope.Object;
      --  Rope of the omniobject

      Is_Fwd : CORBA.Boolean;
      --  False if Rope_And_Key corresponds to the rope and key stored in
      --  the IOP profile. True otherwise.

      Reuse : CORBA.Boolean := False;
      --  True if this is not the first Sync object instantiated to use the
      --  Strand. False otherwise.

      GIOP_Client : GIOP_C.Controlled_Wrapper;
      --  GIOP_c object used forcommunications with the ORB

      Message_Size : CORBA.Unsigned_Long;
      --  Size of the message to pass to the giop_c object

      Result : GIOP.Reply_Status_Type;
      --  Result of call to reply_completed

   begin
      loop
         pragma Debug
           (O ("invoke : enter, retries = " &
               CORBA.Unsigned_Long'Image (Retries)));

         --  Verify that the underlying omniobject is not null

         if OmniObj_Ptr = null then
            pragma Debug (O ("invoke : raise Constraint_Error"));
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "cannot call invoke subprogram on nil reference");
         end if;

         --  Verify that the object exists
         OmniORB.Assert_Object_Existent (OmniObj_Ptr.all);

         pragma Debug (O ("invoke : assert object existent"));

         --  Get the current values of the rope and the key
         OmniORB.Get_Rope_And_Key
           (OmniObj_Ptr.all,
            Rope_And_Key.Real,
            Is_Fwd);

         pragma Debug (O ("invoke : forward is " & Is_Fwd'Img));

         begin
            --  Get a GIOP driven strand
            The_Rope := OmniRopeAndKey.Get_Rope (Rope_And_Key.Real);

            pragma Debug (O ("invoke : rope available"));

            GIOP_C.Init (GIOP_Client.Real, The_Rope);

            --  Does the giop_client reuse an existing connection ?
            Reuse := NetBufferedStream.Is_Reusing_Existing_Connection
              (GIOP_Client.Real);

            pragma Debug (O ("invoke : reuse is " & Boolean'Image (Reuse)));

            pragma Debug (O ("invoke : key_size is " &
                             OmniRopeAndKey.Key_Size (Rope_And_Key.Real)'Img));

            --  Compute message size and then header size.
            declare
               use Ada.Strings.Unbounded;

               Desc : CORBA.String := OmniProxyCallDesc.Operation (Call_Desc);
            begin
               Message_Size :=
                 GIOP_C.Request_Header_Size
                 (GIOP_Client.Real,
                  OmniRopeAndKey.Key_Size (Rope_And_Key.Real),
                  CORBA.Unsigned_Long (Length (Unbounded_String (Desc))));
            end;

            pragma Debug (O ("invoke : old msg size is " & Message_Size'Img));

            --  And then the size of the message itself
            Message_Size := OmniProxyCallDesc.Align_Size
              (Call_Desc, Message_Size);

            pragma Debug (O ("invoke : new msg size is " & Message_Size'Img));

            --  Initialize the request
            GIOP_C.Initialize_Request
              (GIOP_Client.Real,
               OmniRopeAndKey.Get_Key (Rope_And_Key.Real),
               OmniRopeAndKey.Key_Size (Rope_And_Key.Real),
               OmniProxyCallDesc.Operation (Call_Desc),
               Message_Size,
               False);

            pragma Debug (O ("invoke : initialize request"));

            --  Marshall the arguments to the operation
            OmniProxyCallDesc.Marshal_Arguments (Call_Desc, GIOP_Client.Real);

            pragma Debug (O ("invoke : marshal arguments"));

            --  Wait for the reply
            GIOP_C.Receive_Reply (GIOP_Client.Real, Result);

            pragma Debug (O ("invoke : reply received " &
                             GIOP.Reply_Status_Type'Pos (Result)'Img));


            case Result is
               when GIOP.No_Exception =>
                  pragma Debug (O ("invoke : No_Exception"));

                  --  Unmarshall the results and out/inout arguments
                  OmniProxyCallDesc.Unmarshal_Returned_Values
                    (Call_Desc, GIOP_Client.Real);

                  --  Inform the ORB that the request was completed
                  GIOP_C.Request_Completed (GIOP_Client.Real);

                  pragma Debug (O ("invoke : unmarshalled"));
                  return;

               when GIOP.User_Exception =>
                  pragma Debug (O ("invoke : User_Exception"));

                  --  Check if the exception is due to the proxycalldesc
                  if not OmniProxyCallDesc.Has_User_Exceptions (Call_Desc) then
                     declare
                        Excpt_Members : CORBA.Unknown_Members;
                     begin
                        pragma Debug (O ("invoke : no such user exception"));

                        --  Inform the ORB that the message was skiped
                        GIOP_C.Request_Completed (GIOP_Client.Real, True);

                        --  Raise an Unknown exception
                        Excpt_Members := (0, CORBA.Completed_Maybe);
                        CORBA.Raise_CORBA_Exception
                          (CORBA.Unknown'Identity, Excpt_Members);
                     end;
                  end if;

                  --  Retrieve the interface repository ID of the exception
                  declare
                     Repoid : CORBA.String;
                  begin
                     pragma Debug (O ("invoke : unmarshall raised exception"));

                     --  Unmarshalls the RepoID
                     NetBufferedStream.Unmarshall (Repoid, GIOP_Client.Real);

                     pragma Debug (O ("invoke : call user_exception"));

                     --  May be simplified, it was done like this in C++
                     --  for memory allocation
                     OmniProxyCallDesc.User_Exception
                       (Call_Desc, GIOP_Client.Real, Repoid);

                     --  Never reach this point, the preceding operations
                     --  must raise either a user exception or
                     --  CORBA.Marshal

                     pragma Debug (O ("invoke : raise Fatal_error"));

                     Ada.Exceptions.Raise_Exception
                       (CORBA.AdaBroker_Fatal_Error'Identity,
                        "incorrect User_Exception in invoke subprogram");
                  end;

               when GIOP.System_Exception =>
                  --  Never reach here since the C equivalent of
                  --  receivereply throws the system exceptions if any
                  Ada.Exceptions.Raise_Exception
                    (CORBA.AdaBroker_Fatal_Error'Identity,
                     "invoke : System_Exception ");

               when GIOP.Location_Forward =>
                  pragma Debug (O ("invoke : reply is location_forward"));
                  declare
                     Obj_Ref      : CORBA.Object.Ref;
                     OmniObj_Ptr2 : OmniORB.OmniObject_Ptr;
                     R            : OmniRopeAndKey.Controlled_Wrapper;
                     Unneeded_Result : CORBA.Boolean;
                  begin
                     --  Unmarshall the object
                     CORBA.Object.OmniORB.Unmarshall
                       (Obj_Ref, GIOP_Client.Real);

                     --  Inform the ORB that the request was completed
                     GIOP_C.Request_Completed (GIOP_Client.Real);

                     --  Verify that the object is not null
                     if CORBA.Object.Is_Nil (Obj_Ref) then
                        declare
                           Excpt_Members : CORBA.Comm_Failure_Members;
                        begin
                           --  Raise a Comm_Failure exception
                           Excpt_Members := (0, CORBA.Completed_No);
                           CORBA.Raise_CORBA_Exception
                             (CORBA.Comm_Failure'Identity,
                              Excpt_Members);
                        end;
                     end if;

                     --  Get the underlying omniobject object
                     OmniObj_Ptr2 := CORBA.Object.Get_Implementation (Obj_Ref);

                     --  Verify it is not null
                     if OmniObj_Ptr2 = null then
                        Ada.Exceptions.Raise_Exception
                          (CORBA.AdaBroker_Fatal_Error'Identity,
                           "null omni object in invoke subprogram " &
                           "(omniProxyCallWrapper L 216)");
                     end if;

                     --  Get the rope and the key of the object
                     OmniORB.Get_Rope_And_Key
                       (OmniObj_Ptr2.all,
                        R.Real,
                        Unneeded_Result);

                     --  And set these rope and key to OmniObj
                     OmniORB.Set_Rope_And_Key (OmniObj_Ptr.all, R.Real);

                     return;

                  end;
            end case;

         exception

            when E : CORBA.Comm_Failure =>
               pragma Debug (O ("invoke : caught CORBA.Comm_Failure"));
               declare
                  Member : CORBA.Comm_Failure_Members;
               begin
                  Retries := Retries + 1;
                  CORBA.Get_Members (E, Member);
                  if Reuse or Is_Fwd then
                     pragma Debug (O ("invoke : Reuse or Is_Fwd  = True"));
                     if Is_Fwd then
                        OmniORB.Reset_Rope_And_Key (OmniObj_Ptr.all);
                     end if;

                     if not Omni_Call_Transient_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        declare
                           Member2 : CORBA.Transient_Members
                             := (Member.Minor, Member.Completed);
                        begin
                           CORBA.Raise_CORBA_Exception
                             (CORBA.Transient'Identity, Member2);
                        end;
                     end if;
                  else
                     pragma Debug (O ("invoke : Reuse or Is_Fwd  = False"));
                     pragma Debug (O ("Omni_Comm_Failure_Exception_Handler"));

                     if not Omni_Comm_Failure_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        CORBA.Raise_CORBA_Exception
                          (CORBA.Comm_Failure'Identity, Member);
                     end if;
                  end if;
               end;

            when CORBA.Transient =>
               pragma Debug (O ("invoke : caught CORBA.Transient"));
               declare
                  Member : CORBA.Transient_Members;
               begin
                  Retries := Retries + 1;
                  if not Omni_Call_Transient_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (CORBA.Transient'Identity,
                        Member);
                  end if;
               end;

            when CORBA.Object_Not_Exist =>
               declare
                  Member : CORBA.Object_Not_Exist_Members;
               begin
                  if Is_Fwd then
                     --  if Is_Fwd = True, we have to reset the rope and
                     --  the key Of the object according to IOP profile.
                     OmniORB.Reset_Rope_And_Key (OmniObj_Ptr.all);
                     Retries := Retries + 1;
                     if not Omni_Call_Transient_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        declare
                           Member2 : CORBA.Transient_Members
                             := (Member.Minor, Member.Completed);
                        begin
                           CORBA.Raise_CORBA_Exception
                             (CORBA.Transient'Identity, Member2);
                        end;
                     end if;
                  else
                     Retries := Retries + 1;
                     if not Omni_System_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        CORBA.Raise_CORBA_Exception
                          (CORBA.Object_Not_Exist'Identity, Member);
                     end if;
                  end if;
               end;

            when E : CORBA.Unknown =>
               declare
                  Member : CORBA.Unknown_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Bad_Param =>
               declare
                  Member : CORBA.Bad_Param_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.No_Memory =>
               declare
                  Member : CORBA.No_Memory_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Imp_Limit =>
               declare
                  Member : CORBA.Imp_Limit_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Inv_Objref =>
               declare
                  Member : CORBA.Inv_Objref_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.No_Permission =>
               declare
                  Member : CORBA.No_Permission_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Internal =>
               declare
                  Member : CORBA.Internal_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Marshal =>
               declare
                  Member : CORBA.Marshal_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Initialization_Failure =>
               declare
                  Member : CORBA.Initialization_Failure_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.No_Implement =>
               declare
                  Member : CORBA.No_Implement_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Bad_Typecode =>
               declare
                  Member : CORBA.Bad_Typecode_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Bad_Operation =>
               declare
                  Member : CORBA.Bad_Operation_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.No_Resources =>
               declare
                  Member : CORBA.No_Resources_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.No_Response =>
               declare
                  Member : CORBA.No_Response_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Persist_Store =>
               declare
                  Member : CORBA.Persist_Store_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Bad_Inv_Order =>
               declare
                  Member : CORBA.Bad_Inv_Order_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Free_Mem =>
               declare
                  Member : CORBA.Free_Mem_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Inv_Ident =>
               declare
                  Member : CORBA.Inv_Ident_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Inv_Flag =>
               declare
                  Member : CORBA.Inv_Flag_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Intf_Repos =>
               declare
                  Member : CORBA.Intf_Repos_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Bad_Context =>
               declare
                  Member : CORBA.Bad_Context_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Obj_Adapter =>
               declare
                  Member : CORBA.Obj_Adapter_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;

            when E : CORBA.Data_Conversion =>
               declare
                  Member : CORBA.Data_Conversion_Members;
               begin
                  Handle_CORBA_Exception (OmniObj_Ptr, Member, E, Retries);
               end;
         end;
      end loop;
   end Invoke;

   -------------
   -- One_Way --
   -------------

   procedure One_Way
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class)
   is
      OmniObj_Ptr : OmniORB.OmniObject_Ptr
        := CORBA.Object.Get_Implementation (Obj);
      --  Pointer on the underlying omniobject

      Retries : CORBA.Unsigned_Long := 0;
      --  Current number of retries

      Rope_And_Key : OmniRopeAndKey.Controlled_Wrapper;
      --  Rope and key of the omniobject

      Is_Fwd : CORBA.Boolean;
      --  False if Rope_And_Key corresponds to the rope and key stored in
      --  the IOP profile. True otherwise.

      Reuse : CORBA.Boolean := False;
      --  True if this is not the first Sync object instantiated to use the
      --  Strand. False otherwise.

      GIOP_Client : GIOP_C.Controlled_Wrapper;
      --  GIOP_c object used for communications with the ORB

      Message_Size : CORBA.Unsigned_Long;
      --  Size of the message to pass to the giop_c object

      Result : GIOP.Reply_Status_Type;
      --  Result of call to reply_completed
   begin
      loop
         --  Verify that the underlying omniobject is not null
         if OmniObj_Ptr = null then
            pragma Debug (O ("one_way : raise Constraint_Error"));

            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "cannot invoke one_way subprogram on nil reference");
         end if;

         --  Verify that the object exists
         OmniORB.Assert_Object_Existent (OmniObj_Ptr.all);

         --  Get the current values of the rope and the key
         OmniORB.Get_Rope_And_Key
           (OmniObj_Ptr.all, Rope_And_Key.Real, Is_Fwd);

         begin
            --  Get a GIOP driven strand
            GIOP_C.Init
              (GIOP_Client.Real, OmniRopeAndKey.Get_Rope (Rope_And_Key.Real));

            --  Do the giop_client reuse an existing connection ?
            Reuse := NetBufferedStream.Is_Reusing_Existing_Connection
              (GIOP_Client.Real);

            --  Calculates the size of the message first the size of the
            --  header
            declare
               use Ada.Strings.Unbounded;

               Desc : CORBA.String := OmniProxyCallDesc.Operation (Call_Desc);
            begin
               Message_Size :=
                 GIOP_C.Request_Header_Size
                 (GIOP_Client.Real,
                  OmniRopeAndKey.Key_Size (Rope_And_Key.Real),
                  CORBA.Unsigned_Long (Length (Unbounded_String (Desc))));
            end;

            --  And then the size of the message itself
            Message_Size := OmniProxyCallDesc.Align_Size
              (Call_Desc,
               Message_Size);

            --  Initialise the request
            GIOP_C.Initialize_Request
              (GIOP_Client.Real,
               OmniRopeAndKey.Get_Key (Rope_And_Key.Real),
               OmniRopeAndKey.Key_Size (Rope_And_Key.Real),
               OmniProxyCallDesc.Operation (Call_Desc),
               Message_Size,
               True);

            --  Marshal the arguments to the operation
            OmniProxyCallDesc.Marshal_Arguments (Call_Desc, GIOP_Client.Real);

            --  Wait for the reply
            GIOP_C.Receive_Reply (GIOP_Client.Real, Result);

            case Result is
               when GIOP.No_Exception =>
                  --  Inform the ORB that the request was completed
                  GIOP_C.Request_Completed (GIOP_Client.Real);
                  return;

               when GIOP.User_Exception   |
                    GIOP.System_Exception |
                    GIOP.Location_Forward =>
                  GIOP_C.Request_Completed (GIOP_Client.Real, True);

                  Ada.Exceptions.Raise_Exception
                    (CORBA.AdaBroker_Fatal_Error'Identity,
                     "incorrect code detected in one_way subprogram " &
                     "(see omniproxycallwrapper L 422");
            end case;

         exception
            when E : CORBA.Comm_Failure =>
               declare
                  Member : CORBA.Comm_Failure_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  if Reuse or Is_Fwd then
                     if Is_Fwd then
                        OmniORB.Reset_Rope_And_Key (OmniObj_Ptr.all);
                     end if;
                     Retries := Retries + 1;
                     if not Omni_Call_Transient_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        declare
                           Member2 : CORBA.Transient_Members
                             := (Member.Minor,
                                 Member.Completed);
                        begin
                           CORBA.Raise_CORBA_Exception
                             (CORBA.Transient'Identity,
                              Member2);
                        end;
                     end if;
                  else
                     Retries := Retries + 1;
                     if not Omni_Comm_Failure_Exception_Handler
                       (OmniObj_Ptr.all,
                        Retries,
                        Member.Minor,
                        Member.Completed)
                     then
                        CORBA.Raise_CORBA_Exception
                          (CORBA.Comm_Failure'Identity,
                           Member);
                     end if;
                  end if;
               end;

            when CORBA.Transient =>
               declare
                  Member : CORBA.Transient_Members;
               begin
                  Retries := Retries + 1;
                  if not Omni_Call_Transient_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (CORBA.Transient'Identity,
                        Member);
                  end if;
               end;

            when E : CORBA.Unknown =>
               declare
                  Member : CORBA.Unknown_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Bad_Param =>
               declare
                  Member : CORBA.Bad_Param_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.No_Memory =>
               declare
                  Member : CORBA.No_Memory_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Imp_Limit =>
               declare
                  Member : CORBA.Imp_Limit_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;
            when E : CORBA.Inv_Objref =>
               declare
                  Member : CORBA.Inv_Objref_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.No_Permission =>
               declare
                  Member : CORBA.No_Permission_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Internal =>
               declare
                  Member : CORBA.Internal_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Marshal =>
               declare
                  Member : CORBA.Marshal_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Initialization_Failure =>
               declare
                  Member : CORBA.Initialization_Failure_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.No_Implement =>
               declare
                  Member : CORBA.No_Implement_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Bad_Typecode =>
               declare
                  Member : CORBA.Bad_Typecode_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Bad_Operation =>
               declare
                  Member : CORBA.Bad_Operation_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.No_Resources =>
               declare
                  Member : CORBA.No_Resources_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.No_Response =>
               declare
                  Member : CORBA.No_Response_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Persist_Store =>
               declare
                  Member : CORBA.Persist_Store_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Bad_Inv_Order =>
               declare
                  Member : CORBA.Bad_Inv_Order_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Free_Mem =>
               declare
                  Member : CORBA.Free_Mem_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Inv_Ident =>
               declare
                  Member : CORBA.Inv_Ident_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Inv_Flag =>
               declare
                  Member : CORBA.Inv_Flag_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Intf_Repos =>
               declare
                  Member : CORBA.Intf_Repos_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Bad_Context =>
               declare
                  Member : CORBA.Bad_Context_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Obj_Adapter =>
               declare
                  Member : CORBA.Obj_Adapter_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

            when E : CORBA.Data_Conversion =>
               declare
                  Member : CORBA.Data_Conversion_Members;
               begin
                  CORBA.Get_Members (E, Member);
                  Retries := Retries + 1;
                  if not Omni_System_Exception_Handler
                    (OmniObj_Ptr.all,
                     Retries,
                     Member.Minor,
                     Member.Completed)
                  then
                     CORBA.Raise_CORBA_Exception
                       (Ada.Exceptions.Exception_Identity (E),
                        Member);
                  end if;
               end;

         end;
      end loop;
   end One_Way;

   --------------------------------
   -- Completion_Status_To_C_Int --
   --------------------------------

   function Completion_Status_To_C_Int
     (Status : in CORBA.Completion_Status)
      return Interfaces.C.int is
   begin
      case Status is
         when CORBA.Completed_Yes =>
            return Interfaces.C.int (0);
         when CORBA.Completed_No =>
            return Interfaces.C.int (1);
         when CORBA.Completed_Maybe =>
            return Interfaces.C.int (2);
      end case;
   end Completion_Status_To_C_Int;

   --------------------------------
   -- C_Int_To_Completion_Status --
   --------------------------------

   function C_Int_To_Completion_Status
     (N : in Interfaces.C.int)
      return CORBA.Completion_Status is
   begin
      case N is
         when 1 =>
            return CORBA.Completed_Yes;
         when 2 =>
            return CORBA.Completed_No;
         when 3 =>
            return CORBA.Completed_Maybe;
         when others =>
            Ada.Exceptions.Raise_Exception
              (CORBA.AdaBroker_Fatal_Error'Identity,
               "incorrect Completion_Status in C_Int_To_Completion_Status " &
               "(see corba_exceptions.adb L210)");
      end case;
   end C_Int_To_Completion_Status;

   --------------------------------------------
   -- C_Omni_Call_Transient_Exeption_Handler --
   -------------------------------------------
   function C_Omni_Call_Transient_Exeption_Handler
     (Obj     : in System.Address;
      Retries : in Interfaces.C.unsigned_long;
      Minor   : in Interfaces.C.unsigned_long;
      Status  : in Interfaces.C.int)
      return Sysdep.Bool;
   pragma Import
     (CPP, C_Omni_Call_Transient_Exeption_Handler,
      "_omni_callTransientExceptionHandler_" &
      "_FP10omniObjectUlRCQ25CORBA9TRANSIENT");

   -------------------------------------------
   -- Omni_Call_Transient_Exception_Handler --
   -------------------------------------------

   function Omni_Call_Transient_Exception_Handler
     (Obj     : in OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean
   is
      C_Obj     : System.Address;
      C_Retries : Interfaces.C.unsigned_long;
      C_Minor   : Interfaces.C.unsigned_long;
      C_Status  : Interfaces.C.int;
      C_Result  : Sysdep.Bool;
   begin
      --  Transform the arguments in a C type ...
      C_Obj     := Obj'Address;
      C_Retries := Interfaces.C.unsigned_long (Retries);
      C_Minor   := Interfaces.C.unsigned_long (Minor);
      C_Status  := Completion_Status_To_C_Int (Status);

      --  Call the C function ...
      C_Result := C_Omni_Call_Transient_Exeption_Handler
        (C_Obj, C_Retries, C_Minor, C_Status);

      --  Transform the result into an Ada type
      return Sysdep.To_Boolean (C_Result);
   end Omni_Call_Transient_Exception_Handler;

   -------------------------------------------
   -- C_Omni_Comm_Failure_Exception_Handler --
   -------------------------------------------

   function C_Omni_Comm_Failure_Exception_Handler
     (Obj     : in System.Address;
      Retries : in Interfaces.C.unsigned_long;
      Minor   : in Interfaces.C.unsigned_long;
      Status  : in Interfaces.C.int)
      return Sysdep.Bool;

   pragma Import
     (CPP, C_Omni_Comm_Failure_Exception_Handler,
      "_omni_callCommFailureExceptionHandler_" &
      "_FP10omniObjectUlRCQ25CORBA12COMM_FAILURE");

   -----------------------------------------
   -- Omni_Comm_Failure_Exception_Handler --
   -----------------------------------------

   function Omni_Comm_Failure_Exception_Handler
     (Obj     : in OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean
   is
      C_Obj     : System.Address;
      C_Retries : Interfaces.C.unsigned_long;
      C_Minor   : Interfaces.C.unsigned_long;
      C_Status  : Interfaces.C.int;
      C_Result  : Sysdep.Bool;
   begin
      --  Transform the arguments in a C type ...
      C_Obj     := Obj'Address;
      C_Retries := Interfaces.C.unsigned_long (Retries);
      C_Minor   := Interfaces.C.unsigned_long (Minor);
      C_Status  := Completion_Status_To_C_Int (Status);

      --  Call the C function
      C_Result := C_Omni_Comm_Failure_Exception_Handler
        (C_Obj, C_Retries, C_Minor, C_Status);

      --  Transform the result into an Ada type
      return Sysdep.To_Boolean (C_Result);
   end Omni_Comm_Failure_Exception_Handler;

   -------------------------------------
   -- C_Omni_System_Exception_Handler --
   -------------------------------------

   function C_Omni_System_Exception_Handler
     (Obj     : in System.Address;
      Retries : in Interfaces.C.unsigned_long;
      Minor   : in Interfaces.C.unsigned_long;
      Status  : in Interfaces.C.int)
      return Sysdep.Bool;

   pragma Import
     (CPP, C_Omni_System_Exception_Handler,
      "_omni_callSystemExceptionHandler_" &
      "_FP10omniObjectUlRCQ25CORBA15SystemException");

   -----------------------------------
   -- Omni_System_Exception_Handler --
   -----------------------------------

   function Omni_System_Exception_Handler
     (Obj     : in OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean
   is
      C_Obj     : System.Address;
      C_Retries : Interfaces.C.unsigned_long;
      C_Minor   : Interfaces.C.unsigned_long;
      C_Status  : Interfaces.C.int;
      C_Result  : Sysdep.Bool;
   begin
      --  Transform the arguments in a C type ...
      C_Obj     := Obj'Address;
      C_Retries := Interfaces.C.unsigned_long (Retries);
      C_Minor   := Interfaces.C.unsigned_long (Minor);
      C_Status  := Completion_Status_To_C_Int (Status);

      --  Call the C function
      C_Result := C_Omni_System_Exception_Handler
        (C_Obj, C_Retries, C_Minor, C_Status);

      pragma Debug
         (O ("omni_system_exception_handler : result computed : " &
             Boolean'Image (Sysdep.To_Boolean (C_Result))));

      --  Transform the result into an Ada type
      return Sysdep.To_Boolean (C_Result);
   end Omni_System_Exception_Handler;

end AdaBroker.OmniProxyCallWrapper;
