------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P R O T O C O L S . G I O P . C O M M O N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any.ExceptionList;
with PolyORB.Errors.Helper;
with PolyORB.Exceptions;
with PolyORB.GIOP_P.Exceptions;
with PolyORB.Log;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR.Common;
with PolyORB.Request_QoS;
with PolyORB.Requests;
with PolyORB.Servants.Iface;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.Common is

   use PolyORB.Buffers;
   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Representations.CDR;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Request_QoS;
   use PolyORB.QoS;
   use PolyORB.QoS.Service_Contexts;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.giop.common");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ----------------------
   -- Generic_Marshall --
   ----------------------

   procedure Generic_Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Table_Type)
   is
      Index : constant Target_Type := Table_Type'Pos (Val);
   begin
      Marshall (Buffer, Index);
   end Generic_Marshall;

   ------------------------
   -- Generic_Unmarshall --
   ------------------------

   function Generic_Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Table_Type
   is
      Index : constant Target_Type := Unmarshall (Buffer);
   begin
      return Table_Type'Val (Index);
   end Generic_Unmarshall;

   -------------
   -- Helpers --
   -------------

   procedure Marshall_Aux is new Generic_Marshall
     (Reply_Status_Type, Types.Unsigned_Long, Marshall);

   procedure Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Reply_Status_Type) is
   begin
      --  XXX not necessary, instantiate it in spec

      Marshall_Aux (Buffer, Val);
   end Marshall;

   procedure Marshall_Aux is new Generic_Marshall
     (Locate_Reply_Type, Types.Unsigned_Long, Marshall);

   procedure Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Locate_Reply_Type) is
   begin
      --  XXX not necessary, instantiate it in spec

      Marshall_Aux (Buffer, Val);
   end Marshall;

   function Unmarshall_Aux is new Generic_Unmarshall
     (Reply_Status_Type, Types.Unsigned_Long, Unmarshall);

   function Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Reply_Status_Type is
   begin
      --  XXX not necessary, instantiate it in spec

      return Unmarshall_Aux (Buffer);
   end Unmarshall;

   function Unmarshall_Aux is new Generic_Unmarshall
     (Locate_Reply_Type, Types.Unsigned_Long, Unmarshall);

   function Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Locate_Reply_Type is
   begin
      --  XXX not necessary, instantiate it in spec

      return Unmarshall_Aux (Buffer);
   end Unmarshall;

   -----------------------
   -- Common_Send_Reply --
   -----------------------

   procedure Common_Send_Reply
     (Sess           : access GIOP_Session;
      Request        :        Requests.Request_Access;
      MCtx           : access GIOP_Message_Context'Class;
      Error          : in out Errors.Error_Container)
   is
      use PolyORB.Annotations;
      use PolyORB.Any;
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.Errors.Helper;
      use PolyORB.Types;
      use type PolyORB.Any.TypeCode.Object;

      Buffer_Out      : Buffer_Access := new Buffer_Type;
      Header_Buffer   : Buffer_Access := new Buffer_Type;
      Header_Space    : constant Reservation :=
        Reserve (Buffer_Out, GIOP_Header_Size);
      Reply_Status    : Reply_Status_Type;
      N               : Request_Note;
      Request_Id      : Types.Unsigned_Long renames N.Id;
      CORBA_Occurence : PolyORB.Any.Any;
      Data_Alignment  : Stream_Element_Offset :=
        Sess.Implem.Data_Alignment;

   begin
      pragma Assert (Sess.Implem.Version in GIOP_V1_0 .. GIOP_V1_2);

      Get_Note (Request.Notepad, N);

      pragma Debug (O ("Process reply of request id =" & Request_Id'Img));

      if PolyORB.Any.Is_Empty (Request.Exception_Info) then
         Reply_Status := No_Exception;
         pragma Debug (O ("Sending reply, Status: " & Reply_Status'Img));

      else
         if Get_Type (Request.Exception_Info) = TC_ForwardRequest then
            Reply_Status := Location_Forward;
            pragma Debug (O ("Sending reply, Status: " & Reply_Status'Img));

         elsif Get_Type (Request.Exception_Info) = TC_ForwardRequestPerm then
            Reply_Status := Location_Forward_Perm;
            pragma Debug (O ("Sending reply, Status: " & Reply_Status'Img));

         elsif Get_Type (Request.Exception_Info) = TC_NeedsAddressingMode then
            Reply_Status := Needs_Addressing_Mode;
            pragma Debug (O ("Sending reply, Status: " & Reply_Status'Img));

         else
            declare
               Exception_Id : constant String :=
                 To_Standard_String
                 (TypeCode.Id (Get_Type (Request.Exception_Info)));

            begin
               if PolyORB.GIOP_P.Exceptions.Is_System_Exception
                 (Exception_Id)
               then
                  Reply_Status := System_Exception;

               else
                  Reply_Status := User_Exception;
               end if;

               pragma Debug
                 (O ("Sending reply, Status: " & Reply_Status'Img));
               pragma Debug (O ("Exception ID: " & Exception_Id));
            end;
         end if;
      end if;

      --  Set parameter for header request marshalling

      MCtx.Request_Id   := Request_Id;
      MCtx.Reply_Status := Reply_Status;

      --  Marshall reply header

      Marshall_GIOP_Header_Reply
        (Sess.Implem, Sess, Request, MCtx, Buffer_Out);

      case Reply_Status is
         when User_Exception | System_Exception =>
            if Reply_Status = System_Exception then
               CORBA_Occurence :=
                 PolyORB.GIOP_P.Exceptions.To_CORBA_Exception
                 (Request.Exception_Info);
               --  It is a system exception: we translate it to a GIOP
               --  specific exception occurence

            else
               CORBA_Occurence := Request.Exception_Info;
               --  It is a user exception, nothing is done.

            end if;

            Pad_Align (Buffer_Out, Sess.Implem.Data_Alignment);
            Marshall (Buffer_Out,
                      Any.TypeCode.Id
                      (Any.Get_Type (CORBA_Occurence)));
            Marshall_From_Any
              (Sess.Repr.all,
               Buffer_Out,
               Get_Container (CORBA_Occurence).all,
               Error);

            if Found (Error) then
               Replace_Marshal_5_To_Bad_Param_23 (Error, Completed_Yes);
               --  An error in the marshalling of wchar data implies
               --  the server did not provide a valid codeset service
               --  context. We convert this exception to Bad_Param 23.

               Release (Header_Buffer);
               Release (Buffer_Out);
               return;
            end if;

         when No_Exception =>
            if TypeCode.Kind (Get_Type (Request.Result.Argument)) /=
              Tk_Void then
               Pad_Align (Buffer_Out, Data_Alignment);
               Data_Alignment := 1;
            end if;

            Marshall_From_Any
              (Sess.Repr.all,
               Buffer_Out,
               Get_Container (Request.Result.Argument).all,
               Error);

            if Found (Error) then

               --  An error in the marshalling of wchar data implies
               --  the server did not provide a valid codeset service
               --  context. We convert this exception to Bad_Param 23.

               Replace_Marshal_5_To_Bad_Param_23 (Error, Completed_Yes);

               --  The error was encountered while marshalling a reply
               --  with a No_Exception status: we know that the servant
               --  executed the request succesfully.

               if Error.Member.all in System_Exception_Members then
                  System_Exception_Members (Error.Member.all).Completed :=
                    Completed_Yes;
               end if;

               Release (Header_Buffer);
               Release (Buffer_Out);
               return;
            end if;

            Marshall_Argument_List
              (Sess.Implem,
               Buffer_Out,
               Sess.Repr.all,
               Request.Args,
               PolyORB.Any.ARG_OUT,
               Data_Alignment,
               Error);

            if Found (Error) then
               Replace_Marshal_5_To_Bad_Param_23 (Error, Completed_Yes);
               --  An error in the marshalling of wchar data implies
               --  the server did not provide a valid codeset service
               --  context. We convert this exception to Bad_Param 23.

               Release (Header_Buffer);
               Release (Buffer_Out);
               return;
            end if;

         when Location_Forward =>
            declare
               Member : constant ForwardRequest_Members
                 := From_Any (Request.Exception_Info);
               Ref    : References.Ref;
            begin
               References.Set
                 (Ref, Smart_Pointers.Entity_Of (Member.Forward_Reference));

               Pad_Align (Buffer_Out, Sess.Implem.Data_Alignment);
               Marshall (Buffer_Out, Ref);
            end;

         when Location_Forward_Perm =>
            pragma Assert (Sess.Implem.Version = GIOP_V1_2);

            declare
               Member : constant ForwardRequestPerm_Members
                 := From_Any (Request.Exception_Info);
               Ref    : References.Ref;

            begin
               References.Set
                 (Ref, Smart_Pointers.Entity_Of (Member.Forward_Reference));

               Pad_Align (Buffer_Out, Sess.Implem.Data_Alignment);
               Marshall (Buffer_Out, Ref);
            end;

         when Needs_Addressing_Mode =>
            pragma Assert (Sess.Implem.Version = GIOP_V1_2);

            declare
               Member : constant NeedsAddressingMode_Members
                 := From_Any (Request.Exception_Info);
               Mode   : Short;

            begin
               case Member.Mode is
                  when Key =>
                     Mode := 0;

                  when Profile =>
                     Mode := 1;

                  when Reference =>
                     Mode := 2;
               end case;

               Pad_Align (Buffer_Out, Sess.Implem.Data_Alignment);
               Marshall (Buffer_Out, Mode);
            end;
      end case;

      --  Marshall Header

      MCtx.Message_Size := Types.Unsigned_Long
        (Length (Buffer_Out) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess, MCtx, Header_Buffer);

      --  Copy Header

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      --  Emit reply

      Emit_Message (Sess.Implem, Sess, MCtx, Buffer_Out, Error);

      Release (Buffer_Out);
      pragma Debug (O ("Reply sent"));
   end Common_Send_Reply;

   -------------------------
   -- Common_Locate_Reply --
   -------------------------

   procedure Common_Locate_Reply
     (Sess               : access GIOP_Session;
      MCtx               : access GIOP_Message_Context'Class;
      Loc_Type           : Locate_Reply_Type;
      Forward_Ref        : References.Ref;
      Error              : in out Errors.Error_Container)
   is
      use PolyORB.Components;
      use PolyORB.Types;

      Buffer        : Buffer_Access := new Buffer_Type;
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation :=
                        Reserve (Buffer, GIOP_Header_Size);

   begin
      pragma Assert (Sess.Implem.Version in GIOP_V1_0 .. GIOP_V1_2);

      pragma Debug (O ("Sending Locate Reply, Request Id :"
                       & MCtx.Request_Id'Img
                       & " , type: "
                       & Loc_Type'Img));

      Marshall (Buffer, MCtx.Request_Id);
      Marshall (Buffer, Loc_Type);

      if Loc_Type = Object_Forward then
         References.IOR.Marshall_IOR (Buffer, Forward_Ref);
      end if;

      MCtx.Message_Size :=
        Types.Unsigned_Long (Length (Buffer) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess, MCtx, Header_Buffer);

      --  Copy Header

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      Emit_Message (Sess.Implem, Sess, MCtx, Buffer, Error);
      Release (Buffer);
   end Common_Locate_Reply;

   ---------------------------------
   -- Common_Process_Locate_Reply --
   ---------------------------------

   procedure Common_Process_Locate_Reply
     (Sess              : access GIOP_Session;
      Locate_Request_Id :        Types.Unsigned_Long;
      Loc_Type          :        Locate_Reply_Type)
   is
      use type PolyORB.Utils.Strings.String_Ptr;

      ORB : constant PolyORB.ORB.ORB_Access
        := PolyORB.ORB.ORB_Access (Sess.Server);

   begin
      pragma Debug (O ("Locate Reply received, Request Id :"
                       & Locate_Request_Id'Img
                       & " , type: "
                       & Loc_Type'Img));

      case Loc_Type is
         when Object_Here | Unknown_Object =>
            declare
               use PolyORB.Errors;

               Req     : Pending_Request_Access;
               Success : Boolean;
               Error   : Errors.Error_Container;
            begin
               Get_Pending_Request_By_Locate
                 (Sess,
                  Locate_Request_Id,
                  Req,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;

               if Loc_Type /= Object_Here then
                  --  The object was no found, propagate error.

                  Throw (Error,
                         Object_Not_Exist_E,
                         System_Exception_Members'(
                                              Minor     => 1,
                                              Completed => Completed_No));

               elsif not PolyORB.References.Is_Nil (Req.Req.Target) then
                  --  The request has a non-null target, finish the
                  --  processing of the locate_reply message and send
                  --  the request.

                  Send_Request (Sess.Implem, Sess, Req, Error);

               else
                  --  Null target, no error, finish processing of the
                  --  locate_reply message.

                  PolyORB.Requests.Destroy_Request (Req.Req);

                  Remove_Pending_Request_By_Locate
                    (Sess,
                     Locate_Request_Id,
                     Success);
               end if;

               if Found (Error) then
                  Set_Exception (Req.Req, Error);
                  Catch (Error);

                  Expect_GIOP_Header (Sess);
                  Components.Emit_No_Reply
                    (Components.Component_Access (ORB),
                     Servants.Iface.Executed_Request'(Req => Req.Req));

                  Remove_Pending_Request_By_Locate
                    (Sess,
                     Locate_Request_Id,
                     Success);

                  if not Success then
                     raise GIOP_Error;
                  end if;
               else
                  Expect_GIOP_Header (Sess);
               end if;
            end;

         when Object_Forward =>
            declare
               Req     : Pending_Request_Access;
               Success : Boolean;

            begin
               Get_Pending_Request_By_Locate
                 (Sess,
                  Locate_Request_Id,
                  Req,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;

               declare
                  Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);

               begin
                  Req.Req.Exception_Info :=
                    PolyORB.Errors.Helper.To_Any
                    (PolyORB.Errors.ForwardRequest_Members'
                     (Forward_Reference => Smart_Pointers.Ref (Ref)));
               end;

               Expect_GIOP_Header (Sess);
               Components.Emit_No_Reply
                 (Components.Component_Access (ORB),
                  Servants.Iface.Executed_Request'
                  (Req => Req.Req));

               Remove_Pending_Request_By_Locate
                 (Sess,
                  Locate_Request_Id,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;
            end;

         when Object_Forward_Perm =>
            declare
               Req     : Pending_Request_Access;
               Success : Boolean;

            begin
               Get_Pending_Request_By_Locate
                 (Sess,
                  Locate_Request_Id,
                  Req,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;

               declare
                  Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);

               begin
                  Req.Req.Exception_Info :=
                    PolyORB.Errors.Helper.To_Any
                    (PolyORB.Errors.ForwardRequestPerm_Members'
                     (Forward_Reference => Smart_Pointers.Ref (Ref)));
               end;

               Expect_GIOP_Header (Sess);
               Components.Emit_No_Reply
                 (Components.Component_Access (ORB),
                  Servants.Iface.Executed_Request'
                  (Req => Req.Req));

               Remove_Pending_Request_By_Locate
                 (Sess,
                  Locate_Request_Id,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;
            end;

         when others =>
            raise GIOP_Error;
      end case;
   end Common_Process_Locate_Reply;

   ----------------------------------
   -- Common_Process_Abort_Request --
   ----------------------------------

   procedure Common_Process_Abort_Request
     (Sess  : access GIOP_Session;
      R     :        Request_Access;
      MCtx  : access GIOP_Message_Context'Class;
      Error : in out Errors.Error_Container)
   is
      use PolyORB.Annotations;
      use PolyORB.Types;

      Current_Req   : Pending_Request;
      Current_Note  : Request_Note;
      Buffer        : Buffer_Access;
      Success       : Boolean;

   begin
      pragma Assert (Sess.Implem.Version in GIOP_V1_0 .. GIOP_V1_2);

      Get_Note (R.Notepad, Current_Note);
      Get_Pending_Request (Sess, Current_Note.Id, Current_Req, Success);
      if not Success then
         raise GIOP_Error;
      end if;

      Buffer := new Buffer_Type;
      MCtx.Message_Size := Types.Unsigned_Long'Size / Types.Octet'Size;
      Marshall_Global_GIOP_Header (Sess, MCtx, Buffer);
      Marshall (Buffer, Current_Req.Request_Id);

      --  Sending the message

      Emit_Message (Sess.Implem, Sess, MCtx.all'Access, Buffer, Error);

      Release (Buffer);
   end Common_Process_Abort_Request;

   ---------------------------
   -- Common_Reply_Received --
   ---------------------------

   procedure Common_Reply_Received
     (Sess             : access GIOP_Session;
      Request_Id       : Types.Unsigned_Long;
      Reply_Status     : Reply_Status_Type;
      Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use PolyORB.Any;
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.ORB;

      Current_Req  : Pending_Request;
      Success      : Boolean;

      ORB          : constant ORB_Access := ORB_Access (Sess.Server);
      Arguments_Alignment : Buffers.Alignment_Type
        := Sess.Implem.Data_Alignment;
      Error        : Errors.Error_Container;
   begin
      pragma Assert (Sess.Implem.Version in GIOP_V1_0 .. GIOP_V1_2);

      pragma Debug (O ("Reply received: status = "
                       & Reply_Status_Type'Image (Reply_Status)
                       & ", id ="
                       & Types.Unsigned_Long'Image (Request_Id)));

      Get_Pending_Request (Sess, Request_Id, Current_Req, Success);
      if not Success then
         raise GIOP_Error;
      end if;

      Add_Reply_QoS
        (Current_Req.Req,
         GIOP_Service_Contexts,
         QoS_Parameter_Access (Service_Contexts));
      Rebuild_Reply_QoS_Parameters (Current_Req.Req);

      case Reply_Status is
         when No_Exception =>

            --  Unmarshall reply body.

            if TypeCode.Kind
              (Get_Type (Current_Req.Req.Result.Argument))
              /= Tk_Void
            then
               Align_Position (Sess.Buffer_In, Arguments_Alignment);
               Arguments_Alignment := 1;
            end if;

            Unmarshall_To_Any
              (Sess.Repr.all,
               Sess.Buffer_In,
               Get_Container (Current_Req.Req.Result.Argument).all,
               Error);

            if Found (Error) then
               Replace_Marshal_5_To_Inv_Objref_2 (Error, Completed_Yes);
               --  An error in the marshalling of wchar data implies
               --  the server did not provide a valid codeset
               --  component. We convert this exception to Inv_ObjRef 2.

               Set_Exception (Current_Req.Req, Error);
               Catch (Error);

            else
               Unmarshall_Argument_List
                 (Sess.Implem, Sess.Buffer_In, Sess.Repr.all,
                  Current_Req.Req.Args, PolyORB.Any.ARG_OUT,
                  Arguments_Alignment, Error);

               if Found (Error) then
                  Replace_Marshal_5_To_Inv_Objref_2 (Error, Completed_Yes);
                  --  An error in the marshalling of wchar data implies
                  --  the server did not provide a valid codeset
                  --  component. We convert this exception to Inv_ObjRef 2.

                  Set_Exception (Current_Req.Req, Error);
                  Catch (Error);
               end if;
            end if;

            Expect_GIOP_Header (Sess);
            Emit_No_Reply
              (Current_Req.Req.Requesting_Component,
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));

         when System_Exception =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            Unmarshall_System_Exception_To_Any
              (Sess.Buffer_In, Sess.Repr.all, Current_Req.Req.Exception_Info);

            Expect_GIOP_Header (Sess);
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));

         when User_Exception =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);
            declare
               use PolyORB.Types;

               RepositoryId : constant PolyORB.Types.RepositoryId
                 := Unmarshall (Sess.Buffer_In);
               Except_Index : constant PolyORB.Types.Unsigned_Long
                 := Any.ExceptionList.Search_Exception_Id
                 (Current_Req.Req.Exc_List, Types.String (RepositoryId));
            begin
               pragma Debug (O ("Exception repository ID:"
                                & To_Standard_String (RepositoryId)));
               if Except_Index = 0 then
                  declare
                     --  Received an unexpected exception: we'll
                     --  have to conjure up a minimal exception
                     --  TypeCode to get at least the repo. ID
                     --  right. Note that we cannot map exceptions
                     --  that are not in Exc_List to 'unknown',
                     --  because the applicative personality above
                     --  us may be able to do something with an
                     --  unknown exception.

                     --  Actually we could be more clever here
                     --  and ask the ORB to provide a TypeCode
                     --  (maybe by querying an application
                     --  personality or interface repository
                     --  for information about this repository ID),
                     --  which would even allow us to unmarshall
                     --  the valuation of an unknown exception.

                     Exception_Name : constant String
                       := Exceptions.Exception_Name
                       (To_Standard_String (RepositoryId));
                     Slash, Next_Slash : Integer;

                     TC : Any.TypeCode.Object := TypeCode.TC_Except;
                  begin
                     Slash := Exception_Name'First - 1;
                     loop
                        Next_Slash := Utils.Find
                          (Exception_Name, Slash + 1, '/');
                        exit when Next_Slash > Exception_Name'Last;
                        pragma Assert (Next_Slash > Slash);
                        Slash := Next_Slash;
                     end loop;
                     if Slash = Exception_Name'First - 1 then
                        Slash := Slash + 1;
                     end if;

                     TypeCode.Add_Parameter
                       (TC, To_Any (To_PolyORB_String
                                    (Exception_Name
                                     (Slash .. Exception_Name'Last))));
                     TypeCode.Add_Parameter
                       (TC, To_Any (Types.String (RepositoryId)));
                     Current_Req.Req.Exception_Info
                       := PolyORB.Any.Get_Empty_Any_Aggregate (TC);
                  end;
               else
                  Current_Req.Req.Exception_Info
                    := PolyORB.Any.Get_Empty_Any
                    (Any.ExceptionList.Item
                     (Current_Req.Req.Exc_List, Except_Index));
                  Unmarshall_To_Any
                    (Sess.Repr.all,
                     Sess.Buffer_In,
                     Get_Container (Current_Req.Req.Exception_Info).all,
                     Error);

                  if Found (Error) then
                     Replace_Marshal_5_To_Inv_Objref_2 (Error, Completed_Yes);
                     --  An error in the marshalling of wchar data implies
                     --  the server did not provide a valid codeset
                     --  component. We convert this exception to Inv_ObjRef 2.

                     Set_Exception (Current_Req.Req, Error);
                     Catch (Error);
                  end if;

                  pragma Debug
                    (O ("Exception: "
                        & Any.Image (Current_Req.Req.Exception_Info)));
               end if;

               Expect_GIOP_Header (Sess);
               Emit_No_Reply
                 (Component_Access (ORB),
                  Servants.Iface.Executed_Request'
                  (Req => Current_Req.Req));
            end;

         when Location_Forward =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            declare
               Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);
            begin
               Current_Req.Req.Exception_Info :=
                 PolyORB.Errors.Helper.To_Any
                 (PolyORB.Errors.ForwardRequest_Members'
                  (Forward_Reference => Smart_Pointers.Ref (Ref)));
            end;

            Expect_GIOP_Header (Sess);
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));

         when Location_Forward_Perm =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            declare
               Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);
            begin
               Current_Req.Req.Exception_Info :=
                 PolyORB.Errors.Helper.To_Any
                 (PolyORB.Errors.ForwardRequestPerm_Members'
                  (Forward_Reference => Smart_Pointers.Ref (Ref)));
            end;

            Expect_GIOP_Header (Sess);
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));

         when Needs_Addressing_Mode =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            declare
               Mode    : constant Types.Short := Unmarshall (Sess.Buffer_In);
               Members : PolyORB.Errors.NeedsAddressingMode_Members;

            begin
               case Mode is
                  when 0 =>
                     Members.Mode := Key;

                  when 1 =>
                     Members.Mode := Profile;

                  when 2 =>
                     Members.Mode := Reference;

                  when others =>
                     raise Program_Error;
               end case;

               Current_Req.Req.Exception_Info :=
                 PolyORB.Errors.Helper.To_Any (Members);
            end;

            Expect_GIOP_Header (Sess);
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));
      end case;
   end Common_Reply_Received;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Buf_In  : PolyORB.Buffers.Buffer_Access;
      Buf_Out : PolyORB.Buffers.Buffer_Access;
      Count   : Types.Unsigned_Long)
   is
      Temp :  Types.Octet;

   begin
      for K in 1 .. Count loop
         Temp := Unmarshall (Buf_In);
         Marshall (Buf_Out, Temp);
      end loop;
   end Copy;

   ---------------------------------------
   -- Replace_Marshal_5_To_Bad_Param_23 --
   ---------------------------------------

   procedure Replace_Marshal_5_To_Bad_Param_23
     (Error  : in out Errors.Error_Container;
      Status : Errors.Completion_Status)
   is
      use PolyORB.Errors;
      use type Types.Unsigned_Long;
   begin
      if Error.Kind = Marshal_E
        and then System_Exception_Members'Class (Error.Member.all).Minor = 5
      then
         Error.Kind := Bad_Param_E;
         System_Exception_Members'Class (Error.Member.all).Minor := 23;
         System_Exception_Members'Class (Error.Member.all).Completed := Status;
      end if;
   end Replace_Marshal_5_To_Bad_Param_23;

   ---------------------------------------
   -- Replace_Marshal_5_To_Inv_Objref_2 --
   ---------------------------------------

   procedure Replace_Marshal_5_To_Inv_Objref_2
     (Error  : in out Errors.Error_Container;
      Status : Errors.Completion_Status)
   is
      use PolyORB.Errors;
      use type Types.Unsigned_Long;
   begin
      if Error.Kind = Marshal_E
        and then System_Exception_Members'Class (Error.Member.all).Minor = 5
      then
         Error.Kind := Inv_Objref_E;
         System_Exception_Members'Class (Error.Member.all).Minor := 2;
         System_Exception_Members'Class (Error.Member.all).Completed := Status;
      end if;
   end Replace_Marshal_5_To_Inv_Objref_2;

end PolyORB.Protocols.GIOP.Common;
