------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P R O T O C O L S . G I O P . C O M M O N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any.ExceptionList;
with PolyORB.Buffers;
with PolyORB.Exceptions;
with PolyORB.GIOP_P.Exceptions;
with PolyORB.Log;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR;
with PolyORB.Servants.Interface;
with PolyORB.Smart_Pointers;

package body PolyORB.Protocols.GIOP.Common is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Representations.CDR;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.giop.common");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

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

   --------------------------
   -- Common_Process_Reply --
   --------------------------

   procedure Common_Process_Reply
     (Sess           : access GIOP_Session;
      Request        :        Requests.Request_Access;
      Request_Id_Ptr : access Types.Unsigned_Long;
      Reply_Stat_Ptr : access Reply_Status_Type)
   is
      use PolyORB.Annotations;
      use PolyORB.Any;
      use PolyORB.Buffers;
      use PolyORB.Components;
      use PolyORB.Exceptions;
      use PolyORB.Representations.CDR;
      use PolyORB.Types;
      use type PolyORB.Any.TypeCode.Object;

      Buffer_Out      : Buffer_Access := new Buffer_Type;
      Header_Buffer   : Buffer_Access := new Buffer_Type;
      Header_Space    : constant Reservation :=
        Reserve (Buffer_Out, GIOP_Header_Size);
      Reply_Status    : Reply_Status_Type;
      Except_Id       : Types.String;
      N               : Request_Note;
      Request_Id      : Types.Unsigned_Long renames N.Id;
      CORBA_Occurence : PolyORB.Any.Any;
      Data_Alignment  : Stream_Element_Offset :=
        Sess.Implem.Data_Alignment;

   begin
      pragma Assert ((Sess.Implem.Version = GIOP_Version'(1, 0)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 1)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 2)));

      Get_Note (Request.Notepad, N);

      pragma Debug (O ("Process reply of request id =" & Request_Id'Img));

      if PolyORB.Any.Is_Empty (Request.Exception_Info) then
         Reply_Status := No_Exception;
         pragma Debug (O ("Sending reply, Status : " & Reply_Status'Img));

      else
         if Get_Type (Request.Exception_Info) = TC_ForwardRequest then
            Reply_Status := Location_Forward;
            pragma Debug (O ("Sending reply, Status : " & Reply_Status'Img));

         else
            declare
               Except_Id_2 : constant String :=
                 To_Standard_String
                 (TypeCode.Id (Get_Type (Request.Exception_Info)));
            begin
               if PolyORB.GIOP_P.Exceptions.Is_System_Exception
                 (Except_Id_2) then
                  Reply_Status := System_Exception;
               else
                  Reply_Status := User_Exception;
               end if;

               Except_Id := To_PolyORB_String (Except_Id_2);

               pragma Debug
                 (O ("Sending reply, Status : " & Reply_Status'Img));
               pragma Debug (O ("Exception ID : " & Except_Id_2));
            end;
         end if;
      end if;

      --  Set parameter for header request marshalling

      Request_Id_Ptr.all := Request_Id;
      Reply_Stat_Ptr.all := Reply_Status;

      --  Marshall reply header

      Marshall_GIOP_Header_Reply (Sess.Implem, Sess, Request, Buffer_Out);

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
            Marshall_From_Any (Buffer_Out, CORBA_Occurence);

         when No_Exception =>
            if TypeCode.Kind (Get_Type (Request.Result.Argument)) /=
              Tk_Void then
               Pad_Align (Buffer_Out, Data_Alignment);
               Data_Alignment := 1;
            end if;

            Marshall_From_Any (Buffer_Out, Request.Result.Argument);

            Marshall_Argument_List
              (Sess.Implem,
               Buffer_Out,
               Request.Args,
               PolyORB.Any.ARG_OUT,
               Data_Alignment);

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

         when others =>
            raise Not_Implemented;
      end case;

      --  Marshall Header

      Sess.Ctx.Message_Size := Types.Unsigned_Long
        (Length (Buffer_Out) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess, Header_Buffer);

      --  Copy Header

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      --  Emit reply

      Emit_Message (Sess.Implem, Sess, Buffer_Out);

      Release (Buffer_Out);
      pragma Debug (O ("Reply sent"));
   end Common_Process_Reply;

   -------------------------
   -- Common_Locate_Reply --
   -------------------------

   procedure Common_Locate_Reply
     (Sess        : access GIOP_Session;
      Request_Id  :        Types.Unsigned_Long;
      Loc_Type    :        Locate_Reply_Type;
      Forward_Ref :        References.Ref)
   is
      use PolyORB.Components;
      use PolyORB.Types;

      Buffer        : Buffer_Access := new Buffer_Type;
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation :=
        Reserve (Buffer, GIOP_Header_Size);
   begin
      pragma Assert ((Sess.Implem.Version = GIOP_Version'(1, 0)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 1)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 2)));

      pragma Debug (O ("Sending Locate Reply, Request Id :"
                       & Request_Id'Img
                       & " , type : "
                       & Loc_Type'Img));

      Marshall (Buffer, Request_Id);
      Marshall (Buffer, Loc_Type);

      if Loc_Type = Object_Forward then
         References.IOR.Marshall_IOR (Buffer, Forward_Ref);
      end if;

      Sess.Ctx.Message_Size :=
        Types.Unsigned_Long (Length (Buffer) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess, Header_Buffer);

      --  Copy Header

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      Emit_Message (Sess.Implem, Sess, Buffer);
      Release (Buffer);
   end Common_Locate_Reply;

   ---------------------------------
   -- Common_Process_Locate_Reply --
   ---------------------------------

   procedure Common_Process_Locate_Reply
     (Sess       : access GIOP_Session;
      Request_Id :        Types.Unsigned_Long;
      Loc_Type   :        Locate_Reply_Type) is
   begin
      pragma Debug (O ("Locate Reply received, Request Id :"
                       & Request_Id'Img
                       & " , type : "
                       & Loc_Type'Img));

      case Loc_Type is
         when Object_Here =>
            declare
               Req     : Pending_Request_Access;
               Success : Boolean;
            begin
               Get_Pending_Request_By_Locate
                 (Sess,
                  Request_Id,
                  Req,
                  Success);
               if Success then
                  Send_Request (Sess.Implem, Sess, Req);
               end if;
            end;

         when Object_Forward =>
            declare
               ORB     : constant PolyORB.ORB.ORB_Access
                 := PolyORB.ORB.ORB_Access (Sess.Server);
               Req     : Pending_Request_Access;
               Success : Boolean;

            begin
               Get_Pending_Request_By_Locate
                 (Sess,
                  Request_Id,
                  Req,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;

               declare
                  Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);

               begin
                  Req.Req.Exception_Info :=
                    PolyORB.Exceptions.To_Any
                    (PolyORB.Exceptions.ForwardRequest_Members'
                     (Forward_Reference => Smart_Pointers.Ref (Ref)));
               end;

               Components.Emit_No_Reply
                 (Components.Component_Access (ORB),
                  Servants.Interface.Executed_Request'
                  (Req => Req.Req));

               Remove_Pending_Request_By_Locate
                 (Sess,
                  Request_Id,
                  Success);

               if not Success then
                  raise GIOP_Error;
               end if;
            end;

         when others =>
            raise GIOP_Error;
      end case;

      Expect_GIOP_Header (Sess);
   end Common_Process_Locate_Reply;

   ----------------------------------
   -- Common_Process_Abort_Request --
   ----------------------------------

   procedure Common_Process_Abort_Request
     (Sess : access GIOP_Session;
      R    : in     Request_Access)
   is
      use PolyORB.Annotations;
      use PolyORB.Types;

      Current_Req   : Pending_Request;
      Current_Note  : Request_Note;
      Buffer        : Buffer_Access;
      Success       : Boolean;
   begin
      pragma Assert ((Sess.Implem.Version = GIOP_Version'(1, 0)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 1)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 2)));

      Get_Note (R.Notepad, Current_Note);
      Get_Pending_Request (Sess, Current_Note.Id, Current_Req, Success);
      if not Success then
         raise GIOP_Error;
      end if;

      Buffer := new Buffer_Type;

      Sess.Ctx.Message_Size := Types.Unsigned_Long'Size / Types.Octet'Size;
      Marshall_Global_GIOP_Header (Sess, Buffer);

      Marshall (Buffer, Current_Req.Request_Id);

      --  Sending the message

      Emit_Message (Sess.Implem, Sess, Buffer);

      Release (Buffer);
   end Common_Process_Abort_Request;


   ---------------------------
   -- Common_Reply_Received --
   ---------------------------

   procedure Common_Reply_Received
     (Sess         : access GIOP_Session;
      Request_Id   : in     Types.Unsigned_Long;
      Reply_Status : in     Reply_Status_Type)
   is
      use PolyORB.Any;
      use PolyORB.ORB;
      use PolyORB.Components;

      Current_Req  : Pending_Request;
      Success      : Boolean;

      ORB          : constant ORB_Access := ORB_Access (Sess.Server);
      Arguments_Alignment : Buffers.Alignment_Type
        := Sess.Implem.Data_Alignment;
   begin
      pragma Assert ((Sess.Implem.Version = GIOP_Version'(1, 0)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 1)) or
                     (Sess.Implem.Version = GIOP_Version'(1, 2)));

      pragma Debug (O ("Reply received: status = "
                       & Reply_Status_Type'Image (Reply_Status)
                       & ", id ="
                       & Types.Unsigned_Long'Image (Request_Id)));

      Get_Pending_Request (Sess, Request_Id, Current_Req, Success);
      if not Success then
         raise GIOP_Error;
      end if;

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
              (Sess.Buffer_In, Current_Req.Req.Result.Argument);

            Unmarshall_Argument_List
              (Sess.Implem, Sess.Buffer_In, Current_Req.Req.Args,
               PolyORB.Any.ARG_OUT, Arguments_Alignment);

            Emit_No_Reply
              (Current_Req.Req.Requesting_Component,
               Servants.Interface.Executed_Request'
               (Req => Current_Req.Req));

         when System_Exception =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            Unmarshall_System_Exception_To_Any
              (Sess.Buffer_In, Current_Req.Req.Exception_Info);
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Interface.Executed_Request'
               (Req => Current_Req.Req));

         when User_Exception =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);
            declare
               use PolyORB.Types;
               use PolyORB.Exceptions;

               RepositoryId : constant PolyORB.Types.String
                 := Unmarshall (Sess.Buffer_In);
               Except_Index : constant PolyORB.Types.Unsigned_Long
                 := Any.ExceptionList.Search_Exception_Id
                 (Current_Req.Req.Exc_List, RepositoryId);
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
                       (TC, To_Any (RepositoryId));
                     Current_Req.Req.Exception_Info
                       := PolyORB.Any.Get_Empty_Any_Aggregate (TC);
                  end;
               else
                  Current_Req.Req.Exception_Info
                    := PolyORB.Any.Get_Empty_Any
                    (Any.ExceptionList.Item
                     (Current_Req.Req.Exc_List, Except_Index));
                  Unmarshall_To_Any
                    (Sess.Buffer_In,
                     Current_Req.Req.Exception_Info);
                  pragma Debug
                    (O ("Exception: "
                        & Any.Image (Current_Req.Req.Exception_Info)));
               end if;
               Emit_No_Reply
                 (Component_Access (ORB),
                  Servants.Interface.Executed_Request'
                  (Req => Current_Req.Req));
            end;

         when Location_Forward =>
            Align_Position (Sess.Buffer_In, Sess.Implem.Data_Alignment);

            declare
               Ref : constant References.Ref := Unmarshall (Sess.Buffer_In);
            begin
               Current_Req.Req.Exception_Info :=
                 PolyORB.Exceptions.To_Any
                 (PolyORB.Exceptions.ForwardRequest_Members'
                  (Forward_Reference => Smart_Pointers.Ref (Ref)));
            end;

            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Interface.Executed_Request'
               (Req => Current_Req.Req));

         when others =>
            raise Not_Implemented;
      end case;

      Expect_GIOP_Header (Sess);
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

end PolyORB.Protocols.GIOP.Common;
