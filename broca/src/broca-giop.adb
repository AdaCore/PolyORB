with Broca.Refs;
with Broca.Marshalling;
with Broca.Exceptions;
with Broca.Flags;
with Broca.Sequences;
with Broca.Orb;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Giop is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   procedure O is new Broca.Debug.Output (Flag);

   procedure Create_Giop_Header
     (Stream : in out Buffer_Descriptor;
      Message_Type : CORBA.Unsigned_Long;
      Message_Size : CORBA.Unsigned_Long)
   is
      use Broca.Marshalling;
   begin
      Stream.Pos := 0;
      Stream.Little_Endian := Is_Little_Endian;

      --  1.2.1 The message header.
      --  Magic
      Marshall (Stream, 'G');
      Marshall (Stream, 'I');
      Marshall (Stream, 'O');
      Marshall (Stream, 'P');

      --  Version
      Marshall (Stream, CORBA.Octet'(1));
      Marshall (Stream, CORBA.Octet'(0));

      --  Byte order
      Marshall (Stream, Is_Little_Endian);

      --  message type
      Marshall (Stream, CORBA.Octet (Message_Type));

      --  message size
      Marshall (Stream, Message_Size);

      --  Internal check.
      if Stream.Pos /= Message_Header_Size then
         Broca.Exceptions.Raise_Internal (2000, CORBA.Completed_No);
      end if;
   end Create_Giop_Header;

   procedure Create_Reply_System_Exception
     (Stream : in out Buffer_Descriptor;
      Request_Id : CORBA.Unsigned_Long;
      Occurence : CORBA.Exception_Occurrence)
   is
      use Broca.Marshalling;
   begin
      Stream.Pos := Message_Header_Size;
      --  service context.
      Marshall_Size_Unsigned_Long (Stream);
      --  request_id
      Marshall_Size_Unsigned_Long (Stream);
      --  reply_status
      Marshall_Size_Unsigned_Long (Stream);
      --  Exception
      Broca.Exceptions.Marshall_Size (Stream, Occurence);

      Allocate_Buffer_And_Set_Pos (Stream, Stream.Pos);

      Broca.Giop.Create_Giop_Header
        (Stream, Broca.Giop.Reply,
         CORBA.Unsigned_Long (Stream.Pos));
      --  service context.
      Marshall (Stream, CORBA.Unsigned_Long (No_Context));
      --  Request_id
      Marshall (Stream, Request_Id);
      --  reply_status
      Marshall (Stream, Broca.Giop.System_Exception);
      --  exception.
      Broca.Exceptions.Marshall (Stream, Occurence);
   end Create_Reply_System_Exception;

   procedure Create_Reply_Location_Forward
     (Stream : in out Buffer_Descriptor;
      Request_Id : CORBA.Unsigned_Long;
      Reference : CORBA.Object.Ref)
   is
      use Broca.Marshalling;
      N_Ref : Buffer_Descriptor;
   begin
      N_Ref := Broca.Refs.Object_To_IOR (CORBA.Object.Get (Reference).all);
      Stream.Pos := Message_Header_Size;

      --  service context.
      Marshall_Size_Unsigned_Long (Stream);
      --  request_id
      Marshall_Size_Unsigned_Long (Stream);
      --  reply_status
      Marshall_Size_Unsigned_Long (Stream);
      --  IOR
      Compute_Size (Stream, N_Ref);

      Allocate_Buffer_And_Set_Pos (Stream, Stream.Pos);

      Broca.Giop.Create_Giop_Header
        (Stream, Broca.Giop.Reply,
         CORBA.Unsigned_Long (Stream.Pos));
      --  service context.
      Marshall (Stream, CORBA.Unsigned_Long (No_Context));
      --  Request_id
      Marshall (Stream, Request_Id);
      --  reply_status
      Marshall (Stream, Broca.Giop.Location_Forward);
      --  exception.
      Append_Buffer (Stream, N_Ref);
   end Create_Reply_Location_Forward;

   Nobody_Principal : constant CORBA.String :=
     CORBA.To_CORBA_String ("nobody");

   procedure Send_Request_Size (Handler : in out Request_Handler;
                                Object : Broca.Object.Object_Acc;
                                Operation : CORBA.Identifier)
   is
      use Broca.Marshalling;
   begin
      if Handler.Nbr_Tries > Broca.Flags.Max_Tries then
         Broca.Exceptions.Raise_Inv_Objref;
      else
         Handler.Nbr_Tries := Handler.Nbr_Tries + 1;
      end if;

      --  1. Send a GIOP message.
      Handler.Profile := Broca.Object.Find_Profile (Object);
      Handler.Connection := Broca.Object.Find_Connection (Handler.Profile);

      --  1.1: compute the size of the message

      --  1.1.1: size of GIOP header.
      Handler.Buffer.Pos := Broca.Giop.Message_Header_Size;

      --  1.1.2: Size of request header.
      Handler.Buffer.Pos := Handler.Buffer.Pos
        + 4 --  service_context.  Not yet supported
        + 4 --  request_id
        + 4; --  reponse_expected + reserved
      --  Size of object_key.
      Marshall_Size_Primitive_Sequence
        (Handler.Buffer, 1,
         Broca.Sequences.IDL_SEQUENCE_Octet.Length
         (Broca.Object.Get_Object_Key (Handler.Profile.all)));
      --  Size of operation
      Marshall_Size (Handler.Buffer, CORBA.String (Operation));
      --  Size of Principal
      --  See 13.3.4: encoded as sequence <octet>
      Marshall_Size (Handler.Buffer, Nobody_Principal);
   end Send_Request_Size;

   procedure Send_Request_Marshall (Handler : in out Request_Handler;
                                    Reponse_Expected : Boolean;
                                    Operation : CORBA.Identifier)
   is
      use Broca.Marshalling;
      Message_Size : CORBA.Unsigned_Long;
   begin
      pragma Debug (O ("Send_Request_Marshall : enter"));
      Message_Size := CORBA.Unsigned_Long
        (Handler.Buffer.Pos - Broca.Giop.Message_Header_Size);
      pragma Debug (O ("Send_Request_Marshall : Message_Size = " & Message_Size'Img));
      Allocate_Buffer (Handler.Buffer);

      --  1.2 marshall the request.
      --  1.2.1 The message header.
      Broca.Giop.Create_Giop_Header
        (Handler.Buffer, Broca.Giop.Request, Message_Size);

      --  1.2.2 The request message header.
      --  service context.
      Marshall (Handler.Buffer, CORBA.Unsigned_Long (No_Context));

      --  request_id
      Handler.Request_Id := Broca.Object.Get_Request_Id (Handler.Connection);
      Marshall (Handler.Buffer, Handler.Request_Id);

      --  response expected
      Marshall (Handler.Buffer, Reponse_Expected);

      --  object key
      Broca.Sequences.Marshall
        (Handler.Buffer, Broca.Object.Get_Object_Key (Handler.Profile.all));

      --  Operation
      Marshall (Handler.Buffer, CORBA.String (Operation));

      --  principal
      Marshall (Handler.Buffer, Nobody_Principal);
   end Send_Request_Marshall;

   procedure Send_Request_Send (Handler : in out Request_Handler;
                                Object : Broca.Object.Object_Acc;
                                Reponse_Expected : Boolean;
                                Res : out Send_Request_Result_Type)
   is
      use Broca.Marshalling;
      use CORBA;
      Message_Type    : CORBA.Octet;
      Message_Size    : CORBA.Unsigned_Long;
      Service_Context : CORBA.Unsigned_Long;
      Reply_Status    : CORBA.Unsigned_Long;
      Request_Id      : CORBA.Unsigned_Long;
      Tmp : Buffer_Descriptor;
   begin
      --  1.3 Send request.
      Broca.Object.Send (Handler.Connection, Handler.Buffer);

      if not Reponse_Expected then
         Broca.Object.Release_Connection (Handler.Connection);
         Res := Sr_No_Reply;
         return;
      end if;

      --  1.4 Receive reply
      --  1.4.1 the message header
      Allocate_Buffer_And_Set_Pos
         (Handler.Buffer, Broca.Giop.Message_Header_Size);
      Broca.Object.Receive (Handler.Connection, Handler.Buffer);
      if Handler.Buffer.Pos /= Broca.Giop.Message_Header_Size then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  1.4.2 Check magic, giop version
      --  FIXME: todo

      --  1.4.3 decode byte_order.
      Handler.Buffer.Pos := 6;
      Unmarshall (Handler.Buffer, Handler.Buffer.Little_Endian);

      --  1.4.3 check message type.
      Unmarshall (Handler.Buffer, Message_Type);
      if Message_Type /= CORBA.Octet (Broca.Giop.Reply) then
         --  FIXME: check for closeconnection or messageerror.
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  1.4.4 message_size
      Unmarshall (Handler.Buffer, Message_Size);

      --  Allocate enough bytes for the message.
      Allocate_Buffer_And_Set_Pos
        (Tmp, Buffer_Index_Type (Message_Size));

      --  1.4.5 Receive the reply header and body.
      Broca.Object.Receive (Handler.Connection, Tmp);
      Broca.Object.Release_Connection (Handler.Connection);

      Allocate_Buffer_And_Set_Pos
        (Handler.Buffer,
         Buffer_Index_Type (Message_Size + Message_Header_Size));
      Handler.Buffer.Buffer (Message_Header_Size .. Handler.Buffer.Pos - 1)
        := Tmp.Buffer.all;
      Handler.Buffer.Pos := Message_Header_Size;
      Free (Tmp.Buffer);

      --  service_context.
      Unmarshall (Handler.Buffer, Service_Context);
      if Service_Context /= No_Context then
         pragma Debug (O ("Send_Request_Send : incorrect context" & Service_Context'Img));
         raise Program_Error;
      end if;

      --  Request id
      Unmarshall (Handler.Buffer, CORBA.Unsigned_Long (Request_Id));
      if Request_Id /= Handler.Request_Id then
         pragma Debug (O ("Send_Request_Send : incorrect request id" & Request_Id'Img));
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  reply status.
      Unmarshall (Handler.Buffer, Reply_Status);
      case Reply_Status is
         when Broca.Giop.No_Exception =>
            Res := Sr_Reply;
            return;
         when Broca.Giop.System_Exception =>
            Broca.Exceptions.Unmarshall_And_Raise (Handler.Buffer);
         when Broca.Giop.Location_Forward =>
            declare
               New_Ref : CORBA.Object.Ref;
            begin
               Broca.Orb.IOR_To_Object (Handler.Buffer, New_Ref);
               --  FIXME: check type, use a lock ?
               Object.Profiles :=
                 Broca.Object.Object_Acc (CORBA.Object.Get (New_Ref)).Profiles;
            end;
            Res := Sr_Forward;
            return;
         when Broca.Giop.User_Exception =>
            Res := Sr_User_Exception;
            return;
         when others =>
            raise Program_Error;
      end case;
   end Send_Request_Send;
end Broca.Giop;
