--  A protocol similar to the HTTP protocol
--  SRP : Simple Request Protocol

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Buffers;
with PolyORB.Objects;
--  with PolyORB.ORB;
with PolyORB.Types;
with PolyORB.Utils.SRP;

package PolyORB.Protocols.SRP is
   pragma Elaborate_Body;

   use PolyORB.Buffers;
   use PolyORB.Objects;
   use PolyORB.Utils.SRP;
--   use PolyORB.ORB;

   SRP_Error : exception;

   type SRP_Protocol is new Protocol with private;

   --  Message types that can be used with SRP
   type Msg_Type is
     (Req,
      Reply);

   type Reply_Status_Type is
     (Ack,
      Error);

   procedure Create
     (Proto   : access SRP_Protocol;
      Session : out Filter_Access);

   type SRP_Session is new Session with private;

   procedure Connect (S : access SRP_Session);
   procedure Invoke_Request (S : access SRP_Session; R : Request_Access);
   procedure Abort_Request (S : access SRP_Session; R :  Request_Access);
   --  Do nothing.

   procedure Send_Reply (S : access SRP_Session; R :  Request_Access);
   --  Send a reply to the user.

   procedure Handle_Connect_Indication (S : access SRP_Session);
   --  Send a greeting banner to user.

   procedure Handle_Connect_Confirmation (S : access SRP_Session);
   --  Setup client dialog.

   procedure Handle_Data_Indication (S : access SRP_Session);
   --  Handle data received from user.

   procedure Handle_Disconnect (S : access SRP_Session);
   --  Handle disconnection from user.

   procedure Unmarshall_Request_Message (Buffer : access Buffer_Type;
                                         Oid    : access Object_Id;
                                         Method : access Types.String);
--                                          Oid    : out Objects.Object_Id;
--                                          Method : out Types.String);
   --  Get from the buffer the Object_Id and the Method to be called

   procedure Unmarshall_Args (Buffer : access Buffer_Type;
                              Args   : in out Any.NVList.Ref);
   --  Unmarshall the arguments from Buffer in Args
   --  Args must be an arg list with empty any(s), but with their type set

   procedure Unmarshall (Args : in out Any.NVList.Ref; Info_SRP : Split_SRP);
   --  Get the values stored in Info_SRP and unmarshall them in Args
   --  Attention: Args already should contain the types
   --  (cf. Obj_Adapters.Get_Empty_Arg_List)

private

   type SRP_Protocol is new Protocol with null record;

   type SRP_Session is new Session with record
--      Mess_Type_Received : Msg_Type;
--      Role               : ORB.Endpoint_Role := Client;
      Buffer_In          : Buffers.Buffer_Access;
      Buffer_Out         : Buffers.Buffer_Access;
   end record;

end PolyORB.Protocols.SRP;
