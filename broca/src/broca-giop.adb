with Broca.Refs;
with Broca.Marshalling;  use Broca.Marshalling;
with Broca.Exceptions;
with Broca.Flags;
with Broca.Sequences;
with Broca.ORB;
with Broca.Buffers;      use Broca.Buffers;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.GIOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   procedure O is new Broca.Debug.Output (Flag);

   Magic : constant Buffer_Type :=
     (Character'Pos ('G'),
      Character'Pos ('I'),
      Character'Pos ('O'),
      Character'Pos ('P'));

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 0;

   Nobody_Principal : constant CORBA.String :=
     CORBA.To_CORBA_String ("nobody");

   MsgType_To_Octet :
     constant array (MsgType'Range) of CORBA.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (ReplyStatusType'Range) of CORBA.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3);

   LocateStatusType_To_Unsigned_Long :
     constant array (LocateStatusType'Range) of CORBA.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2);

   Octet_To_MsgType :
     constant array (CORBA.Octet range 0 .. 7) of MsgType
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 3) of ReplyStatusType
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward);

   Unsigned_Long_To_LocateStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 2) of LocateStatusType
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward);

   ------------------------------
   -- Compute_GIOP_Header_Size --
   ------------------------------

   procedure Compute_GIOP_Header_Size
     (Buffer : in out Buffer_Descriptor) is
   begin
      Allocate_Buffer_And_Clear_Pos (Buffer, 0);
      Compute_New_Size (Buffer, O_Size, Message_Header_Size);
   end Compute_GIOP_Header_Size;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Buffer       : in out Buffer_Descriptor;
      Message_Type : in MsgType)
   is
      use Broca.Marshalling;
      Message_Size : Buffer_Index_Type;
   begin
      Allocate_Buffer (Buffer);

      Message_Size := Full_Size (Buffer) - Message_Header_Size;
      --  1.2.1 The message header.
      --  Magic
      Write (Buffer, Magic);

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Byte order
      Marshall (Buffer, Is_Little_Endian);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long (Message_Size));
   end Marshall_GIOP_Header;

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Buffer       : in out Buffer_Descriptor;
      Message_Type          : out MsgType;
      Message_Size          : out CORBA.Unsigned_Long;
      Success               : out Boolean)
   is
      use Broca.Marshalling;
      Magic_Num : Buffer_Type := Magic;
      Message_Major_Version : CORBA.Octet;
      Message_Minor_Version : CORBA.Octet;
      Endianess : CORBA.Boolean;
   begin
      Success := False;

      --  Magic
      Read (Buffer, Magic_Num);
      if Magic_Num /= Magic then
         return;
      end if;

      Unmarshall (Buffer, Message_Major_Version);
      Unmarshall (Buffer, Message_Minor_Version);
      if not CORBA."=" (Message_Major_Version, Major_Version)
        or else CORBA."<" (Minor_Version, Message_Minor_Version) then
         return;
      end if;

      --  Byte order
      Unmarshall (Buffer, Endianess);
      Set_Endianess (Buffer, Endianess);

      --  Message type
      Unmarshall (Buffer, Message_Type);

      --  Message size
      Unmarshall (Buffer, Message_Size);

      Success := True;
   end Unmarshall_GIOP_Header;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer     : in out Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence)
   is
      use Broca.Marshalling;
   begin
      --  Service context
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  Request id
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  Reply status
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  Exception
      Broca.Exceptions.Compute_New_Size (Buffer, Occurence);
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer     : in out Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence)
   is
      use Broca.Marshalling;
   begin
      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply status
      Marshall (Buffer, Broca.GIOP.System_Exception);

      --  Exception
      Broca.Exceptions.Marshall (Buffer, Occurence);
   end Marshall;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer     : in out Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref)
   is
      use Broca.Marshalling;
   begin
      --  Service context
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  Request id
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  Reply status
      Compute_New_Size (Buffer, UL_Size, UL_Size);

      --  IOR
      Broca.Refs.Compute_New_Size
        (Buffer, Broca.Refs.Ref (Reference));
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer     : in out Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref)
   is
      use Broca.Marshalling;
   begin
      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply status
      Marshall (Buffer, Broca.GIOP.Location_Forward);

      --  Reference
      Broca.Refs.Marshall
        (Buffer, Broca.Refs.Ref (Reference));
   end Marshall;

   -----------------------
   -- Send_Request_Size --
   -----------------------

   procedure Send_Request_Size
     (Handler   : in out Request_Handler;
      Target    : in Object.Object_Ptr;
      Operation : in CORBA.Identifier)
   is
      use Broca.Marshalling;
   begin
      if Handler.Nbr_Tries > Broca.Flags.Max_Tries then
         Broca.Exceptions.Raise_Inv_Objref;
      else
         Handler.Nbr_Tries := Handler.Nbr_Tries + 1;
      end if;

      --  1. Send a GIOP message.
      Handler.Profile := Object.Find_Profile (Target);
      Handler.Connection := IOP.Find_Connection (Handler.Profile);

      Compute_GIOP_Header_Size (Handler.Buffer);

      --  Service context
      Compute_New_Size (Handler.Buffer, UL_Size, UL_Size);

      --  Request id
      Compute_New_Size (Handler.Buffer, UL_Size, UL_Size);

      --  Response expected + Reserved
      Compute_New_Size (Handler.Buffer, UL_Size, UL_Size);

      Compute_New_Size
        (Buffer       => Handler.Buffer,
         Length_Size  => UL_Size,
         Element_Size => 1,
         Array_Length => Broca.Sequences.Octet_Sequences.Length
         (IOP.Get_Object_Key (Handler.Profile.all)));

      --  Operation
      Compute_New_Size (Handler.Buffer, CORBA.String (Operation));

      --  Principal - See 13.3.4: encoded as sequence <octet>
      Compute_New_Size (Handler.Buffer, Nobody_Principal);
   end Send_Request_Size;

   ---------------------------
   -- Send_Request_Marshall --
   ---------------------------

   procedure Send_Request_Marshall
     (Handler          : in out Request_Handler;
      Reponse_Expected : in Boolean;
      Operation        : in CORBA.Identifier)
   is
      use Broca.Marshalling;
   begin
      Marshall_GIOP_Header (Handler.Buffer, Broca.GIOP.Request);

      --  Service context
      Marshall (Handler.Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Handler.Request_Id := IOP.Get_Request_Id (Handler.Connection);
      Marshall (Handler.Buffer, Handler.Request_Id);

      --  Response expected
      Marshall (Handler.Buffer, Reponse_Expected);

      --  Object key
      Broca.Sequences.Marshall
        (Handler.Buffer,
         IOP.Get_Object_Key (Handler.Profile.all));

      --  Operation
      Marshall (Handler.Buffer, CORBA.String (Operation));

      --  Principal
      Marshall (Handler.Buffer, Nobody_Principal);
   end Send_Request_Marshall;

   -----------------------
   -- Send_Request_Send --
   -----------------------

   procedure Send_Request_Send
     (Handler          : in out Request_Handler;
      Target           : in Object.Object_Ptr;
      Reponse_Expected : in Boolean;
      Result           : out Send_Request_Result_Type)
   is
      use Broca.Marshalling;
      use CORBA;
      Message_Type    : MsgType;
      Message_Size    : CORBA.Unsigned_Long;
      Service_Context : CORBA.Unsigned_Long;
      Reply_Status    : ReplyStatusType;
      Request_Id      : CORBA.Unsigned_Long;
      Nothing         : Buffer_Type (1 .. 0);
      Header_Correct  : Boolean;
   begin
      --  1.3 Send request.
      IOP.Send (Handler.Connection, Handler.Buffer);

      if not Reponse_Expected then
         IOP.Release_Connection (Handler.Connection);
         Result := Sr_No_Reply;
         return;
      end if;

      --  1.4 Receive reply
      --  1.4.1 the message header
      Allocate_Buffer_And_Clear_Pos (Handler.Buffer, Message_Header_Size);

      pragma Debug (O ("Receive answer ..."));
      IOP.Receive (Handler.Connection, Handler.Buffer);
      pragma Debug (O ("Receive answer done"));

      Unmarshall_GIOP_Header (Handler.Buffer,
                              Message_Type, Message_Size,
                              Header_Correct);

      if not (Header_Correct and then Message_Type = Reply) then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Allocate enough bytes for the message.
      Allocate_Buffer_And_Clear_Pos
        (Handler.Buffer,
         Buffer_Index_Type (Message_Size) + Message_Header_Size);
      Skip_Bytes (Handler.Buffer, Message_Header_Size);

      --  1.4.5 Receive the reply header and body.
      IOP.Receive (Handler.Connection, Handler.Buffer);
      IOP.Release_Connection (Handler.Connection);

      --  Service context
      Read (Handler.Buffer, Nothing);
      Skip_Bytes (Handler.Buffer, Message_Header_Size);
      Unmarshall (Handler.Buffer, Service_Context);
      if Service_Context /= No_Context then
         pragma Debug
           (O ("Send_Request_Send : incorrect context" & Service_Context'Img));
         raise Program_Error;
      end if;

      --  Request id
      Unmarshall (Handler.Buffer, CORBA.Unsigned_Long (Request_Id));
      if Request_Id /= Handler.Request_Id then
         pragma Debug
           (O ("Send_Request_Send : incorrect request id" & Request_Id'Img));
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Reply status
      Unmarshall (Handler.Buffer, Reply_Status);
      case Reply_Status is
         when Broca.GIOP.No_Exception =>
            Result := Sr_Reply;
            return;

         when Broca.GIOP.System_Exception =>
            Broca.Exceptions.Unmarshall_And_Raise (Handler.Buffer);

         when Broca.GIOP.Location_Forward =>
            declare
               New_Ref : CORBA.Object.Ref;
            begin
               Broca.ORB.IOR_To_Object (Handler.Buffer, New_Ref);
               --  FIXME: check type, use a lock ?
               Target.Profiles :=
                 Object.Object_Ptr (CORBA.Object.Get (New_Ref)).Profiles;
            end;
            Result := Sr_Forward;
            return;

         when Broca.GIOP.User_Exception =>
            Result := Sr_User_Exception;
            return;

         when others =>
            raise Program_Error;
      end case;
   end Send_Request_Send;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in MsgType) is
   begin
      Compute_New_Size (Buffer, O_Size, O_Size);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in ReplyStatusType) is
   begin
      Compute_New_Size (Buffer, UL_Size, UL_Size);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in LocateStatusType) is
   begin
      Compute_New_Size (Buffer, UL_Size, UL_Size);
   end Compute_New_Size;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in MsgType) is
   begin
      Marshall (Buffer, MsgType_To_Octet (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in ReplyStatusType) is
   begin
      Marshall (Buffer, ReplyStatusType_To_Unsigned_Long (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in LocateStatusType) is
   begin
      Marshall (Buffer, LocateStatusType_To_Unsigned_Long (Value));
   end Marshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out MsgType)
   is
      O : CORBA.Octet;
   begin
      Unmarshall (Buffer, O);
      Result := Octet_To_MsgType (O);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out ReplyStatusType)
   is
      UL : CORBA.Unsigned_Long;
   begin
      Unmarshall (Buffer, UL);
      Result := Unsigned_Long_To_ReplyStatusType (UL);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out LocateStatusType)
   is
      UL : CORBA.Unsigned_Long;
   begin
      Unmarshall (Buffer, UL);
      Result := Unsigned_Long_To_LocateStatusType (UL);
   end Unmarshall;

end Broca.GIOP;
