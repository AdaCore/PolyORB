------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . G I O P                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
------------------------------------------------------------------------------



with Ada.Streams;                use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with Sequences.Unbounded;

with Droopi.Any;
with Droopi.Annotations;
with Droopi.Any.NVList;
with Droopi.Buffers;             use Droopi.Buffers;
with Droopi.Opaque;
with Droopi.Binding_Data;        use Droopi.Binding_Data;
with Droopi.Binding_Data.IIOP;
with Droopi.Binding_Data.Local;
with Droopi.Components;
with Droopi.Filters;
with Droopi.Filters.Interface;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);
with Droopi.Obj_Adapters;
with Droopi.Objects;
with Droopi.ORB;
with Droopi.ORB.Interface;
with Droopi.Protocols;           use Droopi.Protocols;
with Droopi.Protocols.GIOP.GIOP_1_0;
with Droopi.Protocols.GIOP.GIOP_1_1;
with Droopi.Protocols.GIOP.GIOP_1_2;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Representations;     use Droopi.Representations;
with Droopi.Representations.CDR;
with Droopi.Requests;
with Droopi.Transport;
with Droopi.Types;

package body Droopi.Protocols.GIOP is

   use Droopi.Any.NVList;
   use Droopi.Binding_Data.IIOP;
   use Droopi.Components;
   use Droopi.Log;
   use Droopi.ORB;
   use Droopi.ORB.Interface;
   use Droopi.Requests;
   use Droopi.Representations.CDR;
   use Droopi.Transport;
   use Droopi.Types;
   use Droopi.Annotations;

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   Current_Request_Id : Types.Unsigned_Long := 1;

   type Request_Note is new Note with record
     Id : Types.Unsigned_Long;
   end record;


   MsgType_To_Octet :
     constant array (Msg_Type'Range) of Types.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (Reply_Status_Type'Range) of Types.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3,
         Location_Forward_Perm => 4,
         Needs_Addressing_Mode => 5);

   LocateStatusType_To_Unsigned_Long :
     constant array (Locate_Status_Type'Range) of Types.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2,
         Object_Forward_Perm => 3,
         Loc_System_Exception => 4,
         Loc_Needs_Addressing_Mode => 5);

   Octet_To_MsgType :
     constant array (Types.Octet range 0 .. 7) of Msg_Type
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (Types.Unsigned_Long range 0 .. 5) of Reply_Status_Type
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward,
         4 => Location_Forward_Perm,
         5 => Needs_Addressing_Mode);

   Unsigned_Long_To_LocateStatusType :
     constant array (Types.Unsigned_Long range 0 .. 5) of Locate_Status_Type
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward,
         3 => Object_Forward_Perm,
         4 => Loc_System_Exception,
         5 => Loc_Needs_Addressing_Mode);


   ------------------------------
   --   Utility function for testing
   ------------------------------

   procedure To_Buffer (S   : access GIOP_Session;
                     Octets : access Encapsulation)
   is
      use Droopi.Representations.CDR;
      use Droopi.Opaque;
      Endianness1 : Endianness_Type;
      Z : constant Zone_Access
        := Zone_Access'(Octets.all'Unchecked_Access);
   begin

      if Droopi.Types.Boolean'Val
        (Droopi.Types.Octet (Octets (Octets'First))) then
         Endianness1 := Little_Endian;
      else
         Endianness1 := Big_Endian;
      end if;

      Initialize_Buffer
        (Buffer               => S.Buffer_In,
         Size                 => Octets'Length - 1,
         Data                 =>
           (Zone   => Z,
            Offset => Z'First + 1),
         --  Bypass runtime accessibility check.
         Endianness           => Endianness1,
         Initial_CDR_Position => 0);

      Show (S.Buffer_In.all);

   end To_Buffer;


   --------------------------------
   -- Marshalling Messages Types --
   --------------------------------

   --  Implementations


   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Msg_Type) is
   begin
      Marshall (Buffer, MsgType_To_Octet (Value));
   end Marshall;


   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Reply_Status_Type) is
   begin
      Marshall (Buffer, ReplyStatusType_To_Unsigned_Long (Value));
   end Marshall;


   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Locate_Status_Type) is
   begin
      Marshall (Buffer, LocateStatusType_To_Unsigned_Long (Value));
   end Marshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return Msg_Type is
   begin
      return Octet_To_MsgType (Unmarshall (Buffer));
   end Unmarshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return Reply_Status_Type is
   begin
      return Unsigned_Long_To_ReplyStatusType (Unmarshall (Buffer));
   end Unmarshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return Locate_Status_Type is
   begin
      return Unsigned_Long_To_LocateStatusType (Unmarshall (Buffer));
   end Unmarshall;

   ------------------------------------
   -- Marshalling the Version Number --
   ------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Version) is

   begin
      Marshall (Buffer, Version_To_Octet (Value));
   end Marshall;


   function Unmarshall
     (Buffer : access Buffer_Type)
     return Version
   is
      V : constant Types.Octet := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Got version value: (ulong)" & V'Img));
      return Octet_To_Version (V);
   end Unmarshall;


   --------------------------
   ---  Spec
   -------------------------

   procedure Unmarshall_Locate_Request
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Object_Key    : out Objects.Object_Id);

   procedure Request_Received
     (Ses : access GIOP_Session);

   procedure Reply_Received (Ses : access GIOP_Session);

   procedure Locate_Request_Receive
     (Ses : access GIOP_Session);

   procedure Initialize_Factory
     (Prof_Factory : in out Binding_Data.Profile_Factory_Access);

   -----------------------------
   -- Cancel_Request_Marshall --
   -----------------------------

   procedure Marshall_Cancel_Request
     (Buffer     : access Buffer_Type;
      Request_Id : in Types.Unsigned_Long) is
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Cancel_Request;

   -----------------------------
   -- Locate_Request_Marshall --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer           : access Buffer_Type;
      Request_Id       : in Types.Unsigned_Long;
      Object_Key       : in Objects.Object_Id_Access)
   is
      use Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Object Key
      Marshall (Buffer, Stream_Element_Array (Object_Key.all));

   end  Marshall_Locate_Request;

   ----------------------------
   --- Marshall Locate Reply --
   ----------------------------

   procedure  Marshall_Locate_Reply
     (Buffer         : access Buffer_Type;
      Request_Id     : in Types.Unsigned_Long;
      Locate_Status  : in Locate_Status_Type) is
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Locate Status
      Marshall (Buffer, Locate_Status);

   end  Marshall_Locate_Reply;

   ----------------------------
   -- GIOP_Header_Unmarshall --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Ses                   : access GIOP_Session;
      Message_Type          : out Msg_Type;
      Message_Size          : out Types.Unsigned_Long;
      Fragment_Next         : out Types.Boolean;
      Success               : out Boolean)
   is

      Buffer : Buffer_Access renames Ses.Buffer_In;

      Stream_Header          : constant Stream_Element_Array
        := To_Stream_Element_Array (Buffer);
      Message_Magic          : Stream_Element_Array (Magic'Range);
      Message_Major_Version  : Version;
      Message_Minor_Version  : Version;
      Message_Endianness     : Endianness_Type;
      Endianness             : Endianness_Type;
      Flags                  : Types.Octet;


   begin

      Success := False;

      if Types.Boolean'Val
           (Types.Octet (Stream_Header
                         (Stream_Header'First
                          + Byte_Order_Offset)) and 1) then
         Message_Endianness := Little_Endian;
      else
         Message_Endianness := Big_Endian;
      end if;
      pragma Debug (O ("Message_Endianness = " & Message_Endianness'Img));
      Set_Endianness (Buffer, Message_Endianness);

      --  Magic
      for I in Message_Magic'Range loop
         Message_Magic (I) := Stream_Element
           (Types.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         pragma Debug (O ("Unmarshall_GIOP_Header: Bad magic!"));
         return;
      end if;

      --  Test if the GIOP version of the Message received is supported
      Message_Major_Version := Unmarshall (Buffer);
      Message_Minor_Version := Unmarshall (Buffer);

      if not (Message_Major_Version =  Ses.Major_Version)
        or else (Ses.Minor_Version < Message_Minor_Version)
      then
         pragma Debug
           (O ("Unmarshall_GIOP_Header: GIOP version not supported"));
         return;
      end if;

      Flags := Unmarshall (Buffer);
      if Is_Set (Flags, Endianness_Bit) then
         Endianness := Little_Endian;
      else
         Endianness := Big_Endian;
      end if;

      pragma Debug (O ("Flags =" & Flags'Img));

      pragma Assert (Message_Endianness = Endianness);

      if Message_Minor_Version /= Ver0 then
         Fragment_Next := Is_Set (Flags, Fragment_Bit);
      end if;


      --  Message type
      Message_Type := Unmarshall (Buffer);

      --  Message size
      Message_Size := Unmarshall (Buffer);

      Put_Line ("Size: " &
      Types.Unsigned_Long'Image (Message_Size));

      --  Everything allright
      Ses.Major_Version := Message_Major_Version;
      Ses.Minor_Version := Message_Minor_Version;

      Put_Line ("Major:" & Version'Image (Ses.Major_Version));
      Put_Line ("Major:" & Version'Image (Ses.Minor_Version));

      Success := True;
      Release_Contents (Buffer.all);

   end Unmarshall_GIOP_Header;

   --------------------------------
   -- Unmarshall_Locate_Message --
   --------------------------------

   procedure Unmarshall_Locate_Request
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Object_Key    : out Objects.Object_Id) is
   begin
      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Object key
      Object_Key := Objects.Object_Id
        (Stream_Element_Array' (Unmarshall (Buffer)));
   end Unmarshall_Locate_Request;

   ------------------------------
   --  Unmarshall_Locate_Reply --
   ------------------------------

   procedure Unmarshall_Locate_Reply
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Locate_Status : out Locate_Status_Type) is
   begin
      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Locate_Status := Unmarshall (Buffer);
   end Unmarshall_Locate_Reply;

   ---------------------
   -- Request_Message --
   ---------------------

   procedure Request_Message
     (Ses                    : access GIOP_Session;
      Pend_Req               : access Pending_Request;
      Response_Expected      : in Boolean;
      Fragment_Next          : out Boolean)
   is
      use Internals;
      use Internals.NV_Sequence;
      use Droopi.Objects;

      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Sync          : Sync_Scope;
      Arg           : Any.NamedValue;
      List          : NV_Sequence_Access;
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin
      Get_Note (Pend_Req.Req.Notepad, N);
      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Marshall_Request_Message
              (Ses.Buffer_Out, Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               To_Standard_String (Pend_Req.Req.Operation));

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_Request_Message
              (Ses.Buffer_Out, Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               To_Standard_String (Pend_Req.Req.Operation));

         when Ver2 =>

                  if Response_Expected then
                     Sync := WITH_TARGET;
                  else
                     Sync := NONE;
                  end if;

                  pragma Debug  (O (Image (IIOP_Profile_Type
                     (Pend_Req.Target_Profile.all))));

                  declare
                     Key : aliased Object_Id :=  Binding_Data.IIOP.
                       Get_Object_Key
                       (IIOP_Profile_Type (Pend_Req.Target_Profile.all));
                  begin

                     GIOP.GIOP_1_2.Marshall_Request_Message
                       (Ses.Buffer_Out,
                        Request_Id,
                        Target_Address'
                        (Address_Type => Key_Addr,
                         Object_Key   => Key'Unchecked_Access),
                         Sync,
                        To_Standard_String (Pend_Req.Req.Operation));
                  end;
      end case;


      --  Marshall the request's Body not yet implemented
      List :=  List_Of (Pend_Req.Req.Args);
      for I in 1 ..  Get_Count (Pend_Req.Req.Args) loop
         Arg := NV_Sequence.Element_Of (List.all, Positive (I));
         Marshall (Ses.Buffer_Out, Arg);
      end loop;

      if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
         Fragment_Next := True;
      end if;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request,  Length (Ses.Buffer_Out));

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length (Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length (Ses.Buffer_Out),
               Fragment_Next);
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      Show (Ses.Buffer_Out.all);
   end Request_Message;

   ------------------------
   -- No_Exception_Reply --
   ------------------------

   procedure No_Exception_Reply
     (Ses           : access GIOP_Session;
      Request       :        Requests.Request_Access;
      Fragment_Next :    out Boolean)
   is
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin
      Get_Note (Request.Notepad, N);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);
      end case;

      --  Marshall the reply Body
      Marshall (Ses.Buffer_Out, Request.Result);

      if Length (Ses.Buffer_Out)  > Maximum_Message_Size then
         Fragment_Next := True;
      end if;

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out),
               Fragment_Next);

      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end No_Exception_Reply;

   ---------------------
   -- Exception_Reply --
   ---------------------

   procedure Exception_Reply
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Exception_Type  : in Reply_Status_Type;
      Occurence       : in CORBA.Exception_Occurrence;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin

      Get_Note (Request.Notepad, N);

      pragma Assert (Exception_Type in User_Exception  .. System_Exception);

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_Exception
              (Ses.Buffer_Out, Request_Id,
               Exception_Type, Occurence);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.GIOP_1_1.Marshall_Exception
              (Ses.Buffer_Out,
               Request_Id,
               Exception_Type,
               Occurence);

            if Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>

            GIOP.GIOP_1_2.Marshall_Exception
              (Ses.Buffer_Out,
               Request_Id,
               Exception_Type,
               Occurence);

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);


      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

   end Exception_Reply;

   -------------------------------------------------------
   --  Location Forward
   --------------------------------------------------------

   procedure Location_Forward_Reply
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Forward_Ref     : in Droopi.References.IOR.IOR_Type;
      Fragment_Next   : out Boolean)

   is
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin

      Get_Note (Request.Notepad, N);
      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               Forward_Ref);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.GIOP_1_1.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               Forward_Ref);

            if  Length (Ses.Buffer_Out) >
              Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>

            GIOP.GIOP_1_2.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               GIOP.Location_Forward,
               Forward_Ref);

            if  Length (Ses.Buffer_Out) >
              Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Location_Forward_Reply;


   -------------------------------------------------------
   --  Need_Addressing_Mode_Message
   --------------------------------------------------------

   procedure Need_Addressing_Mode_Message
     (Ses             : access GIOP_Session;
      Request          : Requests.Request_Access;
      Address_Type    : in Addressing_Disposition)

   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin

      Get_Note (Request.Notepad, N);

      if Ses.Minor_Version /=  Ver2 then
         raise GIOP_Error;
      end if;

      GIOP.GIOP_1_2.Marshall_Needs_Addressing_Mode
        (Ses.Buffer_Out, Request_Id, Address_Type);

      GIOP.GIOP_1_2.Marshall_GIOP_Header
        (Header_Buffer,
         GIOP.Reply, Length (Ses.Buffer_Out),
         False);

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Need_Addressing_Mode_Message;

   ----------------------------
   -- Cancel_Request_Message --
   ----------------------------

   procedure Cancel_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin

      Get_Note (Request.Notepad, N);
      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when Ver2 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when others =>
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Cancel_Request_Message;


   ----------------------------
   -- Locate_Request_Message --
   ----------------------------

   procedure Locate_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Object_Key      : in Objects.Object_Id_Access;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin
      Get_Note (Request.Notepad, N);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Object_Key);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out));

         when Ver1 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Object_Key);

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Target_Address'(Address_Type => Key_Addr,
                               Object_Key =>  Object_Key));

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

   end Locate_Request_Message;

   --------------------------
   -- Locate_Reply_Message --
   --------------------------

   procedure Locate_Reply_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Locate_Status   : in Locate_Status_Type)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin

      Get_Note (Request.Notepad, N);

      if (Ses.Minor_Version = Ver0 or else Ses.Minor_Version = Ver1) and then
        (Locate_Status = Object_Forward_Perm or else
        Locate_Status = Loc_System_Exception or else
        Locate_Status = Loc_Needs_Addressing_Mode)
      then
         raise GIOP_Error;
      end if;

      GIOP.Marshall_Locate_Reply
        (Ses.Buffer_Out, Request_Id, Locate_Status);

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply, Length (Ses.Buffer_Out));

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply,
               Length (Ses.Buffer_Out), False);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply,
               Length (Ses.Buffer_Out), False);
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Locate_Reply_Message;

   --------------------
   -- Select_Profile --
   --------------------

   function Select_Profile
     (Buffer  : access Buffer_Type) return
      Profile_Access;

   function Select_Profile
     (Buffer  : access Buffer_Type) return
      Profile_Access
   is
      use Droopi.References;
      use Droopi.References.IOR;

      New_Ref    : Droopi.References.IOR.IOR_Type := Unmarshall (Buffer);
      Prof_Array : Droopi.References.Profile_Array
        := Profiles_Of (New_Ref.Ref);
      Prof_Temp  : Profile_Access;

   begin
      pragma Debug (O ("Reply Message : Received Location_Forward"));
      for I in Prof_Array'Range loop
         if Prof_Array (I).all in Binding_Data.IIOP.IIOP_Profile_Type then
            Prof_Temp := Prof_Array (I);
            exit;
         end if;
      end loop;
      return Prof_Temp;
   end Select_Profile;


   -------------------
   -- Store Profile --
   -------------------

   procedure Store_Profile
     (Ses      :  access GIOP_Session;
      Profile  :  Profile_Access)
   is
   begin
      Ses.Current_Profile := Profile;
   end Store_Profile;


   -------------------
   -- Store_Request --
   -------------------

   procedure Store_Request
     (Ses     :  access GIOP_Session;
      R       :  Requests.Request_Access;
      Profile :  Profile_Access;
      Pending :  out Pending_Request)
   is
      use Req_Seq;
   begin
      Set_Note (R.Notepad, Request_Note'(Annotations.Note with
                                          Id => Current_Request_Id));
      Pending  := Pending_Request'(Req => R,
                      Target_Profile => Profile);
      Append (Ses.Pending_Rq, Pending);
      Current_Request_Id := Current_Request_Id + 1;
   end Store_Request;

   ----------------------
   -- Request_Received --
   ----------------------

   procedure Request_Received
     (Ses : access GIOP_Session)
   is
      use Binding_Data.IIOP;
      use Binding_Data.Local;
      use Internals;
      use Internals.NV_Sequence;

      use References;

      use Droopi.Objects;


      Request_Id        :  Types.Unsigned_Long;
      Response_Expected :  Boolean;
      Object_Key        :  Objects.Object_Id_Access;
      Operation         :  Types.String;


      Req    : Request_Access;
      Args   : Any.NVList.Ref;
      Result : Any.NamedValue;

      Target_Profile : Binding_Data.Profile_Access
        := new Local_Profile_Type;
      Target : References.Ref;
      Target_Ref  : Target_Address_Access;

      ORB : constant ORB_Access := ORB_Access (Ses.Server);
      Temp_Arg : Any.NamedValue;
      List     : NV_Sequence_Access;

   begin

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key,
               Operation);

         when Ver1 =>
            GIOP.GIOP_1_1.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key,
               Operation);

         when Ver2 =>
            raise Program_Error;

--             GIOP.GIOP_1_2.Unmarshall_Request_Message
--               (Ses.Buffer_In,
--                Request_Id,
--                Response_Expected,
--                Target_Ref.all,
--                --  XXX statically dereferencing null (non-initialised)
--                --  pointer. CONSTRAINT_ERROR will be raised at run time.
--                Operation);

--             if Target_Ref.Address_Type = Key_Addr then
--                Object_Key := Target_Ref.Object_Key;
--             end if;

      end case;

      Args := Obj_Adapters.Get_Empty_Arg_List
        (Object_Adapter (ORB),
         Object_Key.all,
         To_Standard_String (Operation));

      --  Unmarshalling of arguments
      List :=  List_Of (Args);
      for I in 1 .. Get_Count (Args) loop
         Temp_Arg :=  NV_Sequence.Element_Of (List.all, Positive (I));
         Unmarshall (Ses.Buffer_In, Temp_Arg);
         --  XXX Change name of procedure Unmarshall to be explicit
         --    about the behaviour performed.
         --    A subprogram named Unmarshall shall be a function,
         --    take only one Buffer_Access argument and return the
         --    unmarshalled value.
         NV_Sequence.Replace_Element (List.all, Positive (I), Temp_Arg);
      end loop;

      Result := (Name     => To_Droopi_String ("Result"),
                 Argument => Obj_Adapters.Get_Empty_Result
                 (Object_Adapter (ORB),
                  Object_Key.all,
                  To_Standard_String (Operation)),
                 Arg_Modes => 0);

      --  XXX Target_Ref is statically null (see above).
      --  Since we have already raised Program_Error if
      --  Ses.Minor_Version = Ver2, we known that it won't
      --  actually be dereferenced, and we can ignore the
      --  warning for now.
      pragma Warnings (Off);
      if Ses.Minor_Version = Ver2
        and then Target_Ref.Address_Type /= Key_Addr
      then
         pragma Warnings (On);
         if Target_Ref.Address_Type = Profile_Addr then
            Create_Reference ((1 => Target_Ref.Profile), Target);
         else
            Target := Target_Ref.Ref.IOR.Ref;
         end if;
      else
         Create_Local_Profile
           (Object_Key.all, Local_Profile_Type (Target_Profile.all));
         Create_Reference ((1 => Target_Profile), Target);
      end if;

      Create_Request
        (Target    => Target,
         Operation => To_Standard_String (Operation),
         Arg_List  => Args,
         Result    => Result,
         Req       => Req);

      Set_Note (Req.Notepad, Request_Note'(Annotations.Note with
                                          Id => Request_Id));

      Emit_No_Reply
        (Component_Access (ORB),
         Queue_Request'
         (Request => Req,
          Requestor => Component_Access (Ses),
          Requesting_Task => null));
      Free (Object_Key);
   end Request_Received;


   --------------------------------
   -- Receiving a  Reply Message --
   --------------------------------

   procedure Reply_Received (Ses : access GIOP_Session) is
      use References.IOR;
      use Binding_Data.IIOP;
      use Req_Seq;
      Reply_Status  : Reply_Status_Type;
      Request_Id    : Types.Unsigned_Long;
      Current_Req   : Pending_Request;
      N  : Request_Note;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);

   begin

      case Ses.Minor_Version is
         when  Ver0 =>
            GIOP.GIOP_1_0.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);
            if Reply_Status = Location_Forward_Perm or
              Reply_Status = Needs_Addressing_Mode then
               raise GIOP_Error;
            end if;

         when Ver1 =>
            GIOP.GIOP_1_1.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);

            if Reply_Status = Location_Forward_Perm
              or else Reply_Status = Needs_Addressing_Mode
            then
               raise GIOP_Error;
            end if;

         when  Ver2 =>
            GIOP.GIOP_1_2.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);
      end case;

      for I in 1 .. Length (Ses.Pending_Rq) loop
         Current_Req := Element_Of (Ses.Pending_Rq, I);
         Get_Note (Current_Req.Req.Notepad, N);
         if N.Id  = Request_Id then
            Delete (Ses.Pending_Rq, I, 1);
            exit;
         end if;
         raise GIOP_Error;
      end loop;


      case Reply_Status is

         when No_Exception =>

            Current_Req.Req.Result :=
              (Name     => To_Droopi_String ("Result"),
               Argument => Obj_Adapters.Get_Empty_Result
               (Object_Adapter (ORB),
                Get_Object_Key
                (IIOP_Profile_Type (Current_Req.Target_Profile.all)),
                 To_Standard_String (Current_Req.Req.Operation)),
               Arg_Modes => Any.ARG_OUT);

            Unmarshall (Ses.Buffer_In, Current_Req.Req.Result);
            Emit_No_Reply
              (Component_Access (ORB),
               Queue_Request'(Request   => Current_Req.Req,
                              Requestor => Component_Access (Ses),
                              Requesting_Task => null));


         when User_Exception =>
            raise Not_Implemented;

         when System_Exception =>
            Unmarshall_And_Raise (Ses.Buffer_In);

         when Location_Forward | Location_Forward_Perm =>

            declare
               TE      : Transport_Endpoint_Access;
               New_Ses : Session_Access;
               Prof    : Profile_Access;

            begin
               Prof := Select_Profile (Ses.Buffer_In);
               Binding_Data.IIOP.Bind_Profile
                 (IIOP_Profile_Type (Prof.all),
                  TE,
                  Component_Access (New_Ses));

               --  release the previous session buffers
               Release (Ses.Buffer_In);
               Release (Ses.Buffer_Out);

               Store_Request (Ses, Current_Req.Req, Prof, Current_Req);

               Invoke_Request (GIOP_Session (New_Ses.all)'Access,
                                   Current_Req.Req);
            end;

         when Needs_Addressing_Mode =>
            raise Not_Implemented;

      end case;
   end Reply_Received;


   ---------------------------------------------
   ---   receiving a locate request
   ----------------------------------------------

   procedure Locate_Request_Receive
     (Ses : access GIOP_Session)
   is
      --      Reply_Status  : Reply_Status_Type;
      Request_Id    : Types.Unsigned_Long;
      Object_Key    : Objects.Object_Id_Access := null;
      Target_Ref    : Target_Address_Access := null;
   begin

      if Ses.Minor_Version /= Ver2 then
         GIOP.Unmarshall_Locate_Request
           (Ses.Buffer_In,
            Request_Id,
            Object_Key.all);
      else
         GIOP.GIOP_1_2.Unmarshall_Locate_Request
           (Ses.Buffer_In,
            Request_Id,
            Target_Ref.all);
      end if;
   end Locate_Request_Receive;


   ------------------------------------
   -- Initialize the Profile_Factory --
   ------------------------------------

   procedure Initialize_Factory
     (Prof_Factory : in out Binding_Data.Profile_Factory_Access)
   is
   begin
      Prof_Factory := new Binding_Data.IIOP.IIOP_Profile_Factory;
   end Initialize_Factory;

   -------------------------
   -- Visible subprograms --
   -------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access)
   is

   begin
      Session := new GIOP_Session;
      GIOP_Session (Session.all).Buffer_In  := new Buffers.Buffer_Type;
      GIOP_Session (Session.all).Buffer_Out := new Buffers.Buffer_Type;
   end Create;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (S   : access GIOP_Session;
      R   : Requests.Request_Access)
   is
      use Buffers;
      use Binding_Data.IIOP;
      use Droopi.Filters.Interface;
      use Droopi.Objects;
      use Droopi.Requests;

      Fragment_Next  : Boolean := False;
      Current_Req    : aliased Pending_Request;

   begin

      if S.Role  = Server then
         raise GIOP_Error;
      end if;

      Release_Contents (S.Buffer_Out.all);
      Store_Request (S, R, S.Current_Profile, Current_Req);

      --  fragmentation not yet implemented
      --  Message_Size:= Length (Buf1);

      --  if Message_Size > Maximum_Body_Size then
      --     Buf2 :=
      --  end if;

      if S.Object_Found = False then
         if  S.Nbr_Tries <= Max_Nb_Tries then
            declare
               Oid : Object_Id := Get_Object_Key
                   (Current_Req.Target_Profile.all);
               Obj : Object_Id_Access := new Object_Id'(Oid);
            begin
               Locate_Request_Message (S, Current_Req.Req,
                        Obj, Fragment_Next);
               S.Nbr_Tries := S.Nbr_Tries + 1;
               pragma Debug (O ("Locate Request Message"));
            end;
         else
            pragma Debug (O ("Number of tries exceeded"));

            return;
         end if;
      else
         Request_Message (S, Current_Req'Access, True, Fragment_Next);
         S.Object_Found := True;
         S.Nbr_Tries := 0;
      end if;

      --  Sending the message
      --  Sending the data to lower layers
      Emit_No_Reply (Lower (S), Data_Out' (Out_Buf => S.Buffer_Out));
   end Invoke_Request;


   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (S : access GIOP_Session;
      R : Requests.Request_Access)
   is
      use Droopi.Filters.Interface;
      use Req_Seq;
      Current_Req   : aliased Pending_Request;
      Pending_Note  : Request_Note;
      Current_Note  : Request_Note;

   begin

      if S.Role  = Server then
         raise GIOP_Error;
      end if;

      Get_Note (R.Notepad, Current_Note);
      for I in 1 .. Length (S.Pending_Rq) loop
         Get_Note (Element_Of (S.Pending_Rq, I).Req.Notepad, Pending_Note);
         if Pending_Note.Id = Current_Note.Id then
            Current_Req := Element_Of (S.Pending_Rq, I);
            Delete (S.Pending_Rq, I, 1);
            exit;
         end if;
         raise GIOP_Error;
      end loop;

      Release_Contents (S.Buffer_Out.all);
      Cancel_Request_Message (S, Current_Req.Req);

      --  Sending the message
      Emit_No_Reply (Lower (S), Data_Out' (Out_Buf => S.Buffer_Out));

   end Abort_Request;

   ----------------
   -- Send Reply --
   ----------------

   procedure Send_Reply
     (S : access GIOP_Session;
      R :        Requests.Request_Access)
   is
      use Buffers;
      use Representations.CDR;
      use Droopi.Filters.Interface;
      use Req_Seq;
      Fragment_Next : Boolean := False;

   begin

      if S.Role  = Client then
         raise GIOP_Error;
      end if;

      Release_Contents (S.Buffer_Out.all);
      No_Exception_Reply (S, R, Fragment_Next);

      pragma Debug (O ("Sending Reply"));

      --  Sending the message
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer_Out));

      pragma Debug (O ("After Sending Reply"));
   end Send_Reply;

   procedure Expect_Message (S : access GIOP_Session);
   --  Prepare S to receive next GIOP message.
   --  This must be called once when a session is established
   --  (in Handle_Connect_Indication for a server session,
   --  in Handle_Connect_Confirmation for a client session),
   --  and then exactly once after a message has been received.
   --  This must not be called after sending a message (because
   --  message sends and receives can be interleaved in an
   --  arbitrary way, and Expect_Message must not be called
   --  twice in a row).

   procedure Expect_Message (S : access GIOP_Session) is
   begin
      Expect_Data (S, S.Buffer_In, Message_Header_Size);
      S.Expect_Header := True;
   end Expect_Message;

   ----------------------------------
   --  Handle Connect Indication ----
   ----------------------------------

   procedure Handle_Connect_Indication (S : access GIOP_Session)
   is

   begin
      pragma Debug (O ("Received new connection ..."));
      S.Role := Server;
      Expect_Message (S);
   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation (S : access GIOP_Session) is
   begin
      pragma Debug (O (" Connection established to server ..."));
      S.Role := Client;
      Expect_Message (S);
   end Handle_Connect_Confirmation;


   procedure Handle_Data_Indication (S : access GIOP_Session)
   is
      use Binding_Data.IIOP;
      use Objects;
      use ORB;
      use References;
      use References.IOR;
      use Req_Seq;
      Mess_Type     : Msg_Type;
      Mess_Size     : Types.Unsigned_Long;
      Fragment_Next : Boolean;
      Success       : Boolean;

   begin
      pragma Debug (O ("Received data on socket service..."));
      pragma Debug (Buffers.Show (S.Buffer_In.all));

      if S.Expect_Header then
         Unmarshall_GIOP_Header (S, Mess_Type, Mess_Size,
                                 Fragment_Next, Success);
         if not Success then
            raise GIOP_Error;
         end if;
         S.Mess_Type_Received  := Mess_Type;
         S.Expect_Header := False;
         Expect_Data (S, S.Buffer_In, Stream_Element_Count (Mess_Size));
         return;
      end if;

      --  if Fragment_Next then
      --   Expect_Data ();
      --   return;
      --  end if

      case S.Mess_Type_Received  is
         when Request =>
            if S.Role = Server then
               Request_Received (S);
            else
               raise GIOP_Error;
            end if;
         when Reply =>
            if S.Role = Client then
               Reply_Received (S);
            else
               raise GIOP_Error;
            end if;

         when Cancel_Request =>
            if S.Role = Client then
               raise Not_Implemented;
            else
               raise GIOP_Error;
            end if;

         when Locate_Request =>
            if S.Role = Server then
               --  not yet implemented
               raise Not_Implemented;
            else
               raise GIOP_Error;
            end if;

         when Locate_Reply =>
            if S.Role = Client  then
               declare
                  Req_Id        : Types.Unsigned_Long;
                  Locate_Status : Locate_Status_Type;
               begin
                  Unmarshall_Locate_Reply
                    (S.Buffer_In, Req_Id, Locate_Status);
                  case Locate_Status is
                     when Object_Here =>
                        declare
                           Current_Req : Pending_Request;
                           Current_Note : Request_Note;
                        begin
                           S.Object_Found := True;
                           for I in 1 .. Length (S.Pending_Rq) loop
                              Current_Req := Element_Of (S.Pending_Rq, I);
                              Get_Note (Current_Req.Req.Notepad, Current_Note);
                              if Current_Note.Id = Req_Id then
                                 exit;
                              end if;
                              raise GIOP_Error;
                           end loop;
                           Invoke_Request (S, Current_Req.Req);
                        end;
                     when Unknown_Object =>
                        pragma Debug (O ("Object not found"));
                        Release (S.Buffer_In);
                        Release (S.Buffer_Out);
                        return;

                     when Object_Forward | Object_Forward_Perm =>
                        declare
                           TE          : Transport_Endpoint_Access;
                           New_Ses     : Session_Access;
                           Current_Req : Pending_Request;
                           Current_Note : Request_Note;
                        begin

                           for I in 1 .. Length (S.Pending_Rq) loop
                              Current_Req := Element_Of (S.Pending_Rq, I);
                              Get_Note (Current_Req.Req.Notepad, Current_Note);
                              if Current_Note.Id = Req_Id then
                                 Current_Req.Target_Profile :=
                                    Select_Profile (S.Buffer_In);
                                 Replace_Element (S.Pending_Rq,
                                     I, Current_Req);
                              end if;
                              raise GIOP_Error;
                           end loop;

                           Binding_Data.IIOP.Bind_Profile
                             (IIOP_Profile_Type
                              (Current_Req.Target_Profile.all),
                              TE, Component_Access (New_Ses));

                           --  Release the previous session buffers
                           Release (S.Buffer_In);
                           Release (S.Buffer_Out);
                           Invoke_Request (GIOP_Session (New_Ses.all)'Access,
                                           Current_Req.Req);
                        end;

                     when Loc_Needs_Addressing_Mode =>
                        raise Not_Implemented;

                     when Loc_System_Exception =>
                        Unmarshall_And_Raise (S.Buffer_In);

                  end case;
               end;
            else
               raise GIOP_Error;
            end if;

         when Close_Connection =>
            if S.Role = Server or else S.Minor_Version = Ver2 then
               raise Program_Error;
            else
               raise Not_Implemented;
            end if;

         when Message_Error =>
            raise GIOP_Error;

         when Fragment =>
            raise Not_Implemented;
      end case;

      Buffers.Release_Contents (S.Buffer_In.all);

      Expect_Message (S);
      --  Prepare to receive next message.

   end Handle_Data_Indication;


   procedure Handle_Disconnect (S : access GIOP_Session) is
   begin
      Release (S.Buffer_In);
      Release (S.Buffer_Out);
   end Handle_Disconnect;

   procedure Set
     (Bit_Field : in out Types.Octet;
      Bit_Order : Bit_Order_Type;
      Bit_Value : Boolean) is
   begin
      if Bit_Value then
         Bit_Field := Bit_Field or (2 ** Bit_Order);
      else
         Bit_Field := Bit_Field and not (2 ** Bit_Order);
      end if;
   end Set;

   function Is_Set
     (Bit_Field : Types.Octet;
      Bit_Order : Bit_Order_Type)
     return Boolean is
   begin
      return (Bit_Field and (2 ** Bit_Order)) /= 0;
   end Is_Set;




end Droopi.Protocols.GIOP;

