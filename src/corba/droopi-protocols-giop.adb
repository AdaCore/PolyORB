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
with Ada.Unchecked_Deallocation;

with CORBA;
with CORBA.NVList; use CORBA.NVList;
with CORBA.AbstractBase;
with CORBA.Exceptions;


with Sequences.Unbounded;

with Broca.Exceptions;
with Droopi.Opaque;              use Droopi.Opaque;
with Droopi.Buffers;             use Droopi.Buffers;
with Droopi.Binding_Data;        use Droopi.Binding_Data;
with Droopi.Binding_Data.IIOP;
with Droopi.Protocols;           use Droopi.Protocols;
with Droopi.Protocols.GIOP.GIOP_1_0;
with Droopi.Protocols.GIOP.GIOP_1_1;
with Droopi.Protocols.GIOP.GIOP_1_2;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Representations;     use Droopi.Representations;
with Droopi.Representations.CDR;
with Droopi.Obj_Adapters.Simple;
with Droopi.Transport;
with Droopi.Transport.Sockets;
with Droopi.ORB;
with Droopi.ORB.Interface;
with Droopi.Objects;
with Droopi.Requests;
with Droopi.Components;
with Droopi.Filters;
with Droopi.Filters.Slicers;
with Droopi.Filters.Interface;
with Droopi.Log;


pragma Elaborate_All (Droopi.Log);


package body Droopi.Protocols.GIOP is

  use Droopi.Components;
  use Droopi.Requests;
  use ORB;
  use Droopi.ORB.Interface;
  use Droopi.Transport;
  use Droopi.Transport.Sockets;
  use Droopi.Representations.CDR;
  use Droopi.Binding_Data.IIOP ;


   --  The offset of the byte_order boolean field in a GIOP message
   --  header.

   Prof_Factory : Binding_Data.Profile_Factory_Access;

   --  XXX Remove dynamic allocation from elaboration
   --  (move it to an Initialize procedure).

   Pend_Req : Pending_Request;

   Current_Request_Id : CORBA.Unsigned_Long := 1;

   MsgType_To_Octet :
     constant array (Msg_Type'Range) of CORBA.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (Reply_Status_Type'Range) of CORBA.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3,
         Location_Forward_Perm => 4,
         Needs_Addressing_Mode => 5);

   LocateStatusType_To_Unsigned_Long :
     constant array (Locate_Status_Type'Range) of CORBA.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2,
         Object_Forward_Perm => 3,
         Loc_System_Exception => 4,
         Loc_Needs_Addressing_Mode => 5);

   Octet_To_MsgType :
     constant array (CORBA.Octet range 0 .. 7) of Msg_Type
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 5) of Reply_Status_Type
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward,
         4 => Location_Forward_Perm,
         5 => Needs_Addressing_Mode);

   Unsigned_Long_To_LocateStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 5) of Locate_Status_Type
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward,
         3 => Object_Forward_Perm,
         4 => Loc_System_Exception,
         5 => Loc_Needs_Addressing_Mode);



   --------------------------------------
    -- Marshalling Messages Types --
   --------------------------------------


  -- Implementations


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

  ----------------------------------
  -- Marshalling the Version Number
  ------------------------------

  procedure Marshall
   (Buffer : access Buffer_Type;
    Value  : in Version) is

  begin
     Marshall (Buffer, Version_To_Unsigned_Long (Value));
  end Marshall;


  function Unmarshall
     (Buffer : access Buffer_Type)
    return Version
  is
  begin
    return Unsigned_Long_To_Version(Unmarshall(Buffer));
  end Unmarshall;




   -----------------------------
   -- Cancel_Request_Marshall --
   -----------------------------

   procedure Marshall_Cancel_Request
     (Buffer     : access Buffer_Type;
      Request_Id : in CORBA.Unsigned_Long) is
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Cancel_Request;

   -----------------------------
   -- Locate_Request_Marshall --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Object_Key       : in Objects.Object_Id_Access)
   is
      use Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Object Key
      Marshall (Buffer, Stream_Element_Array(Object_Key.all));

   end  Marshall_Locate_Request;

   ----------------------------
   --- Marshall Locate Reply --
   ----------------------------

   procedure  Marshall_Locate_Reply
     (Buffer         : access Buffer_Type;
      Request_Id     : in CORBA.Unsigned_Long;
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
      Message_Size          : out CORBA.Unsigned_Long;
      Fragment_Next         : out CORBA.Boolean;
      Success               : out Boolean)
   is

      use CORBA;

      -- Message_Header_Message : aliased Buffer_Type := Ses.Buffer_In'All;
      Message_Magic          : Stream_Element_Array (Magic'Range);
      Message_Major_Version  : Version;
      Message_Minor_Version  : Version;
    --  Endianness             : Endianness_Type;
      Message_Endianness     : Endianness_Type;
      Flags                  : CORBA.Octet;
      Op_Ptr                 : Opaque_Pointer;

   begin

      Success := False;

      --  Magic
      for I in Message_Magic'Range loop
         Message_Magic (I) := Stream_Element(CORBA.Octet'(Unmarshall (Ses.Buffer_In)));
      end loop;

      if Message_Magic /= Magic then
         pragma Debug (O ("Unmarshall_GIOP_Header: Bad magic!"));
         return;
      end if;

      --  Test if the GIOP version of the Message received is supported
      Message_Major_Version := Unmarshall (Ses.Buffer_In);
      Message_Minor_Version := Unmarshall (Ses.Buffer_In);

      if not (Message_Major_Version =  Ses.Major_Version)
        or else (Ses.Minor_Version < Message_Minor_Version)
       then
         pragma Debug(O ("Unmarshall_GIOP_Header: GIOP version not supported"));
         return;
      end if;

     Flags := Unmarshall (Ses.Buffer_In);
     if (Flags and 2 ** Endianness_Bit) /= 0 then
          Message_Endianness := Little_Endian;
     else
          Message_Endianness := Big_Endian;
     end if;


     if Message_Minor_Version /= Ver0 then
        Fragment_Next := ((Flags and 2 ** Fragment_Bit) /= 0);
     end if;


   --   Buffers.Extract_Data(Ses.Buffer_In.Contents, Op_Ptr,
   --                Byte_Order_Offset, 1);

--     if CORBA.Boolean'Val
--           (CORBA.Octet(Op_Ptr.Zone(Op_Ptr.Offset) and 1))
--     then
--             Endianness := Little_Endian;
--     else
--             Endianness := Big_Endian;
--     end if;

--    Buffers.Initialize_Buffer
--      (Ses.Buffer_In, Message_Header_Size, Opaque_Pointer'(Zone=>null, Offset=>0),
--       Endianness, 0);


      --  Message type
      Message_Type := Unmarshall (Ses.Buffer_In);

      --  Message size
      Message_Size := Unmarshall (Ses.Buffer_In);

      --  Everything allright
      Ses.Major_Version := Message_Major_Version;
      Ses.Minor_Version := Message_Minor_Version;

      Success := True;
   end Unmarshall_GIOP_Header;

   --------------------------------
   -- Unmarshall_Locate_Message --
   --------------------------------

   procedure Unmarshall_Locate_Request
     (Buffer        : access Buffer_Type;
      Request_Id    : out CORBA.Unsigned_Long;
      Object_Key    : out Objects.Object_Id) is
   begin
      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Object key
      Object_Key := Objects.Object_Id
                    (Stream_Element_Array'( Unmarshall (Buffer)));
   end Unmarshall_Locate_Request;

   ------------------------------
   --  Unmarshall_Locate_Reply --
   ------------------------------

   procedure Unmarshall_Locate_Reply
     (Buffer        : access Buffer_Type;
      Request_Id    : out CORBA.Unsigned_Long;
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
     (Ses               : access GIOP_Session;
      Response_Expected : in Boolean;
      Fragment_Next     : out Boolean)
   is
      use Internals;
      use Internals.NV_Sequence;
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Sync          : Sync_Scope;
      Arg           : CORBA.NamedValue;
      List          : NV_Sequence_Access;

   begin
      Fragment_Next := False;

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Marshall_Request_Message
              (Ses.Buffer_Out, Pend_Req.Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               Pend_Req.Req.Operation.all);

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_Request_Message
              (Ses.Buffer_Out, Pend_Req.Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               Pend_Req.Req.Operation.all);

         when Ver2 =>
            declare
              Key : Objects.Object_Id_access := null;
            begin
              if Response_Expected then
                Sync := WITH_TARGET;
              else
                Sync := NONE;
              end if;

              Key.all := Binding_Data.IIOP.Get_Object_Key(
                 IIOP_Profile_Type(Pend_Req.Target_Profile.all));

              GIOP.GIOP_1_2.Marshall_Request_Message
                (Ses.Buffer_Out,
                 Pend_Req.Request_Id,
                 Target_Address'(Address_Type => Key_Addr,
                                 Object_Key   => Key),
                 Sync,
                 Pend_Req.Req.Operation.all);
            end;
      end case;

      --  Marshall the request's Body not yet implemented
      List :=  List_Of (Pend_Req.Req.Args);
      for I in 1 ..  Get_Count (Pend_Req.Req.Args) loop
          Arg := NV_Sequence.Element_Of(List.all, Positive(I));
          Marshall (Ses.Buffer_Out, Arg);
      end loop;

      if  Length (Ses.Buffer_Out) > Maximum_Message_Size
      then
           Fragment_Next := True;
      end if;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request,  Length(Ses.Buffer_Out));

         when Ver1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length(Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length(Ses.Buffer_Out),
               Fragment_Next);
      end case;

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end Request_Message;

   ------------------------
   -- No_Exception_Reply --
   ------------------------


   procedure No_Exception_Reply
     (Ses           : access GIOP_Session;
      Request_Id    : in CORBA.Unsigned_Long;
      Fragment_Next : out Boolean)

   is
      Header_Buffer : Buffer_Access := new Buffer_Type;
   begin


      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

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


       --  Marshall the reply's Body
      Marshall (Ses.Buffer_Out, Pend_Req.Req.Result);

      if Length(Ses.Buffer_Out)  > Maximum_Message_Size then
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
               Length(Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out),
               Fragment_Next);

      end case;

      Prepend(Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end No_Exception_Reply;

   ---------------------
   -- Exception_Reply --
   ---------------------

   procedure Exception_Reply
     (Ses             : access GIOP_Session;
      Exception_Type  : in Reply_Status_Type;
      Occurence       : in CORBA.Exception_Occurrence;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
   begin

      pragma Assert (Exception_Type in User_Exception  .. System_Exception);

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_Exception
              (Ses.Buffer_Out, Pend_Req.Request_Id,
               Exception_Type, Occurence);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length(Ses.Buffer_Out));

         when Ver1 =>

            GIOP.GIOP_1_1.Marshall_Exception
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Exception_Type,
               Occurence);

            if Length (Ses.Buffer_Out) > Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length(Ses.Buffer_Out),
               Fragment_Next);

         when Ver2 =>

            GIOP.GIOP_1_2.Marshall_Exception
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Exception_Type,
               Occurence);

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);


      end case;

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end  Exception_Reply;

   -------------------------------------------------------
   --  Location Forward
   --------------------------------------------------------

   procedure Location_Forward_Reply
     (Ses             : access GIOP_Session;
      Forward_Ref     : in Droopi.References.IOR.IOR_Type;
      Fragment_Next   : out Boolean)

   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
   begin

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.GIOP_1_0.Marshall_Location_Forward
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Forward_Ref);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.GIOP_1_1.Marshall_Location_Forward
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
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
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               GIOP.Location_Forward,
               Forward_Ref);

            if  Length (Ses.Buffer_Out) >
              Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length(Ses.Buffer_Out),
               Fragment_Next);
      end case;

     Prepend (Header_Buffer.all, Ses.Buffer_Out);
     Release (Header_Buffer);


   end Location_Forward_Reply;


   -------------------------------------------------------
   --  Need_Addressing_Mode_Message
   --------------------------------------------------------

   procedure Need_Addressing_Mode_Message
     (Ses             : access GIOP_Session;
      Address_Type    : in Addressing_Disposition)

   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;

   begin

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      if Ses.Minor_Version /=  Ver2 then
         raise GIOP_Error;
      end if;

      GIOP.GIOP_1_2.Marshall_Needs_Addressing_Mode
        (Ses.Buffer_Out, Pend_Req.Request_Id, Address_Type);

      GIOP.GIOP_1_2.Marshall_GIOP_Header
        (Header_Buffer,
         GIOP.Reply, Length (Ses.Buffer_Out),
         False);

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end Need_Addressing_Mode_Message;

   ----------------------------
   -- Cancel_Request_Message --
   ----------------------------

   procedure Cancel_Request_Message
     (Ses             : access GIOP_Session)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;

   begin

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      case Ses.Minor_Version is
         when Ver0 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Pend_Req.Request_Id);
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out));

         when Ver1 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Pend_Req.Request_Id);
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when Ver2 =>

            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Pend_Req.Request_Id);
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when others =>
            raise GIOP_ERROR;
      end case;

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end Cancel_Request_Message;


   ----------------------------
   -- Locate_Request_Message --
   ----------------------------

   procedure Locate_Request_Message
     (Ses             : access GIOP_Session;
      Object_key      : in Objects.Object_Id_Access;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;

   begin

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Object_Key);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out));

         when Ver1 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Object_Key);

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);

         when Ver2 =>
            GIOP.GIOP_1_2.Marshall_Locate_Request
              (Ses.Buffer_Out,
               Pend_Req.Request_Id,
               Target_Address'(Address_Type => Key_Addr,
                               Object_Key =>  Object_Key));

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);
      end case;

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
      Release (Header_Buffer);

   end Locate_Request_Message;

   --------------------------
   -- Locate_Reply_Message --
   --------------------------

   procedure Locate_Reply_Message
     (Ses             : access GIOP_Session;
      Locate_Status   : in Locate_Status_Type)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;

   begin
      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_Out, Message_Header_Size);

      if (Ses.Minor_Version = Ver0 or else Ses.Minor_Version = Ver1) and then
        (Locate_Status = Object_Forward_Perm or else
        Locate_Status = Loc_System_Exception or else
        Locate_Status = Loc_Needs_Addressing_Mode)
      then
         raise GIOP_Error;
      end if;

      GIOP.Marshall_Locate_Reply
        (Ses.Buffer_Out, Pend_Req.Request_Id, Locate_Status);

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

      Prepend (Header_Buffer.all, Ses.Buffer_Out);
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
      New_Ref    : Droopi.References.IOR.IOR_Type := Unmarshall(Buffer);
      Prof_Array : Droopi.References.Profile_Array := Profiles_Of(New_Ref.Ref) ;
      Prof_Temp  : Profile_Access;

   begin
      pragma Debug (O ("Reply Message : Received Location_Forward"));
      for I in Prof_Array'Range loop
         if Prof_Array(I).all in Binding_Data.IIOP.IIOP_Profile_Type then
            Prof_Temp := Prof_Array(I);
            exit;
         end if;
      end loop;
      return Prof_Temp;
   end Select_Profile;

   -------------------
   -- Store_Request --
   -------------------

   procedure Store_Request
     (R       :  Requests.Request_Access;
      Profile : Profile_Access)
   is
      use CORBA;
   begin
      Pend_Req.Req := R;
      Pend_Req.Request_Id := Current_Request_Id;
      Current_Request_Id := Current_Request_Id + 1;
      Pend_Req.Target_Profile := Profile;
   end Store_Request;



  ----------------------
  -- Request_Received --
  ----------------------

  procedure Request_Received
     (Ses : access GIOP_Session)
  is
      use CORBA;
      use CORBA.Impl;
      use CORBA.AbstractBase;
      use Obj_Adapters.Simple;
      use Binding_Data.IIOP;
      use Internals;
      use Internals.NV_Sequence;

      use References;

      use Droopi.Objects;

      Request_Id        :  CORBA.Unsigned_Long;
      Response_Expected :  Boolean;
      Object_Key        :  Objects.Object_Id_Access := null;
      Operation         :  CORBA.String_Ptr := null;
      Principal         :  CORBA.String;

      Req    : Request_Access := null;
      Args   : CORBA.NVList.Ref;
      Obj    : Object_Ptr;
      Result : CORBA.NamedValue;
      Temp   : CORBA.NamedValue;
      Target_Profile : Binding_Data.Profile_Access := new IIOP_Profile_Type;
      Target : References.Ref;
      Target_Ref  : Target_Address_Access := null ;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);
      Transp_AP : Droopi.Transport.Transport_Access_Point_Access
                    := new Socket_Access_Point;
      Temp_Arg : CORBA.NamedValue;
      List     : NV_Sequence_Access;

   begin

      case Ses.Minor_Version is
         when Ver0 =>
            GIOP.GIOP_1_0.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key.all,
               Operation.all);


         when Ver1 =>
            GIOP.GIOP_1_1.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key.all,
               Operation.all);

         when Ver2 =>
            GIOP.GIOP_1_2.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Target_Ref.all,
               Operation.all);

            case Target_Ref.Address_Type is
             when Key_Addr =>
                    Object_Key := Target_Ref.Object_Key;
             when Profile_Addr =>
                    Object_Key.all := Get_Object_key(Target_Ref.Profile.all);
             when Reference_Addr =>
                    raise Not_Implemented;
            end case;

      end case;

      Args := Obj_Adapters.Get_Empty_Arg_List
             (Object_Adapter (ORB).all, Object_Key.all, To_Standard_String(Operation.all));


      -- Unmarshalling of arguments
      List :=  List_Of (Pend_Req.Req.Args);
      for I in 1 .. Get_Count (Args) loop
         Temp_Arg :=  NV_Sequence.Element_Of(List.all, Positive(I));
         Unmarshall(Ses.Buffer_In, Temp_Arg);
         NV_Sequence.Replace_Element(List.all, Positive(I), Temp_Arg);
      end loop;

      ----
      Result := (Name     => To_CORBA_String ("Result"),
                 Argument => Obj_Adapters.Get_Empty_Result
                 (Object_Adapter (ORB).all, Object_Key.all, To_Standard_String(Operation.all)),
                 Arg_Modes => 0);

      if Ses.Minor_Version = Ver2 and
              then  Target_Ref.Address_Type /= Key_Addr
         then

           if  Target_Ref.Address_Type = Profile_Addr then
             References.Create_Reference ( (1 => Target_Ref.Profile), Target);
           else
             Target := Target_Ref.Ref.IOR.Ref;
           end if;

      --else
          -- Creating the profile need the Socket Address
          --  Transp_AP.Socket := Lower(Lower(Ses)).Socket;
          -- Target_Profile := Create_Profile(Prof_Factory, Transp_AP, Object_Key);
          --  Create_Reference ((1 => Target_Profile), Target);
      end if;

      Create_Request
        (Target    => Target,
         Operation => To_Standard_String(Operation.all),
         Arg_List  => Args,
         Result    => Result,
         Req       => Req);

      Emit_No_Reply
        (Component_Access (ORB),
         Queue_Request'(Request => Req,
                        Requestor => Component_Access (Ses)));

      Pend_Req.Req := Req;
      Pend_Req.Request_Id := Request_Id;


  end Request_Received;


   -----------------------------------------
   ---   Receiving a  Reply Message
   -----------------------------------------

   procedure Reply_Received
     (Ses               : access GIOP_Session)

   is
      use CORBA;
      use CORBA.Exceptions;
      use References.IOR;
      use Binding_Data.IIOP;
      Reply_Status  : Reply_Status_Type;
      Result : CORBA.NamedValue;
      Request_Id : CORBA.Unsigned_Long;

      Req    : Request_Access := null;
      Args   : CORBA.NVList.Ref;
      Target_Profile : Binding_Data.Profile_Access
        := new IIOP_Profile_Type;
      Target : References.Ref;
   --   Target_Switch : Target_Address_Access;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);

      Transp_AP : Transport_Access_Point_Access := new Socket_Access_Point;

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

      if Request_Id /= Pend_Req.Request_Id then
         raise GIOP_Error;
      end if;

      case Reply_Status is

         when No_Exception =>

            Pend_Req.Req.Result :=
              (Name     => To_CORBA_String ("Result"),
               Argument => Obj_Adapters.Get_Empty_Result
               (Object_Adapter (ORB).all,
                Get_Object_Key(IIOP_Profile_Type(Pend_Req.Target_Profile.all)),
                Pend_Req.Req.Operation.all), Arg_Modes => ARG_OUT);

            Unmarshall (Ses.Buffer_In, Pend_Req.Req.Result);
            Emit_No_Reply
              (Component_Access (ORB),
               Queue_Request'(Request   => Pend_Req.Req,
                              Requestor => Component_Access (Ses)));


         when User_Exception =>
            raise Not_Implemented;

         when System_Exception =>
            Unmarshall_And_Raise(Ses.Buffer_In);

         when Location_Forward | Location_Forward_Perm =>

            declare
               Ref     : References.IOR.IOR_Type := Unmarshall(Ses.Buffer_In);
               TE      : Transport_Endpoint_Access;
               New_Ses : Session_Access;
            begin
               Pend_Req.Target_Profile := Select_Profile (Ses.Buffer_In);
               Binding_Data.IIOP.Bind_Profile(IIOP_Profile_Type(Pend_Req.Target_Profile.all),
                     TE, Component_Access(New_Ses));
               -- release the previous session not yet implemented
               Invoke_Request (New_Ses, Pend_Req.Req.all);
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
      Request_Id    : CORBA.Unsigned_Long;
      Object_Key    : Objects.Object_Id_Access :=null;
      Target_ref    : Target_Address_Access := null;
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





  ------------------------------------------------------
  --  Initialize the Profile factory
  -----------------------------------------------------

  procedure Initialize_Factory
     (Prof_Factory : in out Binding_Data.Profile_Factory_Access)
  is
  begin
        Prof_Factory := new Binding_Data.IIOP.IIOP_Profile_Factory;
  end Initialize_Factory;




   ------------------------------------
   --     exported functions
   ------------------------------------

   -----------------------------------------
   ---  Create the Session ---------------
   ---------------------------------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access)
   is

   begin
      Session := new GIOP_Session;
      GIOP_Session(Session.all).Buffer_In  := new Buffers.Buffer_Type;
      GIOP_Session(Session.all).Buffer_Out  := new Buffers.Buffer_Type;
   end Create;


   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (S   : access GIOP_Session;
      R   : Requests.Request)
   is
      use Buffers;
      use Binding_Data.IIOP;
      use Droopi.Filters.Slicers;
      use Droopi.Filters.Interface;
      use Droopi.Objects;
      Buf            : Buffer_Access := new Buffer_Type;
      Sli            : Filter_Access;
      Fragment_Next  : Boolean := False;
      Arg_Count      : CORBA.Long;
      Arg            : CORBA.NamedValue;
      Data_Send      : Data_Out; --Root_Data_Unit := new Data_Out;
      Message_Size   : CORBA.Unsigned_Long;
   begin

      if S.Role  = Server then
         raise GIOP_Error;
      end if;

      Sli := Filter_Access(Lower (S));
      Release_Contents (S.Buffer_Out.all);

      --  fragmentation not yet implemented
      --  Message_Size:= Length (Buf1);

      --  if Message_Size > Maximum_Body_Size then
      --     Buf2 :=
      --  end if;

      if S.Object_Found = False then
         if  S.Nbr_Tries <= Max_Nb_Tries then
            declare
               Oid : Object_Id:= Get_Object_Key(Pend_Req.Target_Profile.all);
               Obj : Object_Id_Access := new Object_Id'(Oid);
            begin
               Locate_Request_Message(S, Obj, Fragment_Next);
               S.Nbr_Tries := S.Nbr_Tries + 1;
            end;
         else
            pragma Debug (O ("Number of tries exceeded"));
            Release (Buf);
            return;
         end if;
      else
         Request_Message(S, True, Fragment_Next);
         S.Object_Found := True;
         S.Nbr_Tries := 0;
      end if;


      --  Sending the message
      -- sending the data to lower layers
      Data_Send.Out_Buf := S.Buffer_In;
  --    Handle_Message(Ses, Data_Send);

      --  Expecting data
      Expect_Data (S, S.Buffer_In, Message_Header_Size);
      Release (Buf);

   exception

      when others =>
         Release (Buf);
         raise;

   end Invoke_Request;


   -------------------------------------
   --  Abort Request
   -------------------------------------

   procedure Abort_Request (S : access GIOP_Session;
                            R :  Requests.Request)
   is

   begin
      raise Not_Implemented;
   end Abort_Request;



   -------------------------------------
   --  Send Reply
   --------------------------------------

   procedure Send_Reply (S  : access GIOP_Session;
                         R   : Requests.Request)
   is
      use Buffers;
      use Representations.Cdr;
      use Droopi.Filters.Interface;
      Buf        : Buffer_Access := new Buffer_Type;
      Data_Send  : Data_Out;
      Fragment_Next  : Boolean := False;

   begin
      if S.Role = Client then
         raise GIOP_Error;
      end if;

      --Sli := Lower (S);

      Release_Contents (S.Buffer_Out.all);

      No_Exception_Reply (S, Pend_Req.Request_Id, Fragment_Next);

      --  Sending the message
    --  Sli.In_Buf := Ses.Buffer_Out;
      Data_Send.Out_Buf := S.Buffer_In;
  --    Handle_Message (S, Data_Send);

      --  Expecting data
      Expect_Data (S, S.Buffer_In, Message_Header_Size);

   end Send_Reply;


   ----------------------------------
   --  Handle Connect Indication ----
   ----------------------------------

   procedure Handle_Connect_Indication (S : access GIOP_Session)
   is

   begin
      pragma Debug (O ("Received new connection to echo service..."));

      Expect_Data (S, S.Buffer_In, Message_Header_Size);

   end Handle_Connect_Indication;


   procedure Handle_Connect_Confirmation (S : access GIOP_Session) is
   begin
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;


   procedure Handle_Data_Indication (S : access GIOP_Session)
   is
      use CORBA;
      use CORBA.NVList;

      use Binding_Data.IIOP;
      use Objects;
      use ORB;
      use References;
      use References.IOR;

   begin
      pragma Debug (O ("Received data on socket service..."));
      pragma Debug (Buffers.Show (S.Buffer_In.all));

      declare
      --   Sli    : Filters.Slicers.Slicer_Filter;
         Req           : Request_Access := null;
         Mess_Type     : Msg_Type;
         Mess_Size     :  CORBA.Unsigned_Long;
         Fragment_Next : Boolean;
         Success       : Boolean;

      begin


         if S.Expect_Header then
            Unmarshall_GIOP_Header (S, Mess_Type, Mess_Size,
                                    Fragment_Next, Success);
            if not Success then
               raise GIOP_Error;
            end if;
            S.Mess_Type_Received  := Mess_Type;
            S.Expect_Header := False;
            Expect_Data(S, S.Buffer_In, Stream_Element_Count(Mess_Size));
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
                     Req_Id        : CORBA.Unsigned_Long;
                     Locate_Status : Locate_Status_Type;
                  begin
                     Unmarshall_Locate_Reply
                           (S.Buffer_In, Req_Id, Locate_Status);
                     case Locate_Status is
                        when Object_Here =>
                           S.Object_Found := True;
                           Invoke_Request (S, Pend_Req.Req.all);

                        when Unknown_Object =>
                           raise GIOP_Error;

                        when Object_Forward | Object_Forward_Perm =>
                           declare
                              Ref     : References.IOR.IOR_Type := Unmarshall(S.Buffer_In);
                              TE      : Transport_Endpoint_Access;
                              New_Ses : Session_Access;
                           begin
                              Pend_Req.Target_Profile := Select_Profile (S.Buffer_In);
                              Binding_Data.IIOP.Bind_Profile(IIOP_Profile_Type(Pend_Req.Target_Profile.all),
                                                             TE, Component_Access(New_Ses));
                              -- release the previous session not yet implemented
                              Invoke_Request (New_Ses, Pend_Req.Req.all);
                           end;
                        when Loc_Needs_Addressing_Mode =>
                           raise Not_Implemented;

                        when Loc_System_Exception =>
                           raise Not_Implemented;


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
               raise Not_Implemented;

            when Fragment =>
               raise Not_Implemented;
         end case;
      end;

      S.Expect_Header := True;
      Buffers.Release_Contents (S.Buffer_In.all);
      Expect_Data (S, S.Buffer_In, Message_Header_Size);

      --  Prepare to receive next message.
   end Handle_Data_Indication;


   procedure Handle_Disconnect (S : access GIOP_Session)
   is

   begin
    raise Not_Implemented;
   end Handle_Disconnect;



end Droopi.Protocols.GIOP;

