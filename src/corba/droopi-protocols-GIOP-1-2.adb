------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           G I O P . G I O P 1.2                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; Uses Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;
--with CORBA.Impl;

with CORBA.CDR;         use CORBA.CDR;
with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque;  use Droopi.Opaque;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols; use Droopi.Protocols;



pragma Elaborate_All (Broca.Debug);

package body Droopi.Protocols.GIOP.GIOP_1_2 is

   --Flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   --procedure O is new Broca.Debug.Output (Flag);

   -- added from broca.sequences

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 2;


   --Byte_Order_Offset : constant := 6;
   --  The offset of the byte_order boolean field in
   --  a GIOP message header.

   Response_Flags: constant array(range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#) ;

   type Bits_8 is CORBA.Octet;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure GIOP_Header_Marshall
     (Buffer        : access Buffer_Type;
      Message_Type  : in MsgType;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)
   is
      use Broca.CDR;
      Flags:Bits_8:=0;

   begin

      --  1.2.1 The message header.

      --  Magic
      if (Fragment_Next=True) and  (Message_Size+12 mod 8 =0) then
          raise GIOP_ERROR;
      end if;

      for I in Magic'Range loop
         Marshall (Buffer, CORBA.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Flags
      if(Endianness(Buffer.all) = Little_Endian) then
         Flags = Flags or 2**Endianess_bit;
      end if;

      if(Fragment_Next=True) then
         Flags = Flags or 2**Fragment_bit;
      end if;

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long (Message_Size));

   end Marshall_GIOP_Header;


  ------------------------------------------------
  --  Marshaling of the  GIOP 1.2 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------



  procedure Request_Message_Marshall
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Operation         : in Requests.Operation_Id;
      Addess_Type       : in AddressingDisposition;
      Target_Ref        : in TargetAddress;
      Sync_Type         : in CORBA.SyncScope)

  is
      use Broca.CDR;

      Reserved: constant Corba.Octet:=0;
      I: Integer;

  begin
      --pragma Debug (O ("Request_Message_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      --Ses.Request_Id := Get_Request_Id(Ses);
      Marshall (Buffer, Request_Id);

      -- Response_Flags
      Marshall(Buffer, Response_Flags(CORBA.SyncScope'Pos(Sync_Type)));

      -- Reserved
      for I in 1..3 loop
       Marshall(Buffer , Reserved);
      end loop;

      -- Target Address Not yet implemented
      Marshall(Buffer,
             CORBA.Unsigned_Short(Addressing_Disposition'Pos(Address_Type)));

      case Address_Type is
       when KeyAddr =>
         Marshall(Ses.Buffer_Out,
          Binding_Data.Get_Objet_Key(Target_Ref.Profile.all));
       when ProfileAddr =>
         -- not yet implemented
         Marshall_Tagged_Profile
           (Ses.Buffer_Out, Target_Ref.Profile.all);

        -- IOR not yet implemented
       --when ReferenceAddr =>
      end case

      --  Operation
      Marshall (Buffer, CORBA.String'(Request_Id));

      --  Service context
      Marshall (Buffer,  Stream_Element_Array'(Service_Context_List_1_2));
   end Send_Request_Marshall;



   -----------------------------
   -- Marshalling of Reply messages
   --
   -- Marshalling of Reply with
   ----------------------------------

   -------------------------------------
   --  Exception Marshall
   -------------------------------------


  procedure Exception_Marshall
    (
      Buffer      : access Buffers.Buffer_Type;
      Request_Id   : access CORBA.Unsigned_Long ;
      Reply_Type  : in ReplyStatusType_1_2 range User_Exception..System_Exception;
      Occurence   : in CORBA.Exception_Occurrence)

   is
     use Corba.CDR
   begin
      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);

      --  Service context
      Marshall (Buffer,  Stream_Element_Array'(Service_Context_List_1_2));

      --  Occurrence
      Marshall(Buffer, Ocurrence);
   end Marshall;



  -------------------------------------
  -- Location Forward Reply Marshall
  ------------------------------------

    procedure Location_Forward_Marshall
    (
      Buffer        :   access Buffers.Buffer_Type;
      Request_Id    :   in  CORBA.Unsigned_Long;
      Reply_Type    :   in  ReplyStatusType_1_2 range Location_Forward .. Location_Forward_Perm;
      Target_Ref    :   in  Droopi.References)
    is
       use Corba.CDR;
    begin

         -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);

      --  Service context
      Marshall (Buffer,  Stream_Element_Array'(Service_Context_List_1_2));

      -- not yet implemented
      --Broca.CDR.Marshall(Message_Body_Buffer'Access,
      --     Target_Ref);
    end;

    ------------------------------------
    -- Needs Addessing Mode Marshall
    ------------------------------------

    procedure Needs_Addressing_Mode_Marshall
    ( Buffer              : access Buffers.Buffer_Type;
      Request_Id          : in CORBA.Unsigned_Long;
      Address_Type        : in GIOP.Addressing_Disposition)
    is

    begin

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);

      --  Service context
      Marshall (Buffer,  Stream_Element_Array'(Service_Context_List_1_2));

      --  Address Disposition
      Marshall(Buffer,
             CORBA.Unsigned_Short(Addressing_Disposition'Pos(Address_Type)));

    end;


   -------------------------------------
   --- Cancel Request Message Marshalling
   -------------------------------------



   --procedure Cancel_Request_Marshall
   -- (Buffer     : access Buffers.Buffer_Type;
   --  Request_Id : in CORBA.Unsigned_Long)

   --is
   --   use Corba.CDR;
   --   --I: Integer;

   --begin
   --   --pragma Debug (O ("Send_Request_Marshall: invoking "
   --   --  & CORBA.To_Standard_String (Operation)));

   --   --  Reserve space for message header
   --   Set_Initial_Position
   --     (Buffer, Message_Header_Size);

   --   -- Request id
   --   Marshall(Buffer, Req.Request_Id);

   --  end Cancel_Request_Marshall;

 ------------------------------------------------
 ---  Locate Request Message Marshall
 -----------------------------------------------


   procedure Locate_Request_Marshall
    (Buffer            : access Buffer_Type;
     Request_Id        : in Corba.Unsigned_long;
     Address_Type      : in Addressing_Disposition;
     Target_Ref        : in Target_Address)

   is
      use Corba.CDR;
      I: Integer;

   begin

     -- pragma Debug (O ("Send_Request_Marshall: invoking "
     --   & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffe, Message_Header_Size);

      -- Request id
      Marshall (Buffer, Request_Id);


      -- Target Address Not yet implemented

      Marshall(Handler.Buffer'Access,
       CORBA.Unsigned_Short(Addressing_Disposition'Pos(Address_Type)));

      case Address_Type is
       when KeyAddr =>
         Corba.CDR.Marshall(Sess.Buffer_Out,
          Binding_Data.Get_Objet_Key(Sess.Profile.all));
       when ProfileAddr =>
        Marshall_Tagged_Profile
             (Ses.Buffer_Out, Sess.Profile.all);
       --when ReferenceAdd =>

      end case;

   end Locate_Request_Marshall;


   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


   procedure Fragment_Marshall
    ( Buffer       : access Buffers.Buffer_Type;
      Request_Id   : in CORBA.Unsigned_Long)
   is

   begin
     --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      Marshall(Buffer, Request_Id);

   end  Fragment_Marshall;





  --procedure Fragment_Marshall
  --  ( Handler           : in out Request_Handler;
  --    Target_Ref        : in CORBA.AbstractBase.Ref'Class;
  --  )

   --is
   --   use Broca.CDR;
   --   Target : constant Broca.Object.Object_Ptr
   --     := Broca.Object.Object_Ptr
   --     (CORBA.AbstractBase.Object_Of (Target_Ref));
   --   I: Integer;

   --begin
   --   pragma Debug (O ("Send_Request_Marshall: invoking "
   --     & CORBA.To_Standard_String (Operation)));

   --   Handler.Profile := Object.Find_Profile(Target);
   --   Handler.Connection := Binding_Data.Find_Connection (Handler.Profile);


      --  Reserve space for message header
   --   Set_Initial_Position
   --     (Handler.Buffer'Access, Message_Header_Size);

      -- Request id
   --   Handler.Request_Id := Protocols.Get_Request (Handler.Connection);
   --   Marshall (Handler.Buffer'Access, Handler.Request_Id);


   --end Fragment_Marshall;

end Droopi.Protocols.GIOP.GIOP_1_2;
