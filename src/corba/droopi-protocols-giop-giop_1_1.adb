------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           DROOPI.G I O P. GIOP 1.1                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;

with CORBA.CDR;         use CORBA.CDR;
with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque; use Droopi.Opaque;
with Droopi.Buffers;use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols; use Droopi.Protocols;


pragma Elaborate_All (Droopi.Log);

package body Droopi.Protocols.GIOP.GIOP_1_1 is



   Endianess_Bit: constant Integer:=1;
   Fragment_Bit: constant Integer:=2;

   Response_Flags: constant array(range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#) ;

   type Bits_8 is CORBA.Octet;

   Nobody_Principal : constant Ada.Strings.Unbounded.Unbounded_String
      := Ada.Strings.Unbounded.To_Unbounded_String ("nobody");


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------
   -- we must redefine this procedure of Marshalling different from GIOP 1.0

   procedure GIOP_Header_Marshall
     ( Buffer        : access Buffer_Type;
       Message_Type  : in MsgType;
       Message_Size  : in Stream_Element_Offset;
       Fragment_Next : in Boolean)

   is
      use representations.CDR;
      Flags: Bits_8:=0;
   begin

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
      if(Buffers.Endianness(Buffer.all) = Little_Endian) then
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

   end  GIOP_Header_Marshall;


  ------------------------------------------------
  --  Marshaling of the  GIOP 1.1 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------

   procedure Request_Message_Marshall
     (Buffer             : access Buffers.Buffer_Type;
      Request_Id         : in CORBA.Unsigned_Long
      Profile_Ref        : in Binding_Data.Profile_Type;
      Response_Expected  : in Boolean;
      Operation          : in Requests.Operation_Id)

    is
      use representations.CDR;
      Reserved: constant Corba.Octet:=0;

    begin

      --  Reserve space for message header
      Set_Initial_Position
        (Ses.Buffer_out, Message_Header_Size);

      -- Service context
      Marshall(Buffer,  Stream_Element_Array'(Service_Context_List_1_1));


      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response expected
      Marshall (Buffer, Response_Expected);

      -- Reserved
      for I in 1..3 loop
       Marshall(Buffer_Out, Reserved);
      end loop;

       --  Object key
      Marshall
        (Buffer,Binding_Data.Get_Object_Key(Profile_Ref));

      --  Operation
      Marshall (Buffer, CORBA.String(Operation));


      --  Principal
      Marshall(Ses.Buffer_Out,
               Corba.String'(Nobody_Principal));

    end Request_Message_Marshall;


   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure No_Exception_Marshall
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long)

   is
    use  representations.CDR;
   begin
     Set_Initial_Position
        (Buffer, Message_Header_Size);

     --  Service context
     Marshall(Buffer,  Stream_Element_Array'(Service_Context_List_1_1));

     -- Request id
     Marshall(Buffer, Request_Id);

     -- Reply Status
     Marshall(Buffer, GIOP.No_Exception);

   end No_Exception_Marshall;



   -------------------------------------
   --  System Exception Marshall
   -------------------------------------

   procedure Exception_Marshall
    ( Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_long;
      Exception_Type   : in ReplyStatusType range User_Exception..System_Exception;
      Occurence        : in CORBA.Exception_Occurrence)
   is
     use representations.CDR;
   begin

      Set_Initial_Position
       (Buffer, Message_Header_Size);

      --  Service context
      Marshall(Buffer,  Stream_Element_Array'(Service_Context_List_1_1));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Exception_Type);

      -- Occurrence
      Marshall(Buffer, Occurence);
   end  Exception_Marshall;



    -------------------------------------
    -- Location Forward Reply Marshall
    ------------------------------------

    procedure Location_Forward_Marshall
    ( Buffer           : access Buffer_Type;
      Request_Id       : in  CORBA.Unsigned_Long;
      Forward_Ref      : in  Droopi.References.Ref)
    is

    begin

     Set_Initial_Position
      (Buffer, Message_Header_Size);

      --  Service context
      Marshall(Buffer,  Stream_Element_Array'(Service_Context_List_1_1));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, GIOP.Location_Forward);


      -- Object reference
      Marshall(Buffer, Forward_Ref);

    end Location_Forward_Marshall;



   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


  procedure Fragment_Marshall
    ( Buffer      : access Buffer_Type;
      Request_Id  : in CORBA.Unsigned_Long)

   is
      use representations.CDR;

   begin

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      Marshall(Buffer, Request_Id);

   end Fragment_Marshall;

   ----------------------------------
   --- Request Unmarshalling
   -----------------------------------------

   procedure Request_Message_Unmarshall
     ( Buffer            : access Buffer_Type;
       Request_Id        : out Corba.Unisgned_Long;
       Response_Expected : out Boolean;
       Object_Key        : out Objects.Object_Id;
       Operation         : out Requests.Operation_Id)
   is
       Service_Context1  : CORBA.Unsigned_Long;
       Service_Context2  : CORBA.Unsigned_Long;
       Reserved          : CORBA.Octet;
       Principal         : CORBA.String;
   begin

      --  Service context
      Service_Context1 := Unmarshall (Buffer);
      Service_Context1 := Unmarshall (Buffer);

      if Service_Context /= ServiceId'Pos( Service_Context_List_1_1(0)) or
          Service_Context /= ServiceId'Pos( Service_Context_List_1_1(1)

         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"
                             & Service_Context'Img));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall(Buffer);

      --  Response expected
      Response_Expected := Unmarshall(Buffer);

      -- Reserved
      for I in 1..3 loop
       Reserved := Marshall(Buffer);
      end loop;

      --  Object Key
      Object_Key := Unmarshall (Buffer);

      -- Operation
      Operation :=  Unmarshall (Buffer);

      -- Principal
      Principal :=  Unmarshall (Buffer);

   end Request_Message_Unmarshall;


   -----------------------------------------
   --- Reply  Unmarshalling
   -----------------------------------------

   procedure Reply_Message_Unmarshall
      (Buffer       : access Buffer_Type;
       Request_Id   : out Corba.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)

   is
      Service_Context1  : CORBA.Unsigned_Long;
      Service_Context2  : CORBA.Unsigned_Long;
   begin

      --  Service context
      if Service_Context /= ServiceId'Pos( Service_Context_List_1_1(0)) or
          Service_Context /= ServiceId'Pos( Service_Context_List_1_1(1)) then

         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"
                             & Service_Context'Img));
         raise GIOP_Error;
      end if;

       --  Request id
      Request_Id := Unmarshall(Buffer);

      -- Reply Status

      Unmarshall(Reply_Status);

   end Reply_Message_Unmarshall;



end Droopi.Protocols.GIOP.GIOP_1_1;
