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

with Ada.Streams; Uses Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;

with CORBA.CDR;         use CORBA.CDR;
with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque; use Droopi.Opaque;
with Droopi.Buffers;use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols; use Droopi.Protocols;


pragma Elaborate_All (Broca.Debug);

package body Droopi.Protocols.GIOP.GIOP_1_1 is


   -- debug information not yet implemented

   --Flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   --procedure O is new Broca.Debug.Output (Flag);


   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 1;

   Endianess_Bit: constant Integer:=1;
   Fragment_Bit: constant Integer:=2;

   Response_Flags: constant array(range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#) ;

   type Bits_8 is CORBA.Octet;

   Default_Principal : Principal;


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------
   -- we must redefine this procedure of Marshalling different from GIOP 1.0

   procedure GIOP_Header_Marshall
     ( Buffer        : access Buffer_Type;
       Message_Type  : in MsgType;
       Message_Size  : in Stream_Element_Offset;
       Fragment_Next :in Boolean)

   is
      use Corba.CDR;
      Flags: Bits_8:=0;
      I : Integer;
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

   end Marshall_GIOP_Header;


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
      use Corba.CDR;
      Reserved: constant Corba.Octet:=0;
      I: Integer;

    begin
      --pragma Debug (O ("Request_Message_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

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
      Corba.CDR.Marshall
        (Ses.Buffer_Out,
           Stream_Element_Array'(Default_Principal));
    end Send_Request_Marshall;


   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


  procedure Fragment_Marshall
    ( Buffer      : access Buffer_Type;
      Request_Id  : in CORBA.Unsigned_Long
    )

   is
      use Broca.CDR;
      --I: Integer;

   begin
      --pragma Debug (O ("Send_Request_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      Marshall(Buffer, Request_Id);

   end Fragment_Marshall;

end Droopi.Protocols.GIOP.GIOP_1_1;
