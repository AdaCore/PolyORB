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
with Ada.Strings;
with Ada.Strings.Unbounded;

with CORBA; use CORBA;
with CORBA.Exceptions;
with CORBA.Exceptions.Stack;

with Droopi.Opaque;              use Droopi.Opaque;
with Droopi.Buffers;             use Droopi.Buffers;
with Droopi.Binding_Data;        use Droopi.Binding_Data;

with Droopi.Protocols;           use Droopi.Protocols;
with Droopi.References;          use Droopi.References;
with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Log;

with Sequences.Unbounded;

pragma Elaborate_All (Droopi.Log);

package body Droopi.Protocols.GIOP.GIOP_1_1 is


   Endianess_Bit: constant Integer:=1;
   Fragment_Bit: constant Integer:=2;

   --Response_Flags: constant array(range 0..3) of CORBA.Octet:=
   --                (0,16#1#, 16#2#, 16#3#) ;

   --type Bits_8 is CORBA.Octet;

   Nobody_Principal : constant Ada.Strings.Unbounded.Unbounded_String
      := Ada.Strings.Unbounded.To_Unbounded_String ("nobody");


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------
   -- we must redefine this procedure of Marshalling different from GIOP 1.0

   procedure Marshall_GIOP_Header
     ( Buffer        : access Buffer_Type;
       Message_Type  : in Msg_Type;
       Message_Size  : in Stream_Element_Offset;
       Fragment_Next : in Boolean)

   is

      use representations.CDR;
      use Droopi.Buffers;
      Flags: CORBA.Octet:=0;
   begin

      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, CORBA.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Flags
      if Endianness(Buffer.all) = Little_Endian then
         Flags := Flags or 2**Endianess_bit;
      end if;

      if Fragment_Next = True then
         Flags := Flags or 2**Fragment_bit;
      end if;

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long (Message_Size));

   end  Marshall_GIOP_Header;


  ------------------------------------------------
  --  Marshaling of the  GIOP 1.1 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------

   procedure Marshall_Request_Message
     (Buffer             : access Buffers.Buffer_Type;
      Request_Id         : in CORBA.Unsigned_Long;
      Target_Profile     : in Binding_Data.IIOP.IIOP_Profile_Type;
      Response_Expected  : in Boolean;
      Operation          : in Requests.Operation_Id)

    is
      use Representations.CDR;
      Reserved: constant CORBA.Octet:=0;

    begin

      -- Service context
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(0))));
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(1))));


      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response expected
      Marshall (Buffer, Response_Expected);

      -- Reserved
      for I in 1..3 loop
       Marshall(Buffer, Reserved);
      end loop;

       --  Object key
      Marshall
        (Buffer, Binding_Data.IIOP.Get_Object_Key(Target_Profile));

      --  Operation
      Marshall (Buffer, Operation);


      --  Principal
      Marshall(Buffer,
               CORBA.String(Nobody_Principal));

    end Marshall_Request_Message;


   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long)

   is
    use  representations.CDR;
   begin


     --  Service context
     Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(0))));
     Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(1))));

     -- Request id
     Marshall(Buffer, Request_Id);

     -- Reply Status
     Marshall(Buffer, GIOP.No_Exception);

   end Marshall_No_Exception;



   -------------------------------------
   --  System Exception Marshall
   -------------------------------------

   procedure Marshall_Exception
    ( Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_long;
      Exception_Type   : in Reply_Status_Type;
      Occurence        : in CORBA.Exception_Occurrence)
   is
     use representations.CDR;
   begin

      pragma Assert (Exception_Type in User_Exception .. System_Exception);

      --  Service context
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(0))));
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(1))));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Exception_Type);

      -- Occurrence
      Marshall(Buffer, Occurence);
   end Marshall_Exception;



    -------------------------------------
    -- Location Forward Reply Marshall
    ------------------------------------

    procedure Marshall_Location_Forward
    ( Buffer           : access Buffer_Type;
      Request_Id       : in  CORBA.Unsigned_Long;
      Forward_Ref      : in  Droopi.References.IOR.IOR_Type)
    is
      use References.IOR;
    begin

      --  Service context
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(0))));
      Marshall(Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_1(1))));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, GIOP.Location_Forward);

      -- Object reference
      References.IOR.Marshall(Buffer, Forward_Ref);

    end Marshall_Location_Forward;



   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


  procedure Marshall_Fragment
    ( Buffer      : access Buffer_Type;
      Request_Id  : in CORBA.Unsigned_Long)

   is
      use representations.CDR;
   begin


      -- Request id
      Marshall(Buffer, Request_Id);
   end Marshall_Fragment;

   ----------------------------------
   --- Request Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Request_Message
     ( Buffer            : access Buffer_Type;
       Request_Id        : out CORBA.Unsigned_Long;
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
      Service_Context2 := Unmarshall (Buffer);

      if Service_Context1 /= ServiceId'Pos(Service_Context_List_1_1(0)) or
          Service_Context2 /= ServiceId'Pos(Service_Context_List_1_1(1))
      then
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall(Buffer);

      --  Response expected
      Response_Expected := Unmarshall(Buffer);

      -- Reserved
      for I in 1..3 loop
       Reserved := UnMarshall(Buffer);
      end loop;

      --  Object Key
      Object_Key := Unmarshall (Buffer);

      -- Operation
      Operation :=  Unmarshall (Buffer);

      -- Principal
      Principal :=  Unmarshall (Buffer);

   end Unmarshall_Request_Message;


   -----------------------------------------
   --- Reply  Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)

   is
      Service_Context1  : CORBA.Unsigned_Long;
      Service_Context2  : CORBA.Unsigned_Long;
   begin

      --  Service context
      if Service_Context1 /= ServiceId'Pos( Service_Context_List_1_1(0)) or
          Service_Context1 /= ServiceId'Pos( Service_Context_List_1_1(1)) then

         pragma Debug (O (" Request_Message_Unmarshall : incorrect context");
         raise GIOP_Error;
      end if;

       --  Request id
      Request_Id := Unmarshall(Buffer);

      -- Reply Status

      Unmarshall(Reply_Status);

   end Unmarshall_Reply_Message;

end Droopi.Protocols.GIOP.GIOP_1_1;
