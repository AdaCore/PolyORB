------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                        G I O P. G I O P 1.0                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--                                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;

with CORBA.CDR; use CORBA.CDR;
with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque;       use Droopi.Opaque;
with Droopi.Buffers;      use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols;    use Droopi.Protocols;
with Droopi.References;    use Droopi.References;


with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Droopi.Protocols.GIOP.GIOP_1_0 is

   -- debugging
   --flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   --procedure O is new Broca.Debug.Output (Flag);



   -- Version

   Major_Version : constant CORBA.Octet:= 1;
   Minor_Version : constant CORBA.Octet:= 0


   Byte_Order_Offset : constant := 6;
   --  The offset of the byte_order boolean field in
   --  a GIOP message header.


   Default_Principal : Principal;

   ------------------
   -- To_Principal --
   -------------------

   function To_Principal
     (S : String)
     return Principal
   is
      Octets : Stream_Element_Array(1..S'Length + 1);
   begin
      for I in Octets'First .. Octets'Last - 1 loop
         Octets (I) := CORBA.Octet
           (Character'Pos (S (S'First + I - 1)));
      end loop;
      Octets (Octets'Last) := 0;
      return Octets;
   end To_Principal;


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure GIOP_Header_Marshall
     (Buffer       : access Buffer_Type;
      Message_Type : in MsgType;
      Message_Size : in Stream_Element_Offset)
   is
      use Corba.CDR;
   begin

      if Message_Type = Fragment then
         raise GIOP_Error;
      end if;


       --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, CORBA.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Endianness
      Marshall (Buffer, CORBA.Boolean
                (Endianness (Buffer.all) = Little_Endian));

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long (Message_Size));

   end Marshall_GIOP_Header;


  ------------------------------------------------
  -- Marshaling of the  GIOP 1.0 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------

    procedure Request_Message_Marshall
     (Buffer            : access Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Target_Profile    : in Binding_Data.Profile_Type;
      Response_Expected : in Boolean;
      Operation         : in Requests.Operation_Id);

    is
      use Corba.CDR;

    begin
      --pragma Debug (O ("Request_Message_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response expected
      Marshall (Buffer, Response_Expected);

      --  Object key
      Corba.CDR.Marshall
        (Buffer,
         Binding_Data.Get_Object_Key(Target_Profile));

      --  Operation
      Marshall (Buffer, CORBA.String (Operation));

      --  Principal
      Corba.CDR.Marshall
        (Buffer,
           Stream_Element_Array'(Default_Principal));

    end Send_Request_Marshall;


   ----------------------------------
   -- Marshalling of Reply messages
   --
   -- Marshalling of Reply with
   ----------------------------------




   -------------------------------------
   --  System Exception Marshall
   -------------------------------------


   procedure Exception_Marshall
    ( Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_long;
      Exception_Type   : in ReplyStatusType_1_0 range User_Exception..System_Exception;
      Occurence        : in CORBA.Exception_Occurrence
    )
   is
     use CORBA.CDR;
   begin

      Set_Initial_Position
       (Buffer, Message_Header_Size);

      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Exception_Type);

      -- Occurrence
      Marshall(Buffer, Occurence);
   end Marshall;



    -------------------------------------
    -- Location Forward Reply Marshall
    ------------------------------------

    procedure Location_Forward_Marshall
    (
      Buffer           : access Buffer_Type;
      Request_Id       : in  CORBA.Unsigned_Long;
      Forward_Ref      : in  Droopi.References.Ref
    )
    is

    begin

     Set_Initial_Position
       (Buffer, Message_Header_Size);

      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, GIOP_1_0.Location_Forward);


      -- Object reference
      Broca.CDR.Marshall(Buffer, Forward_Ref);
    end;


   -------------------------------------
   --- Cancel Request Message Marshalling
   -------------------------------------


  procedure Cancel_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in Corba.Unsigned_Long;
     )

  is
      use Corba.CDR;
  begin
      --pragma Debug (O ("Send_Request_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      Marshall(Buffer, Request_Id);

  end Cancel_Request_Marshall;

 ------------------------------------------------
 ---  Locate Request Message Marshall
 -----------------------------------------------


   procedure Locate_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Profile_Ref       : in Binding_Data.Profile_Type );
   is
      use Corba.CDR;
   begin

      --pragma Debug (O ("Send_Request_Marshall: invoking "
      --  & CORBA.To_Standard_String (Operation)));

      --  Reserve space for message header
      Set_Initial_Position
        (Buffer, Message_Header_Size);

      -- Request id
      Marshall (Buffer, Request_Id);

      -- Object Key
      Corba.CDR.Marshall(Buffer,
          Binding_Data.Get_Objet_Key(Profile_Ref));

   end Locate_Request_Marshall;

  -------------------------------------
  --- Locate_Reply_Marshall ----------
  ----------------------------------

  procedure Locate_Reply_Marshall
   (
     Buffer         : access Buffer_Type;
     Request_Id     : in Corba.Unsigned_Long;
     Locate_Status  : in LocateReplyStatus_1_0)
  is

  begin
       -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Locate_Status);
  end;


   ---------------------------------
   -- Request_Handler subprograms --
   ---------------------------------

   --procedure Free is new
   --  Ada.Unchecked_Deallocation (Element_Stream_Array, Zone_Access);

   -------------
   -- Release --
   -------------

   --procedure Release (H : in out Request_Handler) is
   --begin
   --   Release (H.Buffer);
   --   Free (H.Data.Message_Body);
   --end Release;

   procedure Set_Default_Principal
     (P : Principal) is
   begin
      Default_Principal := P;
   end Set_Default_Principal;



end Droopi.Protocols.GIOP.GIOP_1_0;

