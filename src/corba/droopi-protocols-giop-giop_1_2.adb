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

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

with CORBA;


with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Exceptions;
with Sequences.Unbounded;
with Droopi.Opaque;  use Droopi.Opaque;
with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Binding_Data; use Droopi.Binding_Data;
with Droopi.Protocols; use Droopi.Protocols;
with Droopi.Binding_Data.Iiop;

pragma Elaborate_All (Droopi.Debug);

package body Droopi.Protocols.GIOP.GIOP_1_2 is


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure GIOP_Header_Marshall
     (Buffer        : access Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)
   is
      use representations.CDR;
      Flags:Bits_8:=0;

   begin

      --  1.2.1 The message header.

      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, CORBA.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Flags
      if Endianness(Buffer.all) = Little_Endian then
         Flags = Flags or 2**Endianess_bit;
      end if;

      if(Fragment_Next=True) then
         Flags = Flags or 2**Fragment_bit;
      end if;

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long(Message_Size));

   end  GIOP_Header_Marshall;


  ------------------------------------------------
  --  Marshaling of the  GIOP 1.2 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------


  procedure Request_Message_Marshall
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Operation         : in Requests.Operation_Id;
      Addess_Type       : in Addressing_Disposition;
      Target_Ref        : in Target_Address;
      Sync_Type         : in CORBA.SyncScope)

  is
      use representations.CDR;
      use Binding_Data.Iiop;
      use Droopi.References.IOR;
      Reserved: constant CORBA.Octet:=0;

  begin



      -- Request id
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
       when 0 =>
         Marshall(Ses.Buffer_Out,
          Binding_Data.Get_Objet_Key(Target_Ref.Profile.all));
       when 1 =>
         Marshall_IIOP_Profile_Body
           (Ses.Buffer_Out, Target_Ref.Profile.all);
       when 2 =>
         Marshall(Ses.Buffer_Out, Target_Ref.Ref.Selected_Profile_Index);
         Representations.IOR.Marshall(Ses.Buffer_Out, Target_Ref.Ref.Ior);
      end case

      --  Operation
      Marshall (Buffer, CORBA.String(Request_Id));

      --  Service context
      Marshall (Buffer,  Stream_Element_Array(Service_Context_List_1_2));

   end Request_Message_Marshall;



   -----------------------------
   -- Marshalling of Reply messages
   --
   -- Marshalling of Reply with
   ----------------------------------

   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure No_Exception_Marshall
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long)

   is
    use  representations.CDR;
   begin


     -- Request id
     Marshall(Buffer, Request_Id);

     -- Reply Status
     Marshall(Buffer, GIOP.No_Exception);

     --  Service context
     Marshall(Buffer,  Stream_Element_Array(Service_Context_List_1_2));

   end No_Exception_Marshall;



   -------------------------------------
   --  Exception Marshall
   -------------------------------------

   procedure Exception_Marshall
    ( Buffer      : access Buffers.Buffer_Type;
      Request_Id  : access CORBA.Unsigned_Long ;
      Reply_Type  : in Reply_Status_Type range User_Exception..System_Exception;
      Occurence   : in CORBA.Exception_Occurrence)

   is
     use  representations.CDR
   begin
      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);

      --  Service context
      Marshall (Buffer,  Stream_Element_Array(Service_Context_List_1_2));

      --  Occurrence
      Marshall(Buffer, Ocurrence);
   end  Exception_Marshall;



  -------------------------------------
  -- Location Forward Reply Marshall
  ------------------------------------

    procedure Location_Forward_Marshall
    ( Buffer        :   access Buffers.Buffer_Type;
      Request_Id    :   in  CORBA.Unsigned_Long;
      Reply_Type    :   in  Reply_Status_Type  range Location_Forward .. Location_Forward_Perm;
      Target_Ref    :   in  Droopi.References)
    is
       use  representations.CDR;
    begin

         -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);

      --  Service context
      Marshall (Buffer,  Stream_Element_Array'(Service_Context_List_1_2));


      Marshall(Message_Body_Buffer'Access,
           Target_Ref);
    end Location_Forward_Marshall;

    ------------------------------------
    -- Needs Addessing Mode Marshall
    ------------------------------------

    procedure Needs_Addressing_Mode_Marshall
    (Buffer              : access Buffers.Buffer_Type;
     Request_Id          : in CORBA.Unsigned_Long;
     Address_Type        : in GIOP.Addressing_Disposition)
    is

        use  representations.CDR

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

    end  Needs_Addressing_Mode_Marshall;


 ------------------------------------------------
 ---  Locate Request Message Marshall
 -----------------------------------------------


   procedure Locate_Request_Marshall
    (Buffer            : access Buffer_Type;
     Request_Id        : in CORBA.Unsigned_long;
     Address_Type      : in Addressing_Disposition;
     Target_Ref        : in Target_Address)

   is
      use representations.CDR;
      use Binding_Data.Iiop;
   begin



      -- Request id
      Marshall (Buffer, Request_Id);


      -- Target Address Not yet implemented

      Marshall(Handler.Buffer'Access,
       CORBA.Unsigned_Short(Addressing_Disposition'Pos(Address_Type)));

      case Address_Type is
       when KeyAddr =>
         CORBA.CDR.Marshall(Sess.Buffer_Out,
          Binding_Data.Get_Objet_Key(Target_Ref.Profile.all));
       when ProfileAddr =>
          Marshall_IIOP_Profile_Body
             (Ses.Buffer_Out, Target_Ref.Profile.all);
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
      use  representations.CDR;
   begin


      -- Request id
      Marshall(Buffer, Request_Id);

   end  Fragment_Marshall;


   -------------------------------------
   --- Request Unmarshall
   -------------------------------------

   procedure Request_Message_Unmarshall
     ( Buffer            : access Buffer_Type;
       Request_Id        : out CORBA.Unsigned_Long;
       Response_Expected : out Boolean;
       Target_Ref        : out Target_Address;
       Operation         : out Requests.Operation_Id)
   is
       use  representations.CDR;
       Service_Context      : array (range 0 ..9) of CORBA.Unsigned_Long;
       Reserved             : CORBA.Octet;
       Received_Flags       : CORBA.Octet;
       Address_Disposition  : CORBA.Octet;
   begin


      --  Request id
      Request_Id := Unmarshall(Buffer);

      --  Response expected
      Response_Flags := Unmarshall(Buffer);

      if(Received_flags= Response_Flags(3)) then
         Response_Expected = True;
      else
         Response_Expected = false;
      end if;

      -- Reserved
      for I in 1..3 loop
       Reserved := UnMarshall(Buffer);
      end loop;

      -- Target Ref
      Target_Address.Address_Type := Unmarshall(Buffer);

      case Address_Disp is
         when 0 =>
           Target_Ref.Object_Key := Unmarshall(Buffer);
         when 1 =>
           Target_Ref.Profile  := Unmarshall(Buffer);
         when 2 =>
            Target_Ref.Ref.Selected_Profile_Index := Unmarshall(Buffer);
            Target_Ref.Ref.Ior  := Unmarshall(Buffer);
      end case;

      -- Operation
      Operation :=  Unmarshall (Buffer);

      --  Service context
      for I in 0 .. 9 loop
        Service_Context(I) := Unmarshall (Buffer);
      end loop;

      for I in 0 .. 9 loop
       if Service_Context(I° /= ServiceId'Pos( Service_Context_List_1_1(I))
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"
                             & Service_Context'Img));
         raise GIOP_Error;
       end if;
      end loop;


   end Request_Message_Unmarshall;


  ---------------------------------------
  --- Reply Message Unmarshall ----------
  --------------------------------------


   procedure Reply_Message_Unmarshall
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)
   is
      use  representations.CDR;
      Service_Context   : array (range 0 ..9) of CORBA.Unsigned_Long;
   begin


      --  Request id
      Request_Id := Unmarshall(Buffer);


      -- Reply Status
      Unmarshall(Reply_Status);

      --  Service context
      for I in 0 .. 9 loop
        Service_Context(I) := Unmarshall (Buffer);
      end loop;

      for I in 0 .. 9 loop
       if Service_Context(I /= ServiceId'Pos( Service_Context_List_1_1(I))
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"
                             & Service_Context'Img));
         raise GIOP_Error;
       end if;
      end loop;

   end Reply_Message_Unmarshall;



end Droopi.Protocols.GIOP.GIOP_1_2;
