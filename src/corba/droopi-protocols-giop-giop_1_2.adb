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
with Droopi.References.IOR;   use  Droopi.References.IOR;
with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Log;

with Sequences.Unbounded;


pragma Elaborate_All (Droopi.Log);

package body Droopi.Protocols.GIOP.GIOP_1_2 is



   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)
   is
      use Representations.CDR;
      Flags : CORBA.Octet :=0;

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
         Flags := Flags or 2**Endianess_bit;
      end if;

      if(Fragment_Next=True) then
         Flags := Flags or 2**Fragment_bit;
      end if;

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long(Message_Size));

   end Marshall_GIOP_Header;


  ------------------------------------------------
  --  Marshaling of the  GIOP 1.2 messages
  ------------------------------------------------
  -- Marshalling of the request Message ----------
  ------------------------------------------------


  procedure Marshall_Request_Message
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Target_Ref        : in Target_Address;
      Sync_Type         : in Sync_Scope;
      Operation         : in Requests.Operation_Id)

  is
      use Representations.CDR;
      use Binding_Data.IIOP;
      use Droopi.References.IOR;
      Reserved: constant CORBA.Octet:=0;

  begin

      -- Request id
      Marshall (Buffer, Request_Id);

      -- Response_Flags
      Marshall(Buffer, CORBA.Octet(Sync_Scope'Pos(Sync_Type)));

      -- Reserved
      for I in 1..3 loop
       Marshall(Buffer , Reserved);
      end loop;

      -- Target Address Not yet implemented
      Marshall(Buffer,
               AddressingDisposition_To_Unsigned_Long(Target_Ref.Address_Type));

      case Target_Ref.Address_Type is
       when Key_Addr  =>
         Marshall(Buffer,
          Binding_Data.Get_Object_Key(Target_Ref.Profile.all));
       when Profile_Addr  =>
         Marshall_IIOP_Profile_Body(Buffer, Target_Ref.Profile);
       when Reference_Addr  =>
         Marshall(Buffer, Target_Ref.Ref.Selected_Profile_Index);
         References.IOR.Marshall(Buffer, Target_Ref.Ref.IOR);
      end case;

      --  Operation
        Marshall (Buffer, Operation);

      --  Service context
      for I in 0 .. 9 loop
       Marshall (Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_2(I))));
      end loop;

   end Marshall_Request_Message;



   -----------------------------
   -- Marshalling of Reply messages
   --
   -- Marshalling of Reply with
   ----------------------------------

   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long)

   is
    use  Representations.CDR;
   begin


     -- Request id
     Marshall(Buffer, Request_Id);

     -- Reply Status
     Marshall(Buffer, GIOP.No_Exception);


      --  Service context
      for I in 0 .. 9 loop
       Marshall (Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_2(I))));
      end loop;


   end Marshall_No_Exception;



   -------------------------------------
   --  Exception Marshall
   -------------------------------------

   procedure Marshall_Exception
    ( Buffer      : access Buffers.Buffer_Type;
      Request_Id  : CORBA.Unsigned_Long ;
      Reply_Type  : in Reply_Status_Type;
      Occurence   : in CORBA.Exception_Occurrence)

   is
     use  Representations.CDR;
   begin

      pragma Assert (Reply_Type in User_Exception .. System_Exception);

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);


      --  Service context
      for I in 0 .. 9 loop
       Marshall (Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_2(I))));
      end loop;

      --  Occurrence
      Marshall(Buffer, Occurence);

   end  Marshall_Exception;



  -------------------------------------
  -- Location Forward Reply Marshall
  ------------------------------------

    procedure Marshall_Location_Forward
    ( Buffer        :   access Buffers.Buffer_Type;
      Request_Id    :   in  CORBA.Unsigned_Long;
      Reply_Type    :   in  Reply_Status_Type;
      Target_Ref    :   in  Droopi.References.IOR.IOR_Type)
    is
       use  Representations.CDR;
       use References.IOR;
    begin

      pragma Assert (Reply_Type in Location_Forward .. Location_Forward_Perm);

         -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, Reply_Type);


      --  Service context
      for I in 0 .. 9 loop
       Marshall (Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_2(I))));
      end loop;

      --  Reference
      References.IOR.Marshall(Buffer, Target_Ref);

    end Marshall_Location_Forward;

    ------------------------------------
    -- Needs Addessing Mode Marshall
    ------------------------------------

    procedure Marshall_Needs_Addressing_Mode
    (Buffer              : access Buffers.Buffer_Type;
     Request_Id          : in CORBA.Unsigned_Long;
     Address_Type        : in GIOP.Addressing_Disposition)
    is

        use  Representations.CDR;

    begin

      -- Request id
      Marshall(Buffer, Request_Id);

      -- Reply Status
      Marshall(Buffer, GIOP.Needs_Addressing_Mode);


      --  Service context
      for I in 0 .. 9 loop
       Marshall (Buffer, CORBA.Octet(ServiceId'Pos(Service_Context_List_1_2(I))));
      end loop;

      --  Address Disposition
      Marshall(Buffer,
               AddressingDisposition_To_Unsigned_Long(Address_Type));

    end  Marshall_Needs_Addressing_Mode;


 ------------------------------------------------
 ---  Locate Request Message Marshall
 -----------------------------------------------


   procedure Marshall_Locate_Request
    (Buffer            : access Buffer_Type;
     Request_Id        : in CORBA.Unsigned_long;
     Target_Ref        : in Target_Address)

   is
      use Representations.CDR;
      use Binding_Data.Iiop;
   begin



      -- Request id
      Marshall (Buffer, Request_Id);


      -- Marshalling the Target Address

      Marshall(Buffer,
                AddressingDisposition_To_Unsigned_Long(Target_Ref.Address_Type));

      case Target_Ref.Address_Type is
       when Key_Addr =>
          Marshall(Buffer,
          Binding_Data.Get_Object_Key(Target_Ref.Profile.all));
       when Profile_Addr =>
          Marshall_IIOP_Profile_Body
             (Buffer, Target_Ref.Profile);
       when Reference_Addr =>
          Marshall(Buffer, Target_Ref.Ref.Selected_Profile_Index);
          References.IOR.Marshall(Buffer, Target_Ref.Ref.IOR);
      end case;

   end Marshall_Locate_Request;


   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


   procedure Marshall_Fragment
    ( Buffer       : access Buffers.Buffer_Type;
      Request_Id   : in CORBA.Unsigned_Long)
   is
      use  Representations.CDR;
   begin


      -- Request id
      Marshall(Buffer, Request_Id);

   end Marshall_Fragment;


   -------------------------------------
   --- Request Unmarshall
   -------------------------------------

   procedure Unmarshall_Request_Message
     ( Buffer            : access Buffer_Type;
       Request_Id        : out CORBA.Unsigned_Long;
       Response_Expected : out Boolean;
       Target_Ref        : out Target_Address;
       Operation         : out Requests.Operation_Id)
   is
       use  Representations.CDR;
       Service_Context      : array (0 ..9) of CORBA.Unsigned_Long;
       Reserved             : CORBA.Octet;
       Received_Flags       : CORBA.Octet;
       Temp_Octet           : CORBA.Octet;

   begin

      --  Request id
      Request_Id := Unmarshall(Buffer);

      --  Response expected
      Received_Flags := Unmarshall(Buffer);

      if Received_Flags = 3 then
         Response_Expected := True;
      else
         Response_Expected := false;
      end if;

      -- Reserved
      for I in 1..3 loop
       Reserved := Unmarshall(Buffer);
      end loop;

      -- Target Ref
      Temp_Octet := Unmarshall(Buffer);

      case  Temp_Octet  is
         when 0  =>
            Target_Ref := Target_Address'(Address_Type => Key_Addr, Object_Key => Unmarshall(Buffer));
         when 1  =>
            Target_Ref := Target_Address'(Address_Type => Profile_Addr,
                   Profile  => Binding_Data.IIOP. Unmarshall_IIOP_Profile_Body(Buffer));
         when 2  =>
            declare
              Temp_Ref :  IOR_Addressing_Info_Access := new IOR_Addressing_Info;
            begin
                Temp_Ref.Selected_Profile_Index := Unmarshall(Buffer);
                Temp_Ref.IOR := Unmarshall(Buffer);
                Target_Ref := Target_Address'(Address_Type => Reference_Addr,
                              Ref  => Temp_Ref);
            end;

         when others =>
           raise GIOP_Error;

      end case;

      -- Operation
      Operation :=  Unmarshall (Buffer);

      --  Service context
      for I in 0 .. 9 loop
        Service_Context(I) := Unmarshall (Buffer);
      end loop;

      for I in 0 .. 9 loop
       if Service_Context(I) /= ServiceId'Pos(Service_Context_List_1_2(I))
       then
          pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
          raise GIOP_Error;
       end if;
      end loop;


   end Unmarshall_Request_Message;


  ---------------------------------------
  --- Reply Message Unmarshall ----------
  --------------------------------------


   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)
   is
      use  Representations.CDR;
      Service_Context   : array(0 ..9) of CORBA.Unsigned_Long;
   begin


      --  Request id
      Request_Id := Unmarshall(Buffer);


      -- Reply Status
      Reply_Status := Unmarshall(Buffer);

      --  Service context
      for I in 0 .. 9 loop
        Service_Context(I) := Unmarshall (Buffer);
      end loop;

      for I in 0 .. 9 loop
       if Service_Context(I) /= ServiceId'Pos(Service_Context_List_1_2(I))
       then
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
       end if;
      end loop;

   end Unmarshall_Reply_Message;



end Droopi.Protocols.GIOP.GIOP_1_2;
