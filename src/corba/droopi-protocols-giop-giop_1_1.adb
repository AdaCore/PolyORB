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

--  $Id$

with Ada.Streams; use Ada.Streams;
with Ada.Strings;
with Ada.Strings.Unbounded;

with CORBA;

with Droopi.Buffers;             use Droopi.Buffers;
with Droopi.Binding_Data;        use Droopi.Binding_Data;
with Droopi.Binding_Data.IIOP;
with Droopi.Protocols;           use Droopi.Protocols;
with Droopi.References;          use Droopi.References;
with Droopi.Representations.CDR; use Droopi.Representations.CDR;
with Droopi.Types;

package body Droopi.Protocols.GIOP.GIOP_1_1 is

   use Droopi.Types;

   Nobody_Principal : constant Ada.Strings.Unbounded.Unbounded_String
      := Ada.Strings.Unbounded.To_Unbounded_String ("nobody");


   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------
   --  we must redefine this procedure of Marshalling different from GIOP 1.0

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean)

   is

      use Representations.CDR;
      use Droopi.Buffers;
      Flags : Types.Octet := 0;

   begin

      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Flags
      if Endianness (Buffer.all) = Little_Endian then
         Flags := Flags or 2**Endianness_Bit;
      end if;

      if Fragment_Next = True then
         Flags := Flags or 2**Fragment_Bit;
      end if;

      Marshall (Buffer, Flags);

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, Types.Unsigned_Long (Message_Size));

   end  Marshall_GIOP_Header;


   ------------------------------------------------
   --  Marshaling of the  GIOP 1.1 messages
   ------------------------------------------------
   -- Marshalling of the request Message ----------
   ------------------------------------------------

   procedure Marshall_Request_Message
     (Buffer             : access Buffers.Buffer_Type;
      Request_Id         : in Types.Unsigned_Long;
      Target_Profile     : in Binding_Data.Profile_Access;
      Response_Expected  : in Boolean;
      Operation          : in Requests.Operation_Id)

   is
      use Representations.CDR;
      Reserved : constant Types.Octet := 0;

   begin

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Response expected
      Marshall (Buffer, Response_Expected);

      --  Reserved
      for I in 1 .. 3 loop
         Marshall (Buffer, Reserved);
      end loop;

      --  Object key
      Marshall (Buffer, Stream_Element_Array
            (Binding_Data.IIOP.Get_Object_Key (IIOP.IIOP_Profile_Type
            (Target_Profile.all))));


      --  Operation
      Marshall (Buffer, Operation);


      --  Principal
      Marshall (Buffer,
               Types.String (Nobody_Principal));

   end Marshall_Request_Message;


   -----------------------------
   ---  No Exception Reply
   ------------------------------

   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in Types.Unsigned_Long)

   is
      use Representations.CDR;
   begin


      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                   (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                   (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.No_Exception);

   end Marshall_No_Exception;



   -------------------------------------
   --  System Exception Marshall
   -------------------------------------

   procedure Marshall_Exception
    (Buffer           : access Buffer_Type;
     Request_Id       : in Types.Unsigned_Long;
     Exception_Type   : in Reply_Status_Type;
     Occurence        : in CORBA.Exception_Occurrence)
   is
      use Representations.CDR;
   begin

      pragma Assert (Exception_Type in User_Exception .. System_Exception);

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
                (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, Exception_Type);

      --  Occurrence
      Marshall (Buffer, Occurence);
   end Marshall_Exception;



   -------------------------------------
   --   Location Forward Reply Marshall
   ------------------------------------

   procedure Marshall_Location_Forward
    (Buffer           : access Buffer_Type;
     Request_Id       : in  Types.Unsigned_Long;
     Forward_Ref      : in  Droopi.References.IOR.IOR_Type)
   is
      use References.IOR;
   begin

      --  Service context
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (0))));
      Marshall (Buffer, Types.Octet (ServiceId'Pos
               (Service_Context_List_1_1 (1))));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply Status
      Marshall (Buffer, GIOP.Location_Forward);

      --  Object reference
      References.IOR.Marshall (Buffer, Forward_Ref);

   end Marshall_Location_Forward;



   ---------------------------------------
   ----- Fragment Message Marshall
   ----------------------------------------


   procedure Marshall_Fragment
    (Buffer      : access Buffer_Type;
     Request_Id  : in Types.Unsigned_Long)

   is
      use Representations.CDR;
   begin

      --   Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Fragment;

   ----------------------------------
   --- Request Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        : out Types.Unsigned_Long;
      Response_Expected : out Boolean;
      Object_Key        : out Objects.Object_Id;
      Operation         : out Types.String)
   is
      use Droopi.Objects;
      Service_Context1  : Types.Unsigned_Long := Unmarshall (Buffer);
      Service_Context2  : Types.Unsigned_Long := Unmarshall (Buffer);
      Reserved          : Types.Octet;
      Principal         : Types.String;
   begin

      --  Service context

      if Service_Context1 /= ServiceId'Pos (Service_Context_List_1_1 (0)) or
          Service_Context2 /= ServiceId'Pos (Service_Context_List_1_1 (1))
      then
         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Response expected
      Response_Expected := Unmarshall (Buffer);

      --  Reserved
      for I in 1 .. 3 loop
         Reserved := Unmarshall (Buffer);
      end loop;

      --  Object Key
      declare
         Obj : Stream_Element_Array := Unmarshall (Buffer);
      begin
         Object_Key := Object_Id (Obj);
      end;

      --  Operation
      Operation :=  Unmarshall (Buffer);

      --  Principal
      Principal :=  Unmarshall (Buffer);

   end Unmarshall_Request_Message;


   -----------------------------------------
   --- Reply  Unmarshalling
   -----------------------------------------

   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out Types.Unsigned_Long;
       Reply_Status : out Reply_Status_Type)

   is
      Service_Context1  : Types.Unsigned_Long := Unmarshall (Buffer);
      Service_Context2  : Types.Unsigned_Long := Unmarshall (Buffer);
   begin

      --  Service context
      if Service_Context1 /= ServiceId'Pos (Service_Context_List_1_1 (0)) or
          Service_Context2 /= ServiceId'Pos (Service_Context_List_1_1 (1)) then

         pragma Debug (O (" Request_Message_Unmarshall : incorrect context"));
         raise GIOP_Error;
      end if;

      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Reply_Status := Unmarshall (Buffer);

   end Unmarshall_Reply_Message;

end Droopi.Protocols.GIOP.GIOP_1_1;
