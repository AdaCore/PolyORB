------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . G I O P                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with CORBA;

with Broca.CDR; use Broca.CDR;
with Broca.Exceptions;
with Broca.Sequences;
with Broca.Opaque; use Broca.Opaque;
with Broca.Buffers;      use Broca.Buffers;
with Broca.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.GIOP is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.giop");
   procedure O is new Broca.Debug.Output (Flag);

   Magic : constant Octet_Array :=
     (Character'Pos ('G'),
      Character'Pos ('I'),
      Character'Pos ('O'),
      Character'Pos ('P'));

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 0;

   Byte_Order_Offset : constant := 6;
   --  The offset of the byte_order boolean field in
   --  a GIOP message header.

   Nobody_Principal : constant Ada.Strings.Unbounded.Unbounded_String
      := Ada.Strings.Unbounded.To_Unbounded_String ("nobody");

   MsgType_To_Octet :
     constant array (MsgType'Range) of CORBA.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (ReplyStatusType'Range) of CORBA.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3);

   LocateStatusType_To_Unsigned_Long :
     constant array (LocateStatusType'Range) of CORBA.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2);

   Octet_To_MsgType :
     constant array (CORBA.Octet range 0 .. 7) of MsgType
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 3) of ReplyStatusType
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward);

   Unsigned_Long_To_LocateStatusType :
     constant array (CORBA.Unsigned_Long range 0 .. 2) of LocateStatusType
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward);

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffer_Type;
      Message_Type : in MsgType;
      Message_Size : in Index_Type)
   is
      use Broca.CDR;
   begin
      --  1.2.1 The message header.
      --  Magic
      for I in Magic'Range loop
         Marshall (Buffer, CORBA.Octet (Magic (I)));
      end loop;

      --  Version
      Marshall (Buffer, Major_Version);
      Marshall (Buffer, Minor_Version);

      --  Byte order
      Marshall (Buffer, CORBA.Boolean
                (Endianness (Buffer.all) = Little_Endian));

      --  Message type
      Marshall (Buffer, Message_Type);

      --  Message size
      Marshall (Buffer, CORBA.Unsigned_Long (Message_Size));
   end Marshall_GIOP_Header;

   -------------------------
   -- Prepend_GIOP_Header --
   -------------------------

   procedure Prepend_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType)
   is
      Header_Buffer : aliased Buffer_Type;
   begin
      Marshall_GIOP_Header (Header_Buffer'Access, Message_Type,
                            Length (Buffer));
      Prepend (Header_Buffer, Buffer);
   end Prepend_GIOP_Header;

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Buffer       : access Buffer_Type;
      Message_Type          : out MsgType;
      Message_Size          : out CORBA.Unsigned_Long;
      Message_Endianness    : out Endianness_Type;
      Success               : out Boolean)
   is
      use CORBA;
      use Broca.CDR;
      Message_Magic : Octet_Array (Magic'Range);
      Message_Major_Version : CORBA.Octet;
      Message_Minor_Version : CORBA.Octet;
   begin
      Success := False;

      --  Magic
      for I in Message_Magic'Range loop
         Message_Magic (I) := Opaque.Octet
           (CORBA.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         return;
      end if;

      --  FIXME
      --  2000-02-14 Thomas.
      --  Check that we adopt the correct behaviour when
      --  the GIOP version in the message is different
      --  from our own.
      Message_Major_Version := Unmarshall (Buffer);
      Message_Minor_Version := Unmarshall (Buffer);
      if not (Message_Major_Version =  Major_Version)
        or else (Minor_Version < Message_Minor_Version) then
         return;
      end if;

      --  Byte order
      if Unmarshall (Buffer) then
         Message_Endianness := Little_Endian;
      else
         Message_Endianness := Big_Endian;
      end if;

      --  Message type
      Message_Type := Unmarshall (Buffer);

      --  Message size
      Message_Size := Unmarshall (Buffer);

      Success := True;
   end Unmarshall_GIOP_Header;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer     : access Buffer_Type;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence)
   is
      use Broca.CDR;
   begin
      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply status
      Marshall (Buffer, Broca.GIOP.System_Exception);

      --  Exception
      Broca.Exceptions.Marshall (Buffer, Occurence);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer     : access Buffer_Type;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref)
   is
      use Broca.CDR;
   begin
      --  Service context
      Marshall (Buffer, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Reply status
      Marshall (Buffer, Broca.GIOP.Location_Forward);

      --  Reference
      Broca.CDR.Marshall
        (Buffer, Reference);
   end Marshall;

   ---------------------------------
   -- Request_Handler subprograms --
   ---------------------------------

   procedure Free is new
     Ada.Unchecked_Deallocation (Broca.Opaque.Octet_Array, Octet_Array_Ptr);

   -------------
   -- Release --
   -------------

   procedure Release (H : in out Request_Handler) is
   begin
      Release (H.Buffer);
      Free (H.Data.Message_Body);
   end Release;

   ---------------------------
   -- Send_Request_Marshall --
   ---------------------------

   procedure Send_Request_Marshall
     (Handler           : in out Request_Handler;
      Target_Ref        : in CORBA.Object.Ref'Class;
      Response_Expected : in Boolean;
      Operation         : in CORBA.Identifier)
   is
      use Broca.CDR;

      Target : constant Broca.Object.Object_Ptr
        := Broca.Object.Object_Ptr
        (CORBA.Object.Object_Of (Target_Ref));
   begin
      Handler.Profile := Object.Find_Profile (Target);
      Handler.Connection := IOP.Find_Connection (Handler.Profile);

      --  Reserve space for message header
      Set_Initial_Position
        (Handler.Buffer'Access, Message_Header_Size);

      --  Service context
      Marshall (Handler.Buffer'Access, CORBA.Unsigned_Long (No_Context));

      --  Request id
      Handler.Request_Id := IOP.Get_Request (Handler.Connection);
      Marshall (Handler.Buffer'Access, Handler.Request_Id);

      --  Response expected
      Marshall (Handler.Buffer'Access, Response_Expected);

      --  Object key
      Broca.Sequences.Marshall
        (Handler.Buffer'Access,
         IOP.Get_Object_Key (Handler.Profile.all));

      --  Operation
      Marshall (Handler.Buffer'Access, CORBA.String (Operation));

      --  Principal
      Marshall (Handler.Buffer'Access, CORBA.String (Nobody_Principal));
   end Send_Request_Marshall;

   -----------------------
   -- Send_Request_Send --
   -----------------------

   procedure Send_Request_Send
     (Handler          : in out Request_Handler;
      Target_Ref       : in out CORBA.Object.Ref'Class;
      Reponse_Expected : in Boolean;
      Result           : out Send_Request_Result_Type)
   is
      use Broca.CDR;
      use CORBA;

      Header_Buffer      : aliased Buffer_Type;
      Message_Type       : MsgType;
      Message_Size       : CORBA.Unsigned_Long;
      Message_Endianness : Endianness_Type;
      Service_Context    : CORBA.Unsigned_Long;
      Reply_Status       : ReplyStatusType;
      Request_Id         : CORBA.Unsigned_Long;
      Header_Correct     : Boolean;
   begin
      pragma Debug (O ("Send_Request_Send : Enter"));
      --  Add GIOP header.
      Marshall_GIOP_Header
        (Header_Buffer'Access,
         Broca.GIOP.Request,
         Length (Handler.Buffer'Access));
      Prepend (Header_Buffer, Handler.Buffer'Access);

      pragma Debug (O ("Send_Request_Send : about to send request"));
      --  1.3 Send request.
      IOP.Send (Handler.Connection, Handler.Buffer'Access);
      Release (Handler.Buffer);

      pragma Debug (O ("Send_Request_Send : request sent"));
      if not Reponse_Expected then
         IOP.Release (Handler.Connection);
         Result := Sr_No_Reply;
         return;
      end if;

      --  1.4 Receive reply
      --  1.4.1 the message header

      pragma Debug (O ("Send_Request_Send : Receive answer ..."));
      declare
         Message_Header : Broca.Opaque.Octet_Array_Ptr
           := IOP.Receive (Handler.Connection,
                           Message_Header_Size);
         Message_Header_Buffer : aliased Buffer_Type;
         Endianness : Endianness_Type;
      begin
         pragma Debug (O ("Send_Request_Send : Receive answer done"));

         if CORBA.Boolean'Val
           (CORBA.Octet (Message_Header
                         (Message_Header'First
                          + Byte_Order_Offset)) and 1) then
            Endianness := Little_Endian;
         else
            Endianness := Big_Endian;
         end if;

         Broca.Buffers.Initialize_Buffer
           (Message_Header_Buffer'Access,
            Message_Header_Size,
            Message_Header.all'Address,
            Endianness,
            0);

         Unmarshall_GIOP_Header
           (Message_Header_Buffer'Access,
            Message_Type, Message_Size, Message_Endianness,
            Header_Correct);

         pragma Assert (Message_Endianness = Endianness);

         Release (Message_Header_Buffer);
         Free (Message_Header);
      end;

      if not (Header_Correct and then Message_Type = Reply) then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      pragma Debug (O ("Send_Request_Send : about to receive reply"));
      --  1.4.5 Receive the reply header and body.
      Handler.Data.Message_Body :=
        IOP.Receive (Handler.Connection,
                     Broca.Opaque.Index_Type (Message_Size));
      declare
         Message_Body : Octet_Array_Ptr := Handler.Data.Message_Body;
         Message_Body_Buffer : Buffer_Type
           renames Handler.Buffer;

      begin
         Broca.Buffers.Initialize_Buffer
           (Message_Body_Buffer'Access,
            Broca.Opaque.Index_Type (Message_Size),
            Message_Body.all'Address,
            Message_Endianness,
            GIOP.Message_Header_Size);

         IOP.Release (Handler.Connection);

         --  Service context
         Service_Context := Unmarshall (Message_Body_Buffer'Access);
         if Service_Context /= No_Context then
            pragma Debug (O ("Send_Request_Send : incorrect context"
                             & Service_Context'Img));
            raise Program_Error;
         end if;

         --  Request id
         Request_Id := Unmarshall (Message_Body_Buffer'Access);
         if Request_Id /= Handler.Request_Id then
            pragma Debug
              (O ("Send_Request_Send : incorrect request id"
                  & Request_Id'Img));
            Broca.Exceptions.Raise_Comm_Failure;
         end if;

         pragma Debug (O ("Send_Request_Send : checking reply status"));
         --  Reply status
         Reply_Status := Unmarshall (Message_Body_Buffer'Access);
         case Reply_Status is
            when Broca.GIOP.No_Exception =>
               Result := Sr_Reply;
               return;

            when Broca.GIOP.System_Exception =>
               Broca.Exceptions.Unmarshall_And_Raise
                 (Message_Body_Buffer'Access);

            when Broca.GIOP.Location_Forward =>
               pragma Debug
                 (O ("Send_Request_Send : Received Location_Forward"));

               declare
                  New_Ref : CORBA.Object.Ref;
               begin
                  Broca.CDR.Unmarshall
                    (Message_Body_Buffer'Access,
                     New_Ref);
                  CORBA.Object.Set
                    (Target_Ref, CORBA.Object.Object_Of (New_Ref));
               end;
               Result := Sr_Forward;
               Release (Handler);
               --  The caller is going to retry the call
               --  with the new location, so we have to
               --  supply him with a virgin buffer.
               return;

            when Broca.GIOP.User_Exception =>
               Result := Sr_User_Exception;
               return;

            when others =>
               raise Program_Error;
         end case;

         Release (Handler);
      end;
   end Send_Request_Send;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in MsgType) is
   begin
      Marshall (Buffer, MsgType_To_Octet (Value));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in ReplyStatusType) is
   begin
      Marshall (Buffer, ReplyStatusType_To_Unsigned_Long (Value));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in LocateStatusType) is
   begin
      Marshall (Buffer, LocateStatusType_To_Unsigned_Long (Value));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return MsgType is
   begin
      return Octet_To_MsgType (Unmarshall (Buffer));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return ReplyStatusType is
   begin
      return Unsigned_Long_To_ReplyStatusType (Unmarshall (Buffer));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return LocateStatusType is
   begin
      return Unsigned_Long_To_LocateStatusType (Unmarshall (Buffer));
   end Unmarshall;

end Broca.GIOP;
