------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . G I O P                            --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with CORBA.Object;
with Broca.Object;
with Broca.Opaque;
with Broca.Buffers;
with Broca.IOP;

package Broca.GIOP is
   --  Declare a few constants for GIOP version 1.0.

   --  GIOP::MsgType
   type MsgType is
     (Request,
      Reply,
      Cancel_Request,
      Locate_Request,
      Locate_Reply,
      Close_Connection,
      Message_Error,
      Fragment);

   --  GIOP::ReplyStatusType
   type ReplyStatusType is
     (No_Exception,
      User_Exception,
      System_Exception,
      Location_Forward);

   --  GIOP::LocateStatusType
   type LocateStatusType is
     (Unknown_Object,
      Object_Here,
      Object_Forward);

   --  Size (in bytes) of struct MessageHeader, major version 1.
   Message_Header_Size : constant := 12;

   No_Context : constant CORBA.Unsigned_Long := 0;

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType);

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType;
      Message_Size : in Opaque.Index_Type);

   procedure Prepend_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType);

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in MsgType);

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in ReplyStatusType);

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in LocateStatusType);

   procedure Marshall
     (Buffer     : access Buffers.Buffer_Type;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence);

   procedure Marshall
     (Buffer     : access Buffers.Buffer_Type;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref);

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
     return MsgType;

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
     return ReplyStatusType;

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
     return LocateStatusType;

   procedure Unmarshall_GIOP_Header
     (Buffer             : access Buffers.Buffer_Type;
      Message_Type       : out MsgType;
      Message_Size       : out CORBA.Unsigned_Long;
      Message_Endianness : out Buffers.Endianness_Type;
      Success            : out Boolean);

   type Request_Handler_Data is limited private;

   type Request_Handler is limited
      record
         Buffer     : aliased Buffers.Buffer_Type;
         Request_Id : CORBA.Unsigned_Long;
         Profile    : IOP.Profile_Ptr;
         Connection : IOP.Connection_Ptr;
         Nbr_Tries  : Natural := 0;
         Data       : Request_Handler_Data;
      end record;

   procedure Release (H : in out Request_Handler);
   --  Free the resources associated with a request
   --  handler after use.

   procedure Send_Request_Marshall
     (Handler           : in out Request_Handler;
      Target            : in Object.Object_Ptr;
      Response_Expected : in Boolean;
      Operation         : in CORBA.Identifier);
   --  Send a request.

   type Send_Request_Result_Type is
     (Sr_No_Reply,
      Sr_Reply,
      Sr_User_Exception,
      Sr_Forward);

   procedure Send_Request_Send
     (Handler          : in out Request_Handler;
      Target           : in Object.Object_Ptr;
      Reponse_Expected : in Boolean;
      Result           : out Send_Request_Result_Type);

private

   type Request_Handler_Data is limited record
     Message_Body : Broca.Opaque.Octet_Array_Ptr := null;
   end record;

end Broca.GIOP;
