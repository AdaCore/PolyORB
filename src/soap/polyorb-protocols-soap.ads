------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . S O A P                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

--  with PolyORB.ORB;
with PolyORB.Representations.SOAP;
with PolyORB.Any.NVList;
with PolyORB.Types;
with PolyORB.Buffers;
with PolyORB.Binding_Data;

with PolyORB.Protocols.HTTP;

--  with PolyORB.Utils.HTTP_Messages;

package PolyORB.Protocols.SOAP is

   pragma Elaborate_Body;

   use PolyORB.Representations.SOAP;
   use PolyORB.Binding_Data;
   use PolyORB.Protocols.HTTP;

   Version_Missmatch    : constant Natural;
   Must_Understand      : constant Natural;
   Invalid_Request      : constant Natural;
   Application_Faulted  : constant Natural;

   type SOAP_Protocol is new Protocol with private;

   type SOAP_Message is private;
   type SOAP_Message_Access is access all SOAP_Message;

   type SOAP_Session is new Session with private;
   type SOAP_Session_Access is access all SOAP_Session;

   type HTTP_Session is private;
   type HTTP_Session_Access is access all HTTP_Session;

   type Role_Type is (Client, Server);

   procedure Request_To_SOAP_Method
     (Operation : PolyORB.Types.Identifier;
      Args  : PolyORB.Any.NVList.Ref;
      Uri : XML_String;
      Method : out XML_Component_Access);

   procedure Response_To_SOAP_Method
     (Operation : PolyORB.Types.Identifier;
      Result    : PolyORB.Any.NamedValue;
      Uri : XML_String;
      Method : out XML_Component_Access);

   procedure Set_Envelope
      (Mess : access SOAP_Message);


   procedure Set_Method
      (Mess    : access SOAP_Message;
       Req     : Requests.Request_Access;
       Role    : Role_Type := Client);

   procedure Set_Fault
     (Mess : access SOAP_Message;
      Faultcode   : Integer;
      Runcode     : XML_String;
      Faultstring : XML_String := XML_Null_String;
      Detail      : XML_String := XML_Null_String);

   function Method
     (Item : XML_Component_Access)
     return XML_Component_Access;


   procedure Build_SOAP_Message
     (Root : XML_Component_Access;
      Mess : out SOAP_Message_Access);

   procedure Release
     (Mess : in out SOAP_Message_Access);

   procedure Release
      (Ses : in out HTTP_Session_Access);

   ------------------------------
   --  Session primitives
   -----------------------------

   procedure Perform_Request (S : access SOAP_Session);

   procedure Receive_Response
     (S : access SOAP_Session);



   ------------------------------
   --  PolyORB primitives
   -----------------------------

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access);

   procedure Store_Profile
     (Ses      :  access SOAP_Session;
      Profile  :  Profile_Access);

   procedure Store_Connection
    (Ses      :  access SOAP_Session;
     C        :  HTTP_Connection_Access);

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access);

   procedure Abort_Request (S : access SOAP_Session;
                 R : Requests.Request_Access);

   procedure Send_Reply (S : access SOAP_Session;
                 R : Requests.Request_Access);

   procedure Handle_Connect_Indication (S : access SOAP_Session);

   procedure Handle_Connect_Confirmation (S : access SOAP_Session);

   procedure Handle_Data_Indication (S : access SOAP_Session);

   procedure Handle_Disconnect (S : access SOAP_Session);


private

   type SOAP_Protocol is new Protocol with null record;


   type HTTP_Session is record
        Connection   : HTTP_Connection_Access;
        Response     : HTTP_Response_Access;
        Request      : HTTP_Request_Access;
        Message_Body : Types.String;
   end record;

   type SOAP_Message is record
      Envelope_Field  : XML_Component_Access;
      Header_Field      : XML_Component_Access;
      Body_Field          : XML_Component_Access;
      Method_NS          : XML_String := XML_Null_String;
   end record;


   type SOAP_Session is new Session with record
      Request       : SOAP_Message_Access;
      Response      : SOAP_Message_Access;
      Role          : Role_Type := Client;
      Pending_Req   : Request_Access := null;
      Method_NS     : XML_String := XML_Null_String;
      Target        : Profile_Access;
      Buffer        : Buffers.Buffer_Access;
      Expect_Header : Boolean := True;
      HTTP_Session  : HTTP_Session_Access;
   end record;

   Version_Missmatch    : constant Natural := 100;
   Must_Understand      : constant Natural := 200;
   Invalid_Request      : constant Natural := 300;
   Application_Faulted  : constant Natural := 400;


   RC_Yes : constant XML_String := To_PolyORB_String ("Yes");
   RC_No : constant XML_String := To_PolyORB_String ("No");
   RC_Maybe : constant XML_String := To_PolyORB_String ("Maybe");

   SOAP_Tag : constant XML_String := To_PolyORB_String
               ("SOAP-ENV");

   Envelope_Tag : constant XML_String := To_PolyORB_String
               ("Envelope");

   Body_Tag : constant XML_String := To_PolyORB_String
               ("Body");

   Header_Tag : constant XML_String := To_PolyORB_String
               ("Header");

   SOAP_XMLNS_Tag :  constant XML_String := To_PolyORB_String
               ("xmlns");

   Encoding_Style_Tag : constant XML_String := To_PolyORB_String
               ("encodingStyle");

   Encoding_Style_URI : constant XML_String := To_PolyORB_String
     ("""http://schemas.xmlsoap.org/soap/encoding/""");

   SOAP_URI : constant XML_String := To_PolyORB_String
     ("""http://schemas.xmlsoap.org/soap/envelope/""");

   Separator_Tag :  constant XML_String := To_PolyORB_String (":");

   Fault_Tag  : constant XML_String :=
                       To_PolyORB_String ("fault");

   Faultcode_Tag  : constant XML_String :=
                       To_PolyORB_String ("faultcode");
   Runcode_Tag : constant XML_String :=
                       To_PolyORB_String ("runcode");
   Faultstring_Tag : constant XML_String :=
                       To_PolyORB_String ("faultstring");
   Detail_Tag : constant XML_String :=
                       To_PolyORB_String ("detail");

   Method_Tag_Reference : constant XML_String :=
             To_PolyORB_String ("m");
   Header_Tag_Reference : constant XML_String :=
             To_PolyORB_String ("t");


end PolyORB.Protocols.SOAP;
