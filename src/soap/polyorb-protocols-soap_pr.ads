------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P R O T O C O L S . S O A P _ P R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Buffers;
with PolyORB.ORB;
with PolyORB.Requests;
with PolyORB.Types;

with PolyORB.SOAP_P.Message.Payload;

package PolyORB.Protocols.SOAP_Pr is

   --  Elaboration: Protocols.SOAP_Pr (spec),
   --  Binding_Data.SOAP (spec+body), Protocols.SOAP_Pr (body).

   type SOAP_Protocol is new Protocol with private;

   type SOAP_Session is new Session with private;

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access);

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access;
      Pro : access Binding_Data.Profile_Type'Class);

   procedure Abort_Request
     (S : access SOAP_Session;
      R : Requests.Request_Access);

   procedure Send_Reply
     (S : access SOAP_Session;
      R : Requests.Request_Access);

   procedure Handle_Connect_Indication (S : access SOAP_Session);

   procedure Handle_Connect_Confirmation (S : access SOAP_Session);

   procedure Handle_Data_Indication
     (S           : access SOAP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container);

   procedure Handle_Unmarshall_Arguments
     (S     : access SOAP_Session;
      Args  : in out PolyORB.Any.NVList.Ref;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Handle_Disconnect
     (S : access SOAP_Session; Error : Errors.Error_Container);

   procedure Handle_Flush (S : access SOAP_Session);

private

   type SOAP_Protocol is new Protocol with null record;

   type SOAP_Session is new Session with record
      In_Buf : PolyORB.Buffers.Buffer_Access;
      Entity_Length : Ada.Streams.Stream_Element_Count;
      Role   : PolyORB.ORB.Endpoint_Role;
      Target : PolyORB.Types.String;
      Current_SOAP_Req : PolyORB.SOAP_P.Message.Payload.Object_Access;
      Pending_Rq : PolyORB.Requests.Request_Access;
   end record;

   function Handle_Message
     (Sess : not null access SOAP_Session;
      S    : Components.Message'Class) return Components.Message'Class;

end PolyORB.Protocols.SOAP_Pr;
