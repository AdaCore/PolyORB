------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . E C H O                --
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

--  A dummy protocol, just for testing.

with PolyORB.Buffers;

package PolyORB.Protocols.Echo is

   pragma Elaborate_Body;

   --  Echo_Protocol:
   --  A very simple protocol that echoes text lines received
   --  from the user.

   type Echo_Protocol is new Protocol with private;

   overriding procedure Create
     (Proto   : access Echo_Protocol;
      Session : out Filter_Access);

   type Echo_Session is new Session with private;

   overriding procedure Invoke_Request
     (S : access Echo_Session;
      R : Request_Access;
      P : access Binding_Data.Profile_Type'Class);

   overriding procedure Abort_Request
     (S : access Echo_Session;
      R : Request_Access);
   --  These are just for show and do nothing

   overriding procedure Send_Reply
     (S : access Echo_Session;
      R : Request_Access);
   --  Send a reply to the user

   overriding procedure Handle_Flush (S : access Echo_Session);
   --  ???

   overriding procedure Handle_Connect_Indication (S : access Echo_Session);
   --  Send a greeting banner to user

   overriding procedure Handle_Connect_Confirmation (S : access Echo_Session);
   --  Setup client dialog

   overriding procedure Handle_Data_Indication
     (S           : access Echo_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container);
   --  Handle data received from user

   overriding procedure Handle_Disconnect
     (S : access Echo_Session; Error : Errors.Error_Container);
   --  Handle disconnection from user

private

   type Echo_Protocol is new Protocol with null record;

   type Echo_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
      Out_Buffer : Buffers.Buffer_Access;
   end record;

end PolyORB.Protocols.Echo;
