------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . E C H O                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A dummy protocol, just for testing.

--  $Id$

with PolyORB.Buffers;

package PolyORB.Protocols.Echo is

   pragma Elaborate_Body;

   --  Echo_Protocol:
   --  A very simple protocol that echoes text lines received
   --  from the user.

   type Echo_Protocol is new Protocol with private;

   procedure Create
     (Proto   : access Echo_Protocol;
      Session : out Filter_Access);

   type Echo_Session is new Session with private;

   procedure Invoke_Request
     (S : access Echo_Session;
      R : Request_Access;
      P : access Binding_Data.Profile_Type'Class);

   procedure Abort_Request (S : access Echo_Session; R : Request_Access);
   --  These are just for show and do nothing.

   procedure Send_Reply (S : access Echo_Session; R : Request_Access);
   --  Send a reply to the user.

   procedure Handle_Connect_Indication (S : access Echo_Session);
   --  Send a greeting banner to user.

   procedure Handle_Connect_Confirmation (S : access Echo_Session);
   --  Setup client dialog.

   procedure Handle_Data_Indication
     (S : access Echo_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count);
   --  Handle data received from user.

   procedure Handle_Disconnect (S : access Echo_Session);
   --  Handle disconnection from user.

private

   type Echo_Protocol is new Protocol with null record;

   type Echo_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
      Out_Buffer : Buffers.Buffer_Access;
   end record;

end PolyORB.Protocols.Echo;
