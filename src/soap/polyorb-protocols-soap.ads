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

package PolyORB.Protocols.SOAP is

   pragma Elaborate_Body;

   type SOAP_Protocol is new Protocol with private;

   type SOAP_Session is new Session with private;

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access);

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

   type SOAP_Session is new Session with null record;

end PolyORB.Protocols.SOAP;
