------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P R O T O C O L S . S O A P                 --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Protocols.SOAP  is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access)
   is
      Result : constant Filter_Access
        := new SOAP_Session;
   begin
      Session := Result;
   end Create;

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Invoke_Request;


   procedure Abort_Request
     (S : access SOAP_Session;
      R : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Abort_Request;

   procedure Send_Reply
      (S : access SOAP_Session;
       R : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Send_Reply;


   procedure Handle_Data_Indication
      (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Data_Indication;

   procedure Handle_Connect_Indication
     (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation
     (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Connect_Confirmation;

   procedure Handle_Disconnect
     (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Disconnect;

end PolyORB.Protocols.SOAP;
