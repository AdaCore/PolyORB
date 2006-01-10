------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              PORTABLEINTERCEPTOR.CLIENTREQUESTINTERCEPTOR                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PortableInterceptor.ClientRequestInfo;
with PortableInterceptor.Interceptor;

package PortableInterceptor.ClientRequestInterceptor is

   type Local_Ref is
      new PortableInterceptor.Interceptor.Local_Ref with null record;

   procedure Send_Request
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Send_Poll
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Reply
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Exception
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   procedure Receive_Other
     (Self : Local_Ref;
      RI   : PortableInterceptor.ClientRequestInfo.Local_Ref);

   --  RepositoryIds

   Root : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ClientRequestInterceptor";

   Repository_Id : constant Standard.String
     := Root & ":1.0";

   Send_Request_Repository_Id : constant Standard.String
     := Root & "/send_request:1.0";

   Send_Poll_Repository_Id : constant Standard.String
     := Root & "/send_poll:1.0";

   Receive_Reply_Repository_Id : constant Standard.String
     := Root & "/receive_reply:1.0";

   Receive_Exception_Repository_Id : constant Standard.String
     := Root & "/receive_exception:1.0";

   Receive_Other_Repository_Id : constant Standard.String
     := Root & "/receive_other:1.0";

end PortableInterceptor.ClientRequestInterceptor;
