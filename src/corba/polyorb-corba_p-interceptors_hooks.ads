------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ H O O K S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Hook to set up request's invoke method used by the CORBA personality.

with PolyORB.Binding_Data;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;

package PolyORB.CORBA_P.Interceptors_Hooks is

   type Client_Invoke_Handler is access procedure
     (Self  : in PolyORB.Requests.Request_Access;
      Flags : in PolyORB.Requests.Flags);

   type Server_Invoke_Handler is access procedure
     (Self    : access PolyORB.Smart_Pointers.Entity'Class;
      --  Actually must be PortableServer.DynamicImplementation'Class.
      Request : in     PolyORB.Requests.Request_Access;
      Profile : in     PolyORB.Binding_Data.Profile_Access);

   type Server_Intermediate_Handler is access procedure
     (Self           : in PolyORB.Requests.Request_Access;
      From_Agruments : in Boolean);

   Client_Invoke : Client_Invoke_Handler := null;

   Server_Invoke : Server_Invoke_Handler := null;
   --  Server side hook initialized in PortableServer module.

   Server_Intermediate : Server_Intermediate_Handler := null;
   --  This hook used for call intermediate interception point Receive_Request.
   --  If program don't use PortableInterceptors this variable have null
   --  value.

end PolyORB.CORBA_P.Interceptors_Hooks;
