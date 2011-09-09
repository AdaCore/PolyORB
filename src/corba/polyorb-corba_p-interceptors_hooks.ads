------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ H O O K S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
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

--  Hook to set up request's invoke method used by the CORBA personality.

with PolyORB.Binding_Data;
with PolyORB.Errors;
with PolyORB.POA;
with PolyORB.Requests;
with PolyORB.Smart_Pointers.Controlled_Entities;

package PolyORB.CORBA_P.Interceptors_Hooks is

   package PSPCE renames PolyORB.Smart_Pointers.Controlled_Entities;

   type Client_Invoke_Handler is access procedure
     (Self  : access PolyORB.Requests.Request;
      Flags : PolyORB.Requests.Flags);

   type Server_Invoke_Handler is access procedure
     (Self    : access PSPCE.Entity'Class;
      --  Actually must be PortableServer.DynamicImplementation'Class.
      Request : access PolyORB.Requests.Request;
      Profile : PolyORB.Binding_Data.Profile_Access);

   type Server_Intermediate_Handler is access procedure
     (Self           : access PolyORB.Requests.Request;
      From_Agruments : Boolean);

   type POA_Create_Handler is access procedure
     (POA   : PolyORB.POA.Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container);

   Client_Invoke : Client_Invoke_Handler := null;

   Server_Invoke : Server_Invoke_Handler := null;
   --  Server side hook initialized in PortableServer module.

   Server_Intermediate : Server_Intermediate_Handler := null;
   --  This hook used for call intermediate interception point Receive_Request.
   --  If program don't use PortableInterceptors this variable have null
   --  value.

   POA_Create : POA_Create_Handler := null;

end PolyORB.CORBA_P.Interceptors_Hooks;
