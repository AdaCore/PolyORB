------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ H O O K S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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
