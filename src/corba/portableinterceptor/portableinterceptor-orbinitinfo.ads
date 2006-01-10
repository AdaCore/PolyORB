------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E I N T E R C E P T O R . O R B I N I T I N F O       --
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

with Ada.Exceptions;

with CORBA.Object;

with IOP.CodecFactory;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.IORInterceptor;
with PortableInterceptor.PolicyFactory;
with PortableInterceptor.ServerRequestInterceptor;

package PortableInterceptor.ORBInitInfo is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  ObjectId type

   type ObjectId is new CORBA.String;

   --  DuplicateName exception

   DuplicateName : exception;

   type DuplicateName_Members is
     new CORBA.IDL_Exception_Members with
   record
      Name : CORBA.String;
   end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out DuplicateName_Members);

   --  InvalidName exception

   InvalidName : exception;

   type InvalidName_Members is
      new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidName_Members);

--   function Get_Arguments
--     (Self : Local_Ref)
--      return CORBA.StringSeq;

   function Get_ORB_Id
     (Self : Local_Ref)
      return CORBA.String;

   function Get_Codec_Factory
     (Self : Local_Ref)
      return IOP.CodecFactory.Local_Ref;

   procedure Register_Initial_Reference
     (Self : Local_Ref;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId;
      Obj  : CORBA.Object.Ref);

   function Resolve_Initial_References
     (Self : Local_Ref;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId)
     return CORBA.Object.Ref;

   procedure Add_Client_Request_Interceptor
     (Self        : Local_Ref;
      Interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   procedure Add_Server_Request_Interceptor
     (Self        : Local_Ref;
      Interceptor :
        PortableInterceptor.ServerRequestInterceptor.Local_Ref);

   procedure Add_IOR_Interceptor
     (Self        : Local_Ref;
      Interceptor : PortableInterceptor.IORInterceptor.Local_Ref);

   function Allocate_Slot_Id
     (Self : Local_Ref)
      return PortableInterceptor.SlotId;

   procedure Register_Policy_Factory
     (Self           : Local_Ref;
      IDL_Type       : CORBA.PolicyType;
      Policy_Factory : PortableInterceptor.PolicyFactory.Local_Ref);

   --  RepositoryIds

   ORBInitInfo_Root_Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/ORBInitInfo";

   Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & ":1.0";

   ObjectId_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/ObjectId:1.0";

   DuplicateName_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/DuplicateName:1.0";

   InvalidName_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/InvalidName:1.0";

   Arguments_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/arguments:1.0";

   ORB_Id_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/orb_id:1.0";

   Codec_Factory_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/codec_factory:1.0";

   Register_Initial_Reference_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/register_initial_reference:1.0";

   Add_Client_Request_Interceptor_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/add_client_request_interceptor:1.0";

   Add_Server_Request_Interceptor_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/add_server_request_interceptor:1.0";

   Add_IOR_Interceptor_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/add_ior_interceptor:1.0";

   Allocate_Slot_Id_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/allocate_slot_id:1.0";

   Register_Policy_Factory_Repository_Id : constant Standard.String
     := ORBInitInfo_Root_Repository_Id & "/register_policy_factory:1.0";

end PortableInterceptor.ORBInitInfo;
