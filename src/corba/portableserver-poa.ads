------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id: //droopi/main/src/corba/portableserver-poa.ads#9 $

with Ada.Exceptions;

with CORBA.Object;
with CORBA.Policy;

with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;

with PolyORB.Exceptions;

package PortableServer.POA is

   type Ref is new CORBA.Object.Ref with null record;

   function To_Ref (Self : CORBA.Object.Ref'Class) return Ref;

   AdapterAlreadyExists : exception;
   AdapterNonExistent   : exception;
   InvalidPolicy        : exception;
   NoServant            : exception;
   ObjectAlreadyActive  : exception;
   ObjectNotActive      : exception;
   ServantAlreadyActive : exception;
   ServantNotActive     : exception;
   WrongAdapter         : exception;
   WrongPolicy          : exception;

   function Create_POA
     (Self         : in Ref;
      Adapter_Name : in CORBA.String;
      A_POAManager : in PortableServer.POAManager.Ref;
      Policies     : in CORBA.Policy.PolicyList)
     return Ref'Class;

   function Find_POA
     (Self         : in Ref;
      Adapter_Name : in CORBA.String;
      Activate_It  : in CORBA.Boolean)
      return Ref'Class;

   procedure Destroy
     (Self                : in out Ref;
      Etherealize_Objects : in     CORBA.Boolean;
      Wait_For_Completion : in     CORBA.Boolean);

   function Get_The_Name
     (Self : in Ref)
     return CORBA.String;

   function Get_The_Parent
     (Self : in Ref)
     return Ref'Class;

   function Get_The_POAManager
     (Self : in Ref)
     return PortableServer.POAManager.Ref;

   function Get_The_Activator
     (Self : in Ref)
     return PortableServer.AdapterActivator.Ref'Class;

   procedure Set_The_Activator
     (Self : in     Ref;
      To   : access PortableServer.AdapterActivator.Ref'Class);

   function Get_Servant_Manager
     (Self : in Ref)
     return PortableServer.ServantManager.Ref'Class;

   procedure Set_Servant_Manager
     (Self : in     Ref;
      Imgr : access PortableServer.ServantManager.Ref'Class);

   function Get_Servant
     (Self : in Ref)
     return Servant;

   procedure Set_Servant
     (Self      : in Ref;
      P_Servant : in Servant);

   function Activate_Object
     (Self      : in Ref;
      P_Servant : in Servant)
     return ObjectId;

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant);

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId);

   function Create_Reference
     (Self : in Ref;
      Intf : in CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id
     (Self : in Ref;
      Oid  : in ObjectId;
      Intf : in CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Servant_To_Id
     (Self      : in Ref;
      P_Servant : in Servant)
     return ObjectId;

   function Servant_To_Reference
     (Self      : in Ref;
      P_Servant : in Servant)
     return CORBA.Object.Ref;

   function Reference_To_Servant
     (Self      : in Ref;
      Reference : in CORBA.Object.Ref'Class)
     return Servant;

   function Reference_To_Id
     (Self      : in Ref;
      Reference : in CORBA.Object.Ref'Class)
     return ObjectId;

   function Id_To_Servant
     (Self : in Ref;
      Oid  : in ObjectId)
     return Servant;

   function Id_To_Reference
     (Self : in Ref;
      Oid  : in ObjectId)
     return CORBA.Object.Ref;

   package Convert is new
     PortableServer.POA_Forward.Convert (Ref);

   -------------------------------
   -- POA Exceptions Management --
   -------------------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container);

   --  AdapterAlreadyExists

   type AdapterAlreadyExists_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members);

   procedure Raise_AdapterAlreadyExists
     (Excp_Memb : in AdapterAlreadyExists_Members);
   pragma No_Return (Raise_AdapterAlreadyExists);

   --  AdapterNonExistent

   type AdapterNonExistent_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterNonExistent_Members);

   procedure Raise_AdapterNonExistent
     (Excp_Memb : in AdapterNonExistent_Members);
   pragma No_Return (Raise_AdapterNonExistent);

   --  InvalidPolicy

   type InvalidPolicy_Members is new CORBA.IDL_Exception_Members with record
      Index : CORBA.Short;
   end record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidPolicy_Members);

   procedure Raise_InvalidPolicy
     (Excp_Memb : in InvalidPolicy_Members);
   pragma No_Return (Raise_InvalidPolicy);

   --  NoServant

   type NoServant_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out NoServant_Members);

   procedure Raise_NoServant
     (Excp_Memb : in NoServant_Members);
   pragma No_Return (Raise_NoServant);

   --  ObjectAlreadyActive

   type ObjectAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectAlreadyActive_Members);

   procedure Raise_ObjectAlreadyActive
     (Excp_Memb : in ObjectAlreadyActive_Members);
   pragma No_Return (Raise_ObjectAlreadyActive);

   --  ObjectNotActive

   type ObjectNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectNotActive_Members);

   procedure Raise_ObjectNotActive
     (Excp_Memb : in ObjectNotActive_Members);
   pragma No_Return (Raise_ObjectNotActive);

   --  ServantAlreadyActive

   type ServantAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ServantAlreadyActive_Members);

   procedure Raise_ServantAlreadyActive
     (Excp_Memb : in ServantAlreadyActive_Members);
   pragma No_Return (Raise_ServantAlreadyActive);

   --  ServantNotActive

   type ServantNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ServantNotActive_Members);

   procedure Raise_ServantNotActive
     (Excp_Memb : in ServantNotActive_Members);
   pragma No_Return (Raise_ServantNotActive);

   --  WrongAdapter

   type WrongAdapter_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out WrongAdapter_Members);

   procedure Raise_WrongAdapter
     (Excp_Memb : in WrongAdapter_Members);
   pragma No_Return (Raise_WrongAdapter);

   --  WrongPolicy

   type WrongPolicy_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out WrongPolicy_Members);

   procedure Raise_WrongPolicy
     (Excp_Memb : in WrongPolicy_Members);
   pragma No_Return (Raise_WrongPolicy);

end PortableServer.POA;
