------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  $Id: //droopi/main/src/corba/portableserver-poa.ads#6 $

with Ada.Exceptions;

with CORBA.Object;
with CORBA.Policy;

with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;

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
     return PortableServer.AdapterActivator.Ref;

   procedure Set_The_Activator
     (Self : in Ref;
      To   : in PortableServer.AdapterActivator.Ref);

   function Get_Servant_Manager
     (Self : Ref)
     return PortableServer.ServantManager.Ref;

   procedure Set_Servant_Manager
     (Self : in Ref;
      Imgr : in PortableServer.ServantManager.Ref);

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

   --  Exception manipulation.

   type AdapterAlreadyExists_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members);

   type AdapterNonExistent_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterNonExistent_Members);

   type InvalidPolicy_Members is new CORBA.IDL_Exception_Members with record
      Index : CORBA.Short;
   end record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidPolicy_Members);

   type NoServant_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out NoServant_Members);

   type ObjectAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectAlreadyActive_Members);

   type ObjectNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectNotActive_Members);

   type ServantAlreadyActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out ServantAlreadyActive_Members);

   type ServantNotActive_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out ServantNotActive_Members);

   type WrongAdapter_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out WrongAdapter_Members);

   type WrongPolicy_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out WrongPolicy_Members);

end PortableServer.POA;
