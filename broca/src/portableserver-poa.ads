------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;
--  with PortableServer.ThreadPolicy;

package PortableServer.POA is

   type Ref is new CORBA.Object.Ref with null record;

   AdapterAlreadyExists : exception;

   type AdapterAlreadyExists_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members);

   AdapterNonExistent : exception;

   type AdapterNonExistent_Members is
     new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To   : out AdapterNonExistent_Members);

   function To_Ref (Self : CORBA.Object.Ref) return Ref;

   AdapterInactive      : exception;
   ObjectAlreadyActive  : exception;
   ObjectNotActive      : exception;
   ServantAlreadyActive : exception;
   ServantNotActive     : exception;
   WrongAdapter         : exception;
   WrongPolicy          : exception;
   NoServant            : exception;

   function Create_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Tp           : ThreadPolicyValue;
      Lp           : LifespanPolicyValue;
      Up           : IdUniquenessPolicyValue;
      Ip           : IdAssignmentPolicyValue;
      Ap           : ImplicitActivationPolicyValue;
      Sp           : ServantRetentionPolicyValue;
      Rp           : RequestProcessingPolicyValue)
     return Ref'Class;

   --    function Create_POA
   --      (Self         : Ref;
   --       Adapter_Name : CORBA.String;
   --       A_POAManager : PortableServer.POAManager.Ref;
   --       Value : ThreadPolicyValue)
   --       Policies     : CORBA.Policy.PolicyList)
   --      return Ref'Class;

   function Find_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Ref'Class;

   procedure Destroy
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean);

   --  Policies

   --    function Create_Thread_Policy
   --      (Self  : Ref;
   --       Value : ThreadPolicyValue)
   --      return PortableServer.ThreadPolicy.Ref;

   function Get_The_Name
     (Self : Ref)
     return CORBA.String;

   function Get_The_Parent
     (Self : Ref)
     return Ref'Class;

   function Get_The_POAManager
     (Self : Ref)
     return PortableServer.POAManager.Ref;

   function Get_The_Activator
     (Self : Ref)
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
     (Self : Ref)
     return Servant;

   procedure Set_Servant
     (Self      : in Ref;
      P_Servant : in Servant);

   function Activate_Object
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId;

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant);

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId);

   function Create_Reference
     (Self : Ref;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Create_Reference_With_Id
     (Self : Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
     return CORBA.Object.Ref;

   function Servant_To_Id
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId;

   function Servant_To_Reference
     (Self      : Ref;
      P_Servant : Servant)
     return CORBA.Object.Ref;

   function Reference_To_Servant
     (Self      : Ref;
      Reference : CORBA.Object.Ref'Class)
     return Servant;

   function Reference_To_Id
     (Self      : Ref;
      Reference : CORBA.Object.Ref'Class)
     return ObjectId;

   function Id_To_Servant
     (Self : Ref;
      Oid  : ObjectId)
     return Servant;

   function Id_To_Reference
     (Self : Ref;
      Oid  : ObjectId)
     return CORBA.Object.Ref;

   package Convert is new PortableServer.POA_Forward.Convert (Ref);

end PortableServer.POA;
