------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id: //droopi/main/src/corba/portableserver-poa.adb#35 $

with Ada.Exceptions;

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Strings;

with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.POA_Config;

--  with PortableServer.ServantManager.Impl;
--  with PortableServer.ServantActivator.Impl;
--  with PortableServer.ServantLocator.Impl;

package body PortableServer.POA is

   use PolyORB.Exceptions;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("portableserver.poa");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Create_Ref (Referenced : PolyORB.Smart_Pointers.Entity_Ptr)
                       return Ref;
   --  Convert a PolyORB.Smart_Pointers.Entity_Ptr into a CORBA.Object.Ref.

   function To_POA (Self : Ref) return PolyORB.POA.Obj_Adapter_Access;
   --  Convert a Ref to a CORBA POA to a PolyORB POA.

   ----------------
   -- Create_Ref --
   ----------------

   function Create_Ref
     (Referenced : PolyORB.Smart_Pointers.Entity_Ptr)
     return Ref
   is
      Res : Ref;
   begin
      Set (Res, Referenced);
      return Res;
   end Create_Ref;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterAlreadyExists'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := AdapterAlreadyExists_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterNonExistent_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterNonExistent'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := AdapterNonExistent_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidPolicy_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InvalidPolicy'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out NoServant_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoServant'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoServant_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectAlreadyActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ObjectAlreadyActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ObjectAlreadyActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectNotActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ObjectNotActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ObjectNotActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ServantAlreadyActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ServantAlreadyActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ServantAlreadyActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out ServantNotActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ServantNotActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ServantNotActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out WrongAdapter_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= WrongAdapter'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := WrongAdapter_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out WrongPolicy_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= WrongPolicy'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := WrongPolicy_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref is
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      return Create_Ref (CORBA.Object.Entity_Of (Self));
   end To_Ref;

   ------------
   -- To_POA --
   ------------

   function To_POA
     (Self : Ref)
     return PolyORB.POA.Obj_Adapter_Access
   is
      use PolyORB.Smart_Pointers;

      Res : constant PolyORB.Smart_Pointers.Entity_Ptr
        := Entity_Of (Self);

   begin
      if Res = null
        or else Res.all not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         use PolyORB.POA_Manager;

         The_POA : constant PolyORB.POA.Obj_Adapter_Access
           := PolyORB.POA.Obj_Adapter_Access (Res);
      begin
         if Is_Nil (The_POA.POA_Manager) then
            CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
         end if;

         return The_POA;
      end;
   end To_POA;

   ------------------
   -- Get_The_Name --
   ------------------

   function Get_The_Name
     (Self : Ref)
     return CORBA.String is
   begin
      return CORBA.String (To_POA (Self).Name);
   end Get_The_Name;

   --------------------
   -- Get_The_Parent --
   --------------------

   function Get_The_Parent
     (Self : Ref)
     return Ref'Class is
   begin
      return Ref'
        (Create_Ref
         (PolyORB.Smart_Pointers.Entity_Ptr
          (To_POA (Self).Father)));
   end Get_The_Parent;

   ------------------------
   -- Get_The_POAManager --
   ------------------------

   function Get_The_POAManager
     (Self : Ref)
     return PortableServer.POAManager.Ref
   is
      use PolyORB.Smart_Pointers;
      use PortableServer.POAManager;

      Res : PortableServer.POAManager.Ref;

   begin
      pragma Debug (O ("Get_The_POAManager: enter"));

      Set (Res, Entity_Ptr (PolyORB.POA_Manager.Entity_Of
                            (To_POA (Self).POA_Manager)));

      pragma Debug (O ("Get_The_POAManager: leave"));
      return Res;
   end Get_The_POAManager;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : Ref)
     return PortableServer.ServantManager.Ref
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_SERVANT_MANAGER then
--          raise WrongPolicy;
--       end if;
--       return POA.Servant_Manager;

      --  return USE_SERVANT_MANAGER
      --    (POA.Request_Processing_Policy).Servant_Manager
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Servant_Manager (Self);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Get_Servant_Manager;

   -------------------------
   -- Set_Servant_Manager --
   -------------------------

   procedure Set_Servant_Manager
     (Self : in Ref;
      Imgr : in PortableServer.ServantManager.Ref)
   is
--       package PSSM renames PortableServer.ServantManager;
--       package PSSA renames PortableServer.ServantActivator;
--       package PSSL renames PortableServer.ServantLocator;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);
--       Servant_Manager : constant PSSM.Impl.Object_Ptr
--         := PSSM.Impl.Object_Ptr (PSSM.Entity_Of (Imgr));

   begin
--       if POA.Request_Processing_Policy /= USE_SERVANT_MANAGER then
--          raise WrongPolicy;
--       end if;

--       if True
--         and then not PSSM.Is_Nil (Imgr)
--         and then
--         ((POA.Servant_Policy = RETAIN
--           and then Servant_Manager.all not in PSSA.Impl.Object'Class)
--         or else
--          (POA.Servant_Policy = NON_RETAIN
--           and then Servant_Manager.all not in PSSL.Impl.Object'Class))
--       then
--          CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
--       end if;

--       POA.Servant_Manager := Imgr;
      raise PolyORB.Not_Implemented;
   end Set_Servant_Manager;

   -----------------------
   -- Get_The_Activator --
   -----------------------

   function Get_The_Activator
     (Self : Ref)
     return PortableServer.AdapterActivator.Ref is
   begin
      --  return To_POA (Self).Activator;
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_The_Activator (Self);
      --  "Possible infinite recursion".
      pragma Warnings (On);

   end Get_The_Activator;

   -----------------------
   -- Set_The_Activator --
   -----------------------

   procedure Set_The_Activator
     (Self : in Ref;
      To   : in PortableServer.AdapterActivator.Ref) is
   begin
      --  To_POA (Self).Activator := To;
      raise PolyORB.Not_Implemented;
   end Set_The_Activator;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Policies     : CORBA.Policy.PolicyList)
     return Ref'Class
   is
      Res : PolyORB.POA.Obj_Adapter_Access;
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Error : PolyORB.Exceptions.Error_Container;

   begin
      pragma Debug (O ("Creating POA "
                       & CORBA.To_Standard_String (Adapter_Name)));

      --  Note : Policy compability is tested by PolyORB.POA.Create_POA.

      PolyORB.POA.Create_POA
        (POA,
         PolyORB.Types.String (Adapter_Name),
         PolyORB.POA_Manager.POAManager_Access
         (PortableServer.POAManager.Entity_Of (A_POAManager)),
         PolyORB.CORBA_P.POA_Config.Convert_PolicyList (Policies),
         Res,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      pragma Debug (O ("POA created"));

      declare
         New_Ref : Ref'Class :=
           Create_Ref (PolyORB.Smart_Pointers.Entity_Ptr (Res));
      begin
         return New_Ref;
      end;

   end Create_POA;

   --------------
   -- Find_POA --
   --------------

   function Find_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Ref'Class
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);

      POA_Ref : PolyORB.POA.Obj_Adapter_Access;

      Res : Ref;

   begin
      POA_Ref := PolyORB.POA.Find_POA
        (POA,
         CORBA.To_Standard_String (Adapter_Name));

      Res := Create_Ref (PolyORB.Smart_Pointers.Entity_Ptr (POA_Ref));

      if Is_Nil (Res) then
         raise AdapterNonExistent;
      end if;

      if Activate_It then
         raise PolyORB.Not_Implemented;
         --  XXX require servant activator.
      end if;

      return Res;
   end Find_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : in out Ref;
      Etherealize_Objects : in     CORBA.Boolean;
      Wait_For_Completion : in     CORBA.Boolean)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

   begin

      PolyORB.POA.Destroy
        (POA,
         PolyORB.Types.Boolean (Etherealize_Objects),
         PolyORB.Types.Boolean (Wait_For_Completion));

      --  XXX CORBA Specifications says 'Self' should be an 'in'
      --  parameter; by doing so 'Self' is still a reference to an
      --  invalid POA --> file an issue against the spec to have Ref
      --  converted to an 'in out' arg...

   end Destroy;

   -----------------
   -- Get_Servant --
   -----------------

   function Get_Servant
     (Self : Ref)
     return Servant
   is
      use type PolyORB.Servants.Servant_Access;

      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);

   begin
--      if Self.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--         raise WrongPolicy;
--      end if;

      if POA.Default_Servant = null then
         raise NoServant;
      end if;

      return Servant (CORBA.Impl.To_CORBA_Servant (POA.Default_Servant));
   end Get_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self      : in Ref;
      P_Servant : in Servant)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);

   begin
--     if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--         raise WrongPolicy;
--      end if;

      POA.Default_Servant := PolyORB.Servants.Servant_Access
        (To_PolyORB_Servant (P_Servant));
   end Set_Servant;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId
   is
      Error : PolyORB.Exceptions.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
   begin
      PolyORB.POA.Activate_Object
        (POA,
         PolyORB.Servants.Servant_Access (To_PolyORB_Servant (P_Servant)),
         null,
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         Oid : constant PolyORB.POA_Types.Object_Id
           := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
      begin
         return ObjectId (Oid);
      end;
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant)
   is
      Error : PolyORB.Exceptions.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      A_Oid : aliased PolyORB.POA_Types.Object_Id
        := PolyORB.POA_Types.Object_Id (Oid);

   begin
      PolyORB.POA.Activate_Object
        (POA,
         PolyORB.Servants.Servant_Access (To_PolyORB_Servant (P_Servant)),
         A_Oid'Unchecked_Access,
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Activate_Object_With_Id;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId)
   is
      Error : PolyORB.Exceptions.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      A_Oid : aliased constant PolyORB.POA_Types.Object_Id
        := PolyORB.POA_Types.Object_Id (Oid);

   begin
      PolyORB.POA.Deactivate_Object (POA, A_Oid, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Deactivate_Object;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Self : Ref;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      use PolyORB.Exceptions;

      Error : PolyORB.Exceptions.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
   begin
      PolyORB.POA.Create_Object_Identification (POA, null, U_Oid, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         Oid : aliased PolyORB.POA_Types.Object_Id
           := PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

         P_Result : PolyORB.References.Ref;
         C_Result : CORBA.Object.Ref;
      begin
         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            Oid'Access,
            CORBA.To_Standard_String (Intf),
            P_Result);
         --  Obtain object reference.

         CORBA.Object.Convert_To_CORBA_Ref (P_Result, C_Result);
         return C_Result;
      end;
   end Create_Reference;

   ------------------------------
   -- Create_Reference_With_Id --
   ------------------------------

   function Create_Reference_With_Id
     (Self : Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      use PolyORB.Exceptions;
      use PolyORB.POA_Types;

      Error : PolyORB.Exceptions.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      OOid : Object_Id_Access := new Object_Id'(Object_Id (Oid));

   begin
      PolyORB.POA.Create_Object_Identification (POA, OOid, U_Oid, Error);
      Free (OOid);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         A_Oid : aliased PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

         P_Result : PolyORB.References.Ref;
         C_Result : CORBA.Object.Ref;
      begin
         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            A_Oid'Access,
            CORBA.To_Standard_String (Intf),
            P_Result);
         --  Obtain object reference.

         CORBA.Object.Convert_To_CORBA_Ref (P_Result, C_Result);
         return C_Result;
      end;
   end Create_Reference_With_Id;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Oid   : PolyORB.POA_Types.Object_Id_Access;
      Error : PolyORB.Exceptions.Error_Container;

   begin
      PolyORB.POA.Servant_To_Id
        (POA,
         To_PolyORB_Servant (P_Servant),
         Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      return ObjectId (Oid.all);
   end Servant_To_Id;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   function Servant_To_Reference
     (Self : Ref;
      P_Servant : Servant)
     return CORBA.Object.Ref
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
      Oid : PolyORB.Objects.Object_Id_Access;

      TID : constant Standard.String
        := CORBA.To_Standard_String (Get_Type_Id (P_Servant));

      P_Result : PolyORB.References.Ref;
      C_Result : CORBA.Object.Ref;

      Error : PolyORB.Exceptions.Error_Container;
   begin
      PolyORB.POA.Export
        (POA,
         To_PolyORB_Servant (P_Servant),
         null,
         Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.ORB.Create_Reference
        (PolyORB.Setup.The_ORB, Oid, TID, P_Result);
      --  Obtain object reference.

      PolyORB.POA_Types.Free (Oid);

      CORBA.Object.Convert_To_CORBA_Ref (P_Result, C_Result);
      return C_Result;
   end Servant_To_Reference;

   ---------------------
   -- Reference_To_Id --
   ---------------------

   function Reference_To_Id
     (Self      : Ref;
      Reference : CORBA.Object.Ref'Class)
     return ObjectId is
   begin
      --  XXX does someone know a better implementation ?

      return Servant_To_Id
        (Self, Reference_To_Servant (Self, Reference));
   end Reference_To_Id;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   function Reference_To_Servant
     (Self      : Ref;
      Reference : CORBA.Object.Ref'Class)
     return Servant
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      The_Servant : PolyORB.Components.Component_Access;
      The_Profile : PolyORB.Binding_Data.Profile_Access;

      Error : Error_Container;

   begin
      PolyORB.References.Binding.Bind
        (CORBA.Object.To_PolyORB_Ref (Reference),
         PolyORB.Setup.The_ORB,
         The_Servant,
         The_Profile,
         True,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Using 'Local_Only' should guarantee that The_Servant
      --  is castable to PolyORB.Servants.Servant_Access.

      return Servant (CORBA.Impl.To_CORBA_Servant
                      (PolyORB.Servants.Servant_Access (The_Servant)));
   end Reference_To_Servant;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : Ref;
      Oid  : ObjectId)
     return Servant
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Error : PolyORB.Exceptions.Error_Container;
      S     : PolyORB.Servants.Servant_Access;

   begin
      PolyORB.POA.Id_To_Servant
        (POA,
         PolyORB.Objects.Object_Id (Oid),
         S,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      return Servant (CORBA.Impl.To_CORBA_Servant (S));
   end Id_To_Servant;

   ---------------------
   -- Id_To_Reference --
   ---------------------

   function Id_To_Reference
     (Self : Ref;
      Oid : ObjectId)
     return CORBA.Object.Ref is
   begin
      return Servant_To_Reference
        (Self, Id_To_Servant (Self, Oid));
   end Id_To_Reference;

   --------------------------------
   -- Raise_AdapterAlreadyExists --
   --------------------------------

   procedure Raise_AdapterAlreadyExists
     (Excp_Memb : in AdapterAlreadyExists_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise AdapterAlreadyExists;
   end Raise_AdapterAlreadyExists;

   ------------------------------
   -- Raise_AdapterNonExistent --
   ------------------------------

   procedure Raise_AdapterNonExistent
     (Excp_Memb : in AdapterNonExistent_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise AdapterNonExistent;
   end Raise_AdapterNonExistent;

   -------------------------
   -- Raise_InvalidPolicy --
   -------------------------

   procedure Raise_InvalidPolicy
     (Excp_Memb : in InvalidPolicy_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise InvalidPolicy;
      --  XXX FIXME: need to marshall some data
   end Raise_InvalidPolicy;

   ---------------------
   -- Raise_NoServant --
   ---------------------

   procedure Raise_NoServant
     (Excp_Memb : in NoServant_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise NoServant;
   end Raise_NoServant;

   -------------------------------
   -- Raise_ObjectAlreadyActive --
   -------------------------------

   procedure Raise_ObjectAlreadyActive
     (Excp_Memb : in ObjectAlreadyActive_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise ObjectAlreadyActive;
   end Raise_ObjectAlreadyActive;

   ---------------------------
   -- Raise_ObjectNotActive --
   ---------------------------

   procedure Raise_ObjectNotActive
     (Excp_Memb : in ObjectNotActive_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise ObjectNotActive;
   end Raise_ObjectNotActive;

   --------------------------------
   -- Raise_ServantAlreadyActive --
   --------------------------------

   procedure Raise_ServantAlreadyActive
     (Excp_Memb : in ServantAlreadyActive_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise ServantAlreadyActive;
   end Raise_ServantAlreadyActive;

   ----------------------------
   -- Raise_ServantNotActive --
   ----------------------------

   procedure Raise_ServantNotActive
     (Excp_Memb : in ServantNotActive_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise ServantNotActive;
   end Raise_ServantNotActive;

   ------------------------
   -- Raise_WrongAdapter --
   ------------------------

   procedure Raise_WrongAdapter
     (Excp_Memb : in WrongAdapter_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise WrongAdapter;
   end Raise_WrongAdapter;

   -----------------------
   -- Raise_WrongPolicy --
   -----------------------

   procedure Raise_WrongPolicy
     (Excp_Memb : in WrongPolicy_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise WrongPolicy;
   end Raise_WrongPolicy;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Assert (Is_Error (Error));

      --  One to one mapping of PolyORB Error_Id to CORBA POA exceptions.

      case Error.Kind is
         when AdapterAlreadyExists_E =>
            declare
               Member : constant AdapterAlreadyExists_Members
                 := AdapterAlreadyExists_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_AdapterAlreadyExists (Member);
            end;

         when AdapterNonExistent_E =>
            declare
               Member : constant AdapterNonExistent_Members
                 := AdapterNonExistent_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_AdapterNonExistent (Member);
            end;

         when InvalidPolicy_E =>
            declare
               Member : constant InvalidPolicy_Members
                 := InvalidPolicy_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_InvalidPolicy (Member);
            end;

         when NoServant_E =>
            declare
               Member : constant NoServant_Members
                 := NoServant_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_NoServant (Member);
            end;

         when ObjectAlreadyActive_E =>
            declare
               Member : constant ObjectAlreadyActive_Members
                 := ObjectAlreadyActive_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_ObjectAlreadyActive (Member);
            end;

         when ObjectNotActive_E =>
            declare
               Member : constant ObjectNotActive_Members
                 := ObjectNotActive_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_ObjectNotActive (Member);
            end;

         when ServantAlreadyActive_E =>
            declare
               Member : constant ServantAlreadyActive_Members
                 := ServantAlreadyActive_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_ServantAlreadyActive (Member);
            end;

         when ServantNotActive_E =>
            declare
               Member : constant ServantNotActive_Members
                 := ServantNotActive_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_ServantNotActive (Member);
            end;

         when WrongAdapter_E =>
            declare
               Member : constant WrongAdapter_Members
                 := WrongAdapter_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_WrongAdapter (Member);
            end;

         when WrongPolicy_E =>
            declare
               Member : constant WrongPolicy_Members
                 := WrongPolicy_Members (Error.Member.all);
            begin
               Free (Error.Member);
               Raise_WrongPolicy (Member);
            end;

         when others =>
            raise Program_Error;
      end case;
   end Raise_From_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Exceptions.POA_Raise_From_Error
        := Raise_From_Error'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"portableserver.poa",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));

end PortableServer.POA;
