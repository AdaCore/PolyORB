------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id: //droopi/main/src/corba/portableserver-poa.adb#28 $

with Ada.Exceptions;

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Exceptions;
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

with PolyORB.CORBA_P.POA_Config;

--  with PortableServer.ServantManager.Impl;
--  with PortableServer.ServantActivator.Impl;
--  with PortableServer.ServantLocator.Impl;

package body PortableServer.POA is

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
     (From : in CORBA.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterAlreadyExists'Identity then
         PolyORB.Exceptions.Raise_Bad_Param;
      end if;

      To := AdapterAlreadyExists_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out AdapterNonExistent_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterNonExistent'Identity then
         PolyORB.Exceptions.Raise_Bad_Param;
      end if;

      To := AdapterNonExistent_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ------------
   -- To_Ref --
   ------------

   function To_Ref (Self : CORBA.Object.Ref'Class)
                   return Ref is
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in PolyORB.POA.Obj_Adapter'Class then
         PolyORB.Exceptions.Raise_Bad_Param;
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
         PolyORB.Exceptions.Raise_Bad_Param;
      end if;

      declare
         use PolyORB.POA_Manager;

         The_POA : constant PolyORB.POA.Obj_Adapter_Access
           := PolyORB.POA.Obj_Adapter_Access (Res);
      begin
         if Is_Nil (The_POA.POA_Manager) then
            PolyORB.Exceptions.Raise_Object_Not_Exist;
         end if;

         return The_POA;
      end;
   end To_POA;

   ------------------
   -- Get_The_Name --
   ------------------

   function Get_The_Name (Self : Ref)
                         return CORBA.String is
   begin
      return CORBA.String (To_POA (Self).Name);
   end Get_The_Name;

   --------------------
   -- Get_The_Parent --
   --------------------

   function Get_The_Parent (Self : Ref)
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
--          PolyORB.Exceptions.Raise_Bad_Param;
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
      use PolyORB.CORBA_P.POA_Config;

      Res : PolyORB.POA.Obj_Adapter_Access;
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
   begin
      pragma Debug (O ("Creating POA "
                       & CORBA.To_Standard_String (Adapter_Name)));

      --  Note : Policy compability is tested by PolyORB.POA.Create_POA.

      Res := PolyORB.POA.Create_POA
        (POA,
         PolyORB.Types.String (Adapter_Name),
         PolyORB.POA_Manager.POAManager_Access
         (PortableServer.POAManager.Entity_Of (A_POAManager)),
         Convert_PolicyList (Policies));

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

      POA_Ref : constant PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Find_POA (POA,
                                 PolyORB.Types.String (Adapter_Name));

      Res : Ref
        := Create_Ref (PolyORB.Smart_Pointers.Entity_Ptr (POA_Ref));

   begin
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
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

   begin

      PolyORB.POA.Destroy
        (POA,
         PolyORB.Types.Boolean (Etherealize_Objects),
         PolyORB.Types.Boolean (Wait_For_Completion));

      --  XXX CORBA Specifications says 'Self' should be an 'in'
      --  parameter; by doin so 'Self' is still a reference to an
      --  invalid POA --> file an issue against the spec to have Ref
      --  converted to an 'in out' arg...

   end Destroy;

   -----------------
   -- Get_Servant --
   -----------------

   function Get_Servant (Self : Ref)
     return Servant
   is
--       POA : constant PolyORB.POA.Obj_Adapter_Access
--         := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--          raise WrongPolicy;
--       end if;

--       if POA.Default_Servant = null then
--          raise NoServant;
--       end if;

--       return POA.Default_Servant;
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Get_Servant (Self);
      --  "Possible infinite recursion".
      pragma Warnings (On);
   end Get_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self      : in Ref;
      P_Servant : in Servant)
   is
--       POA : constant PolyORB.POA.Obj_Adapter_Access
--         := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--          raise WrongPolicy;
--       end if;

--       POA.Default_Servant := P_Servant;
      raise PolyORB.Not_Implemented;
   end Set_Servant;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

   begin
      return ObjectId (PolyORB.POA.Activate_Object
        (POA, PolyORB.Servants.Servant_Access
         (To_PolyORB_Servant (P_Servant))));
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access
        := To_POA (Self);
      A_Oid : aliased PolyORB.POA_Types.Object_Id
        := PolyORB.POA_Types.Object_Id (Oid);

      pragma Warnings (Off);
      R_Oid : constant PolyORB.POA_Types.Object_Id
        := PolyORB.POA.Activate_Object
        (POA, PolyORB.Servants.Servant_Access
         (To_PolyORB_Servant (P_Servant)), A_Oid'Unchecked_Access);
      pragma Unreferenced (R_Oid);
      pragma Warnings (On);
   begin
      null;
   end Activate_Object_With_Id;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      A_Oid : aliased constant PolyORB.POA_Types.Object_Id
        := PolyORB.POA_Types.Object_Id (Oid);

   begin
      PolyORB.POA.Deactivate_Object (POA, A_Oid);
   end Deactivate_Object;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Self : Ref;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Oid : aliased PolyORB.POA_Types.Object_Id :=
        PolyORB.POA_Types.U_Oid_To_Oid
        (PolyORB.POA.Create_Object_Identification (POA));

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
      use PolyORB.POA_Types;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      OOid : constant Object_Id_Access := new Object_Id'(Object_Id (Oid));

      A_Oid : aliased PolyORB.POA_Types.Object_Id :=
        PolyORB.POA_Types.U_Oid_To_Oid
        (PolyORB.POA.Create_Object_Identification (POA, OOid));

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

   begin
      return ObjectId (PolyORB.POA.Servant_To_Id
                       (POA, To_PolyORB_Servant (P_Servant)));
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
      Oid : aliased PolyORB.Objects.Object_Id
        := PolyORB.POA.Export (POA, To_PolyORB_Servant (P_Servant));
      TID : constant Standard.String
        := CORBA.To_Standard_String (Get_Type_Id (P_Servant));

      P_Result : PolyORB.References.Ref;
      C_Result : CORBA.Object.Ref;
   begin
      PolyORB.ORB.Create_Reference
        (PolyORB.Setup.The_ORB, Oid'Access, TID, P_Result);
      --  Obtain object reference.

      CORBA.Object.Convert_To_CORBA_Ref (P_Result, C_Result);
      return C_Result;
   end Servant_To_Reference;

   ---------------------
   -- Reference_To_Id --
   ---------------------

   function Reference_To_Id
     (Self : Ref;
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
   begin
      PolyORB.References.Binding.Bind
        (CORBA.Object.To_PolyORB_Ref (Reference), PolyORB.Setup.The_ORB,
         The_Servant, The_Profile, Local_Only => True);

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

   begin
      return Servant (CORBA.Impl.To_CORBA_Servant
                      (PolyORB.POA.Id_To_Servant
                       (POA, PolyORB.Objects.Object_Id (Oid))));
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

end PortableServer.POA;
