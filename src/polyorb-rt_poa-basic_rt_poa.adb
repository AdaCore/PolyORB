------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R T _ P O A . B A S I C _ R T _ P O A           --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

with PolyORB.Log;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Policies.Implicit_Activation_Policy;
with PolyORB.Smart_Pointers;

package body PolyORB.RT_POA.Basic_RT_POA is

   use PolyORB.Log;
   use PolyORB.POA;

   package L is new Log.Facility_Log ("polyorb.rt_poa.basic_rt_poa");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Set_Policies
     (OA       : access Basic_RT_Obj_Adapter;
      Policies : POA_Policies.PolicyList);
   --  Set OA policies from the values in Policies.

   function To_Non_RT_POA
     (Self : access Basic_RT_Obj_Adapter)
     return PolyORB.POA.Obj_Adapter_Access;
   pragma Inline (To_Non_RT_POA);
   --  Return Non_RT_POA attached to Self

   -------------------
   -- To_Non_RT_POA --
   -------------------

   function To_Non_RT_POA
     (Self : access Basic_RT_Obj_Adapter)
     return PolyORB.POA.Obj_Adapter_Access
   is
   begin
      return PolyORB.POA.Obj_Adapter_Access (Entity_Of (Self.Non_RT_POA));
   end To_Non_RT_POA;

   ------------------
   -- Set_Policies --
   ------------------

   procedure Set_Policies
     (OA       : access Basic_RT_Obj_Adapter;
      Policies :        POA_Policies.PolicyList)
   is
      use PolyORB.POA_Policies;
      use Policy_Sequences;

      Policies_Array : constant Element_Array := To_Element_Array (Policies);
      A_Policy : Policy_Access;

   begin
      for J in Policies_Array'Range loop
         A_Policy := Policies_Array (J);

         if A_Policy.all in PriorityModelPolicy'Class then
            if OA.Priority_Model_Policy /= null then
               pragma Debug
                 (O ("Duplicate in PriorityModelPolicy: using last one"));
               null;
            end if;
            OA.Priority_Model_Policy := PriorityModelPolicy_Access (A_Policy);
            pragma Debug (O ("Setting up PriorityModelPolicy"));
         end if;

         if A_Policy.all in ThreadPoolPolicy'Class then
            if OA.Thread_Pool_Policy /= null then
               pragma Debug
                 (O ("Duplicate in ThreadPoolPolicy: using last one"));
               null;
            end if;
            OA.Thread_Pool_Policy := ThreadPoolPolicy_Access (A_Policy);
            pragma Debug (O ("Setting up ThreadPoolPolicy"));
         end if;
      end loop;
   end Set_Policies;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   ----------------
   -- Create_POA --
   ----------------

   procedure Create_POA
     (Self         : access Basic_RT_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out PolyORB.POA.Obj_Adapter_Access;
      Error        : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

      Non_RT_POA : PolyORB.POA.Obj_Adapter_Access;

   begin
      Create_POA (To_Non_RT_POA (Self),
                  Adapter_Name,
                  A_POAManager,
                  Policies,
                  Non_RT_POA,
                  Error);

      if Found (Error) then
         return;
      end if;

      POA := new Basic_RT_Obj_Adapter;
      Set_Policies (Basic_RT_Obj_Adapter (POA.all)'Access, Policies);

      Set (Basic_RT_Obj_Adapter (POA.all).Non_RT_POA,
           Smart_Pointers.Entity_Ptr (Non_RT_POA));
      POA.POA_Manager
        := To_Non_RT_POA (Basic_RT_Obj_Adapter (POA.all)'Access).POA_Manager;
   end Create_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : access Basic_RT_Obj_Adapter;
      Etherealize_Objects : in     Types.Boolean;
      Wait_For_Completion : in     Types.Boolean)
   is
   begin
      Destroy (To_Non_RT_POA (Self), Etherealize_Objects, Wait_For_Completion);
   end Destroy;

   ----------------------------------
   -- Create_Object_Identification --
   ----------------------------------

   procedure Create_Object_Identification
     (Self  : access Basic_RT_Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Create_Object_Identification (To_Non_RT_POA (Self), Hint, U_Oid, Error);
   end Create_Object_Identification;

   ---------------------
   -- Activate_Object --
   ---------------------

   procedure Activate_Object
     (Self      : access Basic_RT_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Activate_Object (To_Non_RT_POA (Self), P_Servant, Hint, U_Oid, Error);
   end Activate_Object;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self  : access Basic_RT_Obj_Adapter;
      Oid   : in     Object_Id;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Deactivate_Object (To_Non_RT_POA (Self), Oid, Error);
   end Deactivate_Object;

   -------------------
   -- Servant_To_Id --
   -------------------

   procedure Servant_To_Id
     (Self      : access Basic_RT_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Servant_To_Id (To_Non_RT_POA (Self), P_Servant, Oid, Error);
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   procedure Id_To_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Id_To_Servant (To_Non_RT_POA (Self), Oid, Servant, Error);
   end Id_To_Servant;

   --------------
   -- Find_POA --
   --------------

   procedure Find_POA
     (Self        : access Basic_RT_Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out PolyORB.POA.Obj_Adapter_Access;
      Error       : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Find_POA (To_Non_RT_POA (Self), Name, Activate_It, POA, Error);
   end Find_POA;

   -----------------
   -- Get_Servant --
   -----------------

   procedure Get_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Get_Servant (To_Non_RT_POA (Self), Servant, Error);
   end Get_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Set_Servant (To_Non_RT_POA (Self), Servant, Error);
   end Set_Servant;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   procedure Get_Servant_Manager
     (Self    : access Basic_RT_Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Get_Servant_Manager (To_Non_RT_POA (Self), Manager, Error);
   end Get_Servant_Manager;

   -------------------------
   -- Set_Servant_Manager --
   -------------------------

   procedure Set_Servant_Manager
     (Self    : access Basic_RT_Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Set_Servant_Manager (To_Non_RT_POA (Self), Manager, Error);
   end Set_Servant_Manager;

   ----------------------
   -- Copy_Obj_Adapter --
   ----------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_RT_Obj_Adapter;
      To   : access Basic_RT_Obj_Adapter)
   is
      use PolyORB.POA.Basic_POA;

   begin
      PolyORB.POA.Basic_POA.Copy_Obj_Adapter
        (Basic_Obj_Adapter (Entity_Of (From.Non_RT_POA).all),
         Basic_Obj_Adapter (To_Non_RT_POA (To).all)'Access);
   end Copy_Obj_Adapter;

   ------------------------
   -- Remove_POA_By_Name --
   ------------------------

   procedure Remove_POA_By_Name
     (Self       : access Basic_RT_Obj_Adapter;
      Child_Name :        Types.String)
   is
   begin
      Remove_POA_By_Name (To_Non_RT_POA (Self), Child_Name);
   end Remove_POA_By_Name;

   --------------------------------------------------
   -- PolyORB Obj_Adapter interface implementation --
   --------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (OA : access Basic_RT_Obj_Adapter) is
      Non_RT_POA : constant PolyORB.POA_Types.Obj_Adapter_Access
        := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

   begin
      Create (Non_RT_POA);
      Set (OA.Non_RT_POA, Smart_Pointers.Entity_Ptr (Non_RT_POA));

      OA.POA_Manager := To_Non_RT_POA (OA).POA_Manager;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : access Basic_RT_Obj_Adapter) is
   begin
      Destroy (To_Non_RT_POA (OA));
   end Destroy;

   ------------
   -- Export --
   ------------

   procedure Export
     (OA    : access Basic_RT_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

   begin
      pragma Debug (O ("Export: enter"));

      --  Export servant

      Export (To_Non_RT_POA (OA), Obj, Key, Oid, Error);

      if Found (Error) then
         return;
      end if;

      --  XXX Caching must be propagated to all procedures that store
      --  a servant in the POA, to be investigated !!

      if OA.Priority_Model_Policy /= null then
         --  Cache information on Priority_Model_Policy in servant

         Set_Servant_Priority_Information (OA.Priority_Model_Policy.all, Obj);

      end if;

      if OA.Thread_Pool_Policy /= null then
         --  Cache information on Thread_Pool_Policy in servant

         Set_Servant_Lane (OA.Thread_Pool_Policy.all, Obj);

      end if;

      pragma Debug (O ("Export: leave"));
   end Export;

   -------------------------------
   -- Get_Scheduling_Parameters --
   -------------------------------

   procedure Get_Scheduling_Parameters
     (Self                     : access Basic_RT_Obj_Adapter;
      Id                       : in     Object_Id_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

      Servant : Servants.Servant_Access;

   begin
      Find_Servant (Self, Id, Servant, Error);

      if Found (Error) then
         return;
      end if;

      Get_Servant_Priority_Information
        (Servant,
         Model,
         Server_ORB_Priority,
         Server_External_Priority,
         Error);
   end Get_Scheduling_Parameters;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA    : access Basic_RT_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Unexport (To_Non_RT_POA (OA), Id, Error);
   end Unexport;

   ----------------
   -- Object_Key --
   ----------------

   procedure Object_Key
     (OA      : access Basic_RT_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Object_Key (To_Non_RT_POA (OA), Id, User_Id, Error);
   end Object_Key;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : access Basic_RT_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref
   is
   begin
      return Get_Empty_Arg_List (To_Non_RT_POA (OA), Oid, Method);
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (OA     : access Basic_RT_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any
   is
   begin
      return Get_Empty_Result (To_Non_RT_POA (OA), Oid, Method);
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   procedure Find_Servant
     (OA      : access Basic_RT_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      Find_Servant (To_Non_RT_POA (OA), Id, Servant, Error);
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Basic_RT_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access)
   is
   begin
      Release_Servant (To_Non_RT_POA (OA), Id, Servant);
   end Release_Servant;

   ------------------
   -- Is_Proxy_Oid --
   ------------------

   function Is_Proxy_Oid
     (OA  : access Basic_RT_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean
   is
   begin
      return Is_Proxy_Oid (To_Non_RT_POA (OA), Oid);
   end Is_Proxy_Oid;

   ------------------
   -- To_Proxy_Oid --
   ------------------

   procedure To_Proxy_Oid
     (OA    : access Basic_RT_Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      To_Proxy_Oid (To_Non_RT_POA (OA), R, Oid, Error);
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   function Proxy_To_Ref
     (OA  : access Basic_RT_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref
   is
   begin
      return Proxy_To_Ref (To_Non_RT_POA (OA), Oid);
   end Proxy_To_Ref;

   ------------------------------------------------
   -- CORBA-like RT POA interface implementation --
   ------------------------------------------------

   ------------------------------------------------
   -- Create_Object_Identification_With_Priority --
   ------------------------------------------------

   procedure Create_Object_Identification_With_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : in     ORB_Priority;
      Server_External_Priority : in     External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self, Hint, Server_ORB_Priority);
      pragma Unreferenced (Server_External_Priority, U_Oid, Error);
      pragma Warnings (On); --  WAG:3.15

   begin
      raise Not_Implemented;

   end Create_Object_Identification_With_Priority;

   ------------------------------------------
   -- Activate_Object_With_Id_And_Priority --
   ------------------------------------------

   procedure Activate_Object_With_Id_And_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      P_Servant                :        Servants.Servant_Access;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : in     ORB_Priority;
      Server_External_Priority : in     External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;
      use PolyORB.POA_Policies.Implicit_Activation_Policy;

   begin
      --  Check Self's policies are correct

      Ensure_No_Implicit_Activation
        (To_Non_RT_POA (Self).Implicit_Activation_Policy.all,
         Error);

      if Found (Error) then
         return;
      end if;

      if Self.Priority_Model_Policy = null then
         Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
         return;
      end if;

      --  Cache information on Priority_Model_Policy

      Set_Servant_Priority_Information
        (Self.Priority_Model_Policy.all,
         P_Servant,
         Server_ORB_Priority,
         Server_External_Priority,
         Error);

      if Found (Error) then
         return;
      end if;

      --  Activate object

      Activate_Object (Self, P_Servant, Hint, U_Oid, Error);
   end Activate_Object_With_Id_And_Priority;

end PolyORB.RT_POA.Basic_RT_POA;
