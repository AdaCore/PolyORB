--  Basic POA implementation.

--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.Objects;
with Droopi.POA_Policies;
with Droopi.Requests;

package Droopi.POA.Basic_POA is

   pragma Elaborate_Body;

   type Basic_Obj_Adapter is new Droopi.POA.Obj_Adapter
     with private;
   type Basic_Obj_Adapter_Access is access all Basic_Obj_Adapter;
   --  The POA object

   --------------------------------------------------
   --  Procedures and functions required by CORBA  --
   --------------------------------------------------

   function Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        Droopi.POA_Policies.PolicyList_Access)
     return Obj_Adapter_Access;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   procedure Destroy
     (Self                : access Basic_Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean);

   function Activate_Object
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id;

   procedure Activate_Object_With_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access;
      Oid       : in     Object_Id);

   procedure Deactivate_Object
     (Self      : access Basic_Obj_Adapter;
      Oid       : in Object_Id);

   function Servant_To_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id;

   function Id_To_Servant
     (Self : access Basic_Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access;

   --------------------------------------------------------
   --  Functions and procedures to interface with Droopi --
   --------------------------------------------------------

   procedure Create
     (OA : access Basic_Obj_Adapter);

   procedure Destroy
     (OA : access Basic_Obj_Adapter);

   function Export
     (OA  : access Basic_Obj_Adapter;
      Obj :        Droopi.Objects.Servant_Access)
     return Droopi.Objects.Object_Id;

   procedure Unexport
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id);

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.Any;

   function Find_Servant
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
     return Droopi.Objects.Servant_Access;

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      :        Droopi.Objects.Object_Id;
      Servant : in out Droopi.Objects.Servant_Access);

   -------------------------------------------------
   --  Utilities, neither in CORBA nor in Droopi  --
   -------------------------------------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_Obj_Adapter;
      To   : access Basic_Obj_Adapter);

   procedure Remove_POA_By_Name
     (Self       : access Basic_Obj_Adapter;
      Child_Name :        Types.String);
   --  Remove a child POA from Self's list of children
   --  Doesn't lock the list of children

   function Find_POA_Recursively
     (Self : access Basic_Obj_Adapter;
      Name :        Types.String)
     return Basic_Obj_Adapter_Access;
   --  Starting from given POA, looks for the POA in all the descendancy whose
   --  name is Name. Returns null if not found.
   --  ??? Should be private

private

   type Basic_Obj_Adapter is new Droopi.POA.Obj_Adapter
     with null record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Obj_Adapter, Basic_Obj_Adapter_Access);

   type Check_State is (CHECK, NO_CHECK);

end Droopi.POA.Basic_POA;
