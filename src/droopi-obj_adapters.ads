--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with CORBA;
with CORBA.NVList;

with Droopi.Objects; use Droopi.Objects;
with Droopi.Requests;

package Droopi.Obj_Adapters is

   type Obj_Adapter is abstract tagged limited private;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;

   procedure Create (OA : out Obj_Adapter) is abstract;
   --  Initialize.

   procedure Destroy (OA : in out Obj_Adapter) is abstract;
   --  Finalize.

   Invalid_Oid : exception;
   --  An invalid object identifier was passed to an object
   --  adapter subprogram.

   --------------------------------------
   -- Interface to application objects --
   --------------------------------------

   procedure Export
     (OA  : in out Obj_Adapter;
      Obj : Objects.Servant_Access;
      Id  : out Object_Id) is abstract;
   --  Create an identifier for Obj within OA.

   procedure Unexport
     (OA : in out Obj_Adapter;
      Id : Object_Id) is abstract;
   --  Id is an object identifier attributed by OA.
   --  The corresponding association is suppressed.

   ----------------------------------------------------
   -- Interface to ORB (acting on behalf of clients) --
   ----------------------------------------------------

   function Get_Empty_Arg_List
     (OA     : Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id)
     return CORBA.NVList.Ref is abstract;
   --  Return the paramter profile of the given method, so the
   --  protocol layer can unmarshall the message into a Request object.

   function Get_Empty_Result
     (OA     : Obj_Adapter;
      Oid    : Object_Id;
      Method : Requests.Operation_Id)
     return CORBA.Any is abstract;
   --  Return the result profile of the given method.

   function Find_Servant
     (OA : Obj_Adapter;
      Id : Object_Id)
     return Objects.Servant_Access is abstract;
   --  Retrieve the servant managed by OA for logical object Id.
   --  The servant that incarnates the object is return.

   procedure Release_Servant
     (OA : Obj_Adapter;
      Id : Object_Id;
      Servant : in out Servant_Access) is abstract;
   --  Signal to OA that a Servant previously obtained using
   --  Find_Servant won't be used by the client anymore. This
   --  may cause the servant to be destroyed if so is OA's
   --  policy.

private

   type Obj_Adapter is abstract tagged limited null record;

end Droopi.Obj_Adapters;
