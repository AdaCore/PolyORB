--  Object adapters: entities that manage the association
--  of references with servants.

--  $Id$

with Droopi.Objects; use Droopi.Objects;
with Droopi.Refs;

package Droopi.Obj_Adapters is

   type Obj_Adapter is abstract tagged limited private;

   procedure Create (OA : out Obj_Adapter) is abstract;
   --  Initialize.

   procedure Destroy (OA : in out Obj_Adapter) is abstract;
   --  Finalize.

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

--     function Get_Method_Signature
--       (OA  : Obj_Adapter;
--        Ref : Object_Id;
--        Meth : Method_Id)
--       return Any;
   --  Return the signature of the given method, so the protocol layer
   --  can unmarshall the message into a Request object.

   function Resolve
     (OA : Obj_Adapter;
      Id : Object_Id)
     return Objects.Servant_Access is abstract;

private

   type Obj_Adapter is abstract tagged limited null record;

end Droopi.Obj_Adapters;
