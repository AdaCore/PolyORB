with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;     use CORBA.POA_Types;

package CORBA.Policy.Servant_Retention_Policy is

   type ServantRetentionPolicy is abstract new Policy with
     record
         Value : ServantRetentionPolicyValue;
     end record;
   subtype Servant_Retention_Policy is ServantRetentionPolicy;
   type ServantRetentionPolicy_Access is
     access all ServantRetentionPolicy'Class;
   subtype Servant_Retention_Policy_Access is ServantRetentionPolicy_Access;

   function Create (Value : ServantRetentionPolicyValue)
                   return ServantRetentionPolicy_Access;
   function Create (P : ServantRetentionPolicy)
     return ServantRetentionPolicy_Access;
   --  The factory to create the different policies according to
   --  the value of Value

   function Create return ServantRetentionPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Free (P   : in     ServantRetentionPolicy;
                   Ptr : in out Policy_Access)
     is abstract;

--    function Activate_Object
--      (Self             : ServantRetentionPolicy_Access;
--       OA               : CORBA.POA_Types.Obj_Adapter;
--       P_Servant        : in Servant)
--      return Object_Id is abstract;
--    --  Case RETAIN:
--    --  Activates the object (servant) in the Active Objects Map
--    --  The Id_Uniqueness_Policy checks that the servant is not yet active
--    --  The Id_Assign_Policy generates an Object_Id
--    --  Case NON_RETAIN:
--    --  Raises a WrongPolicy exception

--    procedure Activate_Object_With_Id
--      (Self             : ServantRetentionPolicy_Access;
--       OA               : CORBA.POA_Types.Obj_Adapter;
--       Oid              : Object_Id;
--       P_Servant        : in Servant)
--       is abstract;
--    --  Case RETAIN:
--    --  Register the servant in the active objects map
--    --  Checks that the object_id is not yet used
--    --  The Id_Uniqueness_Policy checks that the servant is not yet
--    --  in the active objects map

--    procedure Deactivate_Object
--      (Self             : ServantRetentionPolicy_Access;
--       OA               : CORBA.POA_Types.Obj_Adapter;
--       Oid              : Object_Id)
--       is abstract;
--    --  Case RETAIN:
--    --  Removes the object from the map.
--    --  In case a servant manager is in use, calls ServantActivator.etheralize
--    --  Case NON_RETAIN:
--    --  Raises a WrongPolicy exception

--    function Servant_To_Id
--      (Self             : ServantRetentionPolicy_Access;
--       OA               : CORBA.POA_Types.Obj_Adapter;
--       P_Servant        : Servant)
--      return Object_Id is abstract;
--    --  Case RETAIN:
--    --  First checks that the UNIQUE_ID policy is present.
--    --  Then looks in the objects map for the given servant.
--    --  If not found, calls the implicit activation policy
--    --  Finally, returns a valid Id or Nil
--    --  Case NON_RETAIN:
--    --  returns Nil

--    function Id_To_Servant
--      (Self             : ServantRetentionPolicy_Access;
--       OA               : CORBA.POA_Types.Obj_Adapter;
--       Oid              : Object_Id)
--      return Servant is abstract;
--    --  Case RETAIN:
--    --  Look for the Id in the map ; if not found, returns Nil
--    --  (or raises an exception???)
--    --  Case NON_RETAIN:
--    --  Returns Nil

end CORBA.Policy.Servant_Retention_Policy;
