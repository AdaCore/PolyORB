--  Base types for the various configuration axes (policies)
--  of the DROOPI Portable Object Adapter (libreally inspired from
--  the POA specification in CORBA).

--  $Id$

with Sequences.Unbounded;

with Droopi.POA_Types;
with Droopi.Static_Dict;
--  with Droopi.Types;

with CORBA.Policy_Values; use CORBA.Policy_Values;
--  XXX remove reference to CORBA.

package Droopi.POA_Policies is

   --  type PolicyType is new Droopi.Types.Unsigned_Long;

   type Policy is abstract tagged
      record
         --  Policy_Type : PolicyType;
         Value       : Policy_Value;
      end record;
   type Policy_Access is access all Policy'Class;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Access);
   subtype PolicyList is Policy_Sequences.Sequence;
   type PolicyList_Access is access all PolicyList;

   package Policies_Factory_Pkg is
      new Droopi.Static_Dict
     (Value => Droopi.POA_Policies.Policy_Access,
      Key   => CORBA.Policy_Values.Policy_Value);
   subtype Policies_Factory is Policies_Factory_Pkg.Dict_Access;

   procedure Check_Compatibility
     (Self : Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

   function Create return Policy_Access is abstract;
   --  The creation function, implemented for each type of policy

   procedure Free (P   : in     Policy;
                   Ptr : in out Policy_Access)
     is abstract;

end Droopi.POA_Policies;
