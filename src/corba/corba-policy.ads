--  The root Policy package.

--  $Id$

with CORBA.Policy_Values; use CORBA.Policy_Values;
with Droopi.POA_Types;
with Sequences.Unbounded;
with Generic_Factory;
pragma Elaborate_All (Generic_Factory);

package CORBA.Policy is

   type Policy is abstract tagged
      record
         Policy_Type : PolicyType;
         Value       : Policy_Value;
      end record;
   type Policy_Access is access all Policy'Class;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Access);
   subtype PolicyList is Policy_Sequences.Sequence;
   type PolicyList_Access is access all PolicyList;

   package Policies_Factory_Pkg is
     new Generic_Factory (CORBA.Policy.Policy_Access,
                          CORBA.Policy_Values.Policy_Value);
   subtype Policies_Factory is Policies_Factory_Pkg.Factory_Access;

   procedure Check_Compatibility (Self : Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

   function Create return Policy_Access is abstract;
   --  The creation function, implemented for each type of policy

   procedure Free (P   : in     Policy;
                   Ptr : in out Policy_Access)
     is abstract;

end CORBA.Policy;
