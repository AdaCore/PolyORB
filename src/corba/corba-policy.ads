--  The root Policy package.

--  $Id$

with CORBA.Policy_Types;  use CORBA.Policy_Types;
with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;
with Sequences.Unbounded;

package CORBA.Policy is

   type Policy is abstract tagged
      record
         Policy_Type : PolicyType;
      end record;
   type Policy_Access is access all Policy'Class;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Access);
   subtype PolicyList is Policy_Sequences.Sequence;
   type PolicyList_Access is access all PolicyList;

   procedure Check_Compatibility (Self : Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

   function Create (Value : Policy_Value)
                   return Policy_Access is abstract;
--  The factory that has to be implemented for each family of policies

   function Create return Policy_Access is abstract;
   --  The creation function, implemented for each type of policy

   procedure Free (P   : in     Policy;
                   Ptr : in out Policy_Access)
     is abstract;

end CORBA.Policy;
