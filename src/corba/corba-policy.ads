--  The root Policy package.

--  $Id$

with CORBA.Policy_Types; use CORBA.Policy_Types;
--  with CORBA.POA_Types;
with CORBA.POA_Types;
with Sequences.Unbounded;

package CORBA.Policy is

   type Policy;
   type Policy_Access is access all Policy'Class;
   type Policy is abstract tagged
      record
         Policy_Type : Policy_Types.PolicyType;
      end record;

   package Policy_Sequences is new Sequences.Unbounded (Policy_Access);
   subtype PolicyList is Policy_Sequences.Sequence;
   type PolicyList_Access is access all PolicyList;

   procedure Check_Compatibility (Self        : Policy_Access;
                                  Obj_Adapter : CORBA.POA_Types.Obj_Adapter)
      is abstract;
   --  Check the compatibility of the current policy with the
   --  other policies of the object adapter.

end CORBA.Policy;
