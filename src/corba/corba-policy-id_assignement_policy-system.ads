with Ada.Unchecked_Deallocation;

package CORBA.Policy.Id_Assignement_Policy.System is

   type System_Id_Policy is new IdAssignementPolicy with null record;
   type System_Id_Policy_Access is access all System_Id_Policy;

   function Create return System_Id_Policy_Access;

   procedure Check_Compatibility (Self : System_Id_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

   procedure Free (P   : in     System_Id_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation (System_Id_Policy,
                                                     System_Id_Policy_Access);

end CORBA.Policy.Id_Assignement_Policy.System;
