with Ada.Unchecked_Deallocation;

package Droopi.POA_Policies.Thread_Policy.ORB_Ctrl is

   type ORB_Ctrl_Policy is new ThreadPolicy with null record;
   type ORB_Ctrl_Policy_Access is access all ORB_Ctrl_Policy;

   function Create return ORB_Ctrl_Policy_Access;

   procedure Check_Compatibility
     (Self : ORB_Ctrl_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access);

   function Policy_Id
     (Self : ORB_Ctrl_Policy)
     return String;

   procedure Free
     (P   : in     ORB_Ctrl_Policy;
      Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ORB_Ctrl_Policy,
      ORB_Ctrl_Policy_Access);

end Droopi.POA_Policies.Thread_Policy.ORB_Ctrl;
