with Ada.Unchecked_Deallocation;

package CORBA.Policy.Thread_Policy.Orb_Ctrl is

   type Orb_Ctrl_Policy is new ThreadPolicy with null record;
   type Orb_Ctrl_Policy_Access is access all Orb_Ctrl_Policy;

   function Create return Orb_Ctrl_Policy_Access;
   procedure Check_Compatibility (Self : Orb_Ctrl_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

--    procedure Free (P   : Orb_Ctrl_Policy;
--                    Ptr : Orb_Ctrl_Policy_Access);

   procedure Free (P   : in     Orb_Ctrl_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation (Orb_Ctrl_Policy,
                                                     Orb_Ctrl_Policy_Access);

end CORBA.Policy.Thread_Policy.Orb_Ctrl;
