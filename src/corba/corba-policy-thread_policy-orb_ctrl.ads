package CORBA.Policy.Thread_Policy.Orb_Ctrl is

   type Orb_Ctrl_Policy is new ThreadPolicy with null record;
   type Orb_Ctrl_Policy_Access is access all Orb_Ctrl_Policy;

   function Create return Orb_Ctrl_Policy_Access;
   function Copy (P : Orb_Ctrl_Policy)
     return Orb_Ctrl_Policy_Access;

end CORBA.Policy.Thread_Policy.Orb_Ctrl;
