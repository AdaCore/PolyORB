package body Droopi.Obj_Adapters is

   -------------
   -- Set_ORB --
   -------------

   procedure Set_ORB
     (OA      : access Obj_Adapter;
      The_ORB :        Droopi.Components.Component_Access)
   is
   begin
      OA.ORB := The_ORB;
   end Set_ORB;

end Droopi.Obj_Adapters;
